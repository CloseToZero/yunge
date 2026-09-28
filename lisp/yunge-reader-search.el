;;; yunge-reader-search.el --- Document search -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-jump-history)
(require 'yunge-reader-model)
(require 'yunge-reader-task)

(declare-function yunge-reader--current-position "yunge-reader" (&optional window))
(declare-function yunge-reader-request "yunge-reader"
                  (operation arguments complete &rest options))
(defvar yunge-reader-document)

(defcustom yunge-reader-search-match-limit 64
  "Maximum search matches requested from one reader batch."
  :type '(integer :tag "Matches" 1 200)
  :group 'yunge-reader)

(defcustom yunge-reader-search-unit-limit 8
  "Maximum document units scanned by one reader search batch."
  :type '(integer :tag "Units" 1 64)
  :group 'yunge-reader)

(defvar-local yunge-reader-search-query nil
  "Literal query active in the current reader buffer, or nil.")

(defvar yunge-reader-search-history nil
  "Minibuffer history for document search queries.")

(defvar-local yunge-reader-search-results nil
  "Search results loaded in the active run's navigation order.")

(defvar-local yunge-reader-search-result nil
  "Current `yunge-reader-search-result', or nil.")

(defvar-local yunge-reader-search-highlight-visible nil
  "Whether the active search may display its current result highlight.")

(defvar-local yunge-reader-search-result-hook nil
  "Hook run after the search result or its visibility changes.")

(defvar-local yunge-reader-search--case-sensitive nil
  "Whether the active reader search distinguishes case.")

(defvar-local yunge-reader-search--index nil
  "Zero-based index of the current result in loaded search results.")

(defvar-local yunge-reader-search--cursor nil
  "Opaque `yunge-reader-search-cursor' for the next search batch.")

(defvar-local yunge-reader-search--direction nil
  "Direction of the active search run: `forward' or `backward'.")

(defvar-local yunge-reader-search--origin nil
  "Stable Reader position from which the active search run began.")

(defvar-local yunge-reader-search--wrapped nil
  "Non-nil when the active run restarted at a document boundary.")

(defvar-local yunge-reader-search--detached nil
  "Non-nil after manual reading movement detached search navigation.")

(defvar-local yunge-reader-search--segment-done nil
  "Non-nil after the active search segment reaches its boundary.")

(defvar-local yunge-reader-search--complete nil
  "Non-nil when results cover one complete traversal of the document.")

(defvar-local yunge-reader-search--cycle-seen nil
  "Equal hash table of result endpoints seen before or during a wrap.")

(defvar-local yunge-reader-search--pending nil
  "Non-nil while one search batch is outstanding.")

(defvar-local yunge-reader-search--task nil
  "Cancellable task serving the active search batch.")

(defvar-local yunge-reader-search--in-flight 0
  "Number of physical search requests not yet completed.")

(defvar-local yunge-reader-search--navigation-intent nil
  "Pending search navigation direction: `forward' or `backward'.")

(defvar-local yunge-reader-search--navigation-count 0
  "Number of pending moves in `yunge-reader-search--navigation-intent'.")

(defvar-local yunge-reader-search--generation 0
  "Generation used to reject late reader search completions.")

(defun yunge-reader-search--smart-case-p (query)
  "Return non-nil when QUERY contains an uppercase character."
  (not (equal query (downcase query))))

(defun yunge-reader-search--result-valid-p (result)
  "Return non-nil when RESULT follows the generic search contract."
  (and (yunge-reader-search-result-p result)
       (yunge-reader-position-p
        (yunge-reader-search-result-start result))
       (yunge-reader-position-p
        (yunge-reader-search-result-end result))
       (cl-every
        (lambda (value) (or (null value) (stringp value)))
        (list (yunge-reader-search-result-text result)
              (yunge-reader-search-result-before result)
              (yunge-reader-search-result-after result)))))

(defun yunge-reader-search--batch-valid-p (batch)
  "Return non-nil when BATCH follows the generic search contract."
  (and (yunge-reader-search-batch-p batch)
       (proper-list-p (yunge-reader-search-batch-results batch))
       (cl-every #'yunge-reader-search--result-valid-p
                 (yunge-reader-search-batch-results batch))
       (let ((cursor (yunge-reader-search-batch-cursor batch))
             (done (yunge-reader-search-batch-done batch)))
         (and (memq done '(nil t))
              (if done
                  (null cursor)
                (yunge-reader-search-cursor-p cursor))))))

(defun yunge-reader-search--context (result)
  "Return one compact display context for RESULT."
  (truncate-string-to-width
   (string-trim
    (replace-regexp-in-string
     "[[:space:]]+" " "
     (concat (or (yunge-reader-search-result-before result) "")
             (or (yunge-reader-search-result-text result) "")
             (or (yunge-reader-search-result-after result) ""))))
   100 nil nil t))

(defun yunge-reader-search--result-key (result)
  "Return the stable endpoint identity of search RESULT."
  (list (yunge-reader-search-result-start result)
        (yunge-reader-search-result-end result)))

(defun yunge-reader-search--seen-table ()
  "Return an endpoint table initialized from loaded search results."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (result yunge-reader-search-results)
      (puthash (yunge-reader-search--result-key result) t table))
    table))

(defun yunge-reader-search--split-wrapped-results (results)
  "Split wrapped RESULTS at the first endpoint seen in this traversal.
Return (UNSEEN . REPEATED), where UNSEEN is the prefix before that endpoint
and REPEATED is non-nil when the complete search cycle has closed."
  (let (unseen repeated)
    (while (and results (not repeated))
      (let* ((result (pop results))
             (key (yunge-reader-search--result-key result)))
        (if (gethash key yunge-reader-search--cycle-seen)
            (setq repeated t)
          (puthash key t yunge-reader-search--cycle-seen)
          (push result unseen))))
    (cons (nreverse unseen) repeated)))

(defun yunge-reader-search--set-index (index)
  "Make loaded search result INDEX current and notify the view."
  (let ((result (nth index yunge-reader-search-results)))
    (unless result
      (error "Reader search result index is unavailable: %S" index))
    (when yunge-reader-search-highlight-visible
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (yunge-jump-history-record window)))
    (setq yunge-reader-search--index index
          yunge-reader-search--detached nil
          yunge-reader-search-result result)
    (run-hooks 'yunge-reader-search-result-hook)
    (when yunge-reader-search-highlight-visible
      (message
       "Match %d%s: %s"
       (1+ index)
       (if yunge-reader-search--complete
           (format "/%d" (length yunge-reader-search-results))
         "+")
       (yunge-reader-search--context result)))
    result))

(defun yunge-reader-search--schedule-navigation (buffer generation)
  "Resume BUFFER's search navigation for GENERATION asynchronously."
  (run-at-time
   0 nil
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (= generation yunge-reader-search--generation)
           (yunge-reader-search--drive-navigation)))))))

(defun yunge-reader-search--navigation-steps ()
  "Return the positive number of moves represented by the current intent."
  (if yunge-reader-search--navigation-intent
      (max 1 yunge-reader-search--navigation-count)
    0))

(defun yunge-reader-search--finish-navigation (index steps)
  "Visit loaded result INDEX after consuming STEPS pending moves."
  (setq yunge-reader-search--navigation-count
        (max 0 (- (yunge-reader-search--navigation-steps) steps)))
  (when (zerop yunge-reader-search--navigation-count)
    (setq yunge-reader-search--navigation-intent nil))
  (yunge-reader-search--set-index index))

(defun yunge-reader-search--finish-complete-navigation (steps)
  "Move STEPS through the complete result cycle in the pending direction."
  (let* ((total (length yunge-reader-search-results))
         (delta
          (if (eq yunge-reader-search--navigation-intent
                  yunge-reader-search--direction)
              1
            -1))
         (index
          (mod (+ yunge-reader-search--index (* delta steps)) total)))
    (yunge-reader-search--finish-navigation index steps)))

(defun yunge-reader-search--finish-empty ()
  "Finish pending search navigation when no results exist."
  (setq yunge-reader-search--navigation-intent nil
        yunge-reader-search--navigation-count 0)
  (message "No matches for: %s" yunge-reader-search-query))

(defun yunge-reader-search--navigation-ready-p ()
  "Return non-nil when the current intent needs no additional batch."
  (and yunge-reader-search--navigation-intent
       (or yunge-reader-search--complete
           yunge-reader-search--segment-done
           (and (null yunge-reader-search--index)
                yunge-reader-search-results)
           (and (natnump yunge-reader-search--index)
                (< (1+ yunge-reader-search--index)
                   (length yunge-reader-search-results))))))

(defun yunge-reader-search--result-origin (direction)
  "Return the current result endpoint for search DIRECTION."
  (when yunge-reader-search-result
    (pcase direction
      ('forward
       (yunge-reader-search-result-end yunge-reader-search-result))
      ('backward
       (yunge-reader-search-result-start yunge-reader-search-result))
      (_
       (error "Invalid Reader search direction: %S" direction)))))

(defun yunge-reader-search--start-run
    (direction origin &optional wrapped navigation-count preserve-results)
  "Start a DIRECTION search at stable ORIGIN.
When WRAPPED is non-nil, ORIGIN is the corresponding document boundary.
NAVIGATION-COUNT retains pending moves across a wrapped search run.
When PRESERVE-RESULTS is non-nil, retain the loaded traversal prefix while
starting its wrapped continuation."
  (unless (memq direction '(forward backward))
    (error "Invalid Reader search direction: %S" direction))
  (let ((obsolete yunge-reader-search--task))
    (cl-incf yunge-reader-search--generation)
    (unless preserve-results
      (setq yunge-reader-search-results nil
            yunge-reader-search-result nil
            yunge-reader-search--index nil))
    (setq yunge-reader-search-highlight-visible t
          yunge-reader-search--cursor nil
          yunge-reader-search--direction direction
          yunge-reader-search--origin origin
          yunge-reader-search--wrapped wrapped
          yunge-reader-search--detached nil
          yunge-reader-search--segment-done nil
          yunge-reader-search--complete nil
          yunge-reader-search--cycle-seen
          (and preserve-results (yunge-reader-search--seen-table))
          yunge-reader-search--pending nil
          yunge-reader-search--task nil
          yunge-reader-search--navigation-intent direction
          yunge-reader-search--navigation-count
          (max 1 (or navigation-count 1)))
    (when (yunge-reader-task-active-p obsolete)
      (yunge-reader-task-cancel obsolete "The search run was replaced"))
    (unless preserve-results
      (run-hooks 'yunge-reader-search-result-hook))
    (yunge-reader-search--drive-navigation)))

(defun yunge-reader-search--wrap-run ()
  "Continue the active search from its directional document boundary."
  (let ((direction yunge-reader-search--direction)
        (navigation-count (yunge-reader-search--navigation-steps)))
    (message
     (if (eq direction 'backward)
         "Search wrapped to document end"
       "Search wrapped to document beginning"))
    (yunge-reader-search--start-run
     direction nil t navigation-count t)))

(defun yunge-reader-search--drive-navigation ()
  "Fulfill pending search moves without queuing command events."
  (unless yunge-reader-search--pending
    (when yunge-reader-search--navigation-intent
      (let ((steps (yunge-reader-search--navigation-steps)))
        (cond
         ((and yunge-reader-search--complete
               (natnump yunge-reader-search--index)
               yunge-reader-search-results)
          (yunge-reader-search--finish-complete-navigation steps))
         ((and (natnump yunge-reader-search--index)
               (< (1+ yunge-reader-search--index)
                  (length yunge-reader-search-results)))
          (let ((consumed
                 (min
                  steps
                  (- (length yunge-reader-search-results)
                     (1+ yunge-reader-search--index)))))
            (yunge-reader-search--finish-navigation
             (+ yunge-reader-search--index consumed) consumed)
            (when yunge-reader-search--navigation-intent
              (yunge-reader-search--drive-navigation))))
         ((and (null yunge-reader-search--index)
               yunge-reader-search-results)
          (let ((consumed
                 (min steps (length yunge-reader-search-results))))
            (yunge-reader-search--finish-navigation
             (1- consumed) consumed)
            (when yunge-reader-search--navigation-intent
              (yunge-reader-search--drive-navigation))))
         (yunge-reader-search--segment-done
          (if yunge-reader-search--complete
              (yunge-reader-search--finish-empty)
            (yunge-reader-search--wrap-run)))
         (t
          (yunge-reader-search--request-batch)))))))

(defun yunge-reader-search--complete-batch
    (buffer document generation old-cursor value error-data)
  "Complete BUFFER's search request for DOCUMENT and GENERATION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq yunge-reader-search--in-flight
            (max 0 (1- yunge-reader-search--in-flight)))
      (if (not (and (= generation yunge-reader-search--generation)
                    (eq document yunge-reader-document)))
          (when (and (zerop yunge-reader-search--in-flight)
                     yunge-reader-search--navigation-intent)
            (yunge-reader-search--drive-navigation))
        (setq yunge-reader-search--pending nil
              yunge-reader-search--task nil)
        (cond
         (error-data
          (setq yunge-reader-search--navigation-intent nil
                yunge-reader-search--navigation-count 0)
          (display-warning
           'yunge-reader
           (format "Could not search document: %s"
                   (error-message-string error-data))
           :warning))
         ((not (yunge-reader-search--batch-valid-p value))
          (setq yunge-reader-search--navigation-intent nil
                yunge-reader-search--navigation-count 0)
          (display-warning
           'yunge-reader
           "Reader driver returned an invalid search batch"
           :warning))
         (t
          (let* ((native-results
                  (yunge-reader-search-batch-results value))
                 (native-cursor (yunge-reader-search-batch-cursor value))
                 (native-done (yunge-reader-search-batch-done value))
                 (split
                  (if yunge-reader-search--wrapped
                      (progn
                        (unless (hash-table-p
                                 yunge-reader-search--cycle-seen)
                          (setq yunge-reader-search--cycle-seen
                                (yunge-reader-search--seen-table)))
                        (yunge-reader-search--split-wrapped-results
                         native-results))
                    (cons native-results nil)))
                 (new-results (car split))
                 (repeated (cdr split))
                 (done (or native-done repeated))
                 (complete
                  (or yunge-reader-search--complete
                      (and done
                           (or yunge-reader-search--wrapped
                               (null yunge-reader-search--origin)))))
                 (cursor (and (not repeated) native-cursor)))
            (setq yunge-reader-search-results
                  (append yunge-reader-search-results new-results)
                  yunge-reader-search--cursor cursor
                  yunge-reader-search--segment-done done
                  yunge-reader-search--complete complete
                  yunge-reader-search--cycle-seen
                  (and (not complete) yunge-reader-search--cycle-seen))
            (cond
             ((and (not done) (equal cursor old-cursor))
              (setq yunge-reader-search--navigation-intent nil
                    yunge-reader-search--navigation-count 0)
              (display-warning
               'yunge-reader
               "Reader search cursor did not advance"
               :warning))
             (yunge-reader-search--navigation-intent
              (if (yunge-reader-search--navigation-ready-p)
                  (yunge-reader-search--drive-navigation)
                (yunge-reader-search--schedule-navigation
                 buffer generation)))))))))))

(defun yunge-reader-search--request-batch ()
  "Request the next batch needed by the current search intent."
  (unless yunge-reader-search--pending
    (if (> yunge-reader-search--in-flight 0)
        (yunge-reader-search--loading-message
         yunge-reader-search--direction)
      (let ((buffer (current-buffer))
            (document yunge-reader-document)
            (generation yunge-reader-search--generation)
            (old-cursor yunge-reader-search--cursor))
        (setq yunge-reader-search--pending t)
        (cl-incf yunge-reader-search--in-flight)
        (let ((task
               (yunge-reader-request
                'search
                (list :query yunge-reader-search-query
                      :case-sensitive yunge-reader-search--case-sensitive
                      :direction yunge-reader-search--direction
                      :origin (and (null yunge-reader-search--cursor)
                                   yunge-reader-search--origin)
                      :cursor yunge-reader-search--cursor
                      :match-limit yunge-reader-search-match-limit
                      :unit-limit yunge-reader-search-unit-limit)
                (lambda (value error-data)
                  (yunge-reader-search--complete-batch
                   buffer document generation old-cursor
                   value error-data))
                :revision generation)))
          ;; A synchronous completion may already have started the next
          ;; batch; never overwrite that task with this terminal one.
          (when (and (yunge-reader-task-active-p task)
                     (= generation yunge-reader-search--generation))
            (setq yunge-reader-search--task task)))))))

(defun yunge-reader-search--loading-message (intent)
  "Describe the outstanding search for navigation INTENT."
  (let* ((steps (yunge-reader-search--navigation-steps))
         (status
          (pcase intent
            ('backward "Searching backward...")
            (_ "Searching forward..."))))
    (message
     (if (> steps 1)
         (format "%s (%d steps pending)" status steps)
       status))))

(defun yunge-reader-search--navigate (direction)
  "Navigate in search DIRECTION while preserving repeated pending moves."
  (unless yunge-reader-search-query
    (user-error "There is no active document search"))
  (unless (memq direction '(forward backward))
    (error "Invalid Reader search direction: %S" direction))
  (cond
   (yunge-reader-search--detached
    (yunge-reader-search--start-run
     direction (yunge-reader--current-position)))
   ((and (not yunge-reader-search--complete)
         (not (eq direction yunge-reader-search--direction)))
    (yunge-reader-search--start-run
     direction
     (or (yunge-reader-search--result-origin direction)
         yunge-reader-search--origin
         (yunge-reader--current-position))))
   (t
    (let ((was-visible yunge-reader-search-highlight-visible)
          (navigation-count
           (if (eq yunge-reader-search--navigation-intent direction)
               (1+ yunge-reader-search--navigation-count)
             1)))
      (setq yunge-reader-search-highlight-visible t
            yunge-reader-search--navigation-intent direction
            yunge-reader-search--navigation-count navigation-count)
      (unless was-visible
        (run-hooks 'yunge-reader-search-result-hook)))
    (unless yunge-reader-search--pending
      (yunge-reader-search--drive-navigation))))
  (when (and yunge-reader-search--navigation-intent
             (or yunge-reader-search--pending
                 (> yunge-reader-search--in-flight 0)))
    (yunge-reader-search--loading-message direction)))

(defun yunge-reader-search-cancel-navigation ()
  "Cancel one delayed search jump without discarding discovered results."
  (when yunge-reader-search--navigation-intent
    (setq yunge-reader-search--navigation-intent nil
          yunge-reader-search--navigation-count 0)
    t))

(defun yunge-reader-search-detach-navigation ()
  "Detach an active search from its old result after reading movement."
  (when yunge-reader-search-query
    (setq yunge-reader-search--navigation-intent nil
          yunge-reader-search--navigation-count 0
          yunge-reader-search--detached t)
    t))

(defun yunge-reader-search (query)
  "Search the current document for literal QUERY.
Case is ignored unless QUERY contains an uppercase character."
  (interactive
   (list
    (read-string
     "Search document: " nil 'yunge-reader-search-history)))
  (unless yunge-reader-document
    (user-error "This reader buffer has no open document"))
  (when (string-empty-p query)
    (user-error "Search query must not be empty"))
  (setq yunge-reader-search-query query
        yunge-reader-search--case-sensitive
        (yunge-reader-search--smart-case-p query))
  (message "Searching for: %s" query)
  (yunge-reader-search--start-run
   'forward (yunge-reader--current-position)))

(defun yunge-reader-search-next ()
  "Visit the next match for the active document search."
  (interactive)
  (yunge-reader-search--navigate 'forward))

(defun yunge-reader-search-previous ()
  "Visit the previous match for the active document search."
  (interactive)
  (yunge-reader-search--navigate 'backward))

(defun yunge-reader-search-reset (reason)
  "Cancel the current search for REASON and forget its results.
Keep physical request counts until late completions arrive."
  (let ((obsolete yunge-reader-search--task))
    (cl-incf yunge-reader-search--generation)
    (setq yunge-reader-search-query nil
          yunge-reader-search-results nil
          yunge-reader-search-result nil
          yunge-reader-search-highlight-visible nil
          yunge-reader-search--index nil
          yunge-reader-search--cursor nil
          yunge-reader-search--direction nil
          yunge-reader-search--origin nil
          yunge-reader-search--wrapped nil
          yunge-reader-search--detached nil
          yunge-reader-search--segment-done nil
          yunge-reader-search--complete nil
          yunge-reader-search--cycle-seen nil
          yunge-reader-search--pending nil
          yunge-reader-search--task nil
          yunge-reader-search--navigation-intent nil
          yunge-reader-search--navigation-count 0)
    (when (yunge-reader-task-active-p obsolete)
      (yunge-reader-task-cancel obsolete reason))))

(defun yunge-reader-clear-search ()
  "Clear the active document search and its view highlight."
  (interactive)
  (yunge-reader-search-reset "The search was cleared")
  (run-hooks 'yunge-reader-search-result-hook)
  (message "Cleared document search"))

(defun yunge-reader-hide-search-highlight ()
  "Hide the active search highlight without ending its search session."
  (interactive)
  (when yunge-reader-search-highlight-visible
    (setq yunge-reader-search-highlight-visible nil)
    (run-hooks 'yunge-reader-search-result-hook)
    t))

(provide 'yunge-reader-search)

;;; yunge-reader-search.el ends here
