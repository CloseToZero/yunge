;;; yunge-reader-webview.el --- WebView spike -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'yunge-reader)
(require 'yunge-reader-native)

(define-error 'yunge-reader-webview-native-error
  "The Yunge Reader WebView helper reported an error")

(defcustom yunge-reader-webview-stop-timeout 1.0
  "Seconds allowed for graceful WebView helper shutdown."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-webview-open-timeout 5.0
  "Seconds allowed for the WebView renderer shell to become ready."
  :type 'number
  :group 'yunge-reader)

(defconst yunge-reader-webview-protocol-version 1
  "WebView protocol version understood by this client.")

(defconst yunge-reader-webview--max-location-text-bytes 3072
  "Maximum byte length of one EPUB locator text field.")

(defconst yunge-reader-webview--max-selection-characters 1048576
  "Maximum character length of one EPUB text selection.")

(defconst yunge-reader-webview--max-selection-character-limit 65536
  "Maximum characters requested in one EPUB selection batch.")

(defconst yunge-reader-webview--max-search-query-characters 256
  "Maximum characters accepted in one EPUB search query.")

(defconst yunge-reader-webview--max-search-match-limit 200
  "Maximum matches accepted in one EPUB search batch.")

(defconst yunge-reader-webview--max-search-section-limit 64
  "Maximum spine items searched in one EPUB search batch.")

(defconst yunge-reader-webview--max-search-cursor-offset 1048576
  "Maximum transient match ordinal in one EPUB spine item.")

(defconst yunge-reader-webview--max-search-match-text-bytes 16384
  "Maximum byte length of one EPUB search match text.")

(defconst yunge-reader-webview--max-search-context-bytes 4096
  "Maximum byte length of one EPUB search context field.")

(defconst yunge-reader-webview--max-outline-depth 256
  "Maximum nesting depth accepted from one EPUB outline.")

(defconst yunge-reader-webview--max-outline-items 4096
  "Maximum number of items accepted from one EPUB outline.")

(defconst yunge-reader-webview--max-outline-title-bytes 1024
  "Maximum byte length of one EPUB outline title.")

(defconst yunge-reader-webview--epub-style-keys
  '(font-scale line-height content-width side-padding)
  "Semantic fields in one EPUB reading style.")

(defconst yunge-reader-webview--scroll-bar-modes '(hidden visible)
  "Resolved display modes for an EPUB spine-item scroll bar.")

(defconst yunge-reader-webview--log-buffer-name
  "*Yunge Reader WebView log*"
  "Name of the WebView helper diagnostic buffer.")

(defvar yunge-reader-webview--process nil
  "Running WebView helper process, or nil.")

(defvar yunge-reader-webview--callbacks (make-hash-table :test #'eql)
  "Pending WebView callbacks indexed by request identifier.")

(defvar yunge-reader-webview--outbound-queue nil
  "Protocol lines waiting for the WebView ready handshake.")

(defvar yunge-reader-webview--next-request-id 0
  "Last WebView request identifier allocated by Emacs.")

(defvar yunge-reader-webview--next-view-id 0
  "Last logical WebView identifier allocated by Emacs.")

(defvar yunge-reader-webview--views (make-hash-table :test #'eql)
  "Native WebView surfaces indexed by their current identifier.")

(defvar yunge-reader-webview--logical-views
  (make-hash-table :test #'eq)
  "Live logical WebView records, including temporarily hidden views.")

(defvar yunge-reader-webview--force-stop-timer nil
  "Timer enforcing the graceful WebView shutdown deadline.")

(defvar-local yunge-reader-webview--buffer-view nil
  "WebView record owned by the current spike buffer.")

(cl-defstruct (yunge-reader-webview--view
               (:constructor yunge-reader-webview--make-view))
  "One native child WebView attached to an Emacs window."
  id
  window
  buffer
  created
  native-focused
  focus-release-pending
  destroyed
  persistent
  owns-publication
  bounds
  requested-bounds
  bounds-pending
  publication
  publication-ready
  style
  surface-style
  scroll-bar-mode
  surface-scroll-bar-mode
  location
  pending-target
  outline
  outline-ready
  outline-error
  outline-waiters
  selection
  search-result
  path
  open-deadline
  open-timer
  pending-destroys
  destroy-waiters
  destroy-finished
  location-changed-function
  selection-changed-function
  accelerator-function
  scroll-bar-function)

(define-derived-mode yunge-reader-webview-spike-mode special-mode
  "Yunge-WebView"
  "Major mode used behind a native child WebView spike."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (add-hook 'kill-buffer-hook
            #'yunge-reader-webview--kill-buffer nil t))

(defun yunge-reader-webview--program-available-p ()
  "Return whether the native helper executable is available."
  (file-executable-p (yunge-reader-native-program)))

(defun yunge-reader-webview--cancel-force-stop ()
  "Cancel the outstanding forced-stop timer."
  (when (timerp yunge-reader-webview--force-stop-timer)
    (cancel-timer yunge-reader-webview--force-stop-timer))
  (setq yunge-reader-webview--force-stop-timer nil))

(defun yunge-reader-webview--validate-ready (message)
  "Validate WebView helper ready MESSAGE."
  (let ((expected (yunge-reader-native--build-id)))
    (unless expected
      (error "Yunge Reader native source hash is unavailable"))
    (unless
        (and
         (equal (alist-get 'kind message) "webview-ready")
         (= (or (alist-get 'protocol message) -1)
            yunge-reader-webview-protocol-version)
         (equal (alist-get 'build-id message) expected)
         (equal (alist-get 'platform message) "windows")
         (equal (alist-get 'engine message) "webview2")
         (cl-every
          (lambda (capability)
            (member capability (alist-get 'capabilities message)))
          '("publication-close" "publication-info" "publication-open"
            "publication-resources" "view-bounds"
            "view-clear-selection" "view-create"
            "view-destroy" "view-events" "view-focus"
            "view-focus-parent" "view-info"
            "view-navigate" "view-open-publication"
            "view-search"
            "view-search-result"
            "view-selection-text"
            "view-scroll-bars" "view-status" "view-style"
            "view-visible")))
      (error
       "Incompatible Yunge Reader WebView helper: %S"
       message))))

(defun yunge-reader-webview--send-line (process line)
  "Send one protocol LINE to WebView PROCESS."
  (process-send-string process (concat line "\n")))

(defun yunge-reader-webview--flush-outbound (process)
  "Send queued WebView messages to ready PROCESS in FIFO order."
  (dolist (entry (nreverse yunge-reader-webview--outbound-queue))
    (yunge-reader-webview--send-line process (cdr entry)))
  (setq yunge-reader-webview--outbound-queue nil))

(defun yunge-reader-webview--response-error (message)
  "Return an Emacs error value represented by response MESSAGE."
  (let ((object (alist-get 'error message)))
    (list
     'yunge-reader-webview-native-error
     (or (alist-get 'code object) "webview-error")
     (or (alist-get 'message object)
         "The Yunge Reader WebView helper failed"))))

(defun yunge-reader-webview--open-publication (path callback)
  "Open the local EPUB at PATH and invoke CALLBACK with its result."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (not (file-remote-p path)))
    (error "EPUB publication path must be absolute and local"))
  (yunge-reader-webview--request
   "publication-open" `((path . ,(expand-file-name path))) callback))

(defun yunge-reader-webview--publication-info (publication callback)
  "Query PUBLICATION and invoke CALLBACK with its result."
  (unless (and (integerp publication) (> publication 0))
    (error "Invalid EPUB publication ID: %S" publication))
  (yunge-reader-webview--request
   "publication-info" `((publication . ,publication)) callback))

(defun yunge-reader-webview--close-publication (publication callback)
  "Close PUBLICATION and invoke CALLBACK with its result."
  (unless (and (integerp publication) (> publication 0))
    (error "Invalid EPUB publication ID: %S" publication))
  (yunge-reader-webview--request
   "publication-close" `((publication . ,publication)) callback))

(defun yunge-reader-webview--valid-location-p (location)
  "Return non-nil when LOCATION is a bounded EPUB locator."
  (and
   (listp location)
   (let ((cfi (alist-get 'cfi location))
         (href (alist-get 'href location))
         (fraction (alist-get 'fraction location)))
     (and
      (cl-every
       (lambda (entry)
         (memq (car-safe entry) '(cfi href fraction)))
       location)
      (cl-every
       (lambda (value)
         (and (stringp value)
              (not (string-empty-p value))
              (<= (string-bytes value)
                  yunge-reader-webview--max-location-text-bytes)
              (not (string-match-p "[[:cntrl:]]" value))))
       (list cfi href))
      (string-prefix-p "epubcfi(" cfi)
      (string-suffix-p ")" cfi)
      (yunge-reader-webview--valid-target-href-p href)
      (not (string-match-p "#" href))
      (or (null fraction)
          (and (numberp fraction)
               (= fraction fraction)
                  (<= 0 fraction 1)))))))

(defun yunge-reader-webview--valid-target-href-p (href)
  "Return non-nil when HREF is a bounded internal EPUB target."
  (and
   (stringp href)
   (not (string-empty-p href))
   (<= (string-bytes href)
       yunge-reader-webview--max-location-text-bytes)
   (not (string-match-p "[[:cntrl:]\\\\?]" href))
   (<= (cl-count ?# href) 1)
   (let ((path (car (split-string href "#"))))
     (and
      (not (string-empty-p path))
      (not (string-prefix-p "/" path))
      (not (string-match-p ":" path))
      (cl-every
       (lambda (part)
         (not (member part '("" "." ".."))))
       (split-string path "/"))))))

(defun yunge-reader-webview--valid-target-p (target)
  "Return non-nil when TARGET is a bounded EPUB navigation target."
  (or
   (yunge-reader-webview--valid-location-p target)
   (and
    (listp target)
    (equal (mapcar #'car-safe target) '(href))
    (yunge-reader-webview--valid-target-href-p
     (alist-get 'href target)))))

(defun yunge-reader-webview--valid-outline-item-p (item)
  "Return non-nil when ITEM is a bounded EPUB outline item."
  (and
   (listp item)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(title depth href)))
    item)
   (let ((title (alist-get 'title item))
         (depth (alist-get 'depth item))
         (href (alist-get 'href item)))
     (and
      (stringp title)
      (not (string-empty-p (string-trim title)))
      (<= (string-bytes title)
          yunge-reader-webview--max-outline-title-bytes)
      (not (string-match-p "[[:cntrl:]]" title))
      (natnump depth)
      (<= depth yunge-reader-webview--max-outline-depth)
      (or (null href)
          (yunge-reader-webview--valid-target-href-p href))))))

(defun yunge-reader-webview--valid-outline-p (outline)
  "Return non-nil when OUTLINE is bounded renderer outline data."
  (and
   (listp outline)
   (assq 'items outline)
   (assq 'truncated outline)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(items truncated)))
    outline)
   (let ((items (alist-get 'items outline))
         (truncated (alist-get 'truncated outline)))
     (and
      (listp items)
      (<= (length items)
          yunge-reader-webview--max-outline-items)
      (cl-every #'yunge-reader-webview--valid-outline-item-p items)
      (memq truncated '(nil t))))))

(defun yunge-reader-webview--check-location (location)
  "Return LOCATION or signal when it is not a valid EPUB locator."
  (unless (yunge-reader-webview--valid-location-p location)
    (error "Invalid EPUB location: %S" location))
  location)

(defun yunge-reader-webview--check-target (target)
  "Return TARGET or signal when it is not a valid EPUB target."
  (unless (yunge-reader-webview--valid-target-p target)
    (error "Invalid EPUB navigation target: %S" target))
  target)

(defun yunge-reader-webview--valid-selection-p (selection)
  "Return non-nil when SELECTION is one bounded EPUB spine range."
  (and
   (listp selection)
   (= (length selection) 3)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(href start end)))
    selection)
   (cl-every (lambda (key) (assq key selection))
             '(href start end))
   (let ((href (alist-get 'href selection))
         (start (alist-get 'start selection))
         (end (alist-get 'end selection)))
     (and
      (yunge-reader-webview--valid-target-href-p href)
      (not (string-match-p "#" href))
      (cl-every
       (lambda (cfi)
         (and
          (stringp cfi)
          (not (string-empty-p cfi))
          (<= (string-bytes cfi)
              yunge-reader-webview--max-location-text-bytes)
          (not (string-match-p "[[:cntrl:]]" cfi))
          (string-prefix-p "epubcfi(" cfi)
          (string-suffix-p ")" cfi)))
       (list start end))
      (not (equal start end))))))

(defun yunge-reader-webview--valid-selection-text-result-p
    (result offset character-limit)
  "Return non-nil when RESULT is a consistent EPUB text batch.
OFFSET and CHARACTER-LIMIT describe the request that produced RESULT."
  (and
   (listp result)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(text total next-offset done)))
    result)
   (assq 'text result)
   (assq 'total result)
   (assq 'done result)
   (= (length result)
      (if (assq 'next-offset result) 4 3))
   (let* ((text (alist-get 'text result))
          (total (alist-get 'total result))
          (done (alist-get 'done result))
          (next-entry (assq 'next-offset result))
          (next (cdr-safe next-entry))
          (expected (and (stringp text) (+ offset (length text)))))
     (and
      (stringp text)
      (natnump total)
      (<= total yunge-reader-webview--max-selection-characters)
      (<= (length text) character-limit)
      (<= offset total)
      (memq done '(nil t))
      (if done
          (and (null next-entry) (= expected total))
        (and
         (consp next-entry)
         (natnump next)
         (> (length text) 0)
         (= next expected)
         (< next total)))))))

(defun yunge-reader-webview--selection-text-complete
    (offset character-limit complete result error-data)
  "Validate one EPUB text RESULT before invoking COMPLETE.
OFFSET and CHARACTER-LIMIT are the corresponding request bounds."
  (cond
   (error-data
    (funcall complete nil error-data))
   ((yunge-reader-webview--valid-selection-text-result-p
     result offset character-limit)
    (funcall complete result nil))
   (t
    (funcall
     complete nil
     (list 'error
           (format "Malformed EPUB selection text result: %S" result))))))

(defun yunge-reader-webview--request-selection-text
    (view selection offset character-limit complete)
  "Request one text batch for SELECTION in native VIEW.
OFFSET is a transient Unicode character cursor.  CHARACTER-LIMIT bounds the
returned batch, and COMPLETE receives the validated native result."
  (unless (and (integerp offset)
               (<= 0 offset
                   yunge-reader-webview--max-selection-characters))
    (error "Invalid EPUB selection offset: %S" offset))
  (unless
      (and
       (integerp character-limit)
       (<= 1 character-limit
           yunge-reader-webview--max-selection-character-limit))
    (error "Invalid EPUB selection character limit: %S" character-limit))
  (unless (functionp complete)
    (error "Invalid EPUB selection text completion: %S" complete))
  (yunge-reader-webview--request
   "view-selection-text"
   `((view . ,(yunge-reader-webview--view-id view))
     (selection . ,(copy-tree
                    (if (yunge-reader-webview--valid-selection-p selection)
                        selection
                      (error "Invalid EPUB selection: %S" selection))))
     (offset . ,offset)
     (character-limit . ,character-limit))
   (apply-partially
    #'yunge-reader-webview--selection-text-complete
    offset character-limit complete)))

(defun yunge-reader-webview--valid-search-cursor-p (cursor)
  "Return non-nil when CURSOR is a bounded transient EPUB search cursor."
  (and
   (listp cursor)
   (= (length cursor) 2)
   (cl-every (lambda (entry) (memq (car-safe entry) '(href offset)))
             cursor)
   (assq 'href cursor)
   (assq 'offset cursor)
   (let ((href (alist-get 'href cursor))
         (offset (alist-get 'offset cursor)))
     (and
      (yunge-reader-webview--valid-target-href-p href)
      (not (string-match-p "#" href))
      (natnump offset)
      (<= offset yunge-reader-webview--max-search-cursor-offset)))))

(defun yunge-reader-webview--valid-search-match-p (match)
  "Return non-nil when MATCH is one bounded native EPUB search match."
  (and
   (listp match)
   (= (length match) 6)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(href start end text before after)))
    match)
   (cl-every (lambda (key) (assq key match))
             '(href start end text before after))
   (yunge-reader-webview--valid-selection-p
    `((href . ,(alist-get 'href match))
      (start . ,(alist-get 'start match))
      (end . ,(alist-get 'end match))))
   (let ((text (alist-get 'text match))
         (before (alist-get 'before match))
         (after (alist-get 'after match)))
     (and
      (stringp text)
      (not (string-empty-p text))
      (<= (string-bytes text)
          yunge-reader-webview--max-search-match-text-bytes)
      (stringp before)
      (<= (string-bytes before)
          yunge-reader-webview--max-search-context-bytes)
      (stringp after)
      (<= (string-bytes after)
          yunge-reader-webview--max-search-context-bytes)))))

(defun yunge-reader-webview--valid-search-result-p (result match-limit)
  "Return non-nil when RESULT is a bounded EPUB batch for MATCH-LIMIT."
  (and
   (listp result)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(matches cursor done)))
    result)
   (assq 'matches result)
   (assq 'done result)
   (= (length result) (if (assq 'cursor result) 3 2))
   (let ((matches (alist-get 'matches result))
         (cursor-entry (assq 'cursor result))
         (done (alist-get 'done result)))
     (and
      (proper-list-p matches)
      (<= (length matches) match-limit)
      (cl-every #'yunge-reader-webview--valid-search-match-p matches)
      (memq done '(nil t))
      (if done
          (null cursor-entry)
        (and
         cursor-entry
         (yunge-reader-webview--valid-search-cursor-p
          (cdr cursor-entry))))))))

(defun yunge-reader-webview--search-complete
    (match-limit complete result error-data)
  "Validate one native search RESULT before invoking COMPLETE."
  (cond
   (error-data
    (funcall complete nil error-data))
   ((yunge-reader-webview--valid-search-result-p result match-limit)
    (funcall complete result nil))
   (t
    (funcall
     complete nil
     (list 'error
           (format "Malformed EPUB search result: %S" result))))))

(defun yunge-reader-webview--request-search
    (view query case-sensitive cursor match-limit section-limit complete)
  "Request one bounded native EPUB search batch from VIEW."
  (unless
      (and
       (stringp query)
       (<= 1 (length query)
           yunge-reader-webview--max-search-query-characters)
       (not (string-match-p "[[:cntrl:]]" query)))
    (error "Invalid EPUB search query: %S" query))
  (unless (memq case-sensitive '(nil t))
    (error "Invalid EPUB search case flag: %S" case-sensitive))
  (unless (or (null cursor)
              (yunge-reader-webview--valid-search-cursor-p cursor))
    (error "Invalid EPUB search cursor: %S" cursor))
  (unless
      (and
       (integerp match-limit)
       (<= 1 match-limit
           yunge-reader-webview--max-search-match-limit))
    (error "Invalid EPUB search match limit: %S" match-limit))
  (unless
      (and
       (integerp section-limit)
       (<= 1 section-limit
           yunge-reader-webview--max-search-section-limit))
    (error "Invalid EPUB search section limit: %S" section-limit))
  (unless (functionp complete)
    (error "Invalid EPUB search completion: %S" complete))
  (yunge-reader-webview--request
   "view-search"
   `((view . ,(yunge-reader-webview--view-id view))
     (query . ,query)
     (case-sensitive . ,(if case-sensitive t :false))
     (cursor . ,(copy-tree cursor))
     (match-limit . ,match-limit)
     (section-limit . ,section-limit))
   (apply-partially
    #'yunge-reader-webview--search-complete match-limit complete)))

(defun yunge-reader-webview--valid-style-p (style)
  "Return non-nil when STYLE is a bounded EPUB reading style."
  (and
   (listp style)
   (= (length style)
      (length yunge-reader-webview--epub-style-keys))
   (cl-every
    (lambda (entry)
      (memq (car-safe entry)
            yunge-reader-webview--epub-style-keys))
    style)
   (cl-every (lambda (key) (assq key style))
             yunge-reader-webview--epub-style-keys)
   (let ((font-scale (alist-get 'font-scale style))
         (line-height (alist-get 'line-height style))
         (content-width (alist-get 'content-width style))
         (side-padding (alist-get 'side-padding style)))
     (and
      (numberp font-scale)
      (= font-scale font-scale)
      (<= 0.5 font-scale 3.0)
      (numberp line-height)
      (= line-height line-height)
      (<= 1.0 line-height 3.0)
      (integerp content-width)
      (<= 320 content-width 1600)
      (numberp side-padding)
      (= side-padding side-padding)
      (<= 0 side-padding 20)))))

(defun yunge-reader-webview--check-style (style)
  "Return STYLE or signal when it is not a valid EPUB reading style."
  (unless (yunge-reader-webview--valid-style-p style)
    (error "Invalid EPUB reading style: %S" style))
  style)

(defun yunge-reader-webview--check-scroll-bar-mode (mode)
  "Return resolved EPUB scroll bar MODE or signal."
  (unless (memq mode yunge-reader-webview--scroll-bar-modes)
    (error "Invalid EPUB scroll bar mode: %S" mode))
  mode)

(defun yunge-reader-webview--open-view-publication
    (view publication callback location style bar-mode)
  "Open PUBLICATION in native VIEW with LOCATION, STYLE, and scroll bars.
Invoke CALLBACK when the native request completes."
  (yunge-reader-webview--request
   "view-open-publication"
   (append
    `((view . ,(yunge-reader-webview--view-id view))
      (publication . ,publication))
    (when location
      `((location . ,(yunge-reader-webview--check-location location))))
    (when style
      `((style . ,(yunge-reader-webview--check-style style))))
    `((scroll-bars
       . ,(if (eq (yunge-reader-webview--check-scroll-bar-mode
                   bar-mode)
                  'visible)
              t
            :false))))
   callback))

(defun yunge-reader-webview--navigate-view
    (view command callback &optional location)
  "Ask native VIEW to run semantic COMMAND and invoke CALLBACK.
LOCATION is required only for the go-to command."
  (unless (member command '("previous-screen" "next-screen" "go-to"))
    (error "Unsupported EPUB navigation command: %S" command))
  (when (and (equal command "go-to") (null location))
    (error "EPUB go-to navigation requires a location"))
  (when (and location (not (equal command "go-to")))
    (error "EPUB screen navigation does not accept a location"))
  (yunge-reader-webview--request
   "view-navigate"
   (append
    `((view . ,(yunge-reader-webview--view-id view))
      (command . ,command))
    (when location
      `((location . ,(yunge-reader-webview--check-target location)))))
   callback))

(defun yunge-reader-webview--set-native-view-style
    (view style callback)
  "Apply STYLE to native VIEW, then invoke CALLBACK."
  (yunge-reader-webview--request
   "view-style"
   `((view . ,(yunge-reader-webview--view-id view))
     (style . ,(yunge-reader-webview--check-style style)))
   callback))

(defun yunge-reader-webview--set-native-scroll-bar-mode
    (view mode callback)
  "Apply resolved scroll bar MODE to native VIEW, then invoke CALLBACK."
  (yunge-reader-webview--request
   "view-scroll-bars"
   `((view . ,(yunge-reader-webview--view-id view))
     (visible
      . ,(if (eq (yunge-reader-webview--check-scroll-bar-mode mode)
                 'visible)
             t
           :false)))
   callback))

(defun yunge-reader-webview--style-complete
    (view id style _result error-data)
  "Finish applying STYLE to VIEW surface ID."
  (when (yunge-reader-webview--surface-current-p view id)
    (when error-data
      (when (equal style
                   (yunge-reader-webview--view-surface-style view))
        (setf (yunge-reader-webview--view-surface-style view) nil))
      (display-warning
       'yunge-reader (error-message-string error-data) :warning))))

(defun yunge-reader-webview--sync-view-style (view)
  "Send VIEW's desired style to its ready native surface."
  (let ((style (yunge-reader-webview--view-style view)))
    (when (and style
               (yunge-reader-webview--view-publication-ready view)
               (yunge-reader-webview--surface-current-p
                view (yunge-reader-webview--view-id view))
               (not
                (equal style
                       (yunge-reader-webview--view-surface-style view))))
      (let ((id (yunge-reader-webview--view-id view))
            (requested (copy-tree style)))
        (setf (yunge-reader-webview--view-surface-style view)
              (copy-tree requested))
        (yunge-reader-webview--set-native-view-style
         view requested
         (apply-partially
          #'yunge-reader-webview--style-complete
          view id requested))))))

(defun yunge-reader-webview--set-view-style (view style)
  "Set logical VIEW's desired STYLE and synchronize its live surface."
  (when (or (null view)
            (yunge-reader-webview--view-destroyed view))
    (error "Cannot style a dead EPUB view"))
  (let ((style (copy-tree
                (yunge-reader-webview--check-style style))))
    (setf (yunge-reader-webview--view-style view) style)
    (yunge-reader-webview--sync-view-style view)
    style))

(defun yunge-reader-webview--scroll-bar-complete
    (view id mode _result error-data)
  "Finish applying scroll bar MODE to VIEW surface ID."
  (when (and error-data
             (yunge-reader-webview--surface-current-p view id))
    (when (eq mode
              (yunge-reader-webview--view-surface-scroll-bar-mode view))
      (setf (yunge-reader-webview--view-surface-scroll-bar-mode view)
            nil))
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

(defun yunge-reader-webview--sync-view-scroll-bars (view)
  "Send VIEW's resolved scroll bar mode to its ready native surface."
  (let ((mode (yunge-reader-webview--view-scroll-bar-mode view)))
    (when (and mode
               (yunge-reader-webview--view-publication-ready view)
               (yunge-reader-webview--surface-current-p
                view (yunge-reader-webview--view-id view))
               (not
                (eq mode
                    (yunge-reader-webview--view-surface-scroll-bar-mode
                     view))))
      (let ((id (yunge-reader-webview--view-id view)))
        (setf (yunge-reader-webview--view-surface-scroll-bar-mode view)
              mode)
        (yunge-reader-webview--set-native-scroll-bar-mode
         view mode
         (apply-partially
          #'yunge-reader-webview--scroll-bar-complete view id mode))))))

(defun yunge-reader-webview--resolved-scroll-bar-mode (view window)
  "Return VIEW's resolved scroll bar mode in WINDOW."
  (yunge-reader-webview--check-scroll-bar-mode
   (if-let* ((function
              (yunge-reader-webview--view-scroll-bar-function view)))
       (funcall function window)
     'visible)))

(defun yunge-reader-webview--update-scroll-bar-mode (view window)
  "Resolve VIEW's scroll bar mode for WINDOW and synchronize it."
  (let ((mode
         (yunge-reader-webview--resolved-scroll-bar-mode view window)))
    (unless (eq mode (yunge-reader-webview--view-scroll-bar-mode view))
      (setf (yunge-reader-webview--view-scroll-bar-mode view) mode)
      (yunge-reader-webview--sync-view-scroll-bars view))))

(defun yunge-reader-webview--queue-view-target (view target)
  "Queue one transient EPUB TARGET until VIEW's surface is ready."
  (setf (yunge-reader-webview--view-pending-target view)
        (copy-tree (yunge-reader-webview--check-target target))))

(defun yunge-reader-webview--pending-target-complete
    (_result error-data)
  "Report ERROR-DATA from a queued EPUB navigation target."
  (when error-data
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

(defun yunge-reader-webview--dispatch-pending-target (view)
  "Navigate VIEW to its queued transient target, if any."
  (when-let* ((target
               (prog1 (yunge-reader-webview--view-pending-target view)
                 (setf (yunge-reader-webview--view-pending-target view)
                       nil))))
    (yunge-reader-webview--navigate-view
     view "go-to"
     #'yunge-reader-webview--pending-target-complete
     target)))

(defun yunge-reader-webview--set-buffer-message (view message)
  "Replace VIEW's backing buffer contents with MESSAGE."
  (let ((buffer (yunge-reader-webview--view-buffer view)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert message "\n")
          (set-buffer-modified-p nil))))))

(defun yunge-reader-webview--event-location (message)
  "Return the validated EPUB locator carried by event MESSAGE."
  (let ((location (alist-get 'location message)))
    (unless (yunge-reader-webview--valid-location-p location)
      (error "Malformed EPUB location event: %S" message))
    (copy-tree location)))

(defun yunge-reader-webview--event-location-user (message)
  "Return whether location event MESSAGE came from direct user movement."
  (let ((user (alist-get 'user message)))
    (unless (and (assq 'user message)
                 (memq user '(nil t)))
      (error "Malformed EPUB location user flag: %S" message))
    user))

(defun yunge-reader-webview--store-view-location
    (view message &optional quiet)
  "Store VIEW's locator from MESSAGE.
Unless QUIET is non-nil, notify the logical view's owner."
  (let ((location (yunge-reader-webview--event-location message))
        (user
         (unless quiet
           (yunge-reader-webview--event-location-user message))))
    (setf (yunge-reader-webview--view-location view) location)
    (when-let* (((not quiet))
                (function
                 (yunge-reader-webview--view-location-changed-function
                  view)))
      (condition-case error-data
          (funcall function view user)
        (error
         (display-warning
          'yunge-reader
          (format "Could not record EPUB location: %s"
                  (error-message-string error-data))
          :warning))))))

(defun yunge-reader-webview--event-outline (message)
  "Return the validated EPUB outline carried by event MESSAGE."
  (let ((outline (alist-get 'outline message)))
    (unless (yunge-reader-webview--valid-outline-p outline)
      (error "Malformed EPUB outline event: %S" message))
    (copy-tree outline)))

(defun yunge-reader-webview--event-selection (message)
  "Return the validated EPUB selection carried by event MESSAGE."
  (unless (assq 'selection message)
    (error "EPUB selection event has no selection field: %S" message))
  (let ((selection (alist-get 'selection message)))
    (unless (or (null selection)
                (yunge-reader-webview--valid-selection-p selection))
      (error "Malformed EPUB selection event: %S" message))
    (and selection (copy-tree selection))))

(defun yunge-reader-webview--set-view-selection (view selection)
  "Set VIEW's validated SELECTION and notify its logical owner."
  (unless (or (null selection)
              (yunge-reader-webview--valid-selection-p selection))
    (error "Invalid EPUB view selection: %S" selection))
  (unless (equal selection
                 (yunge-reader-webview--view-selection view))
    (setf (yunge-reader-webview--view-selection view)
          (and selection (copy-tree selection)))
    (when-let* ((function
                 (yunge-reader-webview--view-selection-changed-function
                  view)))
      (condition-case error-data
          (funcall function view)
        (error
         (display-warning
          'yunge-reader
          (format "Could not record EPUB selection: %s"
                  (error-message-string error-data))
          :warning)))))
  (yunge-reader-webview--view-selection view))

(defun yunge-reader-webview--clear-view-selection (view)
  "Ask live native VIEW to clear its publication selection."
  (when (and (integerp (yunge-reader-webview--view-id view))
             (yunge-reader-webview--view-created view)
             (yunge-reader-webview--view-publication-ready view)
             (not (yunge-reader-webview--view-destroyed view))
             (process-live-p yunge-reader-webview--process))
    (yunge-reader-webview--request
     "view-clear-selection"
     `((view . ,(yunge-reader-webview--view-id view)))
     (lambda (_result _error-data)))
    t))

(defun yunge-reader-webview--search-result-complete
    (view id selection _result error-data)
  "Report failure to apply SELECTION to VIEW surface ID."
  (when (and error-data
             (yunge-reader-webview--surface-current-p view id)
             (equal selection
                    (yunge-reader-webview--view-search-result view)))
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

(defun yunge-reader-webview--sync-view-search-result (view &optional reveal)
  "Apply logical VIEW's desired search result to its ready surface.
When REVEAL is non-nil, navigate to the result before painting it."
  (when (and (integerp (yunge-reader-webview--view-id view))
             (yunge-reader-webview--view-created view)
             (yunge-reader-webview--view-publication-ready view)
             (not (yunge-reader-webview--view-destroyed view))
             (process-live-p yunge-reader-webview--process))
    (let ((id (yunge-reader-webview--view-id view))
          (selection
           (copy-tree
            (yunge-reader-webview--view-search-result view))))
      (yunge-reader-webview--request
       "view-search-result"
       `((view . ,id)
         (selection . ,selection)
         (reveal . ,(if reveal t :false)))
       (apply-partially
        #'yunge-reader-webview--search-result-complete
        view id selection)))
    t))

(defun yunge-reader-webview--set-view-search-result (view selection)
  "Set logical VIEW's desired native search-result SELECTION."
  (when (or (null view)
            (yunge-reader-webview--view-destroyed view))
    (error "Cannot set a search result on a dead EPUB view"))
  (unless (or (null selection)
              (yunge-reader-webview--valid-selection-p selection))
    (error "Invalid EPUB search result selection: %S" selection))
  (setf (yunge-reader-webview--view-search-result view)
        (and selection (copy-tree selection)))
  (yunge-reader-webview--sync-view-search-result view t)
  selection)

(defun yunge-reader-webview--focus-owning-window (view)
  "Return native focus from VIEW to its owning live Emacs window."
  (yunge-reader-webview--request-parent-focus view)
  (when-let* ((window (yunge-reader-webview--view-window view))
              ((window-live-p window)))
    (select-window window)
    (select-frame-set-input-focus (window-frame window))))

(defun yunge-reader-webview--request-parent-focus (view)
  "Ask live native VIEW to return keyboard focus to its parent frame."
  (when (and (integerp (yunge-reader-webview--view-id view))
             (yunge-reader-webview--view-created view)
             (not (yunge-reader-webview--view-focus-release-pending view))
             (process-live-p yunge-reader-webview--process))
    (let ((id (yunge-reader-webview--view-id view)))
      (setf (yunge-reader-webview--view-focus-release-pending view) t)
      (yunge-reader-webview--request
       "view-focus-parent" `((view . ,id))
       (lambda (_result error-data)
         (when (yunge-reader-webview--surface-current-p view id)
           (setf
            (yunge-reader-webview--view-focus-release-pending view) nil)
           (if error-data
               (display-warning
                'yunge-reader
                (error-message-string error-data)
                :warning)
             (setf (yunge-reader-webview--view-native-focused view)
                   nil))))))
    t))

(defun yunge-reader-webview--record-native-focus (view focused)
  "Record whether VIEW has native focus and synchronize Emacs selection."
  (setf (yunge-reader-webview--view-native-focused view) focused
        (yunge-reader-webview--view-focus-release-pending view) nil)
  (when (and focused
             (window-live-p (yunge-reader-webview--view-window view))
             (not (eq (selected-window)
                      (yunge-reader-webview--view-window view))))
    (select-window (yunge-reader-webview--view-window view))))

(defun yunge-reader-webview--relay-owning-key (view key)
  "Return focus from VIEW and enqueue normalized Emacs KEY."
  (unless (member key '("SPC" "M-m"))
    (error "Invalid WebView owning key: %s" key))
  (yunge-reader-webview--focus-owning-window view)
  (setq unread-command-events
        (append (listify-key-sequence (kbd key))
                unread-command-events)))

(defun yunge-reader-webview--finish-outline-waiters
    (view outline error-data)
  "Complete VIEW's outline waiters with OUTLINE or ERROR-DATA."
  (let ((waiters
         (prog1 (yunge-reader-webview--view-outline-waiters view)
           (setf (yunge-reader-webview--view-outline-waiters view)
                 nil))))
    (dolist (complete waiters)
      (funcall complete (and outline (copy-tree outline)) error-data))))

(defun yunge-reader-webview--store-view-outline (view message)
  "Store VIEW's bounded outline from publication-ready MESSAGE."
  (let ((outline (yunge-reader-webview--event-outline message)))
    (setf (yunge-reader-webview--view-outline view) outline
          (yunge-reader-webview--view-outline-ready view) t
          (yunge-reader-webview--view-outline-error view) nil)
    (yunge-reader-webview--finish-outline-waiters view outline nil)))

(defun yunge-reader-webview--request-view-outline (view complete)
  "Invoke COMPLETE with VIEW's outline when its publication is ready."
  (unless (functionp complete)
    (error "Invalid EPUB outline completion: %S" complete))
  (cond
   ((or (null view)
        (yunge-reader-webview--view-destroyed view))
    (funcall complete nil
             '(error "The EPUB view is no longer live")))
   ((yunge-reader-webview--view-outline-ready view)
    (funcall complete
             (copy-tree (yunge-reader-webview--view-outline view))
             nil))
   ((yunge-reader-webview--view-outline-error view)
    (funcall complete nil
             (copy-tree
              (yunge-reader-webview--view-outline-error view))))
   (t
    (setf (yunge-reader-webview--view-outline-waiters view)
          (append
           (yunge-reader-webview--view-outline-waiters view)
           (list complete))))))

(defun yunge-reader-webview--handle-event (process message)
  "Handle one asynchronous WebView MESSAGE from PROCESS."
  (unless (eq process yunge-reader-webview--process)
    (error "WebView event belongs to an obsolete process"))
  (let ((event (alist-get 'event message))
        (id (alist-get 'view message)))
    (unless (and (stringp event) (integerp id))
      (error "Malformed Yunge Reader WebView event: %S" message))
    (pcase event
      ("accelerator"
       (let ((key (alist-get 'key message)))
         (unless (member key
                          '("J" "K" "+" "-" "=" "y" "SPC" "M-m"
                            "C-d" "C-u" "C-g" "<escape>"
                            "<next>" "<prior>"))
           (error "Malformed WebView accelerator event: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views))
                     (buffer (yunge-reader-webview--view-buffer view))
                     ((buffer-live-p buffer)))
           (with-current-buffer buffer
             (condition-case error-data
                 (if (member key '("SPC" "M-m"))
                     (yunge-reader-webview--relay-owning-key view key)
                   (when-let*
                       ((function
                         (yunge-reader-webview--view-accelerator-function
                          view)))
                     (funcall function view key)))
               (quit nil)
               (error
                (display-warning
                 'yunge-reader
                 (format "Could not run EPUB key %s: %s"
                         key (error-message-string error-data))
                 :warning))))
           (when (member key '("C-g" "<escape>"))
             (yunge-reader-webview--focus-owning-window view)))))
      ("focus-gained"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--record-native-focus view t)))
      ("focus-lost"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--record-native-focus view nil)))
      ("publication-ready"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--store-view-location view message t)
         (setf (yunge-reader-webview--view-publication-ready view) t)
         (yunge-reader-webview--set-view-selection view nil)
         (yunge-reader-webview--sync-view-style view)
         (yunge-reader-webview--sync-view-scroll-bars view)
         (yunge-reader-webview--sync-view-search-result view)
         (yunge-reader-webview--store-view-outline view message)
         (yunge-reader-webview--dispatch-pending-target view)
         (yunge-reader-webview--set-buffer-message
          view
          (format "EPUB renderer ready: %s"
                  (or (yunge-reader-webview--view-path view)
                      "publication")))))
      ("location"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--store-view-location view message)))
      ("selection"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--set-view-selection
          view (yunge-reader-webview--event-selection message))))
      ("navigation-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB navigation error: %S" message))
         (display-warning 'yunge-reader detail :warning)))
      ("style-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB style error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (setf (yunge-reader-webview--view-surface-style view) nil))
         (display-warning 'yunge-reader detail :warning)))
      ("scroll-bars-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB scroll bar error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (setf
            (yunge-reader-webview--view-surface-scroll-bar-mode view)
            nil))
         (display-warning 'yunge-reader detail :warning)))
      ("publication-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB renderer error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (setf (yunge-reader-webview--view-publication-ready view) nil
                 (yunge-reader-webview--view-surface-style view) nil
                 (yunge-reader-webview--view-surface-scroll-bar-mode view)
                 nil
                 (yunge-reader-webview--view-pending-target view) nil
                 (yunge-reader-webview--view-outline-error view)
                 (list 'error detail))
           (yunge-reader-webview--set-view-selection view nil)
           (yunge-reader-webview--finish-outline-waiters
            view nil (list 'error detail))
           (yunge-reader-webview--set-buffer-message view detail))
         (display-warning 'yunge-reader detail :warning)))
      (_
       (error "Unsupported Yunge Reader WebView event: %s" event)))))

(defun yunge-reader-webview--handle-message (process message)
  "Handle one parsed WebView MESSAGE from PROCESS."
  (cond
   ((not (process-get process 'yunge-reader-webview-ready))
    (yunge-reader-webview--validate-ready message)
    (process-put process 'yunge-reader-webview-ready t)
    (process-put process 'yunge-reader-webview-available
                 (alist-get 'available message))
    (process-put process 'yunge-reader-webview-version
                 (alist-get 'version message))
    (process-put process 'yunge-reader-webview-message
                 (alist-get 'message message))
    (yunge-reader-webview--flush-outbound process))
   ((equal (alist-get 'kind message) "event")
    (yunge-reader-webview--handle-event process message))
   (t
    (let* ((id (alist-get 'id message))
           (callback
            (and (integerp id)
                 (gethash id yunge-reader-webview--callbacks))))
      (unless callback
        (error "Unexpected Yunge Reader WebView response: %S" message))
      (remhash id yunge-reader-webview--callbacks)
      (if (alist-get 'ok message)
          (funcall callback (alist-get 'result message) nil)
        (funcall callback nil
                 (yunge-reader-webview--response-error message)))))))

(defun yunge-reader-webview--filter (process output)
  "Collect and handle complete NDJSON messages in PROCESS OUTPUT."
  (let ((pending
         (concat
          (or (process-get process 'yunge-reader-webview-output) "")
          output))
        newline)
    (while (setq newline (string-match "\n" pending))
      (let ((line
             (string-trim-right (substring pending 0 newline) "\r")))
        (setq pending (substring pending (1+ newline)))
        (unless (string-empty-p line)
          (condition-case error-data
              (yunge-reader-webview--handle-message
               process
               (json-parse-string
                line
                :object-type 'alist
                :array-type 'list
                :null-object nil
                :false-object nil))
            (error
             (display-warning
              'yunge-reader
              (format "Invalid Yunge Reader WebView output: %s"
                      (error-message-string error-data))
              :warning)
             (when (eq process yunge-reader-webview--process)
               (yunge-reader-webview-stop t)))))))
    (process-put process 'yunge-reader-webview-output pending)))

(defun yunge-reader-webview--fail-callbacks (reason)
  "Complete all pending WebView callbacks with REASON."
  (let (callbacks)
    (maphash
     (lambda (_id callback)
       (push callback callbacks))
     yunge-reader-webview--callbacks)
    (clrhash yunge-reader-webview--callbacks)
    (setq yunge-reader-webview--outbound-queue nil)
    (dolist (callback callbacks)
      (funcall callback nil (list 'error reason)))))

(defun yunge-reader-webview--sentinel (process _event)
  "Finalize WebView PROCESS after it exits."
  (when (and (memq (process-status process)
                   '(exit signal failed closed))
             (eq process yunge-reader-webview--process))
    (let ((intentional
           (process-get process 'yunge-reader-webview-intentional-stop)))
      (setq yunge-reader-webview--process nil)
      (yunge-reader-webview--cancel-force-stop)
      (yunge-reader-webview--fail-callbacks
       "The Yunge Reader WebView helper stopped")
      (yunge-reader-webview--forget-all-views)
      (unless intentional
        (display-warning
         'yunge-reader
         "Yunge Reader WebView service stopped unexpectedly"
         :warning)))))

;;;###autoload
(defun yunge-reader-webview-start ()
  "Start the Windows WebView helper and return its process."
  (interactive)
  (unless (eq system-type 'windows-nt)
    (user-error
     "The current WebView embedding spike supports Windows only"))
  (if (process-live-p yunge-reader-webview--process)
      yunge-reader-webview--process
    (unless (yunge-reader-webview--program-available-p)
      (user-error
       (concat
        "Yunge Reader native helper is unavailable; "
        "run M-x yunge-reader-native-setup")))
    (let ((log
           (get-buffer-create yunge-reader-webview--log-buffer-name)))
      (with-current-buffer log
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (setq yunge-reader-webview--outbound-queue nil)
      (let ((process
             (make-process
              :name "yunge-reader-webview"
              :command (list (yunge-reader-native-program) "--webview")
              :connection-type 'pipe
              :coding 'utf-8-unix
              :noquery t
              :stderr log
              :filter #'yunge-reader-webview--filter
              :sentinel #'yunge-reader-webview--sentinel)))
        (process-put process 'yunge-reader-webview-output "")
        (process-put process 'yunge-reader-webview-ready nil)
        (process-put process 'yunge-reader-webview-intentional-stop nil)
        (setq yunge-reader-webview--process process)
        (when (called-interactively-p 'interactive)
          (message "Starting Yunge Reader WebView service..."))
        process))))

(defun yunge-reader-webview--request (operation parameters complete)
  "Send WebView OPERATION with PARAMETERS and call COMPLETE."
  (unless (stringp operation)
    (error "WebView operation must be a string: %S" operation))
  (unless (functionp complete)
    (error "WebView completion must be a function: %S" complete))
  (let* ((process (yunge-reader-webview-start))
         (id (cl-incf yunge-reader-webview--next-request-id))
         (request
          (append
           (list (cons 'id id) (cons 'op operation))
           (when parameters
             (list (cons 'params parameters)))))
         (line
          (json-serialize request :null-object nil :false-object :false)))
    (puthash id complete yunge-reader-webview--callbacks)
    (if (process-get process 'yunge-reader-webview-ready)
        (yunge-reader-webview--send-line process line)
      (push (cons id line) yunge-reader-webview--outbound-queue))
    id))

;;;###autoload
(defun yunge-reader-webview-stop (&optional force)
  "Stop the WebView helper.
Without FORCE, request graceful shutdown and enforce a deadline."
  (interactive "P")
  (if (not (process-live-p yunge-reader-webview--process))
      (progn
        (setq yunge-reader-webview--process nil)
        (yunge-reader-webview--forget-all-views)
        (when (called-interactively-p 'interactive)
          (message "Yunge Reader WebView service is not running"))
        nil)
    (let ((process yunge-reader-webview--process))
      (process-put process 'yunge-reader-webview-intentional-stop t)
      (if force
          (delete-process process)
        (yunge-reader-webview--request "shutdown" nil #'ignore)
        (yunge-reader-webview--cancel-force-stop)
        (setq yunge-reader-webview--force-stop-timer
              (run-at-time
               yunge-reader-webview-stop-timeout nil
               (lambda (child)
                 (when (and (eq child yunge-reader-webview--process)
                            (process-live-p child))
                   (process-put
                    child 'yunge-reader-webview-intentional-stop t)
                   (delete-process child)))
               process)))
      (when (called-interactively-p 'interactive)
        (message
         (if force
             "Terminating Yunge Reader WebView service..."
           "Stopping Yunge Reader WebView service...")))
      process)))

(defun yunge-reader-webview--frame-handle (frame)
  "Return FRAME's native window handle as a positive integer."
  (let ((value (frame-parameter frame 'window-id)))
    (cond
     ((and (integerp value) (> value 0)) value)
     ((and (stringp value)
           (string-match-p "\\`[0-9]+\\'" value))
      (string-to-number value))
     ((and (stringp value)
           (string-match-p "\\`0[xX][[:xdigit:]]+\\'" value))
      (string-to-number (substring value 2) 16))
     (t
      (error "Frame has no usable native window handle: %S" value)))))

(defun yunge-reader-webview--window-bounds (window)
  "Return the body bounds of WINDOW relative to its native frame."
  (pcase-let ((`(,left ,top ,right ,bottom)
               (window-body-pixel-edges window)))
    `((x . ,left)
      (y . ,top)
      (width . ,(- right left))
      (height . ,(- bottom top)))))

(defun yunge-reader-webview--install-hooks ()
  "Install hooks that keep native views aligned with Emacs windows."
  (add-hook 'window-size-change-functions
            #'yunge-reader-webview--sync-views)
  (add-hook 'window-state-change-functions
            #'yunge-reader-webview--sync-views)
  (add-hook 'window-buffer-change-functions
            #'yunge-reader-webview--sync-views)
  (add-hook 'window-selection-change-functions
            #'yunge-reader-webview--sync-native-focus))

(defun yunge-reader-webview--remove-hooks ()
  "Remove native view synchronization hooks."
  (remove-hook 'window-size-change-functions
               #'yunge-reader-webview--sync-views)
  (remove-hook 'window-state-change-functions
               #'yunge-reader-webview--sync-views)
  (remove-hook 'window-buffer-change-functions
               #'yunge-reader-webview--sync-views)
  (remove-hook 'window-selection-change-functions
               #'yunge-reader-webview--sync-native-focus))

(defun yunge-reader-webview--register-view (view)
  "Register logical VIEW and synchronize its native surface."
  (puthash view t yunge-reader-webview--logical-views)
  (yunge-reader-webview--install-hooks)
  (yunge-reader-webview--sync-view view)
  view)

(defun yunge-reader-webview--unregister-view (view)
  "Forget logical VIEW and remove global hooks when none remain."
  (remhash view yunge-reader-webview--logical-views)
  (when (zerop (hash-table-count
                yunge-reader-webview--logical-views))
    (yunge-reader-webview--remove-hooks)))

(defun yunge-reader-webview--surface-current-p (view id)
  "Return whether ID is VIEW's current native surface."
  (and (integerp id)
       (eql id (yunge-reader-webview--view-id view))
       (eq (gethash id yunge-reader-webview--views) view)))

(defun yunge-reader-webview--send-latest-bounds (view)
  "Send VIEW's latest requested bounds unless one is in flight."
  (when (and (yunge-reader-webview--view-created view)
             (not (yunge-reader-webview--view-destroyed view))
             (not (yunge-reader-webview--view-bounds-pending view)))
    (let ((bounds (yunge-reader-webview--view-requested-bounds view)))
      (unless (equal bounds (yunge-reader-webview--view-bounds view))
        (let ((id (yunge-reader-webview--view-id view)))
          (setf (yunge-reader-webview--view-bounds-pending view) t)
          (yunge-reader-webview--request
           "view-bounds"
           `((view . ,id) (bounds . ,bounds))
           (lambda (_result error-data)
             (when (yunge-reader-webview--surface-current-p view id)
               (setf (yunge-reader-webview--view-bounds-pending view)
                     nil)
               (unless error-data
                 (setf (yunge-reader-webview--view-bounds view)
                       bounds))
               (when error-data
                 (display-warning
                  'yunge-reader
                  (error-message-string error-data)
                  :warning))
               (yunge-reader-webview--send-latest-bounds view)))))))))

(defun yunge-reader-webview--visible-window (view)
  "Return a live window displaying VIEW's buffer, if any."
  (let ((window (yunge-reader-webview--view-window view))
        (buffer (yunge-reader-webview--view-buffer view)))
    (when (buffer-live-p buffer)
      (if (and window
               (window-live-p window)
               (eq (window-buffer window) buffer))
          window
        (get-buffer-window buffer t)))))

(defun yunge-reader-webview--sync-view (view)
  "Synchronize logical VIEW with its currently visible window."
  (unless (yunge-reader-webview--view-destroyed view)
    (let ((window (yunge-reader-webview--visible-window view))
          (current (yunge-reader-webview--view-window view))
          (id (yunge-reader-webview--view-id view)))
      (cond
       ((and window id (eq window current))
        (yunge-reader-webview--update-scroll-bar-mode view window)
        (setf (yunge-reader-webview--view-requested-bounds view)
              (yunge-reader-webview--window-bounds window))
        (yunge-reader-webview--send-latest-bounds view))
       (id
        (yunge-reader-webview--release-surface view)
        (if window
            (yunge-reader-webview--start-surface view window)
          (unless (yunge-reader-webview--view-persistent view)
            (yunge-reader-webview--destroy-view view))))
       (window
        (yunge-reader-webview--start-surface view window))
       ((not (yunge-reader-webview--view-persistent view))
        (yunge-reader-webview--destroy-view view))))))

(defun yunge-reader-webview--sync-views (&rest _ignored)
  "Synchronize every logical view after an Emacs window change."
  (let (views)
    (maphash
     (lambda (view _present)
       (push view views))
     yunge-reader-webview--logical-views)
    (dolist (view views)
      (yunge-reader-webview--sync-view view))))

(defun yunge-reader-webview--sync-native-focus (&rest _ignored)
  "Release any focused native child whose Emacs window is not selected."
  (maphash
   (lambda (view _present)
     (when (and (yunge-reader-webview--view-native-focused view)
                (not (eq (selected-window)
                         (yunge-reader-webview--view-window view))))
       (yunge-reader-webview--request-parent-focus view)))
   yunge-reader-webview--logical-views))

(defun yunge-reader-webview--cancel-open-timer (view)
  "Cancel VIEW's renderer readiness timer."
  (when-let* ((timer (yunge-reader-webview--view-open-timer view)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (yunge-reader-webview--view-open-timer view) nil)))

(defun yunge-reader-webview--close-owned-publication (publication)
  "Close PUBLICATION when the WebView helper is still live."
  (when (and publication
             (process-live-p yunge-reader-webview--process))
    (yunge-reader-webview--close-publication
     publication (lambda (_result _error-data)))))

(defun yunge-reader-webview--queue-surface-destroy
    (view id complete)
  "Run COMPLETE after pending surface ID for VIEW is destroyed."
  (let ((entry (assoc id
                      (yunge-reader-webview--view-pending-destroys
                       view))))
    (if entry
        (when complete
          (setcdr entry (append (cdr entry) (list complete))))
      (push (append (list id) (when complete (list complete)))
            (yunge-reader-webview--view-pending-destroys view)))))

(defun yunge-reader-webview--finish-surface-destroy (view id)
  "Finish callbacks waiting for VIEW's obsolete surface ID."
  (let* ((entries
          (yunge-reader-webview--view-pending-destroys view))
         (entry (assoc id entries)))
    (setf (yunge-reader-webview--view-pending-destroys view)
          (delete entry entries))
    (dolist (complete (cdr entry))
      (funcall complete))
    (yunge-reader-webview--maybe-finish-view-destroy view)))

(defun yunge-reader-webview--release-surface
    (view &optional complete)
  "Release VIEW's native surface while retaining its logical state."
  (let ((id (yunge-reader-webview--view-id view))
        (created (yunge-reader-webview--view-created view)))
    (yunge-reader-webview--cancel-open-timer view)
    (when id
      (remhash id yunge-reader-webview--views))
    (setf (yunge-reader-webview--view-id view) nil
          (yunge-reader-webview--view-window view) nil
          (yunge-reader-webview--view-created view) nil
          (yunge-reader-webview--view-native-focused view) nil
          (yunge-reader-webview--view-focus-release-pending view) nil
          (yunge-reader-webview--view-publication-ready view) nil
          (yunge-reader-webview--view-surface-style view) nil
          (yunge-reader-webview--view-surface-scroll-bar-mode view) nil
          (yunge-reader-webview--view-bounds view) nil
          (yunge-reader-webview--view-requested-bounds view) nil
          (yunge-reader-webview--view-bounds-pending view) nil)
    (yunge-reader-webview--set-view-selection view nil)
    (cond
     ((not id)
      (when complete
        (funcall complete)))
     ((not (process-live-p yunge-reader-webview--process))
      (when complete
        (funcall complete)))
     (created
      (yunge-reader-webview--queue-surface-destroy
       view id complete)
      (yunge-reader-webview--request
       "view-destroy" `((view . ,id))
       (lambda (_result _error-data)
         (yunge-reader-webview--finish-surface-destroy view id))))
     (t
      (yunge-reader-webview--queue-surface-destroy
       view id complete)))))

(defun yunge-reader-webview--finish-view-destroy (view)
  "Finish permanent destruction of logical VIEW."
  (unless (yunge-reader-webview--view-destroy-finished view)
    (setf (yunge-reader-webview--view-destroy-finished view) t
          (yunge-reader-webview--view-pending-target view) nil
          (yunge-reader-webview--view-search-result view) nil)
    (yunge-reader-webview--finish-outline-waiters
     view nil '(error "The EPUB view was destroyed before its outline loaded"))
    (let ((publication
           (prog1 (yunge-reader-webview--view-publication view)
             (setf (yunge-reader-webview--view-publication view) nil))))
      (when (yunge-reader-webview--view-owns-publication view)
        (yunge-reader-webview--close-owned-publication publication)))
    (let ((waiters
           (prog1
               (yunge-reader-webview--view-destroy-waiters view)
             (setf (yunge-reader-webview--view-destroy-waiters view)
                   nil))))
      (dolist (complete waiters)
        (funcall complete)))))

(defun yunge-reader-webview--maybe-finish-view-destroy (view)
  "Finish destroyed VIEW after all obsolete surfaces are gone."
  (when (and (yunge-reader-webview--view-destroyed view)
             (null (yunge-reader-webview--view-id view))
             (null
              (yunge-reader-webview--view-pending-destroys view)))
    (yunge-reader-webview--finish-view-destroy view)))

(defun yunge-reader-webview--destroy-view (view &optional complete)
  "Destroy logical VIEW and invoke COMPLETE after its surface is gone."
  (cond
   ((yunge-reader-webview--view-destroy-finished view)
    (when complete
      (funcall complete)))
   ((yunge-reader-webview--view-destroyed view)
    (when complete
      (setf (yunge-reader-webview--view-destroy-waiters view)
            (append
             (yunge-reader-webview--view-destroy-waiters view)
             (list complete)))))
   (t
    (when complete
      (setf (yunge-reader-webview--view-destroy-waiters view)
            (list complete)))
    (setf (yunge-reader-webview--view-destroyed view) t)
    (yunge-reader-webview--unregister-view view)
    (yunge-reader-webview--release-surface view)
    (yunge-reader-webview--maybe-finish-view-destroy view)))
  view)

(defun yunge-reader-webview--forget-all-views ()
  "Forget all native views without sending protocol messages."
  (let (views)
    (maphash
     (lambda (view _present)
       (push view views))
     yunge-reader-webview--logical-views)
    (dolist (view views)
      (yunge-reader-webview--cancel-open-timer view)
      (setf (yunge-reader-webview--view-id view) nil
            (yunge-reader-webview--view-window view) nil
            (yunge-reader-webview--view-created view) nil
            (yunge-reader-webview--view-native-focused view) nil
            (yunge-reader-webview--view-focus-release-pending view) nil
            (yunge-reader-webview--view-destroyed view) t
            (yunge-reader-webview--view-publication-ready view) nil
            (yunge-reader-webview--view-surface-style view) nil
            (yunge-reader-webview--view-surface-scroll-bar-mode view) nil
            (yunge-reader-webview--view-pending-destroys view) nil)
      (yunge-reader-webview--set-view-selection view nil)
      (yunge-reader-webview--finish-view-destroy view)))
  (clrhash yunge-reader-webview--views)
  (clrhash yunge-reader-webview--logical-views)
  (yunge-reader-webview--remove-hooks))

(defun yunge-reader-webview--kill-buffer ()
  "Destroy the native view owned by the current buffer."
  (when yunge-reader-webview--buffer-view
    (yunge-reader-webview--destroy-view
     yunge-reader-webview--buffer-view)))

(defun yunge-reader-webview--open-error-code (error-data)
  "Return the stable helper code in ERROR-DATA, if present."
  (and (eq (car-safe error-data)
           'yunge-reader-webview-native-error)
       (cadr error-data)))

(defun yunge-reader-webview--open-complete
    (view id _result error-data)
  "Finish VIEW surface ID's attempt to attach its publication."
  (when (and error-data
             (yunge-reader-webview--surface-current-p view id))
    (setf (yunge-reader-webview--view-surface-style view) nil
          (yunge-reader-webview--view-surface-scroll-bar-mode view) nil))
  (cond
   ((not (yunge-reader-webview--surface-current-p view id)))
   ((and error-data
         (equal (yunge-reader-webview--open-error-code error-data)
                "view-not-ready")
         (< (float-time)
            (yunge-reader-webview--view-open-deadline view)))
    (setf
     (yunge-reader-webview--view-open-timer view)
     (run-at-time 0.05 nil
                  #'yunge-reader-webview--try-open-publication view)))
   (error-data
    (yunge-reader-webview--set-buffer-message
     view (error-message-string error-data))
    (display-warning
     'yunge-reader (error-message-string error-data) :warning))
   (t
    (yunge-reader-webview--set-buffer-message
     view "Opening the EPUB text start..."))))

(defun yunge-reader-webview--try-open-publication (view)
  "Try to attach VIEW's publication after its renderer shell loads."
  (setf (yunge-reader-webview--view-open-timer view) nil)
  (when (and (yunge-reader-webview--view-created view)
             (not (yunge-reader-webview--view-destroyed view))
             (yunge-reader-webview--view-publication view))
    (let ((id (yunge-reader-webview--view-id view)))
      (setf (yunge-reader-webview--view-surface-style view)
            (and (yunge-reader-webview--view-style view)
                 (copy-tree
                  (yunge-reader-webview--view-style view)))
            (yunge-reader-webview--view-surface-scroll-bar-mode view)
            (yunge-reader-webview--view-scroll-bar-mode view))
      (yunge-reader-webview--open-view-publication
       view
       (yunge-reader-webview--view-publication view)
       (apply-partially
        #'yunge-reader-webview--open-complete view id)
       (yunge-reader-webview--view-location view)
       (yunge-reader-webview--view-style view)
       (yunge-reader-webview--view-scroll-bar-mode view)))))

(defun yunge-reader-webview--current-ready-view ()
  "Return the current buffer's ready EPUB WebView."
  (let ((view yunge-reader-webview--buffer-view))
    (unless (and view
                 (not (yunge-reader-webview--view-destroyed view))
                 (yunge-reader-webview--view-publication-ready view))
      (user-error "The current buffer has no ready EPUB view"))
    view))

(defun yunge-reader-webview--attach-shared-publication
    (publication &optional location location-changed-function
                 selection-changed-function accelerator-function style
                 scroll-bar-function)
  "Attach shared PUBLICATION to the current Reader buffer.
Restore bounded LOCATION when supplied.  Invoke LOCATION-CHANGED-FUNCTION
with the logical view whenever its renderer reports a stable location.
Invoke SELECTION-CHANGED-FUNCTION whenever its logical selection changes.
Invoke ACCELERATOR-FUNCTION with the view and a normalized key when the
focused native child forwards one.  STYLE is copied into the logical view.
SCROLL-BAR-FUNCTION resolves its mode for the owning Emacs window."
  (unless (and (integerp publication) (> publication 0))
    (error "Invalid EPUB publication ID: %S" publication))
  (when location
    (yunge-reader-webview--check-location location))
  (when style
    (yunge-reader-webview--check-style style))
  (when (and location-changed-function
             (not (functionp location-changed-function)))
    (error "Invalid EPUB location callback: %S"
           location-changed-function))
  (when (and selection-changed-function
             (not (functionp selection-changed-function)))
    (error "Invalid EPUB selection callback: %S"
           selection-changed-function))
  (when (and accelerator-function
             (not (functionp accelerator-function)))
    (error "Invalid EPUB accelerator callback: %S"
           accelerator-function))
  (when (and scroll-bar-function
             (not (functionp scroll-bar-function)))
    (error "Invalid EPUB scroll bar callback: %S"
           scroll-bar-function))
  (when (and yunge-reader-webview--buffer-view
             (not (yunge-reader-webview--view-destroyed
                   yunge-reader-webview--buffer-view)))
    (error "Current buffer already owns an EPUB view"))
  (let ((view
         (yunge-reader-webview--make-view
          :buffer (current-buffer)
          :persistent t
          :publication publication
          :style (and style (copy-tree style))
          :location (and location (copy-tree location))
          :location-changed-function location-changed-function
          :selection-changed-function selection-changed-function
          :accelerator-function accelerator-function
          :scroll-bar-function scroll-bar-function)))
    (setq yunge-reader-webview--buffer-view view)
    (yunge-reader-webview--register-view view)
    view))

(defun yunge-reader-webview--detach-shared-publication
    (&optional complete)
  "Detach the current buffer's shared EPUB view and invoke COMPLETE."
  (let ((view yunge-reader-webview--buffer-view))
    (setq yunge-reader-webview--buffer-view nil)
    (if view
        (yunge-reader-webview--destroy-view view complete)
      (when complete
        (funcall complete)))))

(defun yunge-reader-webview--navigation-complete (_result error-data)
  "Report an asynchronous EPUB navigation ERROR-DATA."
  (when error-data
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

;;;###autoload
(defun yunge-reader-webview-previous-screen ()
  "Move the current EPUB spike view backward by one screen."
  (interactive)
  (yunge-reader-webview--navigate-view
   (yunge-reader-webview--current-ready-view)
   "previous-screen"
   #'yunge-reader-webview--navigation-complete))

;;;###autoload
(defun yunge-reader-webview-next-screen ()
  "Move the current EPUB spike view forward by one screen."
  (interactive)
  (yunge-reader-webview--navigate-view
   (yunge-reader-webview--current-ready-view)
   "next-screen"
   #'yunge-reader-webview--navigation-complete))

(defun yunge-reader-webview--destroy-obsolete-surface
    (view id)
  "Destroy obsolete native surface ID and finish VIEW's waiters."
  (if (process-live-p yunge-reader-webview--process)
      (yunge-reader-webview--request
       "view-destroy" `((view . ,id))
       (lambda (_result _error-data)
         (yunge-reader-webview--finish-surface-destroy view id)))
    (yunge-reader-webview--finish-surface-destroy view id)))

(defun yunge-reader-webview--create-complete
    (view id created-bounds _result error-data)
  "Complete native surface ID creation for logical VIEW."
  (cond
   ((not (yunge-reader-webview--surface-current-p view id))
    (if error-data
        (yunge-reader-webview--finish-surface-destroy view id)
      (yunge-reader-webview--destroy-obsolete-surface view id)))
   (error-data
    (remhash id yunge-reader-webview--views)
    (setf (yunge-reader-webview--view-id view) nil
          (yunge-reader-webview--view-window view) nil
          (yunge-reader-webview--view-created view) nil
          (yunge-reader-webview--view-native-focused view) nil
          (yunge-reader-webview--view-focus-release-pending view) nil
          (yunge-reader-webview--view-requested-bounds view) nil)
    (yunge-reader-webview--set-buffer-message
     view (error-message-string error-data))
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)
    (unless (yunge-reader-webview--view-persistent view)
      (yunge-reader-webview--destroy-view view)))
   (t
    (setf (yunge-reader-webview--view-created view) t
          (yunge-reader-webview--view-bounds view)
          created-bounds)
    (yunge-reader-webview--sync-view view)
    (when (and (yunge-reader-webview--surface-current-p view id)
               (yunge-reader-webview--view-publication view))
      (setf (yunge-reader-webview--view-open-deadline view)
            (+ (float-time) yunge-reader-webview-open-timeout))
      (yunge-reader-webview--try-open-publication view)))))

(defun yunge-reader-webview--request-create (view)
  "Ask the helper to create native VIEW."
  (let* ((id (yunge-reader-webview--view-id view))
         (window (yunge-reader-webview--view-window view))
         (frame (window-frame window))
         (bounds
          (copy-tree
           (yunge-reader-webview--view-requested-bounds view))))
    (yunge-reader-webview--request
     "view-create"
     `((view . ,id)
       (parent . ,(yunge-reader-webview--frame-handle frame))
       (bounds . ,bounds)
       (visible . t))
     (apply-partially
      #'yunge-reader-webview--create-complete view id bounds))))

(defun yunge-reader-webview--start-surface (view window)
  "Create VIEW's native surface in live WINDOW."
  (unless (and (window-live-p window)
               (eq (window-buffer window)
                   (yunge-reader-webview--view-buffer view)))
    (error "Cannot attach EPUB surface to an unrelated window"))
  (unless (or (null (yunge-reader-webview--view-id view))
              (yunge-reader-webview--view-destroyed view))
    (error "EPUB view already owns a native surface"))
  (unless (yunge-reader-webview--view-destroyed view)
    (let ((id (cl-incf yunge-reader-webview--next-view-id))
          (bar-mode
           (yunge-reader-webview--resolved-scroll-bar-mode view window)))
      (yunge-reader-webview--set-view-selection view nil)
      (setf (yunge-reader-webview--view-id view) id
            (yunge-reader-webview--view-window view) window
            (yunge-reader-webview--view-created view) nil
            (yunge-reader-webview--view-native-focused view) nil
            (yunge-reader-webview--view-focus-release-pending view) nil
            (yunge-reader-webview--view-publication-ready view) nil
            (yunge-reader-webview--view-surface-style view) nil
            (yunge-reader-webview--view-scroll-bar-mode view)
            bar-mode
            (yunge-reader-webview--view-surface-scroll-bar-mode view) nil
            (yunge-reader-webview--view-outline-error view) nil
            (yunge-reader-webview--view-requested-bounds view)
            (yunge-reader-webview--window-bounds window))
      (puthash id view yunge-reader-webview--views)
      (yunge-reader-webview--request-create view)))
  view)

(defun yunge-reader-webview--publication-open-complete
    (view result error-data)
  "Finish opening VIEW's publication from native RESULT."
  (if error-data
      (progn
        (yunge-reader-webview--set-buffer-message
         view (error-message-string error-data))
        (yunge-reader-webview--destroy-view view)
        (display-warning
         'yunge-reader (error-message-string error-data) :warning))
    (let ((publication (alist-get 'publication result)))
      (unless (and (integerp publication) (> publication 0))
        (error "Malformed EPUB publication result: %S" result))
      (if (yunge-reader-webview--view-destroyed view)
          (yunge-reader-webview--close-owned-publication publication)
        (setf (yunge-reader-webview--view-publication view) publication
              (yunge-reader-webview--view-owns-publication view) t)
        (yunge-reader-webview--register-view view)))))

;;;###autoload
(defun yunge-reader-webview-spike (&optional window)
  "Embed a selectable reflowable WebView test page in WINDOW.
This command is an architecture spike, not an EPUB reader yet."
  (interactive)
  (unless (display-graphic-p)
    (user-error "The WebView spike requires a graphical display"))
  (unless (eq system-type 'windows-nt)
    (user-error "The current WebView spike supports Windows only"))
  (let* ((window (or window (selected-window)))
         (buffer
          (generate-new-buffer
           "*Yunge Reader WebView*"))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer)))
    (with-current-buffer buffer
      (yunge-reader-webview-spike-mode)
      (setq yunge-reader-webview--buffer-view view)
      (let ((inhibit-read-only t))
        (insert "Creating native WebView...\n")
        (set-buffer-modified-p nil)))
    (set-window-buffer window buffer)
    (yunge-reader-webview--register-view view)
    buffer))

;;;###autoload
(defun yunge-reader-webview-epub-spike
    (file &optional window location)
  "Open local EPUB FILE at LOCATION in a native child WebView in WINDOW.
This manual architecture spike does not register EPUB file associations or
save a durable reading position."
  (interactive "fEPUB file: ")
  (unless (display-graphic-p)
    (user-error "The EPUB WebView spike requires a graphical display"))
  (unless (eq system-type 'windows-nt)
    (user-error "The current EPUB WebView spike supports Windows only"))
  (setq file (expand-file-name file))
  (when (file-remote-p file)
    (user-error "The EPUB WebView spike accepts local files only"))
  (unless (and (file-regular-p file) (file-readable-p file))
    (user-error "EPUB file is not readable: %s" file))
  (when location
    (yunge-reader-webview--check-location location))
  (let* ((window (or window (selected-window)))
         (buffer
          (generate-new-buffer
           (format "*Yunge EPUB %s*" (file-name-nondirectory file))))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer
           :location (and location (copy-tree location))
           :path file)))
    (with-current-buffer buffer
      (yunge-reader-webview-spike-mode)
      (setq yunge-reader-webview--buffer-view view)
      (let ((inhibit-read-only t))
        (insert "Validating EPUB publication...\n")
        (set-buffer-modified-p nil)))
    (set-window-buffer window buffer)
    (yunge-reader-webview--open-publication
     file
     (apply-partially
      #'yunge-reader-webview--publication-open-complete view))
    buffer))

(defun yunge-reader-webview--shutdown-for-emacs-exit ()
  "Terminate the WebView helper without delaying Emacs exit."
  (when (process-live-p yunge-reader-webview--process)
    (process-put yunge-reader-webview--process
                 'yunge-reader-webview-intentional-stop t)
    (delete-process yunge-reader-webview--process)))

(add-hook 'kill-emacs-hook
          #'yunge-reader-webview--shutdown-for-emacs-exit)

(provide 'yunge-reader-webview)

;;; yunge-reader-webview.el ends here
