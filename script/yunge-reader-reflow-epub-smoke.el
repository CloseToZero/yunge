;;; yunge-reader-reflow-epub-smoke.el --- Smoke -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(setq native-comp-jit-compilation nil)

(require 'subr-x)

(defconst yunge-reader-reflow-smoke--script-directory
  (file-name-directory
   (or load-file-name
       (error "Reflow EPUB smoke must be loaded from a file"))))

(add-to-list 'load-path yunge-reader-reflow-smoke--script-directory)
(require 'yunge-reader-graphical-smoke)

(defconst yunge-reader-reflow-smoke--context
  (yunge-reader-graphical-smoke-create
   yunge-reader-reflow-smoke--script-directory
   "Reflow EPUB graphical smoke"
   "yunge-reader-reflow-"
   "YUNGE_READER_REFLOW_LOG"))

(condition-case error-data
    (progn
      (yunge-reader-graphical-smoke-initialize
       yunge-reader-reflow-smoke--context)
      (require 'yunge-reader-epub))
  (error
   (yunge-reader-graphical-smoke-cleanup
    yunge-reader-reflow-smoke--context)
   (signal (car error-data) (cdr error-data))))

(declare-function yunge-reader-epub-first-location "yunge-reader-epub")
(declare-function yunge-reader-epub-next-screen "yunge-reader-epub")
(declare-function yunge-reader-clear-search "yunge-reader")
(declare-function yunge-reader-clear-selection "yunge-reader")
(declare-function yunge-reader-copy-selection "yunge-reader")
(declare-function yunge-reader-new-view "yunge-reader")
(declare-function yunge-reader-search "yunge-reader")
(declare-function yunge-reader-search-next "yunge-reader")
(declare-function yunge-reader-outline "yunge-reader")
(declare-function yunge-reader-outline-last-item
                  "yunge-reader-outline")
(declare-function yunge-reader-outline-show "yunge-reader-outline")
(declare-function yunge-reader-webview--select-view-range
                  "yunge-reader-webview")

(defvar yunge-reader-outline--document)
(defvar yunge-reader-outline--entry)
(defvar yunge-reader-outline--items)
(defvar yunge-reader-outline--reader-buffer)
(defvar yunge-reader-outline--reader-window)

(defconst yunge-reader-reflow-smoke--search-query
  "cross chapter beacon")

(defconst yunge-reader-reflow-smoke--outline-destinations
  '(("First chapter" 0 location "OPS/chapter-1.xhtml")
    ("Second chapter" 0 location "OPS/chapter-2.xhtml")
    ("Third chapter" 0 location "OPS/chapter-3.xhtml")))

(defun yunge-reader-native-program ()
  "Use the release helper built by this isolated smoke."
  (yunge-reader-graphical-smoke-context-helper
   yunge-reader-reflow-smoke--context))

(defvar yunge-reader-reflow-smoke--file nil)
(defvar yunge-reader-reflow-smoke--buffer nil)
(defvar yunge-reader-reflow-smoke--replacement nil)
(defvar yunge-reader-reflow-smoke--deadline nil)
(defvar yunge-reader-reflow-smoke--phase 'ready)
(defvar yunge-reader-reflow-smoke--surface-id nil)
(defvar yunge-reader-reflow-smoke--anchor nil)
(defvar yunge-reader-reflow-smoke--search-selection nil)
(defvar yunge-reader-reflow-smoke--outline-buffer nil)
(defvar yunge-reader-reflow-smoke--outline-window nil)
(defvar yunge-reader-reflow-smoke--additional-buffer nil)
(defvar yunge-reader-reflow-smoke--additional-surface-id nil)
(defvar yunge-reader-reflow-smoke--warnings nil)
(defvar yunge-reader-reflow-smoke--observations nil)
(defvar yunge-reader-reflow-smoke--exit-status 0)
(defvar yunge-reader-reflow-smoke--stop-deadline nil)

(defun yunge-reader-reflow-smoke--log (format-string &rest arguments)
  "Write FORMAT-STRING with ARGUMENTS to stdout and the optional log."
  (apply
   #'yunge-reader-graphical-smoke-log
   yunge-reader-reflow-smoke--context
   format-string arguments))

(defun yunge-reader-reflow-smoke--warning
    (original type message &rest arguments)
  "Record Reader MESSAGE, then call ORIGINAL with TYPE and ARGUMENTS."
  (when (eq type 'yunge-reader)
    (push message yunge-reader-reflow-smoke--warnings))
  (apply original type message arguments))

(defun yunge-reader-reflow-smoke--location ()
  "Return a copy of the current native location, or nil."
  (when-let* ((view yunge-reader-webview--buffer-view)
              (location (yunge-reader-webview--view-location view)))
    (copy-tree location)))

(defun yunge-reader-reflow-smoke--chapter-p (location chapter)
  "Return whether LOCATION belongs to fixture CHAPTER."
  (and (yunge-reader-webview--valid-location-p location)
       (string-suffix-p
        (format "chapter-%d.xhtml" chapter)
        (alist-get 'href location))
       (stringp (alist-get 'cfi location))
       (null (alist-get 'x location))
       (null (alist-get 'y location))))

(defun yunge-reader-reflow-smoke--same-anchor-p (left right)
  "Return whether LEFT and RIGHT identify the same reflow anchor."
  (and (equal (alist-get 'href left) (alist-get 'href right))
       (equal (alist-get 'cfi left) (alist-get 'cfi right))))

(defun yunge-reader-reflow-smoke--search-selection ()
  "Return the logical view's desired native search selection."
  (when-let* ((view yunge-reader-webview--buffer-view)
              (selection
               (yunge-reader-webview--view-search-result view)))
    (copy-tree selection)))

(defun yunge-reader-reflow-smoke--search-state ()
  "Return bounded current search state for observations."
  (let* ((result yunge-reader-search-result)
         (start
          (and result (yunge-reader-search-result-start result))))
    `((query . ,yunge-reader-search-query)
      (visible . ,yunge-reader-search-highlight-visible)
      (text . ,(and result
                    (yunge-reader-search-result-text result)))
      (href . ,(and start (yunge-reader-position-unit start)))
      (selection . ,(yunge-reader-reflow-smoke--search-selection)))))

(defun yunge-reader-reflow-smoke--selection-state (view)
  "Return bounded logical and native selection state for VIEW."
  `((native . ,(copy-tree
                (yunge-reader-webview--view-selection view)))
    (start . ,(and yunge-reader-selection
                   (yunge-reader-selection-start
                    yunge-reader-selection)))
    (end . ,(and yunge-reader-selection
                 (yunge-reader-selection-end
                  yunge-reader-selection)))
    (text . ,(and yunge-reader-selection
                  (yunge-reader-selection-text
                   yunge-reader-selection)))
    (copy-pending . ,yunge-reader--copy-pending)
    (kill . ,(and kill-ring (current-kill 0 t)))))

(defun yunge-reader-reflow-smoke--selection-settled-p
    (view selection)
  "Return whether VIEW and Reader own native SELECTION."
  (and
   (equal (yunge-reader-webview--view-selection view) selection)
   (yunge-reader-selection-p yunge-reader-selection)
   (equal
    (yunge-reader-selection-start yunge-reader-selection)
    (make-yunge-reader-position
     :unit (alist-get 'href selection)
     :offset (alist-get 'start selection)))
   (equal
    (yunge-reader-selection-end yunge-reader-selection)
    (make-yunge-reader-position
     :unit (alist-get 'href selection)
     :offset (alist-get 'end selection)))))

(defun yunge-reader-reflow-smoke--outline-item-state (item)
  "Return the bounded state of outline ITEM."
  (let* ((action (yunge-reader-outline-item-action item))
         (position (and action
                        (yunge-reader-action-position action))))
    (list
     (yunge-reader-outline-item-title item)
     (yunge-reader-outline-item-depth item)
     (and action (yunge-reader-action-type action))
     (and position (yunge-reader-position-unit position)))))

(defun yunge-reader-reflow-smoke--outline-items ()
  "Return bounded items from the persistent outline buffer."
  (when (buffer-live-p yunge-reader-reflow-smoke--outline-buffer)
    (with-current-buffer yunge-reader-reflow-smoke--outline-buffer
      (mapcar
       #'yunge-reader-reflow-smoke--outline-item-state
       (append yunge-reader-outline--items nil)))))

(defun yunge-reader-reflow-smoke--outline-valid-p ()
  "Return whether the persistent outline targets this Reader view."
  (and
   (buffer-live-p yunge-reader-reflow-smoke--outline-buffer)
   (with-current-buffer yunge-reader-reflow-smoke--outline-buffer
     (and
      (derived-mode-p 'yunge-reader-outline-mode)
      (eq yunge-reader-outline--reader-buffer
          yunge-reader-reflow-smoke--buffer)
      (window-live-p yunge-reader-outline--reader-window)
      (eq (window-buffer yunge-reader-outline--reader-window)
          yunge-reader-reflow-smoke--buffer)
      (eq yunge-reader-outline--entry
          (buffer-local-value
           'yunge-reader--document-entry
           yunge-reader-reflow-smoke--buffer))
      (eq yunge-reader-outline--document
          (buffer-local-value
           'yunge-reader-document
           yunge-reader-reflow-smoke--buffer))
      (equal
       (yunge-reader-reflow-smoke--outline-items)
       yunge-reader-reflow-smoke--outline-destinations)))))

(defun yunge-reader-reflow-smoke--outline-state ()
  "Return bounded state for the persistent outline buffer."
  (when (buffer-live-p yunge-reader-reflow-smoke--outline-buffer)
    (with-current-buffer yunge-reader-reflow-smoke--outline-buffer
      `((buffer . ,(buffer-name))
        (visible . ,(and (get-buffer-window (current-buffer) t) t))
        (selected
         . ,(buffer-name (window-buffer (selected-window))))
        (reader . ,(and (buffer-live-p
                         yunge-reader-outline--reader-buffer)
                        (buffer-name
                         yunge-reader-outline--reader-buffer)))
        (items . ,(yunge-reader-reflow-smoke--outline-items))))))

(defun yunge-reader-reflow-smoke--follow-outline ()
  "Show the final outline item in the Reader view."
  (unless (and (yunge-reader-reflow-smoke--outline-valid-p)
               (window-live-p
                yunge-reader-reflow-smoke--outline-window))
    (error "Reflow EPUB outline is not ready to follow"))
  (select-window yunge-reader-reflow-smoke--outline-window)
  (with-current-buffer yunge-reader-reflow-smoke--outline-buffer
    (yunge-reader-outline-last-item)
    (yunge-reader-outline-show)))

(defun yunge-reader-reflow-smoke--buffer-view (buffer)
  "Return BUFFER's logical WebView, or nil."
  (when (buffer-live-p buffer)
    (buffer-local-value 'yunge-reader-webview--buffer-view buffer)))

(defun yunge-reader-reflow-smoke--buffer-location (buffer)
  "Return a copy of BUFFER's native location, or nil."
  (when-let* ((view (yunge-reader-reflow-smoke--buffer-view buffer))
              (location (yunge-reader-webview--view-location view)))
    (copy-tree location)))

(defun yunge-reader-reflow-smoke--observe-additional
    (view location additional-view additional-location)
  "Record the two native views and their locations."
  (push
   `((name . additional)
     (primary-surface . ,(yunge-reader-webview--view-id view))
     (primary-location . ,(copy-tree location))
     (additional-surface
      . ,(yunge-reader-webview--view-id additional-view))
     (additional-location . ,(copy-tree additional-location)))
   yunge-reader-reflow-smoke--observations))

(defun yunge-reader-reflow-smoke--search-settled-p
    (location chapter)
  "Return whether search settled at fixture CHAPTER and LOCATION."
  (let* ((result yunge-reader-search-result)
         (start
          (and result (yunge-reader-search-result-start result)))
         (end
          (and result (yunge-reader-search-result-end result)))
         (href (format "OPS/chapter-%d.xhtml" chapter))
         (selection (yunge-reader-reflow-smoke--search-selection)))
    (and result
         yunge-reader-search-highlight-visible
         (not yunge-reader--search-pending)
         (null yunge-reader--search-navigation-intent)
         (equal (yunge-reader-search-result-text result)
                yunge-reader-reflow-smoke--search-query)
         (equal (yunge-reader-position-unit start) href)
         (equal (yunge-reader-position-unit end) href)
         (yunge-reader-webview--valid-selection-p selection)
         (equal (alist-get 'href selection) href)
         (yunge-reader-reflow-smoke--chapter-p location chapter))))

(defun yunge-reader-reflow-smoke--observe (name view location)
  "Record native VIEW and LOCATION under NAME."
  (push
   `((name . ,name)
     (surface . ,(yunge-reader-webview--view-id view))
     (bounds . ,(copy-tree
                 (yunge-reader-webview--view-bounds view)))
     (scale . ,yunge-reader-effective-scale)
     (location . ,(copy-tree location))
     (search . ,(yunge-reader-reflow-smoke--search-state))
     (selection
      . ,(yunge-reader-reflow-smoke--selection-state view))
     (outline . ,(yunge-reader-reflow-smoke--outline-state)))
   yunge-reader-reflow-smoke--observations))

(defun yunge-reader-reflow-smoke--cleanup ()
  "Remove the temporary directory owned by this smoke."
  (yunge-reader-graphical-smoke-cleanup
   yunge-reader-reflow-smoke--context))

(defun yunge-reader-reflow-smoke--diagnostic ()
  "Return current smoke state for failure diagnostics."
  (when (buffer-live-p yunge-reader-reflow-smoke--buffer)
    (with-current-buffer yunge-reader-reflow-smoke--buffer
      (let ((view yunge-reader-webview--buffer-view))
        (list
         :phase yunge-reader-reflow-smoke--phase
         :surface-state
         (and view
              (yunge-reader-webview--view-surface-state view))
         :surface (and view (yunge-reader-webview--view-id view))
         :location (yunge-reader-reflow-smoke--location)
         :anchor yunge-reader-reflow-smoke--anchor
         :search (yunge-reader-reflow-smoke--search-state)
         :search-pending yunge-reader--search-pending
         :search-detached yunge-reader--search-detached
         :search-intent yunge-reader--search-navigation-intent
         :outline (yunge-reader-reflow-smoke--outline-state)
         :observations
         (reverse yunge-reader-reflow-smoke--observations))))))

(defun yunge-reader-reflow-smoke--complete-run ()
  "Clean smoke resources and exit with the recorded status."
  (advice-remove 'display-warning
                 #'yunge-reader-reflow-smoke--warning)
  (condition-case error-data
      (yunge-reader-reflow-smoke--cleanup)
    (error
     (setq yunge-reader-reflow-smoke--exit-status 1)
     (yunge-reader-reflow-smoke--log
      "Reflow EPUB smoke cleanup failed: %S\n" error-data)))
  (when (zerop yunge-reader-reflow-smoke--exit-status)
    (yunge-reader-reflow-smoke--log
     "Reflow EPUB smoke passed: %S\n"
     (nreverse yunge-reader-reflow-smoke--observations)))
  (run-at-time
   0.1 nil #'kill-emacs yunge-reader-reflow-smoke--exit-status))

(defun yunge-reader-reflow-smoke--await-stop ()
  "Wait for the isolated helper to stop, then complete the smoke."
  (cond
   ((and (process-live-p yunge-reader-webview--process)
         (< (float-time) yunge-reader-reflow-smoke--stop-deadline))
    (run-at-time 0.1 nil #'yunge-reader-reflow-smoke--await-stop))
   ((process-live-p yunge-reader-webview--process)
    (yunge-reader-webview-stop t)
    (setq yunge-reader-reflow-smoke--stop-deadline
          (+ (float-time) 1.0))
    (run-at-time 0.1 nil #'yunge-reader-reflow-smoke--await-stop))
   (t
    (yunge-reader-reflow-smoke--complete-run))))

(defun yunge-reader-reflow-smoke--finish (&optional error-data)
  "Record ERROR-DATA, release resources, and finish the smoke."
  (when error-data
    (setq yunge-reader-reflow-smoke--exit-status 1)
    (yunge-reader-reflow-smoke--log
     "Reflow EPUB smoke failed: %S\nState: %S\n"
     error-data (yunge-reader-reflow-smoke--diagnostic)))
  (when yunge-reader-reflow-smoke--warnings
    (setq yunge-reader-reflow-smoke--exit-status 1)
    (yunge-reader-reflow-smoke--log
     "Reflow EPUB smoke warnings: %S\n"
     (nreverse yunge-reader-reflow-smoke--warnings)))
  (when (buffer-live-p yunge-reader-reflow-smoke--additional-buffer)
    (kill-buffer yunge-reader-reflow-smoke--additional-buffer))
  (when (buffer-live-p yunge-reader-reflow-smoke--buffer)
    (kill-buffer yunge-reader-reflow-smoke--buffer))
  (when (buffer-live-p yunge-reader-reflow-smoke--replacement)
    (kill-buffer yunge-reader-reflow-smoke--replacement))
  (setq yunge-reader-reflow-smoke--stop-deadline
        (+ (float-time) 3.0))
  (yunge-reader-webview-stop)
  (run-at-time 0.1 nil #'yunge-reader-reflow-smoke--await-stop))

(defun yunge-reader-reflow-smoke--poll ()
  "Advance the reflowable EPUB smoke state machine."
  (condition-case error-data
      (cond
       ((> (float-time) yunge-reader-reflow-smoke--deadline)
        (error "Reflow EPUB smoke timed out in %S"
               yunge-reader-reflow-smoke--phase))
       ((not (buffer-live-p yunge-reader-reflow-smoke--buffer))
        (run-at-time 0.1 nil #'yunge-reader-reflow-smoke--poll))
       (t
        (with-current-buffer yunge-reader-reflow-smoke--buffer
          (let ((view yunge-reader-webview--buffer-view)
                (location (yunge-reader-reflow-smoke--location)))
            (pcase yunge-reader-reflow-smoke--phase
              ('ready
               (if (and yunge-reader-document
                        (eq (yunge-reader-document-layout
                             yunge-reader-document)
                            'reflow)
                        view
                        (yunge-reader-webview--surface-ready-p view)
                        (yunge-reader-reflow-smoke--chapter-p location 1)
                        (= yunge-reader-effective-scale 1.0))
                   (progn
                     (setq yunge-reader-reflow-smoke--surface-id
                           (yunge-reader-webview--view-id view)
                           yunge-reader-reflow-smoke--anchor
                           (copy-tree location)
                           yunge-reader-reflow-smoke--phase 'retain)
                     (yunge-reader-reflow-smoke--observe
                      'initial view location)
                     (yunge-reader-epub-next-screen)
                     (run-at-time
                      0.1 nil #'yunge-reader-reflow-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-reflow-smoke--poll)))
              ('retain
               (if (and
                    (yunge-reader-reflow-smoke--chapter-p location 1)
                    (not (yunge-reader-reflow-smoke--same-anchor-p
                          location
                          yunge-reader-reflow-smoke--anchor)))
                   (progn
                     (setq yunge-reader-reflow-smoke--anchor
                           (copy-tree location)
                           yunge-reader-reflow-smoke--phase 'hidden
                           yunge-reader-reflow-smoke--replacement
                           (get-buffer-create
                            " *reflow EPUB smoke replacement*"))
                     (yunge-reader-reflow-smoke--observe
                      'retained view location)
                     (switch-to-buffer
                      yunge-reader-reflow-smoke--replacement)
                     (run-at-time
                      0.1 nil #'yunge-reader-reflow-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-reflow-smoke--poll)))
              ('hidden
               (if (and
                    (eq (yunge-reader-webview--view-surface-state view)
                        'detached)
                    (null (yunge-reader-webview--view-id view))
                    (yunge-reader-reflow-smoke--same-anchor-p
                     location yunge-reader-reflow-smoke--anchor))
                   (progn
                     (setq yunge-reader-reflow-smoke--phase 'reopened)
                     (switch-to-buffer yunge-reader-reflow-smoke--buffer)
                     (run-at-time
                      0.1 nil #'yunge-reader-reflow-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-reflow-smoke--poll)))
              ('reopened
               (if (and
                    (yunge-reader-webview--surface-ready-p view)
                    (numberp (yunge-reader-webview--view-id view))
                    (not (eql (yunge-reader-webview--view-id view)
                              yunge-reader-reflow-smoke--surface-id))
                    (yunge-reader-reflow-smoke--same-anchor-p
                     location yunge-reader-reflow-smoke--anchor))
                   (progn
                     (yunge-reader-reflow-smoke--observe
                      'reopened view location)
                     (setq yunge-reader-reflow-smoke--surface-id
                           (yunge-reader-webview--view-id view)
                           yunge-reader-reflow-smoke--phase
                           'search-first)
                     (yunge-reader-search
                      yunge-reader-reflow-smoke--search-query)
                     (run-at-time
                      0.1 nil #'yunge-reader-reflow-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-reflow-smoke--poll)))
              ('search-first
               (if (yunge-reader-reflow-smoke--search-settled-p
                    location 1)
                   (progn
                     (setq yunge-reader-reflow-smoke--phase
                           'search-next)
                     (yunge-reader-reflow-smoke--observe
                      'search-first view location)
                     (yunge-reader-search-next)
                     (run-at-time
                      0.1 nil #'yunge-reader-reflow-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-reflow-smoke--poll)))
              ('search-next
               (let ((selection
                      (yunge-reader-reflow-smoke--search-selection)))
                 (if (and
                      (yunge-reader-reflow-smoke--search-settled-p
                       location 2)
                      selection)
                     (progn
                       (setq yunge-reader-reflow-smoke--search-selection
                             selection
                             yunge-reader-reflow-smoke--phase
                             'search-cleared)
                       (yunge-reader-reflow-smoke--observe
                        'search-next view location)
                       (yunge-reader-clear-search)
                       (run-at-time
                        0.1 nil #'yunge-reader-reflow-smoke--poll))
                   (run-at-time
                    0.1 nil #'yunge-reader-reflow-smoke--poll))))
              ('search-cleared
               (if (and (null yunge-reader-search-query)
                        (null yunge-reader-search-results)
                        (null yunge-reader-search-result)
                        (not yunge-reader-search-highlight-visible)
                        (null
                         (yunge-reader-reflow-smoke--search-selection)))
                   (progn
                     (yunge-reader-reflow-smoke--observe
                      'search-cleared view location)
                     (setq yunge-reader-reflow-smoke--phase
                           'selection-selected)
                     (yunge-reader-webview--select-view-range
                      view yunge-reader-reflow-smoke--search-selection)
                     (run-at-time
                      0.1 nil #'yunge-reader-reflow-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-reflow-smoke--poll)))
              ('selection-selected
               (if (yunge-reader-reflow-smoke--selection-settled-p
                    view yunge-reader-reflow-smoke--search-selection)
                   (progn
                     (yunge-reader-reflow-smoke--observe
                      'selection-selected view location)
                     (setq yunge-reader-reflow-smoke--phase
                           'selection-copied)
                     (yunge-reader-copy-selection)
                     (run-at-time
                      0.1 nil #'yunge-reader-reflow-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-reflow-smoke--poll)))
              ('selection-copied
               (if (and
                    (not yunge-reader--copy-pending)
                    (equal
                     (yunge-reader-selection-text
                      yunge-reader-selection)
                     yunge-reader-reflow-smoke--search-query)
                    (equal
                     (current-kill 0 t)
                     yunge-reader-reflow-smoke--search-query))
                   (progn
                     (yunge-reader-reflow-smoke--observe
                      'selection-copied view location)
                     (setq yunge-reader-reflow-smoke--phase
                           'outline-loading)
                     (yunge-reader-clear-selection)
                     (let ((reader-window
                            (get-buffer-window (current-buffer) t)))
                       (unless reader-window
                         (error
                          "Reflow EPUB Reader window disappeared"))
                       (select-window reader-window))
                     (yunge-reader-outline)
                     (setq
                      yunge-reader-reflow-smoke--outline-buffer
                      (buffer-local-value
                       'yunge-reader--outline-buffer
                       yunge-reader-reflow-smoke--buffer))
                     (unless (buffer-live-p
                              yunge-reader-reflow-smoke--outline-buffer)
                       (error "Reader did not create an outline buffer"))
                     (run-at-time
                      0.1 nil #'yunge-reader-reflow-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-reflow-smoke--poll)))
              ('outline-loading
               (let ((outline-window
                      (and
                       (buffer-live-p
                        yunge-reader-reflow-smoke--outline-buffer)
                       (get-buffer-window
                        yunge-reader-reflow-smoke--outline-buffer t))))
                 (if (and outline-window
                          (yunge-reader-reflow-smoke--outline-valid-p)
                          (eq (selected-window) outline-window))
                     (progn
                       (setq
                        yunge-reader-reflow-smoke--outline-window
                        outline-window
                        yunge-reader-reflow-smoke--phase
                        'outline-shown)
                       (yunge-reader-reflow-smoke--observe
                        'outline-loaded view location)
                       (yunge-reader-reflow-smoke--follow-outline)
                       (run-at-time
                        0.1 nil #'yunge-reader-reflow-smoke--poll))
                   (run-at-time
                    0.1 nil #'yunge-reader-reflow-smoke--poll))))
              ('outline-shown
               (let ((reader-window
                      (get-buffer-window (current-buffer) t)))
                 (if (and
                      reader-window
                      (eql (yunge-reader-webview--view-id view)
                           yunge-reader-reflow-smoke--surface-id)
                      (yunge-reader-reflow-smoke--chapter-p location 3))
                     (let ((additional-window
                            (progn
                              (yunge-reader-reflow-smoke--observe
                               'outline-shown view location)
                              (setq yunge-reader-reflow-smoke--anchor
                                    (copy-tree location)
                                    yunge-reader-reflow-smoke--phase
                                    'additional-opening)
                              (select-window reader-window)
                              (yunge-reader-outline)
                              (split-window
                               reader-window nil 'right))))
                       (select-window additional-window)
                       (setq
                        yunge-reader-reflow-smoke--additional-buffer
                        (yunge-reader-new-view))
                       (run-at-time
                        0.1 nil #'yunge-reader-reflow-smoke--poll))
                   (run-at-time
                    0.1 nil #'yunge-reader-reflow-smoke--poll))))
              ('additional-opening
               (let* ((additional-view
                       (yunge-reader-reflow-smoke--buffer-view
                        yunge-reader-reflow-smoke--additional-buffer))
                      (additional-location
                       (yunge-reader-reflow-smoke--buffer-location
                        yunge-reader-reflow-smoke--additional-buffer)))
                 (if (and
                      additional-view
                      (yunge-reader-webview--surface-ready-p
                       additional-view)
                      (numberp
                       (yunge-reader-webview--view-id
                        additional-view))
                      (not
                       (eql
                        (yunge-reader-webview--view-id additional-view)
                        yunge-reader-reflow-smoke--surface-id))
                      (eql
                       (yunge-reader-webview--view-publication
                        additional-view)
                       (yunge-reader-webview--view-publication view))
                      (yunge-reader-reflow-smoke--same-anchor-p
                       location yunge-reader-reflow-smoke--anchor)
                      (yunge-reader-reflow-smoke--same-anchor-p
                       additional-location
                       yunge-reader-reflow-smoke--anchor))
                     (progn
                       (setq
                        yunge-reader-reflow-smoke--additional-surface-id
                        (yunge-reader-webview--view-id additional-view)
                        yunge-reader-reflow-smoke--phase
                        'additional-moved)
                       (with-current-buffer
                           yunge-reader-reflow-smoke--additional-buffer
                         (yunge-reader-epub-first-location))
                       (run-at-time
                        0.1 nil #'yunge-reader-reflow-smoke--poll))
                   (run-at-time
                    0.1 nil #'yunge-reader-reflow-smoke--poll))))
              ('additional-moved
               (let* ((additional-view
                       (yunge-reader-reflow-smoke--buffer-view
                        yunge-reader-reflow-smoke--additional-buffer))
                      (additional-location
                       (yunge-reader-reflow-smoke--buffer-location
                        yunge-reader-reflow-smoke--additional-buffer)))
                 (if (and
                      additional-view
                      (eql
                       (yunge-reader-webview--view-id additional-view)
                       yunge-reader-reflow-smoke--additional-surface-id)
                      (eql (yunge-reader-webview--view-id view)
                           yunge-reader-reflow-smoke--surface-id)
                      (yunge-reader-reflow-smoke--same-anchor-p
                       location yunge-reader-reflow-smoke--anchor)
                      (yunge-reader-reflow-smoke--chapter-p
                       additional-location 1))
                     (progn
                       (yunge-reader-reflow-smoke--observe-additional
                        view location additional-view
                        additional-location)
                       (yunge-reader-reflow-smoke--finish))
                   (run-at-time
                    0.1 nil #'yunge-reader-reflow-smoke--poll)))))))))
    (error
     (yunge-reader-reflow-smoke--finish error-data))))

(defun yunge-reader-reflow-smoke--launch ()
  "Generate a reflowable EPUB and launch its graphical smoke."
  (condition-case error-data
      (progn
        (unless (display-graphic-p)
          (error "Reflow EPUB smoke requires a graphical Emacs"))
        (let ((cargo
               (yunge-reader-graphical-smoke-build-helper
                yunge-reader-reflow-smoke--context)))
          (setq yunge-reader-reflow-smoke--file
                (expand-file-name
                 "reflow.epub"
                 (yunge-reader-graphical-smoke-context-temporary-root
                  yunge-reader-reflow-smoke--context)))
          (yunge-reader-graphical-smoke-run-process
           yunge-reader-reflow-smoke--context
           cargo
           (list
            "run"
            "--quiet"
            "--manifest-path"
            (yunge-reader-graphical-smoke-context-manifest
             yunge-reader-reflow-smoke--context)
            "--example"
            "reflow_epub_fixture"
            "--"
            yunge-reader-reflow-smoke--file)))
        (setq kill-ring nil
              yunge-reader-reflow-smoke--deadline (+ (float-time) 50))
        (advice-add 'display-warning :around
                    #'yunge-reader-reflow-smoke--warning)
        (set-frame-size nil 900 700 t)
        (set-frame-parameter nil 'visibility nil)
        (setq yunge-reader-reflow-smoke--buffer
              (find-file-noselect yunge-reader-reflow-smoke--file))
        (switch-to-buffer yunge-reader-reflow-smoke--buffer)
        (run-at-time 0.1 nil #'yunge-reader-reflow-smoke--poll))
    (error
     (yunge-reader-reflow-smoke--finish error-data))))

(yunge-reader-reflow-smoke--launch)

;;; yunge-reader-reflow-epub-smoke.el ends here
