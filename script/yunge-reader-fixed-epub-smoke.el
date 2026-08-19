;;; yunge-reader-fixed-epub-smoke.el --- Smoke -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(setq native-comp-jit-compilation nil)

(require 'subr-x)

(defconst yunge-reader-fixed-smoke--script-directory
  (file-name-directory
   (or load-file-name
       (error "Fixed EPUB smoke must be loaded from a file"))))

(add-to-list 'load-path yunge-reader-fixed-smoke--script-directory)
(require 'yunge-reader-graphical-smoke)

(defconst yunge-reader-fixed-smoke--context
  (yunge-reader-graphical-smoke-create
   yunge-reader-fixed-smoke--script-directory
   "Fixed EPUB graphical smoke"
   "yunge-reader-fixed-"
   "YUNGE_READER_FIXED_LOG"))

(condition-case error-data
    (progn
      (yunge-reader-graphical-smoke-initialize
       yunge-reader-fixed-smoke--context)
      (require 'yunge-reader-epub))
  (error
   (yunge-reader-graphical-smoke-cleanup
    yunge-reader-fixed-smoke--context)
   (signal (car error-data) (cdr error-data))))

(declare-function yunge-reader-epub-next-line "yunge-reader-epub")
(declare-function yunge-reader-epub-next-page "yunge-reader-epub")
(declare-function yunge-reader-epub-next-screen "yunge-reader-epub")
(declare-function yunge-reader-epub-first-location "yunge-reader-epub")
(declare-function yunge-reader-epub-last-location "yunge-reader-epub")
(declare-function yunge-reader-epub-previous-page "yunge-reader-epub")

(defun yunge-reader-native-program ()
  "Use the release helper built by this isolated smoke."
  (yunge-reader-graphical-smoke-context-helper
   yunge-reader-fixed-smoke--context))

(defconst yunge-reader-fixed-smoke--variants
  '("ltr" "rtl" "vertical-rl"))

(defvar yunge-reader-fixed-smoke--remaining nil)
(defvar yunge-reader-fixed-smoke--results nil)
(defvar yunge-reader-fixed-smoke--variant nil)
(defvar yunge-reader-fixed-smoke--file nil)
(defvar yunge-reader-fixed-smoke--cargo nil)
(defvar yunge-reader-fixed-smoke--next-action nil)
(defvar yunge-reader-fixed-smoke--stop-deadline nil)
(defvar yunge-reader-fixed-smoke--exit-status 0)

(defvar yunge-reader-fixed-smoke--buffer nil)
(defvar yunge-reader-fixed-smoke--deadline (+ (float-time) 30))
(defvar yunge-reader-fixed-smoke--phase 'ready)
(defvar yunge-reader-fixed-smoke--locations nil)
(defvar yunge-reader-fixed-smoke--observations nil)
(defvar yunge-reader-fixed-smoke--warnings nil)
(defvar yunge-reader-fixed-smoke--replacement nil)
(defvar yunge-reader-fixed-smoke--surface-id nil)
(defvar yunge-reader-fixed-smoke--fit-scale nil)
(defvar yunge-reader-fixed-smoke--resize-bounds nil)
(defvar yunge-reader-fixed-smoke--resize-not-before nil)
(defvar yunge-reader-fixed-smoke--scrolled nil)

(defun yunge-reader-fixed-smoke--log (format-string &rest arguments)
  "Write FORMAT-STRING with ARGUMENTS to stdout and the optional log."
  (apply
   #'yunge-reader-graphical-smoke-log
   yunge-reader-fixed-smoke--context
   format-string arguments))

(defun yunge-reader-fixed-smoke--warning
    (original type message &rest arguments)
  "Record Reader MESSAGE, then call ORIGINAL with TYPE and ARGUMENTS."
  (when (eq type 'yunge-reader)
    (push message yunge-reader-fixed-smoke--warnings))
  (apply original type message arguments))

(defun yunge-reader-fixed-smoke--continue ()
  "Continue polling the current fixed-layout smoke phase."
  (yunge-reader-graphical-smoke-schedule
   #'yunge-reader-fixed-smoke--poll))

(defun yunge-reader-fixed-smoke--location ()
  "Return a copy of the current native location, or nil."
  (when-let* ((view yunge-reader-webview--buffer-view)
              (location (yunge-reader-webview--view-location view)))
    (copy-tree location)))

(defun yunge-reader-fixed-smoke--surface (view)
  "Return VIEW's current native surface, or nil."
  (and view (yunge-reader-webview--view-surface view)))

(defun yunge-reader-fixed-smoke--surface-id (view)
  "Return VIEW's current native surface identifier, or nil."
  (when-let* ((surface (yunge-reader-fixed-smoke--surface view)))
    (yunge-reader-webview--surface-id surface)))

(defun yunge-reader-fixed-smoke--surface-bounds (view)
  "Return VIEW's current native surface bounds, or nil."
  (when-let* ((surface (yunge-reader-fixed-smoke--surface view)))
    (yunge-reader-webview--surface-bounds surface)))

(defun yunge-reader-fixed-smoke--surface-zoom (view)
  "Return VIEW's current applied fixed zoom, or nil."
  (when-let* ((surface (yunge-reader-fixed-smoke--surface view)))
    (yunge-reader-webview--surface-zoom surface)))

(defun yunge-reader-fixed-smoke--page-p (location page)
  "Return whether LOCATION belongs to fixture PAGE."
  (and (yunge-reader-webview--valid-location-p location)
       (string-suffix-p
        (format "page-%d.xhtml" page)
        (alist-get 'href location))
       (numberp (alist-get 'x location))
       (numberp (alist-get 'y location))))

(defun yunge-reader-fixed-smoke--record (name location)
  "Record LOCATION under NAME."
  (push (cons name (copy-tree location))
        yunge-reader-fixed-smoke--locations))

(defun yunge-reader-fixed-smoke--observe (name view location)
  "Record the native VIEW state and LOCATION under NAME."
  (push
   `((name . ,name)
     (surface . ,(yunge-reader-fixed-smoke--surface-id view))
     (bounds . ,(copy-tree
                 (yunge-reader-fixed-smoke--surface-bounds view)))
     (zoom . ,yunge-reader-zoom-mode)
     (scale . ,yunge-reader-effective-scale)
     (location . ,(copy-tree location)))
   yunge-reader-fixed-smoke--observations))

(defun yunge-reader-fixed-smoke--same-viewport-p (left right)
  "Return whether LEFT and RIGHT identify the same fixed viewport."
  (and (equal (alist-get 'href left) (alist-get 'href right))
       (< (abs (- (alist-get 'x left) (alist-get 'x right))) 0.5)
       (< (abs (- (alist-get 'y left) (alist-get 'y right))) 0.5)))

(defun yunge-reader-fixed-smoke--same-surface-p (view)
  "Return whether VIEW still owns the original native surface."
  (eql (yunge-reader-fixed-smoke--surface-id view)
       yunge-reader-fixed-smoke--surface-id))

(defun yunge-reader-fixed-smoke--bounds-settled-p (view)
  "Return whether VIEW applied its latest requested native bounds."
  (when-let* ((surface (yunge-reader-fixed-smoke--surface view)))
    (and (not
          (yunge-reader-webview--surface-bounds-pending surface))
         (equal (yunge-reader-webview--surface-bounds surface)
                (yunge-reader-webview--surface-requested-bounds
                 surface)))))

(defun yunge-reader-fixed-smoke--different-scale-p (left right)
  "Return whether numeric scales LEFT and RIGHT differ materially."
  (and (numberp left)
       (numberp right)
       (> (abs (- left right)) 0.01)))

(defun yunge-reader-fixed-smoke--diagnostic ()
  "Return bounded state for the current fixed-layout smoke."
  (when (buffer-live-p yunge-reader-fixed-smoke--buffer)
    (with-current-buffer yunge-reader-fixed-smoke--buffer
      (let* ((view yunge-reader-webview--buffer-view)
             (surface (yunge-reader-fixed-smoke--surface view)))
        (list
         :phase yunge-reader-fixed-smoke--phase
         :layout (and yunge-reader-document
                      (yunge-reader-document-layout
                       yunge-reader-document))
         :surface-state
         (and surface (yunge-reader-webview--surface-state surface))
         :surface (and surface
                       (yunge-reader-webview--surface-id surface))
         :bounds (and surface
                      (yunge-reader-webview--surface-bounds surface))
         :zoom (and surface
                    (yunge-reader-webview--surface-zoom surface))
         :location (yunge-reader-fixed-smoke--location))))))

(defun yunge-reader-fixed-smoke--cleanup-temporary-root ()
  "Remove the temporary directory owned by this smoke."
  (yunge-reader-graphical-smoke-cleanup
   yunge-reader-fixed-smoke--context))

(defun yunge-reader-fixed-smoke--complete-run ()
  "Clean the smoke resources and exit with the recorded status."
  (advice-remove 'display-warning
                 #'yunge-reader-fixed-smoke--warning)
  (condition-case error-data
      (yunge-reader-fixed-smoke--cleanup-temporary-root)
    (error
     (setq yunge-reader-fixed-smoke--exit-status 1)
     (yunge-reader-fixed-smoke--log
      "Fixed EPUB smoke cleanup failed: %S\n" error-data)))
  (when (zerop yunge-reader-fixed-smoke--exit-status)
    (yunge-reader-fixed-smoke--log
     "Fixed EPUB smoke passed all variants\n"))
  (yunge-reader-graphical-smoke-schedule
   #'kill-emacs yunge-reader-fixed-smoke--exit-status))

(defun yunge-reader-fixed-smoke--await-stop ()
  "Wait for the isolated helper to stop, then run the next action."
  (cond
   ((and (process-live-p yunge-reader-webview--process)
         (< (float-time) yunge-reader-fixed-smoke--stop-deadline))
    (yunge-reader-graphical-smoke-schedule
     #'yunge-reader-fixed-smoke--await-stop))
   ((process-live-p yunge-reader-webview--process)
    (yunge-reader-webview-stop t)
    (setq yunge-reader-fixed-smoke--stop-deadline
          (+ (float-time) 1.0))
    (yunge-reader-graphical-smoke-schedule
     #'yunge-reader-fixed-smoke--await-stop))
   ((eq yunge-reader-fixed-smoke--next-action 'next)
    (yunge-reader-fixed-smoke--start-variant))
   (t
    (yunge-reader-fixed-smoke--complete-run))))

(defun yunge-reader-fixed-smoke--stop-before (action)
  "Stop the helper, then perform ACTION."
  (setq yunge-reader-fixed-smoke--next-action action
        yunge-reader-fixed-smoke--stop-deadline
        (+ (float-time) 3.0))
  (yunge-reader-webview-stop)
  (yunge-reader-graphical-smoke-schedule
   #'yunge-reader-fixed-smoke--await-stop))

(defun yunge-reader-fixed-smoke--finish (value error-data)
  "Record VALUE and ERROR-DATA, then continue or stop the smoke."
  (let* ((warnings (nreverse yunge-reader-fixed-smoke--warnings))
         (failure
          (or error-data
              (and warnings
                   (list 'error "Reader warnings during smoke"))))
         (result
          (list
           :variant yunge-reader-fixed-smoke--variant
           :value (and (not failure) value)
           :error failure
           :state (and failure
                       (yunge-reader-fixed-smoke--diagnostic))
           :locations (nreverse yunge-reader-fixed-smoke--locations)
           :observations
           (nreverse yunge-reader-fixed-smoke--observations)
           :warnings warnings)))
    (push result yunge-reader-fixed-smoke--results)
    (if failure
        (progn
          (setq yunge-reader-fixed-smoke--exit-status 1
                yunge-reader-fixed-smoke--remaining nil)
          (yunge-reader-fixed-smoke--log
           "Fixed EPUB smoke failed: %S\n" result))
      (yunge-reader-fixed-smoke--log
       "Fixed EPUB smoke passed: %s\n"
       yunge-reader-fixed-smoke--variant))
  (when (buffer-live-p yunge-reader-fixed-smoke--buffer)
    (kill-buffer yunge-reader-fixed-smoke--buffer))
  (when (buffer-live-p yunge-reader-fixed-smoke--replacement)
    (kill-buffer yunge-reader-fixed-smoke--replacement))
  (yunge-reader-fixed-smoke--stop-before
   (if (and (not failure)
            yunge-reader-fixed-smoke--remaining)
       'next
     'exit))))

(defun yunge-reader-fixed-smoke--poll ()
  "Advance the fixed-layout smoke state machine."
  (condition-case error-data
      (cond
       ((> (float-time) yunge-reader-fixed-smoke--deadline)
        (error "Fixed EPUB smoke timed out in %S"
               yunge-reader-fixed-smoke--phase))
       ((not (buffer-live-p yunge-reader-fixed-smoke--buffer))
        (yunge-reader-fixed-smoke--continue))
       (t
        (with-current-buffer yunge-reader-fixed-smoke--buffer
          (let ((view yunge-reader-webview--buffer-view)
                (location (yunge-reader-fixed-smoke--location)))
            (pcase yunge-reader-fixed-smoke--phase
              ('ready
               (if (and yunge-reader-document
                        (eq (yunge-reader-document-layout
                             yunge-reader-document)
                            'fixed)
                        view
                         (yunge-reader-webview--surface-ready-p
                          (yunge-reader-fixed-smoke--surface view))
                        (yunge-reader-fixed-smoke--page-p location 1))
                   (progn
                     (unless (eq yunge-reader-zoom-mode 'fit-page)
                       (error "Fixed EPUB did not start at fit page"))
                     (unless (numberp yunge-reader-effective-scale)
                       (error "Fixed EPUB has no effective scale"))
                     (setq yunge-reader-fixed-smoke--surface-id
                           (yunge-reader-fixed-smoke--surface-id view)
                           yunge-reader-fixed-smoke--fit-scale
                           yunge-reader-effective-scale)
                     (yunge-reader-fixed-smoke--record 'initial location)
                     (yunge-reader-fixed-smoke--observe
                      'initial view location)
                     (setq yunge-reader-fixed-smoke--phase 'last)
                     (yunge-reader-epub-last-location)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('last
               (if (yunge-reader-fixed-smoke--page-p location 3)
                   (progn
                     (yunge-reader-fixed-smoke--record 'last location)
                     (setq yunge-reader-fixed-smoke--phase 'first)
                     (yunge-reader-epub-first-location)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('first
               (if (yunge-reader-fixed-smoke--page-p location 1)
                   (progn
                     (yunge-reader-fixed-smoke--record 'first location)
                     (setq yunge-reader-fixed-smoke--phase 'next)
                     (yunge-reader-epub-next-page)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('next
               (if (yunge-reader-fixed-smoke--page-p location 2)
                   (progn
                     (yunge-reader-fixed-smoke--record 'next location)
                     (setq yunge-reader-fixed-smoke--phase 'previous)
                     (yunge-reader-epub-previous-page)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('previous
               (if (yunge-reader-fixed-smoke--page-p location 1)
                   (progn
                     (yunge-reader-fixed-smoke--record
                      'previous location)
                     (setq yunge-reader-fixed-smoke--phase 'fit-width)
                     (yunge-reader-fit-width)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('fit-width
               (if (and
                    (yunge-reader-fixed-smoke--same-surface-p view)
                    (eq yunge-reader-zoom-mode 'fit-width)
                     (eq (yunge-reader-fixed-smoke--surface-zoom view)
                         'fit-width)
                    (yunge-reader-fixed-smoke--different-scale-p
                     yunge-reader-effective-scale
                     yunge-reader-fixed-smoke--fit-scale)
                    (yunge-reader-fixed-smoke--page-p location 1))
                   (progn
                     (setq yunge-reader-fixed-smoke--fit-scale
                           yunge-reader-effective-scale
                           yunge-reader-fixed-smoke--resize-bounds
                           (copy-tree
                             (yunge-reader-fixed-smoke--surface-bounds
                              view))
                           yunge-reader-fixed-smoke--phase
                           'fit-width-resize)
                     (yunge-reader-fixed-smoke--observe
                      'fit-width view location)
                     (set-frame-size nil 700 700 t)
                     (redisplay t)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('fit-width-resize
               (let ((bounds
                      (yunge-reader-fixed-smoke--surface-bounds view)))
                 (if (and
                      (yunge-reader-fixed-smoke--same-surface-p view)
                      (yunge-reader-fixed-smoke--bounds-settled-p view)
                      (numberp (alist-get 'width bounds))
                      (< (alist-get 'width bounds)
                         (alist-get
                          'width
                          yunge-reader-fixed-smoke--resize-bounds))
                      (< yunge-reader-effective-scale
                         yunge-reader-fixed-smoke--fit-scale)
                      (yunge-reader-fixed-smoke--page-p location 1))
                     (progn
                       (yunge-reader-fixed-smoke--observe
                        'fit-width-resized view location)
                       (setq yunge-reader-fixed-smoke--phase
                             'fit-page)
                       (yunge-reader-fit-page)
                       (yunge-reader-fixed-smoke--continue))
                   (yunge-reader-fixed-smoke--continue))))
              ('fit-page
               (if (and
                    (yunge-reader-fixed-smoke--same-surface-p view)
                    (eq yunge-reader-zoom-mode 'fit-page)
                     (eq (yunge-reader-fixed-smoke--surface-zoom view)
                         'fit-page)
                    (yunge-reader-fixed-smoke--different-scale-p
                     yunge-reader-effective-scale
                     yunge-reader-fixed-smoke--fit-scale)
                    (yunge-reader-fixed-smoke--page-p location 1))
                   (progn
                     (setq yunge-reader-fixed-smoke--fit-scale
                           yunge-reader-effective-scale
                           yunge-reader-fixed-smoke--resize-bounds
                           (copy-tree
                             (yunge-reader-fixed-smoke--surface-bounds
                              view))
                           yunge-reader-fixed-smoke--phase
                           'fit-page-resize)
                     (yunge-reader-fixed-smoke--observe
                      'fit-page view location)
                     (set-frame-size nil 700 500 t)
                     (redisplay t)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('fit-page-resize
               (let ((bounds
                      (yunge-reader-fixed-smoke--surface-bounds view)))
                 (if (and
                      (yunge-reader-fixed-smoke--same-surface-p view)
                      (yunge-reader-fixed-smoke--bounds-settled-p view)
                      (numberp (alist-get 'height bounds))
                      (< (alist-get 'height bounds)
                         (alist-get
                          'height
                          yunge-reader-fixed-smoke--resize-bounds))
                      (< yunge-reader-effective-scale
                         yunge-reader-fixed-smoke--fit-scale)
                      (yunge-reader-fixed-smoke--page-p location 1))
                     (progn
                       (yunge-reader-fixed-smoke--observe
                        'fit-page-resized view location)
                       (setq yunge-reader-fixed-smoke--phase 'manual)
                       (yunge-reader-zoom-reset)
                       (yunge-reader-fixed-smoke--continue))
                   (yunge-reader-fixed-smoke--continue))))
              ('manual
               (if (and
                    (yunge-reader-fixed-smoke--same-surface-p view)
                    (eq yunge-reader-zoom-mode 'manual)
                    (= yunge-reader-scale 1.0)
                    (= yunge-reader-effective-scale 1.0)
                     (= (yunge-reader-fixed-smoke--surface-zoom view)
                        1.0)
                    (yunge-reader-fixed-smoke--page-p location 1))
                   (progn
                     (yunge-reader-fixed-smoke--observe
                      'manual view location)
                     (setq yunge-reader-fixed-smoke--phase 'scroll)
                     (yunge-reader-epub-next-screen)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('scroll
               (if (and (yunge-reader-fixed-smoke--page-p location 1)
                        (> (alist-get 'y location) 1.0))
                   (progn
                     (setq yunge-reader-fixed-smoke--scrolled
                           (copy-tree location)
                           yunge-reader-fixed-smoke--resize-bounds
                           (copy-tree
                             (yunge-reader-fixed-smoke--surface-bounds
                              view))
                           yunge-reader-fixed-smoke--resize-not-before
                           (+ (float-time) 0.5)
                           yunge-reader-fixed-smoke--phase
                           'manual-resize)
                     (yunge-reader-fixed-smoke--record
                      'scrolled location)
                     (yunge-reader-fixed-smoke--observe
                      'manual-scrolled view location)
                     (set-frame-size nil 760 600 t)
                     (redisplay t)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('manual-resize
               (let ((bounds
                      (yunge-reader-fixed-smoke--surface-bounds view)))
                 (if (and
                      (> (float-time)
                         yunge-reader-fixed-smoke--resize-not-before)
                      (yunge-reader-fixed-smoke--same-surface-p view)
                      (yunge-reader-fixed-smoke--bounds-settled-p view)
                      (numberp (alist-get 'width bounds))
                      (numberp (alist-get 'height bounds))
                      (> (alist-get 'width bounds)
                         (alist-get
                          'width
                          yunge-reader-fixed-smoke--resize-bounds))
                      (> (alist-get 'height bounds)
                         (alist-get
                          'height
                          yunge-reader-fixed-smoke--resize-bounds))
                      (eq yunge-reader-zoom-mode 'manual)
                      (= yunge-reader-effective-scale 1.0)
                       (= (yunge-reader-fixed-smoke--surface-zoom view)
                          1.0)
                      (yunge-reader-fixed-smoke--same-viewport-p
                       location yunge-reader-fixed-smoke--scrolled))
                     (progn
                       (yunge-reader-fixed-smoke--observe
                        'manual-resized view location)
                       (setq yunge-reader-fixed-smoke--phase
                             'manual-line)
                       (yunge-reader-epub-next-line)
                       (yunge-reader-fixed-smoke--continue))
                   (yunge-reader-fixed-smoke--continue))))
              ('manual-line
               (if (yunge-reader-fixed-smoke--same-viewport-p
                    location yunge-reader-fixed-smoke--scrolled)
                   (yunge-reader-fixed-smoke--continue)
                 (let ((delta
                        (- (alist-get 'y location)
                           (alist-get
                            'y yunge-reader-fixed-smoke--scrolled))))
                   (unless (and
                            (yunge-reader-fixed-smoke--page-p
                             location 1)
                            (< (abs (- delta 40.0)) 1.0))
                     (error
                      "Fixed viewport moved %.3f after resize" delta))
                   (setq yunge-reader-fixed-smoke--scrolled
                         (copy-tree location)
                         yunge-reader-fixed-smoke--phase 'hidden
                         yunge-reader-fixed-smoke--replacement
                         (get-buffer-create
                          " *fixed EPUB smoke replacement*"))
                   (yunge-reader-fixed-smoke--record
                    'manual-line location)
                   (yunge-reader-fixed-smoke--observe
                    'manual-line view location)
                   (switch-to-buffer
                    yunge-reader-fixed-smoke--replacement)
                   (yunge-reader-fixed-smoke--continue))))
              ('hidden
               (if (and
                    (yunge-reader-webview--surface-ready-p
                     (yunge-reader-fixed-smoke--surface view))
                    (eql (yunge-reader-fixed-smoke--surface-id view)
                         yunge-reader-fixed-smoke--surface-id)
                    (equal location
                           yunge-reader-fixed-smoke--scrolled))
                   (progn
                     (setq yunge-reader-fixed-smoke--phase 'reopened)
                     (switch-to-buffer
                      yunge-reader-fixed-smoke--buffer)
                     (yunge-reader-fixed-smoke--continue))
                 (yunge-reader-fixed-smoke--continue)))
              ('reopened
               (if (and
                    (yunge-reader-webview--surface-ready-p
                     (yunge-reader-fixed-smoke--surface view))
                    (numberp
                     (yunge-reader-fixed-smoke--surface-id view))
                    (eql (yunge-reader-fixed-smoke--surface-id view)
                         yunge-reader-fixed-smoke--surface-id)
                    (yunge-reader-fixed-smoke--same-viewport-p
                     location yunge-reader-fixed-smoke--scrolled))
                   (progn
                     (yunge-reader-fixed-smoke--record
                      'reopened location)
                     (yunge-reader-fixed-smoke--finish 'passed nil))
                 (yunge-reader-fixed-smoke--continue))))))))
    (error
     (yunge-reader-fixed-smoke--finish nil error-data))))

(defun yunge-reader-fixed-smoke--start-variant ()
  "Generate and open the next fixed-layout fixture."
  (condition-case error-data
      (progn
        (setq yunge-reader-fixed-smoke--variant
              (pop yunge-reader-fixed-smoke--remaining)
              yunge-reader-fixed-smoke--file
              (expand-file-name
               (concat yunge-reader-fixed-smoke--variant ".epub")
               (yunge-reader-graphical-smoke-context-temporary-root
                yunge-reader-fixed-smoke--context))
              yunge-reader-fixed-smoke--buffer nil
              yunge-reader-fixed-smoke--deadline (+ (float-time) 30)
              yunge-reader-fixed-smoke--phase 'ready
              yunge-reader-fixed-smoke--locations nil
              yunge-reader-fixed-smoke--observations nil
              yunge-reader-fixed-smoke--warnings nil
              yunge-reader-fixed-smoke--replacement nil
              yunge-reader-fixed-smoke--surface-id nil
              yunge-reader-fixed-smoke--fit-scale nil
              yunge-reader-fixed-smoke--resize-bounds nil
              yunge-reader-fixed-smoke--resize-not-before nil
              yunge-reader-fixed-smoke--scrolled nil)
        (yunge-reader-graphical-smoke-run-process
         yunge-reader-fixed-smoke--context
         yunge-reader-fixed-smoke--cargo
         (list
          "run"
          "--quiet"
          "--manifest-path"
          (yunge-reader-graphical-smoke-context-manifest
           yunge-reader-fixed-smoke--context)
          "--example"
          "fixed_epub_fixture"
          "--"
          "--variant"
          yunge-reader-fixed-smoke--variant
          yunge-reader-fixed-smoke--file))
        (set-frame-size nil 900 700 t)
        (make-frame-visible)
        (select-frame-set-input-focus (selected-frame))
        (setq yunge-reader-fixed-smoke--buffer
              (find-file-noselect yunge-reader-fixed-smoke--file))
        (switch-to-buffer yunge-reader-fixed-smoke--buffer)
        (yunge-reader-fixed-smoke--continue))
    (error
     (setq yunge-reader-fixed-smoke--exit-status 1
           yunge-reader-fixed-smoke--remaining nil)
     (yunge-reader-fixed-smoke--log
      "Fixed EPUB smoke setup failed: %S\n" error-data)
     (yunge-reader-fixed-smoke--stop-before 'exit))))

(defun yunge-reader-fixed-smoke--launch ()
  "Build the helper and launch every fixed-layout smoke variant."
  (condition-case error-data
      (progn
        (unless (display-graphic-p)
          (error "Fixed EPUB smoke requires a graphical Emacs"))
        (setq yunge-reader-fixed-smoke--cargo
              (yunge-reader-graphical-smoke-build-helper
               yunge-reader-fixed-smoke--context))
        (advice-add 'display-warning :around
                    #'yunge-reader-fixed-smoke--warning)
        (setq yunge-reader-fixed-smoke--remaining
              (copy-sequence yunge-reader-fixed-smoke--variants))
        (yunge-reader-fixed-smoke--start-variant))
    (error
     (setq yunge-reader-fixed-smoke--exit-status 1)
     (yunge-reader-fixed-smoke--log
      "Fixed EPUB smoke failed to start: %S\n" error-data)
     (yunge-reader-fixed-smoke--complete-run))))

(yunge-reader-fixed-smoke--launch)

;;; yunge-reader-fixed-epub-smoke.el ends here
