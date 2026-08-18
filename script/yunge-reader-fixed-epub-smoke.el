;;; yunge-reader-fixed-epub-smoke.el --- Smoke -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(defconst yunge-reader-fixed-smoke--root
  (file-name-as-directory
   (or (getenv "YUNGE_READER_ROOT")
       (error "YUNGE_READER_ROOT is not set"))))

(setq user-emacs-directory yunge-reader-fixed-smoke--root)
(startup-redirect-eln-cache
 (or (getenv "YUNGE_READER_FIXED_ELN_CACHE")
     (error "YUNGE_READER_FIXED_ELN_CACHE is not set")))
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(require 'yunge-reader-epub)

(defun yunge-reader-native-program ()
  "Use the isolated helper selected by the smoke launcher."
  (or (getenv "YUNGE_READER_NATIVE_PROGRAM")
      (error "YUNGE_READER_NATIVE_PROGRAM is not set")))

(defconst yunge-reader-fixed-smoke--variant
  (or (getenv "YUNGE_READER_FIXED_VARIANT")
      (error "YUNGE_READER_FIXED_VARIANT is not set")))

(unless (member yunge-reader-fixed-smoke--variant
                '("ltr" "rtl" "vertical-rl"))
  (error "Invalid fixed EPUB smoke variant: %S"
         yunge-reader-fixed-smoke--variant))

(defconst yunge-reader-fixed-smoke--file
  (or (getenv "YUNGE_READER_FIXED_FIXTURE")
      (error "YUNGE_READER_FIXED_FIXTURE is not set")))

(defconst yunge-reader-fixed-smoke--output
  (or (getenv "YUNGE_READER_FIXED_RESULT")
      (error "YUNGE_READER_FIXED_RESULT is not set")))

(defvar yunge-reader-fixed-smoke--buffer nil)
(defvar yunge-reader-fixed-smoke--deadline (+ (float-time) 30))
(defvar yunge-reader-fixed-smoke--phase 'ready)
(defvar yunge-reader-fixed-smoke--locations nil)
(defvar yunge-reader-fixed-smoke--warnings nil)
(defvar yunge-reader-fixed-smoke--replacement nil)
(defvar yunge-reader-fixed-smoke--surface-id nil)
(defvar yunge-reader-fixed-smoke--scrolled nil)

(defun yunge-reader-fixed-smoke--warning
    (original type message &rest arguments)
  "Record Reader MESSAGE, then call ORIGINAL with TYPE and ARGUMENTS."
  (when (eq type 'yunge-reader)
    (push message yunge-reader-fixed-smoke--warnings))
  (apply original type message arguments))

(defun yunge-reader-fixed-smoke--location ()
  "Return a copy of the current native location, or nil."
  (when-let* ((view yunge-reader-webview--buffer-view)
              (location (yunge-reader-webview--view-location view)))
    (copy-tree location)))

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

(defun yunge-reader-fixed-smoke--same-viewport-p (left right)
  "Return whether LEFT and RIGHT identify the same fixed viewport."
  (and (equal (alist-get 'href left) (alist-get 'href right))
       (< (abs (- (alist-get 'x left) (alist-get 'x right))) 0.5)
       (< (abs (- (alist-get 'y left) (alist-get 'y right))) 0.5)))

(defun yunge-reader-fixed-smoke--finish (value error-data)
  "Write VALUE and ERROR-DATA, then stop the isolated process."
  (with-temp-file yunge-reader-fixed-smoke--output
    (prin1
     (list :variant yunge-reader-fixed-smoke--variant
           :value value
           :error error-data
           :locations (nreverse yunge-reader-fixed-smoke--locations)
           :warnings (nreverse yunge-reader-fixed-smoke--warnings))
     (current-buffer)))
  (when (buffer-live-p yunge-reader-fixed-smoke--buffer)
    (kill-buffer yunge-reader-fixed-smoke--buffer))
  (when (buffer-live-p yunge-reader-fixed-smoke--replacement)
    (kill-buffer yunge-reader-fixed-smoke--replacement))
  (yunge-reader-webview-stop)
  (advice-remove 'display-warning
                 #'yunge-reader-fixed-smoke--warning)
  (run-at-time 0.5 nil #'kill-emacs))

(defun yunge-reader-fixed-smoke--poll ()
  "Advance the fixed-layout smoke state machine."
  (condition-case error-data
      (cond
       ((> (float-time) yunge-reader-fixed-smoke--deadline)
        (error "Fixed EPUB smoke timed out in %S"
               yunge-reader-fixed-smoke--phase))
       ((not (buffer-live-p yunge-reader-fixed-smoke--buffer))
        (run-at-time 0.1 nil #'yunge-reader-fixed-smoke--poll))
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
                        (yunge-reader-webview--surface-ready-p view)
                        (yunge-reader-fixed-smoke--page-p location 1))
                   (progn
                     (unless (eq yunge-reader-zoom-mode 'fit-page)
                       (error "Fixed EPUB did not start at fit page"))
                     (unless (numberp yunge-reader-effective-scale)
                       (error "Fixed EPUB has no effective scale"))
                     (yunge-reader-fixed-smoke--record 'initial location)
                     (setq yunge-reader-fixed-smoke--phase 'next)
                     (yunge-reader-epub-next-page)
                     (run-at-time
                      0.1 nil #'yunge-reader-fixed-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-fixed-smoke--poll)))
              ('next
               (if (yunge-reader-fixed-smoke--page-p location 2)
                   (progn
                     (yunge-reader-fixed-smoke--record 'next location)
                     (setq yunge-reader-fixed-smoke--phase 'previous)
                     (yunge-reader-epub-previous-page)
                     (run-at-time
                      0.1 nil #'yunge-reader-fixed-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-fixed-smoke--poll)))
              ('previous
               (if (yunge-reader-fixed-smoke--page-p location 1)
                   (progn
                     (yunge-reader-fixed-smoke--record
                      'previous location)
                     (setq yunge-reader-fixed-smoke--phase 'zoom)
                     (yunge-reader-zoom-reset)
                     (run-at-time
                      0.1 nil #'yunge-reader-fixed-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-fixed-smoke--poll)))
              ('zoom
               (if (and (eq yunge-reader-zoom-mode 'manual)
                        (= yunge-reader-scale 1.0)
                        (= yunge-reader-effective-scale 1.0)
                        (= (yunge-reader-webview--view-surface-zoom view)
                           1.0)
                        (yunge-reader-fixed-smoke--page-p location 1))
                   (progn
                     (setq yunge-reader-fixed-smoke--phase 'scroll)
                     (yunge-reader-epub-next-screen)
                     (run-at-time
                      0.1 nil #'yunge-reader-fixed-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-fixed-smoke--poll)))
              ('scroll
               (if (and (yunge-reader-fixed-smoke--page-p location 1)
                        (> (alist-get 'y location) 1.0))
                   (progn
                     (setq yunge-reader-fixed-smoke--scrolled
                           (copy-tree location)
                           yunge-reader-fixed-smoke--surface-id
                           (yunge-reader-webview--view-id view)
                           yunge-reader-fixed-smoke--phase 'hidden
                           yunge-reader-fixed-smoke--replacement
                           (get-buffer-create
                            " *fixed EPUB smoke replacement*"))
                     (yunge-reader-fixed-smoke--record
                      'scrolled location)
                     (switch-to-buffer
                      yunge-reader-fixed-smoke--replacement)
                     (run-at-time
                      0.1 nil #'yunge-reader-fixed-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-fixed-smoke--poll)))
              ('hidden
               (if (and (eq (yunge-reader-webview--view-surface-state view)
                            'detached)
                        (null (yunge-reader-webview--view-id view))
                        (equal location
                               yunge-reader-fixed-smoke--scrolled))
                   (progn
                     (setq yunge-reader-fixed-smoke--phase 'reopened)
                     (switch-to-buffer
                      yunge-reader-fixed-smoke--buffer)
                     (run-at-time
                      0.1 nil #'yunge-reader-fixed-smoke--poll))
                 (run-at-time
                  0.1 nil #'yunge-reader-fixed-smoke--poll)))
              ('reopened
               (if (and (yunge-reader-webview--surface-ready-p view)
                        (numberp (yunge-reader-webview--view-id view))
                        (not (eql (yunge-reader-webview--view-id view)
                                  yunge-reader-fixed-smoke--surface-id))
                        (yunge-reader-fixed-smoke--same-viewport-p
                         location yunge-reader-fixed-smoke--scrolled))
                   (progn
                     (yunge-reader-fixed-smoke--record
                      'reopened location)
                     (yunge-reader-fixed-smoke--finish 'passed nil))
                 (run-at-time
                  0.1 nil #'yunge-reader-fixed-smoke--poll))))))))
    (error
     (yunge-reader-fixed-smoke--finish nil error-data))))

(advice-add 'display-warning :around
            #'yunge-reader-fixed-smoke--warning)
(set-frame-size nil 900 700 t)
(set-frame-parameter nil 'visibility nil)
(setq yunge-reader-fixed-smoke--buffer
      (find-file-noselect yunge-reader-fixed-smoke--file))
(switch-to-buffer yunge-reader-fixed-smoke--buffer)
(run-at-time 0.1 nil #'yunge-reader-fixed-smoke--poll)

;;; yunge-reader-fixed-epub-smoke.el ends here
