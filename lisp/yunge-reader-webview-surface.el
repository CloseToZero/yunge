;;; yunge-reader-webview-surface.el --- Surfaces -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-reader-webview-renderer)
(require 'yunge-reader-webview-service)
(require 'yunge-reader-webview-view)

(declare-function yunge-reader-webview--destroy-view
                  "yunge-reader-webview" (view &optional complete))
(declare-function yunge-reader-webview--maybe-finish-view-destroy
                  "yunge-reader-webview" (view))
(declare-function yunge-reader-webview--resolved-scroll-bar-mode
                  "yunge-reader-webview" (view window))
(declare-function yunge-reader-webview--set-buffer-message
                  "yunge-reader-webview" (view message))
(declare-function yunge-reader-webview--set-view-selection
                  "yunge-reader-webview" (view selection))
(declare-function yunge-reader-webview--sync-view
                  "yunge-reader-webview" (view))

(defcustom yunge-reader-webview-open-timeout 5.0
  "Seconds allowed for the WebView renderer shell to become ready."
  :type 'number
  :group 'yunge-reader)

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

(defun yunge-reader-webview--surface-current-p (view id)
  "Return whether ID is VIEW's current native surface."
  (and (integerp id)
       (eql id (yunge-reader-webview--view-id view))
       (eq (gethash id yunge-reader-webview--views) view)))

(defun yunge-reader-webview--send-latest-bounds (view)
  "Send VIEW's latest requested bounds unless one is in flight."
  (when (and (yunge-reader-webview--surface-created-p view)
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

(defun yunge-reader-webview--cancel-open-timer (view)
  "Cancel VIEW's renderer readiness timer."
  (when-let* ((timer (yunge-reader-webview--view-open-timer view)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (yunge-reader-webview--view-open-timer view) nil)))

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
        (created (yunge-reader-webview--surface-created-p view)))
    (yunge-reader-webview--cancel-open-timer view)
    (when id
      (remhash id yunge-reader-webview--views))
    (yunge-reader-webview--set-surface-state view 'detached)
    (setf (yunge-reader-webview--view-id view) nil
          (yunge-reader-webview--view-window view) nil
          (yunge-reader-webview--view-native-focused view) nil
          (yunge-reader-webview--view-focus-release-pending view) nil
          (yunge-reader-webview--view-surface-appearance view) nil
          (yunge-reader-webview--view-surface-style view) nil
          (yunge-reader-webview--view-surface-zoom view) nil
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
          (yunge-reader-webview--view-surface-appearance view) nil
          (yunge-reader-webview--view-surface-zoom view) nil
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
    (yunge-reader-webview--set-surface-state view 'failed)
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
  (when (and (memq (yunge-reader-webview--view-surface-state view)
                   '(native-ready opening))
             (not (yunge-reader-webview--view-destroyed view))
             (yunge-reader-webview--view-publication view))
    (let ((id (yunge-reader-webview--view-id view)))
      (yunge-reader-webview--set-surface-state view 'opening)
      (setf (yunge-reader-webview--view-surface-style view)
            (and (yunge-reader-webview--view-style view)
                 (copy-tree
                  (yunge-reader-webview--view-style view)))
            (yunge-reader-webview--view-surface-appearance view)
            (yunge-reader-webview--view-appearance view)
            (yunge-reader-webview--view-surface-zoom view)
            (yunge-reader-webview--view-zoom view)
            (yunge-reader-webview--view-surface-scroll-bar-mode view)
            (yunge-reader-webview--view-scroll-bar-mode view))
      (yunge-reader-webview--open-view-publication
       view
       (yunge-reader-webview--view-publication view)
       (apply-partially
        #'yunge-reader-webview--open-complete view id)
       (yunge-reader-webview--view-location view)
       (yunge-reader-webview--view-appearance view)
       (yunge-reader-webview--view-style view)
       (yunge-reader-webview--view-zoom view)
       (yunge-reader-webview--view-scroll-bar-mode view)))))

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
    (yunge-reader-webview--set-surface-state view 'detached)
    (setf (yunge-reader-webview--view-id view) nil
          (yunge-reader-webview--view-window view) nil
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
    (yunge-reader-webview--set-surface-state view 'native-ready)
    (setf (yunge-reader-webview--view-bounds view)
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
      (yunge-reader-webview--set-surface-state view 'creating)
      (setf (yunge-reader-webview--view-id view) id
            (yunge-reader-webview--view-window view) window
            (yunge-reader-webview--view-native-focused view) nil
            (yunge-reader-webview--view-focus-release-pending view) nil
            (yunge-reader-webview--view-surface-appearance view) nil
            (yunge-reader-webview--view-surface-style view) nil
            (yunge-reader-webview--view-surface-zoom view) nil
            (yunge-reader-webview--view-scroll-bar-mode view)
            bar-mode
            (yunge-reader-webview--view-surface-scroll-bar-mode view) nil
            (yunge-reader-webview--view-outline-error view) nil
            (yunge-reader-webview--view-requested-bounds view)
            (yunge-reader-webview--window-bounds window))
      (puthash id view yunge-reader-webview--views)
      (yunge-reader-webview--request-create view)))
  view)

(provide 'yunge-reader-webview-surface)

;;; yunge-reader-webview-surface.el ends here
