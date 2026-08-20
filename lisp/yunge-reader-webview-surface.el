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
(declare-function yunge-reader-webview--resolved-appearance
                  "yunge-reader-webview" (view window))
(declare-function yunge-reader-webview--set-buffer-message
                  "yunge-reader-webview" (view message))
(declare-function yunge-reader-webview--set-view-selection
                  "yunge-reader-webview" (view selection))
(declare-function yunge-reader-webview--sync-view
                  "yunge-reader-webview" (view))

(defcustom yunge-reader-webview-open-timeout 5.0
  "Seconds allowed for the WebView renderer to finish opening an EPUB.
The native backends queue the open script until their renderer page is ready;
this deadline covers EPUB parsing and the final publication-ready callback."
  :type 'number
  :group 'yunge-reader)

(defun yunge-reader-webview--frame-handle (frame)
  "Return FRAME's platform parent identifier as a positive integer.
Windows exposes the frame HWND.  macOS exposes an opaque Emacs frame ID; the
in-process module binds the selected key window when it creates the surface."
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
  "Return frame-relative WebView body bounds for WINDOW."
  (pcase-let ((`(,left ,top ,right ,bottom)
               (window-body-pixel-edges window)))
    (let ((width (- right left))
          (height (- bottom top)))
      `((x . ,left)
        (y . ,top)
        (width . ,width)
        (height . ,height)))))

(defun yunge-reader-webview--frame-bounds (frame)
  "Return FRAME's outer position and native content size, if placed."
  (pcase-let ((`(,left ,top) (frame-position frame)))
    (when (and (integerp left) (integerp top))
      `((x . ,left)
        (y . ,top)
        (width . ,(frame-pixel-width frame))
        (height . ,(frame-pixel-height frame))))))

(defun yunge-reader-webview--surface-current-p (view id)
  "Return whether ID names one registered native surface of VIEW."
  (and (integerp id)
       (eq (gethash id yunge-reader-webview--views) view)
       (or (yunge-reader-webview--view-surface-for-id view id)
           (let ((surface (yunge-reader-webview--view-surface view)))
             (and surface
                  (eql id
                       (yunge-reader-webview--surface-id surface)))))))

(defun yunge-reader-webview--send-latest-bounds (view &optional surface)
  "Send SURFACE's latest requested bounds unless one is in flight.
SURFACE defaults to VIEW's active presentation."
  (when-let* ((surface (or surface
                           (yunge-reader-webview--view-surface view)))
              ((yunge-reader-webview--surface-created-p surface))
              ((not (yunge-reader-webview--view-destroyed view)))
              ((not
                (yunge-reader-webview--surface-bounds-pending surface))))
    (let ((bounds
           (yunge-reader-webview--surface-requested-bounds surface)))
      (unless (equal bounds
                     (yunge-reader-webview--surface-bounds surface))
        (let ((id (yunge-reader-webview--surface-id surface)))
          (setf (yunge-reader-webview--surface-bounds-pending surface)
                t)
          (yunge-reader-webview--request
           "view-bounds"
           `((view . ,id) (bounds . ,bounds))
           (lambda (_result error-data)
             (when (yunge-reader-webview--surface-current-p view id)
               (setf (yunge-reader-webview--surface-bounds-pending
                      surface)
                     nil)
               (unless error-data
                 (setf (yunge-reader-webview--surface-bounds surface)
                       bounds))
               (when error-data
                 (display-warning
                  'yunge-reader
                  (error-message-string error-data)
                  :warning))
               (yunge-reader-webview--send-latest-bounds
                view surface)))))))))

(defun yunge-reader-webview--cancel-open-timer (surface)
  "Cancel SURFACE's renderer readiness timer."
  (when-let* ((timer (yunge-reader-webview--surface-open-timer surface)))
    (when (timerp timer)
      (cancel-timer timer))
    (setf (yunge-reader-webview--surface-open-timer surface) nil)))

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
    (view &optional complete surface)
  "Release one native SURFACE while retaining VIEW's logical state.
SURFACE defaults to VIEW's active presentation."
  (let* ((surface (or surface
                      (yunge-reader-webview--view-surface view)))
         (id (and surface
                  (yunge-reader-webview--surface-id surface)))
         (was-active
          (eq surface (yunge-reader-webview--view-surface view)))
         (created
          (yunge-reader-webview--surface-created-p surface)))
    (when surface
      (yunge-reader-webview--cancel-open-timer surface))
    (when id
      (remhash id yunge-reader-webview--views))
    (when surface
      (yunge-reader-webview--unregister-surface view surface))
    (when surface
      (setf (yunge-reader-webview--surface-selection surface) nil))
    (when (or (null surface) was-active)
      (yunge-reader-webview--set-view-selection view nil))
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

(defun yunge-reader-webview--open-complete
    (view id _result error-data)
  "Finish VIEW surface ID's attempt to attach its publication."
  (let ((surface (yunge-reader-webview--view-surface-for-id view id)))
    (when (yunge-reader-webview--surface-current-p view id)
      (when error-data
        (setf (yunge-reader-webview--surface-style surface) nil
              (yunge-reader-webview--surface-appearance surface) nil
              (yunge-reader-webview--surface-zoom surface) nil
              (yunge-reader-webview--surface-scroll-bar-mode surface)
              nil))
      (cond
       (error-data
        (yunge-reader-webview--set-surface-state surface 'failed)
        (yunge-reader-webview--set-buffer-message
         view (error-message-string error-data))
        (display-warning
         'yunge-reader (error-message-string error-data) :warning))
       (t
        (yunge-reader-webview--set-buffer-message
         view "Opening the EPUB text start...")
        (yunge-reader-webview--cancel-open-timer surface)
        (when (eq (yunge-reader-webview--surface-state surface) 'opening)
          (setf (yunge-reader-webview--surface-open-timer surface)
                (run-at-time
                 yunge-reader-webview-open-timeout nil
                 #'yunge-reader-webview--open-watchdog view id))))))))

(defun yunge-reader-webview--open-watchdog (view id)
  "Fail VIEW surface ID when its renderer does not finish opening."
  (when-let* ((surface
               (yunge-reader-webview--view-surface-for-id view id)))
    (setf (yunge-reader-webview--surface-open-timer surface) nil)
    (when (and (eq (yunge-reader-webview--surface-state surface) 'opening)
               (not (yunge-reader-webview--view-destroyed view)))
      (let ((message "Timed out while opening the EPUB renderer"))
        (yunge-reader-webview--set-surface-state surface 'failed)
        (yunge-reader-webview--set-buffer-message view message)
        (display-warning 'yunge-reader message :warning)))))

(defun yunge-reader-webview--try-open-publication (view &optional surface)
  "Attach VIEW's publication to SURFACE's native renderer.
SURFACE defaults to VIEW's active presentation."
  (when-let* ((surface (or surface
                           (yunge-reader-webview--view-surface view))))
    (setf (yunge-reader-webview--surface-open-timer surface) nil)
    (when (and (memq (yunge-reader-webview--surface-state surface)
                     '(native-ready opening))
               (not (yunge-reader-webview--view-destroyed view))
               (yunge-reader-webview--view-publication view))
      (let ((id (yunge-reader-webview--surface-id surface)))
        (yunge-reader-webview--set-surface-state surface 'opening)
        (setf (yunge-reader-webview--surface-style surface)
              (and (yunge-reader-webview--view-style view)
                   (copy-tree
                    (yunge-reader-webview--view-style view)))
              (yunge-reader-webview--surface-appearance surface)
              (copy-tree
               (or (yunge-reader-webview--surface-desired-appearance
                    surface)
                   (yunge-reader-webview--view-appearance view)))
              (yunge-reader-webview--surface-zoom surface)
              (yunge-reader-webview--view-zoom view)
              (yunge-reader-webview--surface-scroll-bar-mode surface)
              (or (yunge-reader-webview--surface-desired-scroll-bar-mode
                   surface)
                  (yunge-reader-webview--view-scroll-bar-mode view)))
        (let ((active (yunge-reader-webview--view-surface view)))
          (unwind-protect
              (progn
                (setf (yunge-reader-webview--view-surface view) surface)
                (yunge-reader-webview--open-view-publication
                 view
                 (yunge-reader-webview--view-publication view)
                 (apply-partially
                  #'yunge-reader-webview--open-complete view id)
                 (or (yunge-reader-webview--surface-location surface)
                     (yunge-reader-webview--view-location view))
                 (copy-tree
                  (or (yunge-reader-webview--surface-desired-appearance
                       surface)
                      (yunge-reader-webview--view-appearance view)))
                 (yunge-reader-webview--view-style view)
                 (yunge-reader-webview--view-zoom view)
                 (or (yunge-reader-webview--surface-desired-scroll-bar-mode
                      surface)
                     (yunge-reader-webview--view-scroll-bar-mode view))))
            (setf (yunge-reader-webview--view-surface view) active)))))))

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
    (when-let* ((surface
                 (yunge-reader-webview--view-surface-for-id view id)))
      (yunge-reader-webview--unregister-surface view surface))
    (yunge-reader-webview--set-buffer-message
     view (error-message-string error-data))
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)
    (unless (yunge-reader-webview--view-persistent view)
      (yunge-reader-webview--destroy-view view)))
   (t
    (let ((surface
           (yunge-reader-webview--view-surface-for-id view id)))
      (yunge-reader-webview--set-surface-state surface 'native-ready)
      (setf (yunge-reader-webview--surface-bounds surface)
            created-bounds))
    (yunge-reader-webview--sync-view view)
    (when (and (yunge-reader-webview--surface-current-p view id)
               (yunge-reader-webview--view-publication view))
      (yunge-reader-webview--try-open-publication
       view (yunge-reader-webview--view-surface-for-id view id))))))

(defun yunge-reader-webview--request-create (view &optional surface)
  "Ask the helper to create VIEW's native SURFACE."
  (let* ((surface (or surface
                      (yunge-reader-webview--view-surface view)))
         (id (yunge-reader-webview--surface-id surface))
         (window (yunge-reader-webview--surface-window surface))
         (frame (window-frame window))
         (bounds
          (copy-tree
           (yunge-reader-webview--surface-requested-bounds surface))))
    (yunge-reader-webview--request
     "view-create"
     `((view . ,id)
       (renderer-url
        . ,(or (yunge-reader-webview--view-renderer-url view)
               (error "EPUB view has no broker renderer URL")))
       (parent . ,(yunge-reader-webview--frame-handle frame))
       (frame . ,(yunge-reader-webview--frame-bounds frame))
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
  (when (yunge-reader-webview--view-surface-for-window view window)
    (error "EPUB view already owns a native surface for this window"))
  (unless (yunge-reader-webview--view-destroyed view)
    (let ((id (cl-incf yunge-reader-webview--next-view-id))
          (appearance
           (yunge-reader-webview--resolved-appearance view window))
          (bar-mode
           (yunge-reader-webview--resolved-scroll-bar-mode view window)))
      (yunge-reader-webview--set-view-selection view nil)
      (let ((surface
             (yunge-reader-webview--make-surface
             :id id
              :window window
              :desired-appearance (copy-tree appearance)
              :desired-scroll-bar-mode bar-mode
              :location
              (and (yunge-reader-webview--view-location view)
                   (copy-tree
                    (yunge-reader-webview--view-location view)))
              :requested-bounds
              (yunge-reader-webview--window-bounds window))))
        (yunge-reader-webview--register-surface view surface)
        (when (or (null (yunge-reader-webview--view-surface view))
                  (eq window
                      (with-current-buffer
                          (yunge-reader-webview--view-buffer view)
                        (yunge-reader--presentation-window))))
          (setf (yunge-reader-webview--view-surface view) surface))
        (when (eq (yunge-reader-webview--view-surface view) surface)
          (setf (yunge-reader-webview--view-appearance view)
                (copy-tree appearance)
                (yunge-reader-webview--view-scroll-bar-mode view)
                bar-mode))
        (setf (yunge-reader-webview--view-outline-error view) nil)
        (puthash id view yunge-reader-webview--views)
        (yunge-reader-webview--request-create view surface))))
  view)

(provide 'yunge-reader-webview-surface)

;;; yunge-reader-webview-surface.el ends here
