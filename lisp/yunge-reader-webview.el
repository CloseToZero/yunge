;;; yunge-reader-webview.el --- WebView spike -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'yunge-reader)
(require 'yunge-reader-native)

(defcustom yunge-reader-webview-stop-timeout 1.0
  "Seconds allowed for graceful WebView helper shutdown."
  :type 'number
  :group 'yunge-reader)

(defconst yunge-reader-webview-protocol-version 1
  "WebView protocol version understood by this client.")

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
  "Live WebView records indexed by logical view identifier.")

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
  destroyed
  bounds
  requested-bounds
  bounds-pending)

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
          '("view-bounds" "view-create" "view-destroy"
            "view-focus" "view-info" "view-status"
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
     'error
     (or (alist-get 'message object)
         "The Yunge Reader WebView helper failed"))))

(defun yunge-reader-webview--handle-message (process message)
  "Handle one parsed WebView MESSAGE from PROCESS."
  (if (not (process-get process 'yunge-reader-webview-ready))
      (progn
        (yunge-reader-webview--validate-ready message)
        (process-put process 'yunge-reader-webview-ready t)
        (process-put process 'yunge-reader-webview-available
                     (alist-get 'available message))
        (process-put process 'yunge-reader-webview-version
                     (alist-get 'version message))
        (process-put process 'yunge-reader-webview-message
                     (alist-get 'message message))
        (yunge-reader-webview--flush-outbound process))
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
                 (yunge-reader-webview--response-error message))))))

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
            #'yunge-reader-webview--sync-views))

(defun yunge-reader-webview--remove-hooks ()
  "Remove native view synchronization hooks."
  (remove-hook 'window-size-change-functions
               #'yunge-reader-webview--sync-views)
  (remove-hook 'window-state-change-functions
               #'yunge-reader-webview--sync-views)
  (remove-hook 'window-buffer-change-functions
               #'yunge-reader-webview--sync-views))

(defun yunge-reader-webview--send-latest-bounds (view)
  "Send VIEW's latest requested bounds unless one is in flight."
  (when (and (yunge-reader-webview--view-created view)
             (not (yunge-reader-webview--view-destroyed view))
             (not (yunge-reader-webview--view-bounds-pending view)))
    (let ((bounds (yunge-reader-webview--view-requested-bounds view)))
      (unless (equal bounds (yunge-reader-webview--view-bounds view))
        (setf (yunge-reader-webview--view-bounds-pending view) t)
        (yunge-reader-webview--request
         "view-bounds"
         `((view . ,(yunge-reader-webview--view-id view))
           (bounds . ,bounds))
         (lambda (_result error-data)
           (setf (yunge-reader-webview--view-bounds-pending view) nil)
           (unless error-data
             (setf (yunge-reader-webview--view-bounds view) bounds))
           (when error-data
             (display-warning
              'yunge-reader
              (error-message-string error-data)
              :warning))
           (yunge-reader-webview--send-latest-bounds view)))))))

(defun yunge-reader-webview--sync-view (view)
  "Synchronize native VIEW with its tracked Emacs window."
  (let ((window (yunge-reader-webview--view-window view))
        (buffer (yunge-reader-webview--view-buffer view)))
    (if (and (window-live-p window)
             (buffer-live-p buffer)
             (eq (window-buffer window) buffer))
        (progn
          (setf (yunge-reader-webview--view-requested-bounds view)
                (yunge-reader-webview--window-bounds window))
          (yunge-reader-webview--send-latest-bounds view))
      (yunge-reader-webview--destroy-view view))))

(defun yunge-reader-webview--sync-views (&rest _ignored)
  "Synchronize every native view after an Emacs window change."
  (let (views)
    (maphash
     (lambda (_id view)
       (push view views))
     yunge-reader-webview--views)
    (dolist (view views)
      (yunge-reader-webview--sync-view view))))

(defun yunge-reader-webview--destroy-view (view)
  "Destroy native VIEW and forget its Emacs ownership."
  (unless (yunge-reader-webview--view-destroyed view)
    (setf (yunge-reader-webview--view-destroyed view) t)
    (remhash (yunge-reader-webview--view-id view)
             yunge-reader-webview--views)
    (when (and (yunge-reader-webview--view-created view)
               (process-live-p yunge-reader-webview--process))
      (yunge-reader-webview--request
       "view-destroy"
       `((view . ,(yunge-reader-webview--view-id view)))
       (lambda (_result _error-data))))
    (when (zerop (hash-table-count yunge-reader-webview--views))
      (yunge-reader-webview--remove-hooks))))

(defun yunge-reader-webview--forget-all-views ()
  "Forget all native views without sending protocol messages."
  (maphash
   (lambda (_id view)
     (setf (yunge-reader-webview--view-destroyed view) t))
   yunge-reader-webview--views)
  (clrhash yunge-reader-webview--views)
  (yunge-reader-webview--remove-hooks))

(defun yunge-reader-webview--kill-buffer ()
  "Destroy the native view owned by the current buffer."
  (when yunge-reader-webview--buffer-view
    (yunge-reader-webview--destroy-view
     yunge-reader-webview--buffer-view)))

(defun yunge-reader-webview--create-complete
    (view _result error-data)
  "Complete creation of VIEW with ERROR-DATA when it failed."
  (if error-data
      (progn
        (yunge-reader-webview--destroy-view view)
        (let ((buffer (yunge-reader-webview--view-buffer view)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (error-message-string error-data) "\n")))))
        (display-warning
         'yunge-reader (error-message-string error-data) :warning))
    (setf (yunge-reader-webview--view-created view) t)
    (if (yunge-reader-webview--view-destroyed view)
        (when (process-live-p yunge-reader-webview--process)
          (yunge-reader-webview--request
           "view-destroy"
           `((view . ,(yunge-reader-webview--view-id view)))
           (lambda (_value _error))))
      (setf (yunge-reader-webview--view-bounds view)
            (yunge-reader-webview--view-requested-bounds view))
      (yunge-reader-webview--sync-view view))))

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
         (frame (window-frame window))
         (id (cl-incf yunge-reader-webview--next-view-id))
         (buffer
          (generate-new-buffer
           (format "*Yunge Reader WebView %d*" id)))
         (bounds (yunge-reader-webview--window-bounds window))
         (view
          (yunge-reader-webview--make-view
           :id id
           :window window
           :buffer buffer
           :requested-bounds bounds)))
    (with-current-buffer buffer
      (yunge-reader-webview-spike-mode)
      (setq yunge-reader-webview--buffer-view view)
      (let ((inhibit-read-only t))
        (insert "Creating native WebView...\n")))
    (set-window-buffer window buffer)
    (puthash id view yunge-reader-webview--views)
    (yunge-reader-webview--install-hooks)
    (yunge-reader-webview--request
     "view-create"
     `((view . ,id)
       (parent . ,(yunge-reader-webview--frame-handle frame))
       (bounds . ,bounds)
       (visible . t))
     (apply-partially #'yunge-reader-webview--create-complete view))
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
