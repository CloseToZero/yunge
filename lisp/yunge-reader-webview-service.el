;;; yunge-reader-webview-service.el --- Service -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-reader-native)
(require 'yunge-reader-transport)
(require 'yunge-reader-webview-events)
(require 'yunge-reader-webview-protocol)

(defcustom yunge-reader-webview-stop-timeout 1.0
  "Seconds allowed for graceful WebView helper shutdown."
  :type 'number
  :group 'yunge-reader)

(defconst yunge-reader-webview--log-buffer-name
  "*Yunge Reader WebView log*"
  "Name of the WebView helper diagnostic buffer.")

(defvar yunge-reader-webview--process nil
  "Running WebView helper process, or nil.")

(defvar yunge-reader-webview--transport nil
  "NDJSON transport bound to the current WebView helper process.")

(defvar yunge-reader-webview--force-stop-timer nil
  "Timer enforcing the graceful WebView shutdown deadline.")

(defvar yunge-reader-webview-service-stopped-hook nil
  "Hook run after the current WebView helper service stops.")

(defun yunge-reader-webview--program-available-p ()
  "Return whether the native helper executable is available."
  (file-executable-p (yunge-reader-native-program)))

(defun yunge-reader-webview--cancel-force-stop ()
  "Cancel the outstanding forced-stop timer."
  (when (timerp yunge-reader-webview--force-stop-timer)
    (cancel-timer yunge-reader-webview--force-stop-timer))
  (setq yunge-reader-webview--force-stop-timer nil))

(defun yunge-reader-webview--record-ready (process message)
  "Record WebView readiness metadata from MESSAGE on PROCESS."
  (process-put process 'yunge-reader-webview-available
               (alist-get 'available message))
  (process-put process 'yunge-reader-webview-version
               (alist-get 'version message))
  (process-put process 'yunge-reader-webview-message
               (alist-get 'message message)))

(defun yunge-reader-webview--invalid-output (process _error-data)
  "Stop current WebView PROCESS after invalid protocol output."
  (when (eq process yunge-reader-webview--process)
    (yunge-reader-webview-stop t)))

(defun yunge-reader-webview--make-transport ()
  "Return a fresh transport for one WebView helper process."
  (yunge-reader-transport--make-session
   :label "Yunge Reader WebView"
   :validate-ready #'yunge-reader-webview--validate-ready
   :ready-function #'yunge-reader-webview--record-ready
   :event-function #'yunge-reader-webview--handle-event
   :response-error-function #'yunge-reader-webview--response-error
   :invalid-output-function #'yunge-reader-webview--invalid-output))

(defun yunge-reader-webview--handle-message (process message)
  "Handle one parsed WebView MESSAGE from PROCESS."
  (yunge-reader-transport--handle-message
   yunge-reader-webview--transport process message))

(defun yunge-reader-webview--fail-callbacks (reason)
  "Complete all pending WebView callbacks with REASON."
  (when yunge-reader-webview--transport
    (yunge-reader-transport--fail
     yunge-reader-webview--transport (list 'error reason))))

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
      (run-hooks 'yunge-reader-webview-service-stopped-hook)
      (unless intentional
        (display-warning
         'yunge-reader
         "Yunge Reader WebView service stopped unexpectedly"
         :warning)))))

;;;###autoload
(defun yunge-reader-webview-start ()
  "Start the platform WebView helper and return its process."
  (interactive)
  (unless (memq system-type '(windows-nt darwin))
    (user-error
     "Yunge Reader EPUB supports WebView2 on Windows and WKWebView on macOS"))
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
      (setq yunge-reader-webview--transport
            (yunge-reader-webview--make-transport))
      (let ((process
             (make-process
              :name "yunge-reader-webview"
              :command (list (yunge-reader-native-program) "--webview")
              :connection-type 'pipe
              :coding 'utf-8-unix
              :noquery t
              :stderr log
              :filter #'yunge-reader-transport--filter
              :sentinel #'yunge-reader-webview--sentinel)))
        (yunge-reader-transport--bind
         yunge-reader-webview--transport process)
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
  (let ((process (yunge-reader-webview-start)))
    (yunge-reader-transport--request
     yunge-reader-webview--transport process operation parameters complete)))

;;;###autoload
(defun yunge-reader-webview-stop (&optional force)
  "Stop the WebView helper.
Without FORCE, request graceful shutdown and enforce a deadline."
  (interactive "P")
  (if (not (process-live-p yunge-reader-webview--process))
      (progn
        (setq yunge-reader-webview--process nil)
        (run-hooks 'yunge-reader-webview-service-stopped-hook)
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

(defun yunge-reader-webview--shutdown-for-emacs-exit ()
  "Terminate the WebView helper without delaying Emacs exit."
  (when (process-live-p yunge-reader-webview--process)
    (process-put yunge-reader-webview--process
                 'yunge-reader-webview-intentional-stop t)
    (delete-process yunge-reader-webview--process)))

(add-hook 'kill-emacs-hook
          #'yunge-reader-webview--shutdown-for-emacs-exit)

(provide 'yunge-reader-webview-service)

;;; yunge-reader-webview-service.el ends here
