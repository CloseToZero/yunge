;;; yunge-reader-webview-service.el --- Service -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-reader-native)
(require 'yunge-reader-transport)
(require 'yunge-reader-webview-events)
(require 'yunge-reader-webview-protocol)

(declare-function yunge-reader-module-pump "yunge-reader-module" ())
(declare-function yunge-reader-module-request
                  "yunge-reader-module" (line))
(declare-function yunge-reader-module-running-p
                  "yunge-reader-module" ())
(declare-function yunge-reader-module-start
                  "yunge-reader-module" (pipe-process))
(declare-function yunge-reader-module-stop "yunge-reader-module" ())

(defcustom yunge-reader-webview-stop-timeout 1.0
  "Seconds allowed for graceful WebView module shutdown."
  :type 'number
  :group 'yunge-reader)

(defvar yunge-reader-webview--process nil
  "Pipe process connected to the in-process WebView module, or nil.")

(defvar yunge-reader-webview--transport nil
  "NDJSON transport bound to the current WebView module pipe.")

(defvar yunge-reader-webview--force-stop-timer nil
  "Timer enforcing the graceful WebView module shutdown deadline.")

(defvar yunge-reader-webview-service-stopped-hook nil
  "Hook run after the current WebView module service stops.")

(defun yunge-reader-webview--program-available-p ()
  "Return whether the native WebView module is available."
  (file-regular-p (yunge-reader-native-module-file)))

(defun yunge-reader-webview--ensure-module ()
  "Load the native WebView module if necessary."
  (unless (fboundp 'yunge-reader-module-start)
    (module-load (yunge-reader-native-module-file)))
  (unless (and (fboundp 'yunge-reader-module-start)
               (fboundp 'yunge-reader-module-request)
               (fboundp 'yunge-reader-module-pump)
               (fboundp 'yunge-reader-module-stop))
    (error "Yunge Reader WebView module did not define its API")))

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
  "Return a fresh transport for one WebView module session."
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
      (when (and (fboundp 'yunge-reader-module-running-p)
                 (yunge-reader-module-running-p))
        (ignore-errors (yunge-reader-module-stop)))
      (setq yunge-reader-webview--process nil)
      (yunge-reader-webview--cancel-force-stop)
      (yunge-reader-webview--fail-callbacks
       "The Yunge Reader WebView module stopped")
      (run-hooks 'yunge-reader-webview-service-stopped-hook)
      (unless intentional
        (display-warning
         'yunge-reader
         "Yunge Reader WebView service stopped unexpectedly"
         :warning)))))

(defun yunge-reader-webview--module-filter (process output)
  "Route module channel OUTPUT and service wakeups for PROCESS."
  (yunge-reader-transport--filter process output)
  (when (and (eq process yunge-reader-webview--process)
             (fboundp 'yunge-reader-module-pump))
    (yunge-reader-module-pump)))

(defun yunge-reader-webview--module-send-line (_process line)
  "Send one protocol LINE to the in-process WebView module."
  (yunge-reader-module-request line))

;;;###autoload
(defun yunge-reader-webview-start ()
  "Start the in-process platform WebView service and return its pipe."
  (interactive)
  (unless (memq system-type '(windows-nt darwin))
    (user-error
     "Yunge Reader EPUB supports WebView2 on Windows and WKWebView on macOS"))
  (if (process-live-p yunge-reader-webview--process)
      yunge-reader-webview--process
    (unless (yunge-reader-webview--program-available-p)
      (user-error
       (concat
        "Yunge Reader native module is unavailable; "
        "run M-x yunge-reader-native-setup")))
    (yunge-reader-webview--ensure-module)
    (setq yunge-reader-webview--transport
          (yunge-reader-webview--make-transport))
    (let ((process
           (make-pipe-process
            :name "yunge-reader-webview"
            :coding 'utf-8-unix
            :noquery t
            :filter #'yunge-reader-webview--module-filter
            :sentinel #'yunge-reader-webview--sentinel)))
      (yunge-reader-transport--bind
       yunge-reader-webview--transport process
       #'yunge-reader-webview--module-send-line)
      (process-put process 'yunge-reader-webview-intentional-stop nil)
      (setq yunge-reader-webview--process process)
      (condition-case error-data
          (yunge-reader-module-start process)
        (error
         (setq yunge-reader-webview--process nil)
         (delete-process process)
         (signal (car error-data) (cdr error-data))))
      (when (called-interactively-p 'interactive)
        (message "Starting Yunge Reader WebView service..."))
      process)))

(cl-defun yunge-reader-webview--request
    (operation parameters complete &key owner timeout revision)
  "Send WebView OPERATION with PARAMETERS and call COMPLETE.
Return an owned transport task supporting cancellation and deadlines."
  (unless (stringp operation)
    (error "WebView operation must be a string: %S" operation))
  (unless (functionp complete)
    (error "WebView completion must be a function: %S" complete))
  (let* ((process (yunge-reader-webview-start))
         (owner
          (or owner
              (when-let* ((id (alist-get 'view parameters)))
                (gethash id yunge-reader-webview--views)))))
    (yunge-reader-transport--request
     yunge-reader-webview--transport process operation parameters complete
     :owner owner :timeout timeout :revision revision)))

(defun yunge-reader-webview--cancel-view-requests
    (view &optional reason)
  "Cancel pending WebView transport requests owned by VIEW."
  (when yunge-reader-webview--transport
    (yunge-reader-transport-cancel-owner
     yunge-reader-webview--transport view reason)))

;;;###autoload
(defun yunge-reader-webview-stop (&optional force)
  "Stop the in-process WebView service.
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
          (progn
            (when (fboundp 'yunge-reader-module-stop)
              (yunge-reader-module-stop))
            (delete-process process))
        (yunge-reader-webview--request
         "shutdown" nil
         (lambda (_result _error-data)
           (when (and (eq process yunge-reader-webview--process)
                      (process-live-p process))
             (yunge-reader-module-stop)
             (delete-process process))))
        (yunge-reader-webview--cancel-force-stop)
        (setq yunge-reader-webview--force-stop-timer
              (run-at-time
               yunge-reader-webview-stop-timeout nil
               (lambda (child)
                 (when (and (eq child yunge-reader-webview--process)
                            (process-live-p child))
                   (process-put
                    child 'yunge-reader-webview-intentional-stop t)
                   (when (fboundp 'yunge-reader-module-stop)
                     (yunge-reader-module-stop))
                   (delete-process child)))
               process)))
      (when (called-interactively-p 'interactive)
        (message
         (if force
             "Terminating Yunge Reader WebView service..."
           "Stopping Yunge Reader WebView service...")))
      process)))

(defun yunge-reader-webview--shutdown-for-emacs-exit ()
  "Terminate the WebView module without delaying Emacs exit."
  (when (process-live-p yunge-reader-webview--process)
    (process-put yunge-reader-webview--process
                 'yunge-reader-webview-intentional-stop t)
    (when (fboundp 'yunge-reader-module-stop)
      (yunge-reader-module-stop))
    (delete-process yunge-reader-webview--process)))

(add-hook 'kill-emacs-hook
          #'yunge-reader-webview--shutdown-for-emacs-exit)

(provide 'yunge-reader-webview-service)

;;; yunge-reader-webview-service.el ends here
