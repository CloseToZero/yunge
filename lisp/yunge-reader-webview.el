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
  bounds-pending
  publication
  publication-ready
  location
  path
  open-deadline
  open-timer)

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
            "view-status" "view-visible")))
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
      (not (string-prefix-p "/" href))
      (not (string-match-p "[\\\\:?#]" href))
      (cl-every
       (lambda (part)
         (not (member part '("" "." ".."))))
       (split-string href "/"))
      (or (null fraction)
          (and (numberp fraction)
               (= fraction fraction)
               (<= 0 fraction 1)))))))

(defun yunge-reader-webview--check-location (location)
  "Return LOCATION or signal when it is not a valid EPUB locator."
  (unless (yunge-reader-webview--valid-location-p location)
    (error "Invalid EPUB location: %S" location))
  location)

(defun yunge-reader-webview--open-view-publication
    (view publication callback &optional location)
  "Open PUBLICATION in native VIEW at LOCATION, then invoke CALLBACK."
  (yunge-reader-webview--request
   "view-open-publication"
   (append
    `((view . ,(yunge-reader-webview--view-id view))
      (publication . ,publication))
    (when location
      `((location . ,(yunge-reader-webview--check-location location)))))
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
      `((location . ,(yunge-reader-webview--check-location location)))))
   callback))

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

(defun yunge-reader-webview--handle-event (process message)
  "Handle one asynchronous WebView MESSAGE from PROCESS."
  (unless (eq process yunge-reader-webview--process)
    (error "WebView event belongs to an obsolete process"))
  (let ((event (alist-get 'event message))
        (id (alist-get 'view message)))
    (unless (and (stringp event) (integerp id))
      (error "Malformed Yunge Reader WebView event: %S" message))
    (pcase event
      ("escape"
       (when-let* ((view (gethash id yunge-reader-webview--views))
                   (window (yunge-reader-webview--view-window view))
                   ((window-live-p window))
                   ((buffer-live-p
                     (yunge-reader-webview--view-buffer view))))
         (yunge-reader-webview--request
          "view-clear-selection" `((view . ,id))
          (lambda (_result _error-data)))
         (yunge-reader-webview--request
          "view-focus-parent" `((view . ,id))
          (lambda (_result _error-data)))
         (select-window window)
         (select-frame-set-input-focus (window-frame window))))
      ("publication-ready"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (setf (yunge-reader-webview--view-location view)
               (yunge-reader-webview--event-location message))
         (setf (yunge-reader-webview--view-publication-ready view) t)
         (yunge-reader-webview--set-buffer-message
          view
          (format "EPUB renderer ready: %s"
                  (or (yunge-reader-webview--view-path view)
                      "publication")))))
      ("location"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (setf (yunge-reader-webview--view-location view)
               (yunge-reader-webview--event-location message))))
      ("navigation-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB navigation error: %S" message))
         (display-warning 'yunge-reader detail :warning)))
      ("publication-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB renderer error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (setf (yunge-reader-webview--view-publication-ready view) nil)
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

(defun yunge-reader-webview--destroy-view (view)
  "Destroy native VIEW and forget its Emacs ownership."
  (unless (yunge-reader-webview--view-destroyed view)
    (yunge-reader-webview--cancel-open-timer view)
    (setf (yunge-reader-webview--view-destroyed view) t)
    (remhash (yunge-reader-webview--view-id view)
             yunge-reader-webview--views)
    (let ((publication
           (prog1 (yunge-reader-webview--view-publication view)
             (setf (yunge-reader-webview--view-publication view) nil))))
      (if (and (yunge-reader-webview--view-created view)
               (process-live-p yunge-reader-webview--process))
          (yunge-reader-webview--request
           "view-destroy"
           `((view . ,(yunge-reader-webview--view-id view)))
           (lambda (_result _error-data)
             (yunge-reader-webview--close-owned-publication publication)))
        (yunge-reader-webview--close-owned-publication publication)))
    (when (zerop (hash-table-count yunge-reader-webview--views))
      (yunge-reader-webview--remove-hooks))))

(defun yunge-reader-webview--forget-all-views ()
  "Forget all native views without sending protocol messages."
  (maphash
   (lambda (_id view)
     (yunge-reader-webview--cancel-open-timer view)
     (setf (yunge-reader-webview--view-destroyed view) t))
   yunge-reader-webview--views)
  (clrhash yunge-reader-webview--views)
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
    (view _result error-data)
  "Finish one attempt to attach VIEW's EPUB publication."
  (cond
   ((yunge-reader-webview--view-destroyed view))
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
    (yunge-reader-webview--open-view-publication
     view
     (yunge-reader-webview--view-publication view)
     (apply-partially #'yunge-reader-webview--open-complete view)
     (yunge-reader-webview--view-location view))))

(defun yunge-reader-webview--current-ready-view ()
  "Return the current buffer's ready EPUB WebView."
  (let ((view yunge-reader-webview--buffer-view))
    (unless (and view
                 (not (yunge-reader-webview--view-destroyed view))
                 (yunge-reader-webview--view-publication-ready view))
      (user-error "The current buffer has no ready EPUB view"))
    view))

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

(defun yunge-reader-webview--create-complete
    (view _result error-data)
  "Complete creation of VIEW with ERROR-DATA when it failed."
  (if error-data
      (progn
        (yunge-reader-webview--destroy-view view)
        (yunge-reader-webview--set-buffer-message
         view (error-message-string error-data))
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
      (yunge-reader-webview--sync-view view)
      (when (yunge-reader-webview--view-publication view)
        (setf (yunge-reader-webview--view-open-deadline view)
              (+ (float-time) yunge-reader-webview-open-timeout))
        (yunge-reader-webview--try-open-publication view)))))

(defun yunge-reader-webview--request-create (view)
  "Ask the helper to create native VIEW."
  (let* ((window (yunge-reader-webview--view-window view))
         (frame (window-frame window)))
    (yunge-reader-webview--request
     "view-create"
     `((view . ,(yunge-reader-webview--view-id view))
       (parent . ,(yunge-reader-webview--frame-handle frame))
       (bounds . ,(yunge-reader-webview--view-requested-bounds view))
       (visible . t))
     (apply-partially #'yunge-reader-webview--create-complete view))))

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
        (setf (yunge-reader-webview--view-publication view) publication)
        (yunge-reader-webview--request-create view)))))

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
        (insert "Creating native WebView...\n")
        (set-buffer-modified-p nil)))
    (set-window-buffer window buffer)
    (puthash id view yunge-reader-webview--views)
    (yunge-reader-webview--install-hooks)
    (yunge-reader-webview--request-create view)
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
         (id (cl-incf yunge-reader-webview--next-view-id))
         (buffer
          (generate-new-buffer
           (format "*Yunge EPUB %s*" (file-name-nondirectory file))))
         (bounds (yunge-reader-webview--window-bounds window))
         (view
          (yunge-reader-webview--make-view
           :id id
           :window window
           :buffer buffer
           :location (and location (copy-tree location))
           :path file
           :requested-bounds bounds)))
    (with-current-buffer buffer
      (yunge-reader-webview-spike-mode)
      (setq yunge-reader-webview--buffer-view view)
      (let ((inhibit-read-only t))
        (insert "Validating EPUB publication...\n")
        (set-buffer-modified-p nil)))
    (set-window-buffer window buffer)
    (puthash id view yunge-reader-webview--views)
    (yunge-reader-webview--install-hooks)
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
