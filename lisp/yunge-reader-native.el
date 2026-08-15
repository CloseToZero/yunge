;;; yunge-reader-native.el --- Native reader -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'compile)
(require 'json)
(require 'subr-x)
(require 'yunge-reader)
(require 'yunge-state)

(declare-function yunge-reader-setup "yunge-reader-setup" ())
(declare-function yunge-reader-setup--begin "yunge-reader-setup" ())

(defcustom yunge-reader-native-idle-seconds 300
  "Seconds with no native clients before stopping the helper.
Set this to nil to keep an explicitly acquired helper alive until it is
stopped or Emacs exits."
  :type '(choice (const :tag "Never stop when idle" nil) number)
  :group 'yunge-reader)

(defcustom yunge-reader-native-stop-timeout 1.0
  "Seconds allowed for graceful native helper shutdown."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-cache-max-bytes (* 1024 1024 1024)
  "Maximum rendered-page cache size before pruning begins."
  :type 'integer
  :group 'yunge-reader)

(defcustom yunge-reader-cache-target-bytes (* 768 1024 1024)
  "Rendered-page cache size to target when pruning is needed.
This must not exceed `yunge-reader-cache-max-bytes'."
  :type 'integer
  :group 'yunge-reader)

(defconst yunge-reader-native-protocol-version 1
  "Native helper protocol version understood by this client.")

(defconst yunge-reader-native-pdfium-api "7881"
  "PDFium API release required by the native helper.")

(defconst yunge-reader-native--source-directory
  (file-name-directory
   (or load-file-name
       (locate-library "yunge-reader-native")
       (error "Cannot locate the Yunge Reader native client")))
  "Directory containing the loaded native client library.")

(defconst yunge-reader-native--manifest
  (expand-file-name
   "../native/yunge-reader/Cargo.toml"
   yunge-reader-native--source-directory)
  "Cargo manifest of the Yunge Reader native helper.")

(defconst yunge-reader-native--source-hash-file
  (expand-file-name
   "source.sha256"
   (file-name-directory yunge-reader-native--manifest))
  "Tracked source hash embedded in the native helper.")

(defconst yunge-reader-native--build-buffer-name
  "*Yunge Reader native build*"
  "Name of the native helper build log buffer.")

(defconst yunge-reader-native--log-buffer-name
  "*Yunge Reader native log*"
  "Name of the native helper diagnostic buffer.")

(defvar yunge-reader-native--process nil
  "Running native helper process, or nil.")

(defvar yunge-reader-native--build-process nil
  "Process currently building the native helper, or nil.")

(defvar yunge-reader-native--callbacks (make-hash-table :test #'eql)
  "Pending response callbacks indexed by request identifier.")

(defvar yunge-reader-native--outbound-queue nil
  "Protocol lines waiting for the native ready handshake.")

(defvar yunge-reader-native--next-id 0
  "Last native request identifier allocated by Emacs.")

(defvar yunge-reader-native--client-count 0
  "Number of reader components retaining the native service.")

(defvar yunge-reader-native--idle-timer nil
  "Timer that stops an unused native helper, or nil.")

(defvar yunge-reader-native--cache-pruning nil
  "Whether a cache-prune request is outstanding.")

(defvar yunge-reader-native--cache-prune-stop-after nil
  "Whether to stop the helper after the current cache prune.")

(defvar yunge-reader-native--force-stop-timer nil
  "Timer enforcing the graceful shutdown deadline, or nil.")

(defvar yunge-reader-native--restart-after-stop nil
  "Whether the helper should start after its current process exits.")

(defvar yunge-reader-native--build-after-stop nil
  "Setup continuation requested after the current helper exits.")

(defvar yunge-reader-native--restart-count 0
  "Number of automatic crash restarts attempted this session.")

(defun yunge-reader-native--cargo-target-directory ()
  "Return the Cargo target directory for Yunge Reader."
  (yunge-var-subdirectory "yunge-reader/cargo-target"))

(defun yunge-reader-native-program ()
  "Return the expected native helper executable."
  (expand-file-name
   (concat "release/yunge-reader"
           (when (eq system-type 'windows-nt) ".exe"))
   (yunge-reader-native--cargo-target-directory)))

(defun yunge-reader-native-pdfium-directory ()
  "Return the installed directory for the pinned PDFium API."
  (expand-file-name
   yunge-reader-native-pdfium-api
   (yunge-var-subdirectory "yunge-reader/pdfium")))

(defun yunge-reader-native-pdfium-library ()
  "Return the expected installed PDFium dynamic library."
  (expand-file-name
   (pcase system-type
     ('windows-nt "bin/pdfium.dll")
     ('darwin "bin/libpdfium.dylib")
     ('gnu/linux "bin/libpdfium.so")
     (_ "bin/pdfium"))
   (yunge-reader-native-pdfium-directory)))

(defun yunge-reader-native-cache-directory ()
  "Return the directory containing rendered PDF page artifacts."
  (yunge-var-subdirectory "yunge-reader/cache"))

(defun yunge-reader-native--build-id ()
  "Return the expected native helper build ID, or nil."
  (when (file-readable-p yunge-reader-native--source-hash-file)
    (with-temp-buffer
      (insert-file-contents yunge-reader-native--source-hash-file)
      (let ((build-id (string-trim (buffer-string))))
        (unless (string-empty-p build-id)
          build-id)))))

(defun yunge-reader-native--available-p ()
  "Return whether the native helper and PDFium library are available."
  (and (file-executable-p (yunge-reader-native-program))
       (file-regular-p (yunge-reader-native-pdfium-library))))

(defun yunge-reader-native--cancel-timer (symbol)
  "Cancel the timer stored in SYMBOL and set SYMBOL to nil."
  (when (timerp (symbol-value symbol))
    (cancel-timer (symbol-value symbol)))
  (set symbol nil))

(defun yunge-reader-native--validate-ready (message)
  "Validate native helper ready MESSAGE."
  (let ((expected (yunge-reader-native--build-id))
        (actual (alist-get 'build-id message)))
    (unless expected
      (error "Yunge Reader native source hash is unavailable"))
    (unless (and (equal (alist-get 'kind message) "ready")
                 (= (or (alist-get 'protocol message) -1)
                    yunge-reader-native-protocol-version)
                 (equal actual expected)
                 (equal (alist-get 'pdfium-api message)
                        yunge-reader-native-pdfium-api)
                 (member "cache-maintenance"
                         (alist-get 'capabilities message))
                 (member "lifecycle"
                         (alist-get 'capabilities message))
                 (member "pdf-outline"
                         (alist-get 'capabilities message))
                 (member "pdf-render"
                         (alist-get 'capabilities message))
                 (member "pdf-search"
                         (alist-get 'capabilities message))
                 (member "pdf-text"
                         (alist-get 'capabilities message)))
      (error
       "Incompatible Yunge Reader helper: expected protocol %d build %s, got %S"
       yunge-reader-native-protocol-version expected message))))

(defun yunge-reader-native--send-line (process line)
  "Send one protocol LINE to native PROCESS."
  (process-send-string process (concat line "\n")))

(defun yunge-reader-native--flush-outbound (process)
  "Send queued protocol messages to ready PROCESS in FIFO order."
  (dolist (entry (nreverse yunge-reader-native--outbound-queue))
    (yunge-reader-native--send-line process (cdr entry)))
  (setq yunge-reader-native--outbound-queue nil))

(defun yunge-reader-native--response-error (message)
  "Return an Emacs error value represented by response MESSAGE."
  (let ((error-object (alist-get 'error message)))
    (list
     'error
     (or (alist-get 'message error-object)
         "The Yunge Reader native helper failed"))))

(defun yunge-reader-native--handle-message (process message)
  "Handle one parsed native MESSAGE from PROCESS."
  (if (not (process-get process 'yunge-reader-ready))
      (progn
        (yunge-reader-native--validate-ready message)
        (process-put process 'yunge-reader-ready t)
        (yunge-reader-native--flush-outbound process))
    (let* ((id (alist-get 'id message))
           (callback (and (integerp id)
                          (gethash id yunge-reader-native--callbacks))))
      (unless callback
        (error "Unexpected Yunge Reader native response: %S" message))
      (remhash id yunge-reader-native--callbacks)
      (if (alist-get 'ok message)
          (funcall callback (alist-get 'result message) nil)
        (funcall callback nil
                 (yunge-reader-native--response-error message))))))

(defun yunge-reader-native--filter (process output)
  "Collect and handle complete NDJSON messages in PROCESS OUTPUT."
  (let ((pending (concat (or (process-get process 'yunge-reader-output) "")
                         output))
        newline)
    (while (setq newline (string-match "\n" pending))
      (let ((line (string-trim-right (substring pending 0 newline) "\r")))
        (setq pending (substring pending (1+ newline)))
        (unless (string-empty-p line)
          (condition-case error-data
              (yunge-reader-native--handle-message
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
              (format "Invalid Yunge Reader native output: %s"
                      (error-message-string error-data))
              :warning)
             (when (eq process yunge-reader-native--process)
               (yunge-reader-native-stop t)))))))
    (process-put process 'yunge-reader-output pending)))

(defun yunge-reader-native--fail-callbacks (reason)
  "Complete every pending native callback with error REASON."
  (let (callbacks)
    (maphash
     (lambda (_id callback)
       (push callback callbacks))
     yunge-reader-native--callbacks)
    (clrhash yunge-reader-native--callbacks)
    (setq yunge-reader-native--outbound-queue nil)
    (dolist (callback callbacks)
      (funcall callback nil (list 'error reason)))))

(defun yunge-reader-native--start-after-crash ()
  "Restart the helper once after an unexpected exit."
  (when (> yunge-reader-native--client-count 0)
    (condition-case error-data
        (yunge-reader-native-start)
      (error
       (display-warning
        'yunge-reader
        (format "Could not restart Yunge Reader helper: %s"
                (error-message-string error-data))
        :warning)))))

(defun yunge-reader-native--sentinel (process _event)
  "Finalize native PROCESS after it exits."
  (when (and (memq (process-status process)
                   '(exit signal failed closed))
             (eq process yunge-reader-native--process))
    (let ((intentional
           (process-get process 'yunge-reader-intentional-stop))
          (restart yunge-reader-native--restart-after-stop)
          (build yunge-reader-native--build-after-stop))
      (setq yunge-reader-native--process nil
            yunge-reader-native--restart-after-stop nil
            yunge-reader-native--build-after-stop nil)
      (yunge-reader-native--cancel-timer
       'yunge-reader-native--force-stop-timer)
      (yunge-reader-native--fail-callbacks
       (if intentional
           "The Yunge Reader native helper stopped"
         "The Yunge Reader native helper exited unexpectedly"))
      (cond
       ((eq build 'setup)
        (require 'yunge-reader-setup)
        (yunge-reader-setup--begin))
       (build
        (yunge-reader-native--start-build))
       (restart
        (run-at-time 0 nil #'yunge-reader-native--start-after-crash))
       ((and (not intentional)
             (> yunge-reader-native--client-count 0)
             (< yunge-reader-native--restart-count 1))
        (cl-incf yunge-reader-native--restart-count)
        (run-at-time 0 nil #'yunge-reader-native--start-after-crash))
       ((not intentional)
        (display-warning
         'yunge-reader
         "Yunge Reader native service stopped unexpectedly"
         :warning))))))

;;;###autoload
(defun yunge-reader-native-start ()
  "Start the Yunge Reader native helper and return its process."
  (interactive)
  (if (process-live-p yunge-reader-native--process)
      yunge-reader-native--process
    (unless (yunge-reader-native--available-p)
      (user-error
       (concat
        "Yunge Reader native helper is unavailable; "
        "run M-x yunge-reader-native-setup")))
    (let ((log (get-buffer-create yunge-reader-native--log-buffer-name)))
      (with-current-buffer log
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (setq yunge-reader-native--outbound-queue nil)
      (make-directory (yunge-reader-native-cache-directory) t)
      (let* ((process-environment
              (cons
               (concat
                "YUNGE_READER_PDFIUM="
                (yunge-reader-native-pdfium-library))
               (cons
                (concat
                 "YUNGE_READER_CACHE="
                 (yunge-reader-native-cache-directory))
                process-environment)))
             (process
              (make-process
               :name "yunge-reader-native"
               :command (list (yunge-reader-native-program))
               :connection-type 'pipe
               :coding 'utf-8-unix
               :noquery t
               :stderr log
               :filter #'yunge-reader-native--filter
               :sentinel #'yunge-reader-native--sentinel)))
        (process-put process 'yunge-reader-output "")
        (process-put process 'yunge-reader-ready nil)
        (process-put process 'yunge-reader-intentional-stop nil)
        (setq yunge-reader-native--process process)
        (when (called-interactively-p 'interactive)
          (message "Starting Yunge Reader native service..."))
        process))))

(defun yunge-reader-native-request (operation parameters complete)
  "Send native OPERATION with PARAMETERS and call COMPLETE on response.
COMPLETE receives a result and nil, or nil and an Emacs error value."
  (unless (stringp operation)
    (error "Native reader operation must be a string: %S" operation))
  (unless (functionp complete)
    (error "Native reader completion must be a function: %S" complete))
  (let* ((process (yunge-reader-native-start))
         (id (cl-incf yunge-reader-native--next-id))
         (request
          (append
           (list (cons 'id id) (cons 'op operation))
           (when parameters
             (list (cons 'params parameters)))))
         (line
          (json-serialize request :null-object nil :false-object :false)))
    (puthash id complete yunge-reader-native--callbacks)
    (if (process-get process 'yunge-reader-ready)
        (yunge-reader-native--send-line process line)
      (push (cons id line) yunge-reader-native--outbound-queue))
    id))

(defun yunge-reader-native-live-p ()
  "Return whether the Yunge Reader native helper is running."
  (process-live-p yunge-reader-native--process))

;;;###autoload
(defun yunge-reader-native-stop (&optional force)
  "Stop the Yunge Reader native helper.
Without FORCE, request graceful shutdown and terminate the process only after
`yunge-reader-native-stop-timeout'.  Interactively, a prefix argument means
FORCE."
  (interactive "P")
  (if (not (process-live-p yunge-reader-native--process))
      (progn
        (setq yunge-reader-native--process nil)
        (when (called-interactively-p 'interactive)
          (message "Yunge Reader native service is not running"))
        nil)
    (let ((process yunge-reader-native--process))
      (process-put process 'yunge-reader-intentional-stop t)
      (yunge-reader-native--cancel-timer
       'yunge-reader-native--idle-timer)
      (if force
          (delete-process process)
        (yunge-reader-native-request "shutdown" nil #'ignore)
        (yunge-reader-native--cancel-timer
         'yunge-reader-native--force-stop-timer)
        (setq yunge-reader-native--force-stop-timer
              (run-at-time
               yunge-reader-native-stop-timeout nil
               (lambda (child)
                 (when (and (eq child yunge-reader-native--process)
                            (process-live-p child))
                   (process-put child 'yunge-reader-intentional-stop t)
                   (delete-process child)))
               process)))
      (when (called-interactively-p 'interactive)
        (message (if force
                     "Terminating Yunge Reader native service..."
                   "Stopping Yunge Reader native service...")))
      process)))

;;;###autoload
(defun yunge-reader-native-restart ()
  "Restart the Yunge Reader native helper after graceful shutdown."
  (interactive)
  (setq yunge-reader-native--restart-count 0)
  (if (process-live-p yunge-reader-native--process)
      (progn
        (setq yunge-reader-native--restart-after-stop t)
        (yunge-reader-native-stop))
    (yunge-reader-native-start)))

;;;###autoload
(defun yunge-reader-native-status ()
  "Report and return the current native helper status plist."
  (interactive)
  (let ((status
         (list
          :state
          (cond
           ((process-live-p yunge-reader-native--build-process) 'building)
           ((not (process-live-p yunge-reader-native--process)) 'stopped)
           ((process-get yunge-reader-native--process
                         'yunge-reader-intentional-stop)
            'stopping)
           ((process-get yunge-reader-native--process 'yunge-reader-ready)
            'ready)
           (t 'starting))
          :clients yunge-reader-native--client-count
          :cache-pruning yunge-reader-native--cache-pruning
          :program (yunge-reader-native-program)
          :pdfium (yunge-reader-native-pdfium-library)
          :pdfium-api yunge-reader-native-pdfium-api
          :build-id (yunge-reader-native--build-id))))
    (when (called-interactively-p 'interactive)
      (message "Yunge Reader native: %s, %d client%s"
               (plist-get status :state)
               yunge-reader-native--client-count
               (if (= yunge-reader-native--client-count 1) "" "s")))
    status))

(defun yunge-reader-native--cache-prune-parameters ()
  "Return validated native cache-prune parameters."
  (unless (and (integerp yunge-reader-cache-max-bytes)
               (> yunge-reader-cache-max-bytes 0))
    (user-error "Yunge Reader cache maximum must be a positive integer"))
  (unless (and (integerp yunge-reader-cache-target-bytes)
               (>= yunge-reader-cache-target-bytes 0)
               (<= yunge-reader-cache-target-bytes
                   yunge-reader-cache-max-bytes))
    (user-error
     "Yunge Reader cache target must be between zero and its maximum"))
  `((max-bytes . ,yunge-reader-cache-max-bytes)
    (target-bytes . ,yunge-reader-cache-target-bytes)))

(defun yunge-reader-native--cache-prune-complete
    (process result error-data notify)
  "Finish cache pruning by PROCESS with RESULT or ERROR-DATA.
When NOTIFY is non-nil, report successful cleanup in the echo area."
  (let ((stop-after yunge-reader-native--cache-prune-stop-after))
    (setq yunge-reader-native--cache-pruning nil
          yunge-reader-native--cache-prune-stop-after nil)
    (unless (and error-data
                 (process-get process 'yunge-reader-intentional-stop))
      (if error-data
          (display-warning
           'yunge-reader
           (format "Could not prune the Yunge Reader cache: %s"
                   (error-message-string error-data))
           :warning)
        (let ((failed (or (alist-get 'failed-files result) 0))
              (remaining (or (alist-get 'after-bytes result) 0)))
          (when (or (> failed 0) (alist-get 'over-budget result))
            (display-warning
             'yunge-reader
             (format
              "Yunge Reader cache remains at %d bytes (%d failures)"
              remaining failed)
             :warning))
          (when notify
            (message
             "Yunge Reader cache: removed %d files (%d bytes); %d remain"
             (or (alist-get 'removed-files result) 0)
             (or (alist-get 'removed-bytes result) 0)
             remaining)))))
    (when (and stop-after
               (zerop yunge-reader-native--client-count)
               (eq process yunge-reader-native--process)
               (process-live-p process)
               (not (process-get process
                                 'yunge-reader-intentional-stop)))
      (yunge-reader-native-stop))))

(defun yunge-reader-native--cache-prune-request (stop-after &optional notify)
  "Request cache pruning and optionally STOP-AFTER it completes.
When NOTIFY is non-nil, report successful cleanup in the echo area."
  (if yunge-reader-native--cache-pruning
      (progn
        (when stop-after
          (setq yunge-reader-native--cache-prune-stop-after t))
        nil)
    (let ((parameters (yunge-reader-native--cache-prune-parameters))
          process)
      (setq yunge-reader-native--cache-pruning t
            yunge-reader-native--cache-prune-stop-after stop-after)
      (condition-case error-data
          (progn
            (yunge-reader-native-request
             "cache-prune" parameters
             (lambda (result request-error)
               (yunge-reader-native--cache-prune-complete
                process result request-error notify)))
            (setq process yunge-reader-native--process))
        (error
         (setq yunge-reader-native--cache-pruning nil
               yunge-reader-native--cache-prune-stop-after nil)
         (signal (car error-data) (cdr error-data)))))))

;;;###autoload
(defun yunge-reader-cache-prune ()
  "Prune the rendered-page cache while no Reader document is open."
  (interactive)
  (unless (zerop yunge-reader-native--client-count)
    (user-error "Close all Yunge Reader documents before pruning the cache"))
  (when yunge-reader-native--cache-pruning
    (user-error "Yunge Reader cache pruning is already in progress"))
  (yunge-reader-native--cache-prune-request
   (not (process-live-p yunge-reader-native--process)) t))

(defun yunge-reader-native-acquire ()
  "Retain and start the native service for one reader component."
  (yunge-reader-native--cancel-timer 'yunge-reader-native--idle-timer)
  (cl-incf yunge-reader-native--client-count)
  (condition-case error-data
      (yunge-reader-native-start)
    (error
     (cl-decf yunge-reader-native--client-count)
     (signal (car error-data) (cdr error-data)))))

(defun yunge-reader-native--idle-stop ()
  "Prune cached pages, then stop a helper with no clients."
  (setq yunge-reader-native--idle-timer nil)
  (when (zerop yunge-reader-native--client-count)
    (if (not (process-live-p yunge-reader-native--process))
        (setq yunge-reader-native--process nil)
      (condition-case error-data
          (yunge-reader-native--cache-prune-request t)
        (error
         (display-warning
          'yunge-reader
          (format "Could not start Yunge Reader cache pruning: %s"
                  (error-message-string error-data))
          :warning)
         (yunge-reader-native-stop))))))

(defun yunge-reader-native-release ()
  "Release one reader component's native service reference."
  (when (zerop yunge-reader-native--client-count)
    (error "Yunge Reader native service was released without an owner"))
  (cl-decf yunge-reader-native--client-count)
  (when (zerop yunge-reader-native--client-count)
    (yunge-reader-native--cancel-timer 'yunge-reader-native--idle-timer)
    (when yunge-reader-native-idle-seconds
      (setq yunge-reader-native--idle-timer
            (run-at-time yunge-reader-native-idle-seconds nil
                         #'yunge-reader-native--idle-stop))))
  yunge-reader-native--client-count)

(defun yunge-reader-native--smoke-test-complete (result error-data)
  "Finish native setup smoke testing with RESULT or ERROR-DATA."
  (if error-data
      (display-warning
       'yunge-reader
       (format "Yunge Reader helper smoke test failed: %s"
               (error-message-string error-data))
       :error)
    (message "Yunge Reader native helper is ready (backend: %s)"
             (alist-get 'backend result)))
  (when (and (zerop yunge-reader-native--client-count)
             (process-live-p yunge-reader-native--process))
    (yunge-reader-native-stop)))

(defun yunge-reader-native--build-sentinel (process _event)
  "Finish setup after Cargo build PROCESS exits."
  (when (and (memq (process-status process) '(exit signal failed))
             (not (process-get process 'yunge-reader-finished)))
    (process-put process 'yunge-reader-finished t)
    (when (eq process yunge-reader-native--build-process)
      (setq yunge-reader-native--build-process nil))
    (if (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process))
             (yunge-reader-native--available-p))
        (condition-case error-data
            (progn
              (yunge-reader-native-start)
              (yunge-reader-native-request
               "pdfium-info" nil
               #'yunge-reader-native--smoke-test-complete))
          (error
           (display-warning
            'yunge-reader
            (format "Could not start the built Yunge Reader helper: %s"
                    (error-message-string error-data))
            :error)))
      (display-buffer (process-buffer process))
      (display-warning
       'yunge-reader
       (format "Yunge Reader native build failed; see %s"
               (buffer-name (process-buffer process)))
       :error))))

(defun yunge-reader-native--start-build ()
  "Build the native helper asynchronously and run its smoke test."
  (when (process-live-p yunge-reader-native--build-process)
    (user-error "Yunge Reader native helper is already being built"))
  (let ((cargo (executable-find "cargo")))
    (unless cargo
      (user-error "Cargo is required to build Yunge Reader"))
    (let ((buffer
           (get-buffer-create yunge-reader-native--build-buffer-name)))
      (make-directory (yunge-reader-native--cargo-target-directory) t)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Yunge Reader native build\n\n"))
        (setq default-directory yunge-reader-native--source-directory)
        (compilation-mode))
      (setq yunge-reader-native--build-process
            (make-process
             :name "yunge-reader-native-build"
             :buffer buffer
             :command
             (list cargo "build" "--release" "--locked"
                   "--manifest-path" yunge-reader-native--manifest
                   "--target-dir"
                   (yunge-reader-native--cargo-target-directory))
             :connection-type 'pipe
             :coding 'utf-8-unix
             :noquery t
             :sentinel #'yunge-reader-native--build-sentinel))
      (display-buffer buffer)
      (message "Building Yunge Reader native helper...")
      yunge-reader-native--build-process)))

;;;###autoload
(defun yunge-reader-native-setup ()
  "Install PDFium, build the native helper, and run a smoke test.
Normal document opening never downloads or compiles dependencies implicitly."
  (interactive)
  (require 'yunge-reader-setup)
  (yunge-reader-setup))

(defun yunge-reader-native--shutdown-for-emacs-exit ()
  "End native processes without delaying Emacs shutdown."
  (yunge-reader-native--cancel-timer 'yunge-reader-native--idle-timer)
  (yunge-reader-native--cancel-timer
   'yunge-reader-native--force-stop-timer)
  (when (process-live-p yunge-reader-native--process)
    (let ((process yunge-reader-native--process))
      (process-put process 'yunge-reader-intentional-stop t)
      (when (process-get process 'yunge-reader-ready)
        (condition-case nil
            (let* ((id (cl-incf yunge-reader-native--next-id))
                   (line
                    (json-serialize
                     (list (cons 'id id) (cons 'op "shutdown")))))
              (yunge-reader-native--send-line process line))
          (error nil)))))
  (when (process-live-p yunge-reader-native--build-process)
    (set-process-sentinel yunge-reader-native--build-process #'ignore)
    (delete-process yunge-reader-native--build-process)))

(add-hook 'kill-emacs-hook #'yunge-reader-native--shutdown-for-emacs-exit)

(provide 'yunge-reader-native)

;;; yunge-reader-native.el ends here
