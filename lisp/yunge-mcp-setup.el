;;; yunge-mcp-setup.el --- Set up Yunge MCP -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'json)
(require 'yunge-mcp-clients)
(require 'yunge-state)

(defvar server-name)
(defvar server-use-tcp)

(declare-function yunge-server-start "yunge-server")

(defconst yunge-mcp--native-manifest
  (expand-file-name
   "native/yunge-mcp/Cargo.toml" yunge-config-directory)
  "Cargo manifest of the Yunge MCP server.")

(defvar yunge-mcp--build-process nil
  "Process currently building the Yunge MCP server, or nil.")

(defconst yunge-mcp--build-buffer-name "*Yunge MCP build*"
  "Name of the Yunge MCP build log buffer.")

(defun yunge-mcp--state-directory ()
  "Return the mutable state directory owned by Yunge MCP."
  (yunge-var-subdirectory "yunge-mcp"))

(defun yunge-mcp--cargo-target-directory ()
  "Return the Cargo target directory used by Yunge MCP."
  (yunge-var-subdirectory "yunge-mcp/cargo-target"))

(defun yunge-mcp--executable-name ()
  "Return the platform-specific Yunge MCP executable name."
  (concat "yunge-mcp"
          (when (eq system-type 'windows-nt) ".exe")))

(defun yunge-mcp--built-program ()
  "Return the executable produced by Cargo."
  (expand-file-name
   (concat "release/" (yunge-mcp--executable-name))
   (yunge-mcp--cargo-target-directory)))

(defun yunge-mcp-program ()
  "Return the stable installed Yunge MCP executable path."
  (expand-file-name
   (concat "bin/" (yunge-mcp--executable-name))
   (yunge-mcp--state-directory)))

(defun yunge-mcp--runtime-file ()
  "Return the Yunge MCP runtime manifest path."
  (expand-file-name "runtime.json" (yunge-mcp--state-directory)))

(defun yunge-mcp--emacsclient-program ()
  "Return the emacsclient belonging to the running Emacs installation."
  (let ((sibling
         (expand-file-name
          (concat "emacsclient"
                  (when (eq system-type 'windows-nt) ".exe"))
          invocation-directory)))
    (cond
     ((file-executable-p sibling) sibling)
     ((executable-find "emacsclient"))
     (t sibling))))

(defun yunge-mcp--connection-arguments ()
  "Return emacsclient arguments that select the running Yunge server."
  (require 'server)
  (if server-use-tcp
      (list "--server-file"
            (expand-file-name server-name server-auth-dir))
    (list "--socket-name" server-name)))

(defun yunge-mcp--json-write (file object)
  "Write JSON OBJECT to FILE."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert (json-serialize object
                              :null-object nil
                              :false-object :false))
      (json-pretty-print-buffer)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n")))))

(defun yunge-mcp--write-runtime ()
  "Write the connection manifest consumed by the installed server."
  (require 'yunge-server)
  (yunge-server-start)
  (yunge-mcp--json-write
   (yunge-mcp--runtime-file)
   (list
    :version 1
    :emacsclient (yunge-mcp--emacsclient-program)
    :connectionArguments
    (vconcat (yunge-mcp--connection-arguments)))))

;;;###autoload
(defun yunge-mcp-register-clients (clients)
  "Register Yunge MCP in the user configuration of CLIENTS.
CLIENTS are desired targets; they need not currently be installed."
  (interactive (list (yunge-mcp-clients-read)))
  (setq clients
        (yunge-mcp-clients-register clients (yunge-mcp-program)))
  (yunge-mcp-clients-save-choice clients)
  (message "Yunge MCP clients: %s"
           (if clients
               (mapconcat #'yunge-mcp-clients-display-name clients ", ")
             "None")))

(defun yunge-mcp--install-artifacts ()
  "Install the built server and write its runtime manifest."
  (let ((source (yunge-mcp--built-program))
        (target (yunge-mcp-program)))
    (unless (file-executable-p source)
      (error "Cargo did not produce the Yunge MCP executable: %s" source))
    (make-directory (file-name-directory target) t)
    (copy-file source target t t nil t)
    (unless (eq system-type 'windows-nt)
      (set-file-modes target (logior (file-modes target) #o111)))
    (yunge-mcp--write-runtime)))

(defun yunge-mcp--build-sentinel (process _event complete)
  "Finish installing after build PROCESS exits and call COMPLETE."
  (when (and (memq (process-status process) '(exit signal failed))
             (not (process-get process 'yunge-mcp-finished)))
    (process-put process 'yunge-mcp-finished t)
    (when (eq process yunge-mcp--build-process)
      (setq yunge-mcp--build-process nil))
    (let ((failure
           (if (not (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process))))
               (list 'error (format "Yunge MCP build failed; see %s"
                                    (buffer-name (process-buffer process))))
             (condition-case error-data
                 (progn
                   (yunge-mcp--install-artifacts)
                   nil)
               (error error-data)))))
      (if failure
          (progn
            (display-buffer (process-buffer process))
            (display-warning 'yunge-mcp (error-message-string failure) :error))
        (message "Yunge MCP is ready at %s" (yunge-mcp-program)))
      (when complete
        (funcall complete failure)))))

(defun yunge-mcp--start-build (&optional complete)
  "Build and install Yunge MCP, then call COMPLETE with nil or an error."
  (when (process-live-p yunge-mcp--build-process)
    (user-error "Yunge MCP is already being built"))
  (let ((cargo (executable-find "cargo")))
    (unless cargo
      (user-error "Cargo is required to build Yunge MCP"))
    (let ((buffer (get-buffer-create yunge-mcp--build-buffer-name))
          process)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Yunge MCP build\n\n"))
        (setq default-directory yunge-config-directory)
        (compilation-mode))
      (setq process
            (make-process
             :name "yunge-mcp-build"
             :buffer buffer
             :command
             (list cargo "build" "--release" "--locked"
                   "--manifest-path" yunge-mcp--native-manifest
                   "--target-dir"
                   (yunge-mcp--cargo-target-directory))
             :connection-type 'pipe
             :coding 'utf-8-unix
             :noquery t
             :sentinel
             (lambda (child event)
               (yunge-mcp--build-sentinel child event complete))))
      (unless (process-get process 'yunge-mcp-finished)
        (setq yunge-mcp--build-process process))
      (when (memq (process-status process) '(exit signal failed))
        (yunge-mcp--build-sentinel process "finished" complete))
      (display-buffer buffer)
      (message "Building Yunge MCP...")
      process)))

;;;###autoload
(defun yunge-mcp-install (&optional complete)
  "Build/install Yunge MCP and call COMPLETE with nil or an error."
  (interactive)
  (yunge-mcp--start-build complete))

;;;###autoload
(defun yunge-mcp-setup (&optional complete initial-choice)
  "Build Yunge MCP and register clients only on the first successful setup.
COMPLETE receives nil on success or an error value on failure.  INITIAL-CHOICE,
when non-nil, is (t . CLIENTS), a selection made before this call starts work."
  (interactive)
  (let* ((saved (yunge-mcp-clients-choice))
         (choice (and (eq saved :unselected)
                      (or initial-choice (cons t (yunge-mcp-clients-read)))))
         (clients (cdr choice)))
    (yunge-mcp--start-build
     (lambda (failure)
       (when (and (not failure) choice)
         (setq failure
               (condition-case error-data
                   (progn (yunge-mcp-register-clients clients) nil)
                 (error error-data)))
         (when failure
           (display-warning 'yunge-mcp
                            (error-message-string failure) :error)))
       (when complete
         (funcall complete failure))))))

(provide 'yunge-mcp-setup)

;;; yunge-mcp-setup.el ends here
