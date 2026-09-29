;;; yunge-mcp-clients.el --- Register Yunge MCP clients -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'yunge-state)

(defgroup yunge-mcp nil
  "Expose Yunge capabilities through Model Context Protocol."
  :group 'applications)

(defconst yunge-mcp-clients--names
  '((codex . "Codex")
    (claude-code . "Claude Code")
    (gemini . "Gemini CLI")
    (cursor . "Cursor")
    (vscode . "Visual Studio Code"))
  "Supported client identifiers and display names.")

(defun yunge-mcp-clients-display-name (client)
  "Return the display name for CLIENT."
  (or (alist-get client yunge-mcp-clients--names)
      (symbol-name client)))

(defun yunge-mcp-clients-read ()
  "Read desired clients, including an explicit None choice."
  (let* ((names (cons "None" (mapcar #'cdr yunge-mcp-clients--names)))
         (selected
          (completing-read-multiple
           "Register Yunge MCP for clients (or None): " names nil t))
         (none (cl-some (lambda (name)
                          (string-equal-ignore-case name "None"))
                        selected)))
    (when (or (null selected)
              (and none (cdr selected)))
      (user-error "Choose clients or None explicitly"))
    (unless none
      (mapcar
       (lambda (name)
         (car (cl-rassoc name yunge-mcp-clients--names
                         :test #'string-equal-ignore-case)))
       selected))))

(defun yunge-mcp-clients--choice-file ()
  "Return the saved client selection file."
  (expand-file-name "yunge-mcp/clients.json" yunge-var-directory))

(defun yunge-mcp-clients-choice ()
  "Return saved clients, nil for None, or :unselected when never chosen."
  (let ((file (yunge-mcp-clients--choice-file)))
    (if (not (file-exists-p file))
        :unselected
      (let* ((record (yunge-mcp-clients--json-object file))
             (clients (gethash "clients" record :missing)))
        (unless (and (eql (gethash "version" record) 1)
                     (vectorp clients)
                     (cl-every #'stringp clients))
          (user-error "Invalid saved Yunge MCP clients: %s" file))
        (condition-case nil
            (yunge-mcp-clients-validate
             (mapcar #'intern (append clients nil)))
          (error
           (user-error "Invalid saved Yunge MCP clients: %s" file)))))))

(defun yunge-mcp-clients-save-choice (clients)
  "Record CLIENTS after successful registration; nil records None."
  (setq clients (yunge-mcp-clients-validate clients))
  (yunge-mcp-clients--write-json
   (yunge-mcp-clients--choice-file)
   (list :version 1 :clients (vconcat (mapcar #'symbol-name clients)))))

(defun yunge-mcp-clients-validate (clients)
  "Return CLIENTS without duplicates after validating them."
  (let (result)
    (dolist (client clients (nreverse result))
      (unless (assq client yunge-mcp-clients--names)
        (user-error "Unsupported Yunge MCP client: %S" client))
      (unless (memq client result)
        (push client result)))))

(defun yunge-mcp-clients--write-file (file contents)
  "Replace FILE with CONTENTS through a same-directory temporary file.
Preserve an existing file's mode and remove the temporary file on failure."
  (let* ((file (if (file-symlink-p file) (file-truename file) file))
         (directory (file-name-directory file))
         (mode (and (file-exists-p file) (file-modes file)))
         temporary)
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary
                (make-temp-file (expand-file-name ".yunge-mcp-" directory)))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-buffer
              (insert contents)
              (write-region (point-min) (point-max) temporary nil 'silent)))
          (when mode
            (set-file-modes temporary mode))
          (rename-file temporary file t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun yunge-mcp-clients--json-object (file)
  "Read FILE as a JSON object, or return an empty object if absent."
  (if (not (file-exists-p file))
      (make-hash-table :test #'equal)
    (let ((value
           (condition-case error-data
               (with-temp-buffer
                 (insert-file-contents file)
                 (json-parse-buffer
                  :object-type 'hash-table
                  :array-type 'array
                  :null-object :null
                  :false-object :false))
             (error
              (user-error "Cannot read client configuration %s: %s"
                          file (error-message-string error-data))))))
      (unless (hash-table-p value)
        (user-error "Client configuration is not a JSON object: %s" file))
      value)))

(defun yunge-mcp-clients--write-json (file object)
  "Write JSON OBJECT to FILE without truncating the old file on failure."
  (with-temp-buffer
    (insert (json-serialize object
                            :null-object :null
                            :false-object :false))
    (json-pretty-print-buffer)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (yunge-mcp-clients--write-file file (buffer-string))))

(defun yunge-mcp-clients--register-json (file servers-key program &optional type)
  "Register PROGRAM in FILE under SERVERS-KEY.
TYPE, when non-nil, is the stdio type spelling required by the client."
  (let* ((configuration (yunge-mcp-clients--json-object file))
         (missing (make-symbol "missing"))
         (servers (gethash servers-key configuration missing))
         (server (make-hash-table :test #'equal)))
    (when (eq servers missing)
      (setq servers (make-hash-table :test #'equal)))
    (unless (hash-table-p servers)
      (user-error "%s is not an object in %s" servers-key file))
    (when type
      (puthash "type" type server))
    (puthash "command" program server)
    (puthash "args" [] server)
    (puthash "yunge" server servers)
    (puthash servers-key servers configuration)
    (yunge-mcp-clients--write-json file configuration)))

(defun yunge-mcp-clients--codex-config-file ()
  "Return the user-level Codex configuration file."
  (expand-file-name
   "config.toml"
   (file-name-as-directory
    (or (getenv "CODEX_HOME")
        (expand-file-name ".codex/" "~")))))

(defun yunge-mcp-clients--edit-codex-config (program file configuration)
  "Ask PROGRAM to update Codex FILE's TOML CONFIGURATION.
Return the edited TOML text.  Reject missing, failed, or incompatible helpers
before touching FILE."
  (unless (file-executable-p program)
    (user-error "Yunge MCP helper is unavailable; run M-x yunge-mcp-install"))
  (let ((stderr-file (make-temp-file "yunge-mcp-codex-stderr-"))
        (request (json-serialize
                  (list :program program :configuration configuration)))
        status stdout stderr)
    (unwind-protect
        (progn
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8-unix)
                  (coding-system-for-write 'utf-8-unix))
              (setq status
                    (call-process-region
                     request nil program nil (list t stderr-file) nil
                     "edit-codex-config")))
            (setq stdout (buffer-string)))
          (with-temp-buffer
            (insert-file-contents stderr-file)
            (setq stderr (string-trim (buffer-string))))
          (unless (equal status 0)
            (user-error
             "Cannot update Codex configuration %s: %s"
             file
             (if (string-empty-p stderr)
                 (format "helper exited %S; run M-x yunge-mcp-install" status)
               stderr)))
          (let ((response
                 (condition-case nil
                     (json-parse-string stdout
                                        :object-type 'hash-table
                                        :null-object :null)
                   (error nil))))
            (unless (and (hash-table-p response)
                         (stringp (gethash "configuration" response)))
              (user-error
               (concat "Yunge MCP helper returned an invalid Codex edit response; "
                       "run M-x yunge-mcp-install")))
            (gethash "configuration" response)))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun yunge-mcp-clients--register-codex (program)
  "Register PROGRAM in the user-level Codex configuration."
  (let ((file (yunge-mcp-clients--codex-config-file)))
    (let ((configuration
           (if (file-exists-p file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))
             "")))
      (yunge-mcp-clients--write-file
       file (yunge-mcp-clients--edit-codex-config
             program file configuration)))))

(defun yunge-mcp-clients--claude-config-file ()
  "Return the user-level Claude Code configuration file."
  (expand-file-name ".claude.json" "~"))

(defun yunge-mcp-clients--gemini-config-file ()
  "Return the user-level Gemini CLI configuration file."
  (expand-file-name ".gemini/settings.json" "~"))

(defun yunge-mcp-clients--cursor-config-file ()
  "Return the global Cursor MCP configuration file."
  (expand-file-name ".cursor/mcp.json" "~"))

(defun yunge-mcp-clients--vscode-config-file ()
  "Return the default Visual Studio Code profile's MCP file."
  (pcase system-type
    ('windows-nt
     (expand-file-name
      "Code/User/mcp.json"
      (file-name-as-directory
       (or (getenv "APPDATA")
           (expand-file-name "AppData/Roaming/" "~")))))
    ('darwin
     (expand-file-name
      "Library/Application Support/Code/User/mcp.json" "~"))
    (_
     (expand-file-name
      "Code/User/mcp.json"
      (file-name-as-directory
       (or (getenv "XDG_CONFIG_HOME")
           (expand-file-name ".config/" "~")))))))

(defun yunge-mcp-clients-register (clients program)
  "Register PROGRAM in the configuration of CLIENTS.
CLIENTS are desired targets; they need not currently be installed.  Return the
validated list without duplicates.  Registrations proceed one by one; a later
failure does not roll back earlier client files."
  (unless (and (stringp program) (not (string-empty-p program)))
    (user-error "Yunge MCP program must be a non-empty string"))
  (setq clients (yunge-mcp-clients-validate clients))
  (dolist (client clients)
    (pcase client
      ('codex (yunge-mcp-clients--register-codex program))
      ('claude-code
       (yunge-mcp-clients--register-json
        (yunge-mcp-clients--claude-config-file) "mcpServers" program))
      ('gemini
       (yunge-mcp-clients--register-json
        (yunge-mcp-clients--gemini-config-file) "mcpServers" program))
      ('cursor
       (yunge-mcp-clients--register-json
        (yunge-mcp-clients--cursor-config-file) "mcpServers" program))
      ('vscode
       (yunge-mcp-clients--register-json
        (yunge-mcp-clients--vscode-config-file) "servers" program "stdio"))))
  clients)

(provide 'yunge-mcp-clients)

;;; yunge-mcp-clients.el ends here
