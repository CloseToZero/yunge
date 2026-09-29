;;; yunge-mcp-clients.el --- Register Yunge MCP clients -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup yunge-mcp nil
  "Expose Yunge capabilities through Model Context Protocol."
  :group 'applications)

(defcustom yunge-mcp-client-targets
  '(codex claude-code gemini cursor vscode)
  "Clients that `yunge-mcp-setup' registers by default.
Registration expresses the desired configuration and does not depend on
whether a client is currently installed."
  :type '(set
          (const :tag "Codex" codex)
          (const :tag "Claude Code" claude-code)
          (const :tag "Gemini CLI" gemini)
          (const :tag "Cursor" cursor)
          (const :tag "Visual Studio Code" vscode))
  :group 'yunge-mcp)

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
  "Read desired clients without checking their installation state."
  (let* ((names (mapcar #'cdr yunge-mcp-clients--names))
         (defaults
          (mapcar #'yunge-mcp-clients-display-name
                  yunge-mcp-client-targets))
         (selected
          (completing-read-multiple
           "Register Yunge MCP for clients: " names nil t nil nil defaults)))
    (mapcar
     (lambda (name)
       (car (cl-rassoc name yunge-mcp-clients--names
                       :test #'string-equal-ignore-case)))
     selected)))

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

(defun yunge-mcp-clients--toml-string (string)
  "Return STRING encoded as a TOML basic string."
  (concat
   "\""
   (string-replace
    "\"" "\\\""
    (string-replace "\\" "\\\\" string))
   "\""))

(defun yunge-mcp-clients--codex-section-p (header)
  "Return non-nil when TOML HEADER belongs to Yunge MCP."
  (or (equal header "mcp_servers.yunge")
      (string-prefix-p "mcp_servers.yunge." header)
      (equal header "mcp_servers.\"yunge\"")
      (string-prefix-p "mcp_servers.\"yunge\"." header)))

(defun yunge-mcp-clients--register-codex (program)
  "Register PROGRAM in the user-level Codex configuration."
  (let* ((file (yunge-mcp-clients--codex-config-file))
         (section
          (concat
           "[mcp_servers.yunge]\ncommand = "
           (yunge-mcp-clients--toml-string program)
           "\nargs = []\n\n")))
    (with-temp-buffer
      (when (file-exists-p file)
        (insert-file-contents file))
      (goto-char (point-min))
      (let (start end)
        (while (and (not start)
                    (re-search-forward "^\\[\\([^]\n]+\\)\\][ \t]*$" nil t))
          (when (yunge-mcp-clients--codex-section-p (match-string 1))
            (setq start (line-beginning-position))))
        (if start
            (progn
              (goto-char start)
              (forward-line 1)
              (while (and (not end)
                          (re-search-forward
                           "^\\[\\([^]\n]+\\)\\][ \t]*$" nil t))
                (unless (yunge-mcp-clients--codex-section-p (match-string 1))
                  (setq end (line-beginning-position))))
              (delete-region start (or end (point-max)))
              (goto-char start)
              (insert section))
          (goto-char (point-max))
          (unless (or (bobp) (bolp))
            (insert "\n"))
          (unless (or (bobp)
                      (save-excursion
                        (forward-line -1)
                        (looking-at-p "[ \t]*$")))
            (insert "\n"))
          (insert section)))
      (yunge-mcp-clients--write-file file (buffer-string)))))

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
