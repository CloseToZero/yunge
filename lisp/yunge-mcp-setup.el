;;; yunge-mcp-setup.el --- Set up Yunge MCP -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'yunge-state)

(defvar server-name)
(defvar server-use-tcp)

(declare-function yunge-server-start "yunge-server")

(defgroup yunge-mcp nil
  "Expose Yunge capabilities through Model Context Protocol."
  :group 'applications)

(defcustom yunge-mcp-agent-targets
  '(codex claude-code gemini cursor vscode)
  "Agents that `yunge-mcp-setup' registers by default.
Registration expresses the desired configuration and does not depend on
whether an agent is currently installed."
  :type '(set
          (const :tag "Codex" codex)
          (const :tag "Claude Code" claude-code)
          (const :tag "Gemini CLI" gemini)
          (const :tag "Cursor" cursor)
          (const :tag "Visual Studio Code" vscode))
  :group 'yunge-mcp)

(defconst yunge-mcp--agent-names
  '((codex . "Codex")
    (claude-code . "Claude Code")
    (gemini . "Gemini CLI")
    (cursor . "Cursor")
    (vscode . "Visual Studio Code"))
  "Supported agent identifiers and display names.")

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

(defun yunge-mcp--json-read-object (file)
  "Return the JSON object in FILE as a plist, or nil when absent."
  (if (not (file-exists-p file))
      nil
    (condition-case error-data
        (with-temp-buffer
          (insert-file-contents file)
          (if (string-empty-p (string-trim (buffer-string)))
              nil
            (json-parse-buffer
             :object-type 'plist
             :array-type 'array
             :null-object nil
             :false-object :false)))
      (error
       (user-error "Cannot read agent configuration %s: %s"
                   file (error-message-string error-data))))))

(defun yunge-mcp--register-json (file servers-key &optional type)
  "Register Yunge in FILE below SERVERS-KEY.
TYPE, when non-nil, is the stdio type spelling required by the client."
  (let* ((configuration (yunge-mcp--json-read-object file))
         (servers (plist-get configuration servers-key))
         (server
          (append
           (when type (list :type type))
           (list :command (yunge-mcp-program) :args []))))
    (unless (or (null configuration) (listp configuration))
      (user-error "Agent configuration is not a JSON object: %s" file))
    (unless (or (null servers) (listp servers))
      (user-error "%s is not an object in %s" servers-key file))
    (setq servers (plist-put servers :yunge server))
    (yunge-mcp--json-write
     file (plist-put configuration servers-key servers))))

(defun yunge-mcp--codex-config-file ()
  "Return the user-level Codex configuration file."
  (expand-file-name
   "config.toml"
   (file-name-as-directory
    (or (getenv "CODEX_HOME")
        (expand-file-name ".codex/" "~")))))

(defun yunge-mcp--toml-string (string)
  "Return STRING encoded as a TOML basic string."
  (concat
   "\""
   (string-replace
    "\"" "\\\""
    (string-replace "\\" "\\\\" string))
   "\""))

(defun yunge-mcp--codex-section-p (header)
  "Return non-nil when TOML HEADER belongs to Yunge MCP."
  (or (equal header "mcp_servers.yunge")
      (string-prefix-p "mcp_servers.yunge." header)
      (equal header "mcp_servers.\"yunge\"")
      (string-prefix-p "mcp_servers.\"yunge\"." header)))

(defun yunge-mcp--register-codex ()
  "Register Yunge in the user-level Codex configuration."
  (let* ((file (yunge-mcp--codex-config-file))
         (section
          (concat
           "[mcp_servers.yunge]\ncommand = "
           (yunge-mcp--toml-string (yunge-mcp-program))
           "\nargs = []\n\n")))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (when (file-exists-p file)
        (insert-file-contents file))
      (goto-char (point-min))
      (let (start end)
        (while (and (not start)
                    (re-search-forward "^\\[\\([^]\n]+\\)\\][ \t]*$"
                                       nil t))
          (when (yunge-mcp--codex-section-p (match-string 1))
            (setq start (line-beginning-position))))
        (if start
            (progn
              (goto-char start)
              (forward-line 1)
              (while (and (not end)
                          (re-search-forward
                           "^\\[\\([^]\n]+\\)\\][ \t]*$" nil t))
                (unless (yunge-mcp--codex-section-p (match-string 1))
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
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region nil nil file nil 'silent)))))

(defun yunge-mcp--claude-config-file ()
  "Return the user-level Claude Code configuration file."
  (expand-file-name ".claude.json" "~"))

(defun yunge-mcp--gemini-config-file ()
  "Return the user-level Gemini CLI configuration file."
  (expand-file-name ".gemini/settings.json" "~"))

(defun yunge-mcp--cursor-config-file ()
  "Return the global Cursor MCP configuration file."
  (expand-file-name ".cursor/mcp.json" "~"))

(defun yunge-mcp--vscode-config-file ()
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

(defun yunge-mcp--register-agent (agent)
  "Register the stable Yunge MCP program path for AGENT."
  (pcase agent
    ('codex (yunge-mcp--register-codex))
    ('claude-code
     (yunge-mcp--register-json
      (yunge-mcp--claude-config-file) :mcpServers))
    ('gemini
     (yunge-mcp--register-json
      (yunge-mcp--gemini-config-file) :mcpServers))
    ('cursor
     (yunge-mcp--register-json
      (yunge-mcp--cursor-config-file) :mcpServers))
    ('vscode
     (yunge-mcp--register-json
      (yunge-mcp--vscode-config-file) :servers "stdio"))
    (_ (user-error "Unsupported Yunge MCP agent: %S" agent))))

(defun yunge-mcp--agent-display-name (agent)
  "Return the display name for AGENT."
  (or (alist-get agent yunge-mcp--agent-names)
      (symbol-name agent)))

(defun yunge-mcp--agent-from-display-name (name)
  "Return the agent identified by display NAME."
  (car (cl-rassoc name yunge-mcp--agent-names
                  :test #'string-equal-ignore-case)))

(defun yunge-mcp--read-agents ()
  "Read desired agent targets without checking their installation state."
  (let* ((names (mapcar #'cdr yunge-mcp--agent-names))
         (defaults
          (mapcar #'yunge-mcp--agent-display-name
                  yunge-mcp-agent-targets))
         (selected
          (completing-read-multiple
           "Register Yunge MCP for agents: " names nil t nil nil defaults)))
    (mapcar
     #'yunge-mcp--agent-from-display-name
     selected)))

(defun yunge-mcp--validate-agents (agents)
  "Return AGENTS without duplicates after validating them."
  (let (result)
    (dolist (agent agents (nreverse result))
      (unless (assq agent yunge-mcp--agent-names)
        (user-error "Unsupported Yunge MCP agent: %S" agent))
      (unless (memq agent result)
        (push agent result)))))

;;;###autoload
(defun yunge-mcp-register-agents (agents)
  "Register Yunge MCP in the user configuration of AGENTS.
AGENTS are desired targets; they need not currently be installed."
  (interactive (list (yunge-mcp--read-agents)))
  (setq agents (yunge-mcp--validate-agents agents))
  (dolist (agent agents)
    (yunge-mcp--register-agent agent))
  (message
   "Registered Yunge MCP for %s"
   (mapconcat #'yunge-mcp--agent-display-name agents ", ")))

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

(defun yunge-mcp--build-sentinel (process _event agents)
  "Finish installing after build PROCESS exits, then register AGENTS."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'yunge-mcp-finished)))
    (process-put process 'yunge-mcp-finished t)
    (when (eq process yunge-mcp--build-process)
      (setq yunge-mcp--build-process nil))
    (if (not (zerop (process-exit-status process)))
        (progn
          (display-buffer (process-buffer process))
          (display-warning
           'yunge-mcp
           (format "Yunge MCP build failed; see %s"
                   (buffer-name (process-buffer process)))
           :error))
      (condition-case error-data
          (progn
            (yunge-mcp--install-artifacts)
            (when agents
              (yunge-mcp-register-agents agents))
            (message "Yunge MCP is ready at %s" (yunge-mcp-program)))
        (error
         (display-buffer (process-buffer process))
         (display-warning
          'yunge-mcp
          (format "Could not install Yunge MCP: %s"
                  (error-message-string error-data))
          :error))))))

(defun yunge-mcp--start-build (&optional agents)
  "Build and install Yunge MCP, then register optional AGENTS."
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
      (setq agents (yunge-mcp--validate-agents agents))
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
               (yunge-mcp--build-sentinel child event agents))))
      (unless (process-get process 'yunge-mcp-finished)
        (setq yunge-mcp--build-process process))
      (when (memq (process-status process) '(exit signal))
        (yunge-mcp--build-sentinel process "finished" agents))
      (display-buffer buffer)
      (message "Building Yunge MCP...")
      process)))

;;;###autoload
(defun yunge-mcp-install ()
  "Build and install the Yunge MCP server."
  (interactive)
  (yunge-mcp--start-build))

;;;###autoload
(defun yunge-mcp-setup (agents)
  "Build Yunge MCP and register it for the selected AGENTS."
  (interactive (list (yunge-mcp--read-agents)))
  (yunge-mcp--start-build agents))

(provide 'yunge-mcp-setup)

;;; yunge-mcp-setup.el ends here
