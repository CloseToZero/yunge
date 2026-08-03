;;; yunge-ghostel.el --- Terminal integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

(defvar ghostel-mode-hook)
(defvar ghostel-module-auto-install)
(defvar ghostel-module-directory)
(defvar ghostel-shell)
(defvar project-prefix-map)
(defvar project-switch-commands)
(defvar yunge-toggle-map)

(defconst yunge-ghostel-powershell-command
  (concat
   "& {$originalPrompt=$function:prompt;"
   "$escape=[char]27;$bell=[char]7;"
   "$function:global:prompt={try {"
   "$location=$ExecutionContext.SessionState.Path.CurrentLocation;"
   "if ($location.Provider.Name -eq 'FileSystem') {"
   "$path=$location.ProviderPath.Replace('\\','/');"
   "[Console]::Write(\"$escape]7;file:///$path$bell\")"
   "}} catch {};& $originalPrompt}.GetNewClosure()}")
  "PowerShell startup code that reports its directory via OSC 7.
Ghostel does not inject PowerShell integration, so this wraps the existing
prompt without changing the user's PowerShell profile.")

(defconst yunge-ghostel-terminal-bindings
  '(("t" ghostel "terminal")))

(defconst yunge-ghostel-project-bindings
  '(("t" ghostel-project "terminal")))

(defun yunge-ghostel--windows-shell-spec ()
  "Return the preferred Windows shell command for Ghostel."
  (if-let* ((powershell (or (executable-find "pwsh.exe")
                            (executable-find "powershell.exe"))))
      (list powershell "-NoLogo" "-NoExit" "-Command"
            yunge-ghostel-powershell-command)
    (list (or (executable-find "cmd.exe") "cmd.exe"))))

(elpaca ghostel)

(elpaca evil-ghostel
  (setq ghostel-module-auto-install 'download
        ghostel-module-directory
        (expand-file-name "var/ghostel/" user-emacs-directory))
  (when (eq system-type 'windows-nt)
    (setq ghostel-shell (yunge-ghostel--windows-shell-spec)))
  (yunge-key-define yunge-toggle-map yunge-ghostel-terminal-bindings)
  (yunge-key-define project-prefix-map yunge-ghostel-project-bindings)
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(ghostel-project "Ghostel") t))
  (add-hook 'ghostel-mode-hook #'evil-ghostel-mode)
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-toggle-map yunge-ghostel-terminal-bindings)
    (yunge-key-add-which-key-descriptions
     project-prefix-map yunge-ghostel-project-bindings)))

(provide 'yunge-ghostel)

;;; yunge-ghostel.el ends here
