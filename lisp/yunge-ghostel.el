;;; yunge-ghostel.el --- Terminal integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

(declare-function ghostel--send-encoded
                  "ghostel" (key-name mods &optional utf8))
(declare-function ghostel--send-event "ghostel" ())
(declare-function ghostel-alt-screen-p "ghostel" ())
(declare-function ghostel-project "ghostel" (&optional arg))

(defvar evil-ghostel-mode-map)
(defvar ghostel-mode-hook)
(defvar ghostel-module-auto-install)
(defvar ghostel-module-directory)
(defvar ghostel-buffer-name-function)
(defvar ghostel-readonly-fake-cursor)
(defvar ghostel-semi-char-mode-map)
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
  '(("t" yunge-ghostel-project "terminal")))

(defconst yunge-ghostel-history-bindings
  '(("M-n" yunge-ghostel-next-input "next input")
    ("M-p" yunge-ghostel-previous-input "previous input")))

(defconst yunge-ghostel-mouse-bindings
  '(([down-mouse-1] ghostel-mouse-press-or-copy-mode nil)))

(defun yunge-ghostel--windows-shell-spec ()
  "Return the preferred Windows shell command for Ghostel."
  (if-let* ((powershell (or (executable-find "pwsh.exe")
                            (executable-find "powershell.exe"))))
      (list powershell "-NoLogo" "-NoExit" "-Command"
            yunge-ghostel-powershell-command)
    (list (or (executable-find "cmd.exe") "cmd.exe"))))

(defun yunge-ghostel--send-history-key (key)
  "Send shell history KEY without changing alternate-screen shortcuts."
  (if (ghostel-alt-screen-p)
      (ghostel--send-event)
    (ghostel--send-encoded key "")))

(defun yunge-ghostel-next-input ()
  "Request the next shell input in Ghostel."
  (interactive)
  (yunge-ghostel--send-history-key "down"))

(defun yunge-ghostel-previous-input ()
  "Request the previous shell input in Ghostel."
  (interactive)
  (yunge-ghostel--send-history-key "up"))

(defun yunge-ghostel-project (&optional arg)
  "Start a project Ghostel terminal with a stable project-based name.
ARG follows the prefix conventions of `ghostel-project'."
  (interactive "P")
  (require 'ghostel)
  (let ((ghostel-buffer-name-function nil))
    (let ((buffer (ghostel-project arg)))
      (with-current-buffer buffer
        ;; Shell titles often contain the executable path, but project
        ;; terminals are more useful when their project identity stays visible.
        (setq-local ghostel-buffer-name-function nil))
      buffer)))

(defun yunge-ghostel--setup-keys ()
  "Set up shell history and Evil mouse bindings for Ghostel."
  (yunge-key-define ghostel-semi-char-mode-map
                    yunge-ghostel-history-bindings)
  (yunge-key-evil-define '(normal visual operator motion)
                         evil-ghostel-mode-map
                         yunge-ghostel-mouse-bindings))

(elpaca ghostel)

(elpaca evil-ghostel
  (setq ghostel-module-auto-install 'download
        ghostel-module-directory
        (expand-file-name "var/ghostel/" user-emacs-directory)
        ghostel-readonly-fake-cursor nil)
  (when (eq system-type 'windows-nt)
    (setq ghostel-shell (yunge-ghostel--windows-shell-spec)))
  (yunge-key-define yunge-toggle-map yunge-ghostel-terminal-bindings)
  (yunge-key-define project-prefix-map yunge-ghostel-project-bindings)
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(yunge-ghostel-project "Ghostel") t))
  (add-hook 'ghostel-mode-hook #'evil-ghostel-mode)
  (with-eval-after-load 'evil-ghostel
    (yunge-ghostel--setup-keys))
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-toggle-map yunge-ghostel-terminal-bindings)
    (yunge-key-add-which-key-descriptions
     project-prefix-map yunge-ghostel-project-bindings)))

(provide 'yunge-ghostel)

;;; yunge-ghostel.el ends here
