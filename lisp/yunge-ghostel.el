;;; yunge-ghostel.el --- Terminal integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)
(require 'yunge-state)

(declare-function evil-redo "evil-commands" (&optional count))
(declare-function evil-undo "evil-commands" (&optional count))
(declare-function ghostel--send-encoded
                  "ghostel" (key-name mods &optional utf8))
(declare-function ghostel--send-event "ghostel" ())
(declare-function ghostel-alt-screen-p "ghostel" ())
(declare-function ghostel-project "ghostel" (&optional arg))
(declare-function yunge-ghostel-evil--ensure-prompt-input
                  "yunge-ghostel-evil" ())
(declare-function yunge-ghostel-evil--line-mode-p
                  "yunge-ghostel-evil" ())
(declare-function yunge-ghostel-evil--prompt-session-p
                  "yunge-ghostel-evil" ())
(declare-function yunge-ghostel-evil-mode
                  "yunge-ghostel-evil" (&optional arg))

(defvar ghostel--input-mode)
(defvar ghostel-mode-hook)
(defvar ghostel-module-auto-install)
(defvar ghostel-module-directory)
(defvar ghostel-buffer-name-function)
(defvar ghostel-readonly-fake-cursor)
(defvar ghostel-semi-char-mode-map)
(defvar ghostel-shell)
(defvar project-prefix-map)
(defvar project-switch-commands)
(defvar yunge-ghostel-evil-mode-map)
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

(defconst yunge-ghostel-evil-bindings
  '(([remap evil-undo] yunge-ghostel-undo nil)
    ([remap evil-redo] yunge-ghostel-redo nil)))

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

(defun yunge-ghostel--powershell-p ()
  "Return non-nil when the current local Ghostel shell is PowerShell."
  (and (not (file-remote-p default-directory))
       (let* ((program (if (consp ghostel-shell)
                           (car ghostel-shell)
                         ghostel-shell))
              (name (and program
                         (downcase (file-name-base program)))))
         (member name '("powershell" "pwsh")))))

(defun yunge-ghostel-undo (count)
  "Undo shell input COUNT times using the current line editor."
  (interactive "p")
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (yunge-ghostel-evil--ensure-prompt-input)
    (dotimes (_ (or count 1))
      (ghostel--send-encoded
       (if (yunge-ghostel--powershell-p) "z" "_") "ctrl")))
   ((yunge-ghostel-evil--line-mode-p)
    (evil-undo count))
   (t (user-error "Ghostel renderer buffers cannot be undone directly"))))

(defun yunge-ghostel-redo (count)
  "Redo shell input COUNT times using the current line editor."
  (interactive "p")
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (yunge-ghostel-evil--ensure-prompt-input)
    (if (yunge-ghostel--powershell-p)
        (dotimes (_ (or count 1))
          (ghostel--send-encoded "y" "ctrl"))
      (message "Redo is not supported by the terminal line editor")))
   ((yunge-ghostel-evil--line-mode-p)
    (evil-redo count))
   (t (user-error "Ghostel renderer buffers cannot be redone directly"))))

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
  "Set up shell history and Evil bindings for Ghostel."
  (yunge-key-define ghostel-semi-char-mode-map
                    yunge-ghostel-history-bindings)
  (yunge-key-evil-define '(normal visual operator motion)
                         yunge-ghostel-evil-mode-map
                         yunge-ghostel-mouse-bindings)
  (yunge-key-evil-define '(normal visual)
                         yunge-ghostel-evil-mode-map
                         yunge-ghostel-evil-bindings))

(defun yunge-ghostel--enable-evil-in-existing-buffers ()
  "Enable Evil integration in existing Ghostel buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'ghostel-mode)
        (yunge-ghostel-evil-mode 1)))))

(defun yunge-ghostel--setup-evil ()
  "Load and activate Evil integration for Ghostel."
  (require 'yunge-ghostel-evil)
  (add-hook 'ghostel-mode-hook #'yunge-ghostel-evil-mode)
  (yunge-ghostel--setup-keys)
  (yunge-ghostel--enable-evil-in-existing-buffers))

(elpaca ghostel
  (setq ghostel-module-auto-install 'download
        ghostel-module-directory
        (yunge-var-subdirectory "ghostel")
        ghostel-readonly-fake-cursor nil)
  (when (eq system-type 'windows-nt)
    (setq ghostel-shell (yunge-ghostel--windows-shell-spec)))
  (with-eval-after-load 'evil
    (with-eval-after-load 'ghostel
      (yunge-ghostel--setup-evil)))
  (yunge-key-define yunge-toggle-map yunge-ghostel-terminal-bindings)
  (yunge-key-define project-prefix-map yunge-ghostel-project-bindings)
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(yunge-ghostel-project "Ghostel") t))
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-toggle-map yunge-ghostel-terminal-bindings)
    (yunge-key-add-which-key-descriptions
     project-prefix-map yunge-ghostel-project-bindings)))

(provide 'yunge-ghostel)

;;; yunge-ghostel.el ends here
