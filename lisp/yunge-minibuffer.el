;;; yunge-minibuffer.el --- Minibuffer editing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function evil-local-set-key "evil-core")
(declare-function evil-insert "evil-commands")

(defvar evil-echo-state)
(defvar evil-want-minibuffer)

;; Evil reads this before it installs its minibuffer setup hook.
(setq evil-want-minibuffer t)

(defconst yunge-minibuffer--return-keys
  (list (kbd "RET") (kbd "<return>"))
  "Return key events used by terminal and GUI minibuffers.")

(defun yunge-minibuffer--return (event)
  "Run the current minibuffer map's command for Return EVENT."
  (interactive (list last-command-event))
  (let* ((map (current-local-map))
         (key (key-description (vector event)))
         (command (or (keymap-lookup map key)
                      (keymap-lookup map "RET"))))
    (unless (commandp command)
      (user-error "The current minibuffer map has no Return command"))
    (call-interactively command)))

(defun yunge-minibuffer--setup ()
  "Enter Insert state and restore Return in Evil Normal state."
  ;; Do not let Evil's state message overwrite the active prompt.
  (setq-local evil-echo-state nil)
  (evil-insert 1)
  (dolist (key yunge-minibuffer--return-keys)
    (evil-local-set-key 'normal key #'yunge-minibuffer--return)))

(with-eval-after-load 'evil
  ;; Run after Evil creates the minibuffer's state-specific keymaps.
  (add-hook 'minibuffer-setup-hook #'yunge-minibuffer--setup t))

(provide 'yunge-minibuffer)

;;; yunge-minibuffer.el ends here
