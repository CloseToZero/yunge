;;; yunge-grep.el --- Grep results -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-edit)
(require 'yunge-key)

(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function grep-edit-save-changes "grep" ())

(defvar grep-edit-mode-map)
(defvar grep-mode-map)

(defconst yunge-grep-normal-bindings
  '(("j" evil-next-line "next line")
    ("k" evil-previous-line "previous line")
    ("C-j" compilation-next-error "next match")
    ("C-k" compilation-previous-error "previous match")
    ("RET" compile-goto-error "visit")
    ("q" quit-window "quit")
    ("gf" compilation-display-error "show source")
    ("gr" recompile "refresh")
    ("i" grep-change-to-grep-edit-mode "edit results")
    ("]]" compilation-next-file "next file")
    ("[[" compilation-previous-file "previous file")))

(defun yunge-grep--setup-edit-session ()
  "Set up source saving for the current Grep edit session."
  (yunge-edit-setup-result-session #'grep-edit-save-changes))

(with-eval-after-load 'grep
  (add-hook 'grep-edit-mode-hook #'yunge-grep--setup-edit-session)
  (yunge-edit-configure-result-map
   grep-edit-mode-map #'grep-edit-save-changes))

(with-eval-after-load 'evil
  (with-eval-after-load 'grep
    (evil-set-initial-state 'grep-mode 'normal)
    (evil-set-initial-state 'grep-edit-mode 'normal)
    (yunge-key-evil-define 'normal grep-mode-map
                           yunge-grep-normal-bindings)))

(provide 'yunge-grep)

;;; yunge-grep.el ends here
