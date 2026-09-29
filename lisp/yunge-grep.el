;;; yunge-grep.el --- Grep results -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-result-edit)
(require 'yunge-key)

(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function grep-edit-save-changes "grep" ())
(declare-function compilation-next-error "compile" (n &optional different-file pt))
(declare-function compilation-display-error "compile" ())

(defvar grep-edit-mode-map)
(defvar grep-mode-map)

(defconst yunge-grep-normal-bindings
  '(("j" evil-next-line "next line")
    ("k" evil-previous-line "previous line")
    ("C-j" yunge-grep-next-match "next match")
    ("C-k" yunge-grep-previous-match "previous match")
    ("RET" compile-goto-error "visit")
    ("q" quit-window "quit")
    ("gf" compilation-display-error "show source")
    ("gr" recompile "refresh")
    ("i" grep-change-to-grep-edit-mode "edit results")
    ("]]" compilation-next-file "next file")
    ("[[" compilation-previous-file "previous file")))

(defun yunge-grep-next-match (&optional count)
  "Move forward COUNT matches and preview the source, keeping focus here."
  (interactive "p")
  (compilation-next-error (or count 1))
  (compilation-display-error))

(defun yunge-grep-previous-match (&optional count)
  "Move backward COUNT matches and preview the source, keeping focus here."
  (interactive "p")
  (yunge-grep-next-match (- (or count 1))))

(defun yunge-grep--setup-edit-session ()
  "Set up source saving for the current Grep edit session."
  (yunge-result-edit-setup #'grep-edit-save-changes))

(with-eval-after-load 'grep
  (add-hook 'grep-edit-mode-hook #'yunge-grep--setup-edit-session)
  (yunge-result-edit-configure-map
   grep-edit-mode-map #'grep-edit-save-changes))

(with-eval-after-load 'evil
  (with-eval-after-load 'grep
    (evil-set-initial-state 'grep-mode 'normal)
    (evil-set-initial-state 'grep-edit-mode 'normal)
    (yunge-key-evil-define 'normal grep-mode-map
                           yunge-grep-normal-bindings)))

(provide 'yunge-grep)

;;; yunge-grep.el ends here
