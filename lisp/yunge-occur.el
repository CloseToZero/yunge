;;; yunge-occur.el --- Occur results -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-edit)
(require 'yunge-key)

(declare-function evil-set-initial-state "evil-core" (mode state))

(defvar occur-edit-mode-map)
(defvar occur-mode-map)

(defconst yunge-occur-normal-bindings
  '(("j" evil-next-line "next line")
    ("k" evil-previous-line "previous line")
    ("C-j" occur-next "next match")
    ("C-k" occur-prev "previous match")
    ("RET" occur-mode-goto-occurrence "visit")
    ("q" quit-window "quit")
    ("gf" occur-mode-display-occurrence "show source")
    ("gr" revert-buffer "refresh")
    ("i" occur-edit-mode "edit results")))

(defun yunge-occur--setup-edit-session ()
  "Set up source saving for the current Occur edit session."
  (yunge-edit-setup-result-session #'occur-cease-edit))

(with-eval-after-load 'replace
  (add-hook 'occur-edit-mode-hook #'yunge-occur--setup-edit-session)
  (yunge-edit-configure-result-map
   occur-edit-mode-map #'occur-cease-edit))

(with-eval-after-load 'evil
  (with-eval-after-load 'replace
    (evil-set-initial-state 'occur-mode 'normal)
    (evil-set-initial-state 'occur-edit-mode 'normal)
    (yunge-key-evil-define 'normal occur-mode-map
                           yunge-occur-normal-bindings)))

(provide 'yunge-occur)

;;; yunge-occur.el ends here
