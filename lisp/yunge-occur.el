;;; yunge-occur.el --- Occur results -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-result-edit)
(require 'yunge-key)

(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function occur-next "replace" (&optional n))
(declare-function occur-prev "replace" (&optional n))
(declare-function occur-mode-display-occurrence "replace" ())

(defvar occur-edit-mode-map)
(defvar occur-mode-map)

(defconst yunge-occur-normal-bindings
  '(("j" evil-next-line "next line")
    ("k" evil-previous-line "previous line")
    ("C-j" yunge-occur-next-match "next match")
    ("C-k" yunge-occur-previous-match "previous match")
    ("RET" occur-mode-goto-occurrence "visit")
    ("q" quit-window "quit")
    ("gf" occur-mode-display-occurrence "show source")
    ("gr" revert-buffer "refresh")
    ("i" occur-edit-mode "edit results")))

(defun yunge-occur-next-match (&optional count)
  "Move forward COUNT matches and preview the source, keeping focus here."
  (interactive "p")
  (let ((count (or count 1)))
    ;; Start outside this line's match; Evil keeps point before its newline.
    (cond
     ((< count 0)
      (beginning-of-line)
      (occur-prev (- count)))
     ((> count 0)
      (end-of-line)
      (occur-next count))))
  (occur-mode-display-occurrence))

(defun yunge-occur-previous-match (&optional count)
  "Move backward COUNT matches and preview the source, keeping focus here."
  (interactive "p")
  (yunge-occur-next-match (- (or count 1))))

(defun yunge-occur--setup-edit-session ()
  "Set up source saving for the current Occur edit session."
  (yunge-result-edit-setup #'occur-cease-edit))

(with-eval-after-load 'replace
  (add-hook 'occur-edit-mode-hook #'yunge-occur--setup-edit-session)
  (yunge-result-edit-configure-map
   occur-edit-mode-map #'occur-cease-edit))

(with-eval-after-load 'evil
  (with-eval-after-load 'replace
    (evil-set-initial-state 'occur-mode 'normal)
    (evil-set-initial-state 'occur-edit-mode 'normal)
    (yunge-key-evil-define 'normal occur-mode-map
                           yunge-occur-normal-bindings)))

(provide 'yunge-occur)

;;; yunge-occur.el ends here
