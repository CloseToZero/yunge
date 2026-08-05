;;; yunge-expreg.el --- Structural selection -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

(declare-function evil-add-command-properties "evil-common")
(declare-function evil-visual-select "evil-states"
                  (beginning end &optional type direction message))
(declare-function expreg-contract "expreg")
(declare-function expreg-expand "expreg")

(defconst yunge-expreg-bindings
  '(("C-=" yunge-expreg-expand "expand selection")
    ("C--" yunge-expreg-contract "contract selection")))

(defun yunge-expreg--select (command)
  "Run structural selection COMMAND and enter Evil Visual state."
  (funcall command)
  (when (region-active-p)
    ;; Expreg returns an Emacs half-open region.  Let Evil convert those exact
    ;; boundaries instead of treating its end as a Visual character position.
    (evil-visual-select (region-beginning) (region-end) 'char)))

(defun yunge-expreg-expand ()
  "Expand the Visual selection to the next enclosing text unit."
  (interactive)
  (yunge-expreg--select #'expreg-expand))

(defun yunge-expreg-contract ()
  "Contract a selection previously expanded by `yunge-expreg-expand'."
  (interactive)
  (yunge-expreg--select #'expreg-contract))

(defun yunge-expreg--setup ()
  "Expose structural selection after Expreg is ready."
  (with-eval-after-load 'evil
    ;; Expreg owns the active region while these commands run.  Expanding it
    ;; to Evil's inclusive Visual range first would change the next match.
    (evil-add-command-properties 'yunge-expreg-expand :keep-visual t)
    (evil-add-command-properties 'yunge-expreg-contract :keep-visual t)
    (yunge-key-evil-define '(normal visual) global-map
                           yunge-expreg-bindings)))

(elpaca expreg
  (yunge-expreg--setup))

(provide 'yunge-expreg)

;;; yunge-expreg.el ends here
