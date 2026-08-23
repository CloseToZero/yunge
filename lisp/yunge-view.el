;;; yunge-view.el --- Read-only text viewing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function evil-insert-state "evil-states" ())
(declare-function evil-normalize-keymaps "evil-core" (&optional state))
(declare-function View-exit "view" ())

(defvar evil-local-mode)
(defvar yunge-toggle-map)

(defconst yunge-view-normal-bindings
  '(("q" View-quit "quit view")
    ("i" yunge-view-edit "edit"))
  "Normal-state bindings that add meaning beyond ordinary Evil navigation.")

(defconst yunge-view-toggle-bindings
  '(("v" view-mode "view mode")))

(defun yunge-view-edit ()
  "Exit View mode in the current buffer and enter Evil Insert state."
  (interactive)
  (View-exit)
  (evil-insert-state))

(defun yunge-view--normalize-evil-keymaps ()
  "Refresh Evil keymaps after View mode changes in the current buffer."
  (when (bound-and-true-p evil-local-mode)
    (evil-normalize-keymaps)))

(with-eval-after-load 'yunge-evil
  (yunge-key-define yunge-toggle-map yunge-view-toggle-bindings)
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-toggle-map yunge-view-toggle-bindings)))

(with-eval-after-load 'evil
  (add-hook 'view-mode-hook #'yunge-view--normalize-evil-keymaps)
  (with-eval-after-load 'view
    (yunge-key-evil-define-minor-mode
     'normal 'view-mode yunge-view-normal-bindings)))

(provide 'yunge-view)

;;; yunge-view.el ends here
