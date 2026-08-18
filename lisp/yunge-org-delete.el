;;; yunge-org-delete.el --- Org-aware Evil deletion -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'evil)
(require 'org)
(require 'org-list)
(require 'yunge-key)

(defconst yunge-org-delete-bindings
  '(("d" yunge-org-delete "delete")))

(evil-define-operator yunge-org-delete
    (beginning end type register yank-handler)
  "Delete text while preserving surrounding Org structure."
  (interactive "<R><x><y>")
  (let ((repair-list-p (> end (line-end-position)))
        (evil-this-operator 'evil-delete))
    (evil-delete beginning end type register yank-handler)
    (save-excursion
      (when (and repair-list-p (org-at-item-p))
        (org-list-repair))
      (when (and org-auto-align-tags (org-at-heading-p))
        (org-fix-tags-on-the-fly)))))

(yunge-key-evil-define '(normal visual) org-mode-map
                       yunge-org-delete-bindings)

(provide 'yunge-org-delete)

;;; yunge-org-delete.el ends here
