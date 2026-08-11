;;; yunge-org-delete.el --- Org-aware Evil deletion -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'evil)
(require 'org)
(require 'org-list)
(require 'yunge-key)

(defconst yunge-org-delete-bindings
  '(("d" yunge-org-delete "delete")))

(defconst yunge-org-character-delete-bindings
  '(("x" yunge-org-delete-char "delete character")
    ("X" yunge-org-delete-backward-char
     "delete previous character")))

(defconst yunge-org-visual-delete-bindings
  '(("x" evil-delete-char "delete selection")
    ("X" evil-delete-line "delete selected lines")))

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

(defun yunge-org--use-org-character-delete-p ()
  "Return non-nil when character deletion should use Org."
  (or (org-at-table-p)
      (org-at-heading-p)))

(defun yunge-org--yank-character-delete
    (beginning end type register)
  "Record a character deletion from BEGINNING to END.
TYPE and REGISTER have the same meaning as for `evil-delete'."
  (save-excursion
    (unless register
      (evil-set-register
       ?- (filter-buffer-substring beginning end)))
    (let ((evil-was-yanked-without-register nil))
      (evil-yank beginning end type register))))

(evil-define-operator yunge-org-delete-char
    (count beginning end type register)
  "Delete COUNT characters with Org-aware table and tag handling."
  :motion evil-forward-char
  (interactive "p<R><x>")
  (if (and (< beginning end)
           (yunge-org--use-org-character-delete-p))
      (progn
        (yunge-org--yank-character-delete
         beginning end type register)
        (org-delete-char count))
    (evil-delete-char beginning end type register)))

(evil-define-operator yunge-org-delete-backward-char
    (count beginning end type register)
  "Delete COUNT previous characters with Org-aware table and tag handling."
  :motion evil-backward-char
  :move-point nil
  (interactive "p<R><x>")
  (if (and (< beginning end)
           (yunge-org--use-org-character-delete-p))
      (progn
        (yunge-org--yank-character-delete
         beginning end type register)
        (org-delete-backward-char count))
    (evil-delete-backward-char beginning end type register)))

(yunge-key-evil-define '(normal visual) org-mode-map
                       yunge-org-delete-bindings)
(yunge-key-evil-define 'normal org-mode-map
                       yunge-org-character-delete-bindings)
(yunge-key-evil-define 'visual org-mode-map
                       yunge-org-visual-delete-bindings)

(provide 'yunge-org-delete)

;;; yunge-org-delete.el ends here
