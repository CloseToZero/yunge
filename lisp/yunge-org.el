;;; yunge-org.el --- Org integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

(declare-function org-in-regexp "org" (regexp &optional nlines visually))
(declare-function org-region-active-p "org" ())

(defvar org-id-link-consider-parent-id)
(defvar org-id-link-to-org-use-id)
(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(defvar org-link-plain-re)

(setq org-id-link-consider-parent-id t
      org-id-link-to-org-use-id 'create-if-interactive)

(defconst yunge-org-note-bindings
  '(("l" org-insert-link "insert link")
    ("s" org-store-link "store link")))

(defun yunge-org--insert-link-at-normal-state-eol
    (function &rest arguments)
  "Call FUNCTION at the insertion side of a Normal-state EOL.
Keep point unchanged when FUNCTION is editing a region or existing link."
  (if (or (org-region-active-p)
          (org-in-regexp org-link-bracket-re 1)
          (org-in-regexp org-link-angle-re)
          (org-in-regexp org-link-plain-re))
      (apply function arguments)
    (apply #'yunge-evil-call-after-normal-state-eol
           function arguments)))

(advice-add 'org-insert-link :around
            #'yunge-org--insert-link-at-normal-state-eol)

(yunge-key-define yunge-note-map yunge-org-note-bindings)

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-note-map yunge-org-note-bindings))

(provide 'yunge-org)

;;; yunge-org.el ends here
