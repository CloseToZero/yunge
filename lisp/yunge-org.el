;;; yunge-org.el --- Org integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-fold-hide-subtree "org-fold")
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
(declare-function org-in-regexp "org" (regexp &optional nlines visually))
(declare-function org-region-active-p "org" ())
(declare-function yunge-jump-history-track-command
                  "yunge-jump-history" (command))

(defvar org-id-link-consider-parent-id)
(defvar org-id-link-to-org-use-id)
(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(defvar org-link-plain-re)
(defvar org-mode-map)

(setq org-id-link-consider-parent-id t
      org-id-link-to-org-use-id 'create-if-interactive)

(defconst yunge-org-normal-visual-bindings
  '(("<tab>" org-cycle "cycle visibility")
    ("S-TAB" org-shifttab "cycle global visibility")
    ("<backtab>" org-shifttab nil)
    ("M-h" org-metaleft "promote structure")
    ("M-j" org-metadown "move structure down")
    ("M-k" org-metaup "move structure up")
    ("M-l" org-metaright "demote structure")
    ("M-H" org-shiftmetaleft "promote subtree")
    ("M-L" org-shiftmetaright "demote subtree")))

(defconst yunge-org-normal-bindings
  '(("RET" org-open-at-point "open at point")
    ("C-j" org-next-visible-heading "next heading")
    ("C-k" org-previous-visible-heading "previous heading")
    ("gf" org-open-at-point "open at point")
    ("za" org-cycle "toggle fold")
    ("zA" org-shifttab "cycle all folds")
    ("zc" org-fold-hide-subtree "close fold")
    ("zC" yunge-org-close-child-folds "close child folds")
    ("zo" yunge-org-open-fold "open fold")
    ("zO" org-fold-show-subtree "open child folds")))

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
(yunge-jump-history-track-command 'org-open-at-point)

(defun yunge-org-open-fold ()
  "Show the current entry and its direct child headings."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-fold-show-entry)
    (org-fold-show-children)))

(defun yunge-org-close-child-folds ()
  "Show the current entry with each child subtree closed."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-fold-hide-subtree)
    (org-fold-show-entry)
    (org-fold-show-children)))

(yunge-key-define yunge-note-map yunge-org-note-bindings)

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-note-map yunge-org-note-bindings))

(with-eval-after-load 'evil
  (with-eval-after-load 'org
    (yunge-key-evil-define '(normal visual) org-mode-map
                           yunge-org-normal-visual-bindings)
    (yunge-key-evil-define 'normal org-mode-map
                           yunge-org-normal-bindings)))

(provide 'yunge-org)

;;; yunge-org.el ends here
