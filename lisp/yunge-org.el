;;; yunge-org.el --- Org integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

(declare-function evil-add-command-properties "evil-common"
                  (command &rest properties))
(declare-function evil-declare-motion "evil-common" (command))
(declare-function evil-end-of-line-or-visual-line "evil-commands"
                  (count))
(declare-function evil-move-cursor-back "evil-common" (&optional force))
(declare-function evil-append-line "evil-commands" (count))
(declare-function evil-insert "evil-commands" (count))
(declare-function evil-insert-line "evil-commands" (count))
(declare-function evil-open-above "evil-commands" (count))
(declare-function evil-open-below "evil-commands" (count))
(declare-function org-at-heading-or-item-p "org" ())
(declare-function org-at-heading-p "org" (&optional _ignored))
(declare-function org-at-item-checkbox-p "org-list" ())
(declare-function org-at-item-p "org-list" ())
(declare-function org-at-table-p "org-table" (&optional table-type))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-beginning-of-line "org" (&optional n))
(declare-function org-end-of-line "org" (&optional n))
(declare-function org-fold-hide-subtree "org-fold")
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
(declare-function org-in-regexp "org" (regexp &optional nlines visually))
(declare-function org-insert-item "org-list" (&optional checkbox))
(declare-function org-move-item-down "org-list" ())
(declare-function org-region-active-p "org" ())
(declare-function org-table-insert-row "org-table" (&optional arg))
(declare-function yunge-jump-history-track-command
                  "yunge-jump-history" (command))

(defvar org-id-link-consider-parent-id)
(defvar org-id-link-to-org-use-id)
(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(defvar org-link-plain-re)
(defvar org-mode-map)
(defvar evil-move-beyond-eol)
(defvar evil-respect-visual-line-mode)

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

(defconst yunge-org-motion-bindings
  '(("0" yunge-org-beginning-of-line "beginning of content")
    ("$" yunge-org-end-of-line "end of content")
    ("[h" org-backward-heading-same-level
     "previous same-level heading")
    ("]h" org-forward-heading-same-level
     "next same-level heading")
    ("[l" org-previous-link "previous link")
    ("]l" org-next-link "next link")
    ("[c" org-babel-previous-src-block "previous source block")
    ("]c" org-babel-next-src-block "next source block")))

(defconst yunge-org-normal-bindings
  '(("RET" org-open-at-point "open at point")
    ("I" yunge-org-insert-line "insert at content start")
    ("A" yunge-org-append-line "append to content")
    ("o" yunge-org-open-below "open below")
    ("O" yunge-org-open-above "open above")
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

(defun yunge-org-beginning-of-line ()
  "Move to the Org content start.
From the content start, move to the physical beginning of line."
  (interactive)
  (let ((org-special-ctrl-a/e t)
        (visual-line-mode
         (and evil-respect-visual-line-mode visual-line-mode)))
    (org-beginning-of-line)))

(defun yunge-org-end-of-line (count)
  "Move to the Org content end on the COUNTth line.
From a heading's content end, move past its tags."
  (interactive "p")
  (let ((origin (point))
        (org-special-ctrl-a/e t)
        (visual-line-mode
         (and evil-respect-visual-line-mode visual-line-mode)))
    ;; Normal state represents an insertion boundary by the character
    ;; before it.  Advance from there so `$' can move past heading tags,
    ;; as `org-end-of-line' would from the insertion boundary.
    (when (= (or count 1) 1)
      (let ((target
             (save-excursion
               (org-end-of-line count)
               (point))))
        (when (and (< target (line-end-position))
                   (= (point) (1- target)))
          (goto-char target))))
    (org-end-of-line count)
    (if (or (< (point) (line-end-position))
            (invisible-p (point)))
        (unless evil-move-beyond-eol
          (evil-move-cursor-back t))
      (goto-char origin)
      (evil-end-of-line-or-visual-line count))))

(defun yunge-org-insert-line (count)
  "Enter Insert state at Org content start with repeat COUNT.
On headings and list items, skip their structural prefix."
  (interactive "p")
  (if (org-at-heading-or-item-p)
      (progn
        (beginning-of-line)
        (let ((org-special-ctrl-a/e t))
          (org-beginning-of-line))
        (evil-insert count))
    (evil-insert-line count)))

(defun yunge-org-append-line (count)
  "Enter Insert state at Org content end with repeat COUNT.
On headings, stay before trailing tags and fold ellipses."
  (interactive "p")
  (if (org-at-heading-p)
      (progn
        (end-of-line)
        (let ((org-special-ctrl-a/e t))
          (org-end-of-line))
        (evil-insert count))
    (evil-append-line count)))

(defun yunge-org-open-below (count)
  "Open an Org row or list item below, otherwise call Evil with COUNT."
  (interactive "p")
  (cond
   ((org-at-table-p)
    (org-table-insert-row '(4))
    (evil-insert 1))
   ((org-at-item-p)
    (let ((checkbox (org-at-item-checkbox-p)))
      (beginning-of-line)
      (org-insert-item checkbox)
      (org-move-item-down))
    (evil-insert 1))
   (t
    (evil-open-below count))))

(defun yunge-org-open-above (count)
  "Open an Org row or list item above, otherwise call Evil with COUNT."
  (interactive "p")
  (cond
   ((org-at-table-p)
    (org-table-insert-row)
    (evil-insert 1))
   ((org-at-item-p)
    (let ((checkbox (org-at-item-checkbox-p)))
      (beginning-of-line)
      (org-insert-item checkbox))
    (evil-insert 1))
   (t
    (evil-open-above count))))

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
    (require 'yunge-org-delete)
    (require 'yunge-org-shift)
    (require 'yunge-org-text-object)
    (dolist (binding yunge-org-motion-bindings)
      (evil-declare-motion (nth 1 binding)))
    (evil-add-command-properties 'yunge-org-beginning-of-line
                                 :type 'exclusive)
    (evil-add-command-properties 'yunge-org-end-of-line
                                 :type 'inclusive)
    (yunge-key-evil-define 'motion org-mode-map
                           yunge-org-motion-bindings)
    (yunge-key-evil-define '(normal visual) org-mode-map
                           yunge-org-normal-visual-bindings)
    (yunge-key-evil-define 'normal org-mode-map
                           yunge-org-normal-bindings)))

(provide 'yunge-org)

;;; yunge-org.el ends here
