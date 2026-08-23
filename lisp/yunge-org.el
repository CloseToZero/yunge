;;; yunge-org.el --- Org integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
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
(declare-function org-at-table-hline-p "org-table" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-beginning-of-line "org" (&optional n))
(declare-function org-end-of-line "org" (&optional n))
(declare-function org-fold-hide-subtree "org-fold")
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
(declare-function org-in-regexp "org" (regexp &optional nlines visually))
(declare-function org-insert-heading-respect-content
                  "org" (&optional invisible-ok))
(declare-function org-insert-item "org-list" (&optional checkbox))
(declare-function org-insert-todo-heading-respect-content
                  "org" (&optional arg))
(declare-function org-move-item-down "org-list" ())
(declare-function org-open-at-point "org" (&optional arg))
(declare-function org-region-active-p "org" ())
(declare-function org-table-insert-row "org-table" (&optional arg))
(declare-function org-table-begin "org-table" ())
(declare-function org-table-current-column "org-table" ())
(declare-function org-table-end "org-table" ())
(declare-function org-table-goto-column
                  "org-table" (n &optional on-delim force))
(declare-function org-up-heading-safe "org" ())
(declare-function org-edit-src-abort "org-src" ())
(declare-function org-edit-src-exit "org-src" ())
(declare-function shuying-org-preview-overlay-at
                  "shuying-org" (position))
(declare-function yunge-avy-make-projection
                  "yunge-avy" (&rest arguments))
(declare-function yunge-jump-history-track-command
                  "yunge-jump-history" (command))

(defvar org-id-link-consider-parent-id)
(defvar org-id-link-to-org-use-id)
(defvar org-auto-align-tags)
(defvar org-latex-compiler)
(defvar org-latex-packages-alist)
(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(defvar org-link-frame-setup)
(defvar org-link-plain-re)
(defvar org-mode-map)
(defvar org-src-mode-map)
(defvar org-tags-column)
(defvar org-table-automatic-realign)
(defvar evil-move-beyond-eol)
(defvar evil-respect-visual-line-mode)
(defvar yunge-avy-candidate-project-functions)

(autoload 'shuying-org-mode "shuying-org" nil t)
(autoload 'shuying-org-preview "shuying-org" nil t)
(autoload 'shuying-org-preview-buffer "shuying-org" nil t)

(defun yunge-org--avy-shuying-projection (beginning end _window)
  "Project a Shuying preview containing BEGINNING through END for Avy."
  (when-let* (((fboundp 'shuying-org-preview-overlay-at))
              (overlay (shuying-org-preview-overlay-at beginning))
              ((<= end (overlay-end overlay))))
    (let ((anchor (overlay-start overlay)))
      (yunge-avy-make-projection
       :identity overlay
       :beginning anchor
       :end (min (1+ anchor) (overlay-end overlay))
       :target beginning))))

(with-eval-after-load 'yunge-avy
  (add-hook 'yunge-avy-candidate-project-functions
            #'yunge-org--avy-shuying-projection))

(add-hook 'org-mode-hook #'shuying-org-mode)

(setq org-id-link-consider-parent-id t
      org-id-link-to-org-use-id 'create-if-interactive
      org-auto-align-tags nil
      org-tags-column 0
      org-table-automatic-realign nil)

(with-eval-after-load 'ox-latex
  (setq org-latex-compiler "xelatex")
  (add-to-list 'org-latex-packages-alist
               '("" "amssymb" t ("xelatex")))
  (add-to-list 'org-latex-packages-alist
               '("UTF8" "ctex" t ("xelatex"))))

(defun yunge-org-table--parse-line (separator)
  "Return the current table line split around SEPARATOR.
The return value is (PREFIX FIELDS SUFFIX).  PREFIX includes the
opening pipe and SUFFIX includes the closing pipe."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (first (cl-position ?| line))
         (last (cl-position ?| line :from-end t)))
    (when (and first last (< first last))
      (list (substring line 0 (1+ first))
            (split-string
             (substring line (1+ first) last)
             (regexp-quote (char-to-string separator)) nil)
            (substring line last)))))

(defun yunge-org-table--replace-line (parts separator)
  "Replace the current line from PARTS joined with SEPARATOR."
  (let ((beginning (line-beginning-position))
        (end (line-end-position)))
    (delete-region beginning end)
    (goto-char beginning)
    (insert (nth 0 parts)
            (mapconcat #'identity (nth 1 parts)
                       (char-to-string separator))
            (nth 2 parts))))

(defun yunge-org-table--compact-current-row ()
  "Give every field in the current table row minimum padding."
  (let ((column (max 1 (org-table-current-column)))
        (parts (yunge-org-table--parse-line ?|)))
    (when parts
      (setcar (cdr parts)
              (mapcar
               (lambda (field)
                 (concat " " (string-trim field) " "))
               (nth 1 parts)))
      (yunge-org-table--replace-line parts ?|)
      (beginning-of-line)
      (org-table-goto-column column))))

(defun yunge-org-table--transform-hlines (beginning end function)
  "Apply FUNCTION to hline segments between BEGINNING and END."
  (save-excursion
    (goto-char beginning)
    (while (< (point) end)
      (when (org-at-table-hline-p)
        (let ((parts (yunge-org-table--parse-line ?+)))
          (when parts
            (setcar (cdr parts) (funcall function (nth 1 parts)))
            (yunge-org-table--replace-line parts ?+))))
      (forward-line))))

(defun yunge-org-table--call-without-realignment (function &rest arguments)
  "Call FUNCTION with ARGUMENTS without implicit table alignment."
  (cl-letf (((symbol-function 'org-table-align)
             (lambda (&rest _arguments))))
    (apply function arguments)))

(defun yunge-org-table--insert-row-compactly
    (function &optional argument)
  "Call row insertion FUNCTION and compact the inserted row."
  (let ((result
         (yunge-org-table--call-without-realignment
          function argument)))
    (when (and (org-at-table-p) (not (org-at-table-hline-p)))
      (yunge-org-table--compact-current-row))
    result))

(defun yunge-org-table--insert-column-compactly (function)
  "Call column insertion FUNCTION without changing existing widths."
  (let* ((column (max 1 (org-table-current-column)))
         (index (1- column))
         (beginning (copy-marker (org-table-begin)))
         (end (copy-marker (org-table-end) t))
         result)
    (unwind-protect
        (progn
          (setq result
                (yunge-org-table--call-without-realignment function))
          (save-excursion
            (goto-char beginning)
            (while (< (point) end)
              (unless (org-at-table-hline-p)
                (let* ((parts (yunge-org-table--parse-line ?|))
                       (fields (and parts (nth 1 parts))))
                  (when (< index (length fields))
                    (setcar (nthcdr index fields) "  ")
                    (yunge-org-table--replace-line parts ?|))))
              (forward-line)))
          (yunge-org-table--transform-hlines
           beginning end
           (lambda (segments)
             (let ((position (min index (length segments))))
               (append (cl-subseq segments 0 position)
                       (list "--")
                       (nthcdr position segments)))))
          result)
      (set-marker beginning nil)
      (set-marker end nil))))

(defun yunge-org-table--delete-column-without-realignment (function)
  "Call column deletion FUNCTION without changing remaining widths."
  (let* ((index (1- (max 1 (org-table-current-column))))
         (beginning (copy-marker (org-table-begin)))
         (end (copy-marker (org-table-end) t))
         result)
    (unwind-protect
        (progn
          (setq result
                (yunge-org-table--call-without-realignment function))
          (yunge-org-table--transform-hlines
           beginning end
           (lambda (segments)
             (if (< index (length segments))
                 (append (cl-subseq segments 0 index)
                         (nthcdr (1+ index) segments))
               segments)))
          result)
      (set-marker beginning nil)
      (set-marker end nil))))

(defun yunge-org-table--move-column-without-realignment
    (function &optional left)
  "Call column movement FUNCTION without changing cell widths.
When LEFT is non-nil, also move the hline segment to the left."
  (let* ((index (1- (max 1 (org-table-current-column))))
         (other (+ index (if left -1 1)))
         (beginning (copy-marker (org-table-begin)))
         (end (copy-marker (org-table-end) t))
         result)
    (unwind-protect
        (progn
          (setq result
                (yunge-org-table--call-without-realignment
                 function left))
          (yunge-org-table--transform-hlines
           beginning end
           (lambda (segments)
             (when (and (>= other 0) (< other (length segments)))
               (cl-rotatef (nth index segments) (nth other segments)))
             segments))
          result)
      (set-marker beginning nil)
      (set-marker end nil))))

;; These structural commands align unconditionally, independently of
;; `org-table-automatic-realign'.  Keep source layout text-first while
;; leaving an explicit `org-table-align' untouched.
(with-eval-after-load 'org-table
  (advice-add 'org-table-insert-row :around
              #'yunge-org-table--insert-row-compactly)
  (advice-add 'org-table-insert-column :around
              #'yunge-org-table--insert-column-compactly)
  (advice-add 'org-table-delete-column :around
              #'yunge-org-table--delete-column-without-realignment)
  (advice-add 'org-table-move-column :around
              #'yunge-org-table--move-column-without-realignment)
  (dolist (command '(org-table-move-cell-up
                     org-table-move-cell-down
                     org-table-move-cell-left
                     org-table-move-cell-right))
    (advice-add command :around
                #'yunge-org-table--call-without-realignment)))

(defconst yunge-org-command-bindings
  '(("p" shuying-org-preview "preview LaTeX")
    ("P" shuying-org-preview-buffer "preview all LaTeX")
    ("t" org-todo "change TODO state")))

(defvar-keymap yunge-org-command-map
  :doc "Keymap for Org commands.")

(yunge-key-define yunge-org-command-map yunge-org-command-bindings)

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
    ("gh" org-up-element "parent element")
    ("gl" org-down-element "inner element")
    ("gH" yunge-org-top-heading "outermost heading")
    ("[E" org-backward-element "previous element")
    ("]E" org-forward-element "next element")
    ("[h" org-backward-heading-same-level
     "previous same-level heading")
    ("]h" org-forward-heading-same-level
     "next same-level heading")
    ("[l" org-previous-link "previous link")
    ("]l" org-next-link "next link")
    ("[c" org-babel-previous-src-block "previous source block")
    ("]c" org-babel-next-src-block "next source block")))

(defconst yunge-org-normal-bindings
  `(("RET" yunge-org-open-at-point-same-window "open here")
    ("<C-return>" yunge-org-insert-heading-below
     "insert heading below")
    ("<C-S-return>" yunge-org-insert-todo-heading-below
     "insert TODO heading below")
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
    ("zO" org-fold-show-subtree "open child folds")
    ([localleader] ,yunge-org-command-map nil)))

(defvar-keymap yunge-org-link-map
  :doc "Org link commands.")

(defconst yunge-org-link-bindings
  '(("i" org-insert-link "insert link")
    ("s" org-store-link "store link")))

(defconst yunge-org-note-bindings
  `(("l" ,yunge-org-link-map "link")))

(defun yunge-org-beginning-of-line ()
  "Move to the Org content start.
From the content start, move to the physical beginning of line."
  (interactive)
  (let ((org-special-ctrl-a/e t)
        (visual-line-mode
         (and evil-respect-visual-line-mode visual-line-mode)))
    (org-beginning-of-line)))

(defun yunge-org-top-heading ()
  "Move to the outermost heading containing point."
  (interactive)
  (while (org-up-heading-safe)))

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

(defun yunge-org--insert-heading-below (command)
  "Call heading insertion COMMAND at visible line end and enter Insert state."
  (end-of-visible-line)
  (call-interactively command)
  (evil-insert nil))

(defun yunge-org-insert-heading-below ()
  "Insert a heading after the current Org contents and enter Insert state."
  (interactive)
  (yunge-org--insert-heading-below
   #'org-insert-heading-respect-content))

(defun yunge-org-insert-todo-heading-below ()
  "Insert a TODO heading after the current Org contents and enter Insert state."
  (interactive)
  (yunge-org--insert-heading-below
   #'org-insert-todo-heading-respect-content))

(defun yunge-org-open-at-point-same-window ()
  "Open at point, preferring the selected window for file links."
  (interactive)
  (let ((org-link-frame-setup
         (cons (cons 'file #'find-file) org-link-frame-setup)))
    (org-open-at-point)))

(defun yunge-org--silence-mark-ring-push (function &rest arguments)
  "Call FUNCTION with ARGUMENTS without displaying its confirmation."
  (let ((inhibit-message t))
    (apply function arguments)))

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
(advice-add 'org-mark-ring-push :around
            #'yunge-org--silence-mark-ring-push)
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

(yunge-key-define yunge-org-link-map yunge-org-link-bindings)
(yunge-key-define yunge-note-map yunge-org-note-bindings)

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-note-map yunge-org-note-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-org-link-map yunge-org-link-bindings))

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
    (evil-add-command-properties 'yunge-org-top-heading :jump t)
    (yunge-key-evil-define 'motion org-mode-map
                           yunge-org-motion-bindings)
    (yunge-key-evil-define '(normal visual) org-mode-map
                           yunge-org-normal-visual-bindings)
    (yunge-key-evil-define 'normal org-mode-map
                           yunge-org-normal-bindings)))

(with-eval-after-load 'evil
  (with-eval-after-load 'org-src
    (define-key org-src-mode-map [remap evil-save-and-close]
                #'org-edit-src-exit)
    (define-key org-src-mode-map [remap evil-save-modified-and-close]
                #'org-edit-src-exit)
    (define-key org-src-mode-map [remap evil-quit]
                #'org-edit-src-abort)))

(with-eval-after-load 'org
  (define-key org-mode-map [remap org-latex-preview]
              #'shuying-org-preview))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-org-command-map yunge-org-command-bindings))

(provide 'yunge-org)

;;; yunge-org.el ends here
