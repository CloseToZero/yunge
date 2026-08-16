;;; yunge-reader-outline.el --- Reader outline views -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-reader)

(declare-function evil-set-initial-state "evil-core" (mode state))

(defconst yunge-reader-outline-normal-bindings
  '(("G" yunge-reader-outline-last-item "last item")
    ("RET" yunge-reader-outline-visit "visit")
    ("gg" yunge-reader-outline-first-item "first item")
    ("gf" yunge-reader-outline-show "show in reader")
    ("j" yunge-reader-outline-next-item "next item")
    ("k" yunge-reader-outline-previous-item "previous item")
    ("q" quit-window "quit")
    ("<tab>" yunge-reader-outline-toggle "toggle children"))
  "Normal-state bindings for Reader outline views.")

(defvar-keymap yunge-reader-outline-mode-map
  :parent special-mode-map
  "G" #'yunge-reader-outline-last-item
  "RET" #'yunge-reader-outline-visit
  "g g" #'yunge-reader-outline-first-item
  "g f" #'yunge-reader-outline-show
  "j" #'yunge-reader-outline-next-item
  "k" #'yunge-reader-outline-previous-item
  "q" #'quit-window
  "<tab>" #'yunge-reader-outline-toggle)

(defvar-local yunge-reader-outline--reader-buffer nil
  "Reader buffer controlled by this outline view.")

(defvar-local yunge-reader-outline--reader-window nil
  "Reader window controlled by this outline view.")

(defvar-local yunge-reader-outline--entry nil
  "Shared document entry represented by this outline view.")

(defvar-local yunge-reader-outline--document nil
  "Reader document represented by this outline view.")

(defvar-local yunge-reader-outline--data nil
  "Generic outline data rendered in this buffer.")

(defvar-local yunge-reader-outline--items []
  "Vector of outline items rendered in this buffer.")

(defvar-local yunge-reader-outline--collapsed nil
  "Hash table of collapsed outline item indices.")

(defun yunge-reader-outline--header ()
  "Return the header text for the current outline view."
  (let* ((reader yunge-reader-outline--reader-buffer)
         (file
          (and (buffer-live-p reader)
               (with-current-buffer reader
                 (and yunge-reader-document
                      (yunge-reader-document-file
                       yunge-reader-document)))))
         (role
          (and (buffer-live-p reader)
               (with-current-buffer reader
                 (yunge-reader-view-role)))))
    (format " Outline: %s%s"
            (if file (file-name-nondirectory file) "closed document")
            (if role (format "  [%s]" (capitalize (symbol-name role))) ""))))

(define-derived-mode yunge-reader-outline-mode
  special-mode "Reader Outline"
  "Display a document outline for one specific Reader view."
  (setq-local truncate-lines t)
  (setq-local header-line-format
              '(:eval (yunge-reader-outline--header)))
  (setq-local yunge-reader-outline--collapsed
              (make-hash-table :test #'eql)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'yunge-reader-outline-mode 'normal)
  (yunge-key-evil-define
   'normal yunge-reader-outline-mode-map
   yunge-reader-outline-normal-bindings))

(defun yunge-reader-outline--index-at-point ()
  "Return the outline item index at point, or nil."
  (get-text-property
   (line-beginning-position) 'yunge-reader-outline-index))

(defun yunge-reader-outline--item-at-point ()
  "Return the outline item at point, or nil."
  (get-text-property
   (line-beginning-position) 'yunge-reader-outline-item))

(defun yunge-reader-outline--has-children-p (index)
  "Return whether the item at INDEX has outline children."
  (and (natnump index)
       (< (1+ index) (length yunge-reader-outline--items))
       (< (yunge-reader-outline-item-depth
           (aref yunge-reader-outline--items index))
          (yunge-reader-outline-item-depth
           (aref yunge-reader-outline--items (1+ index))))))

(defun yunge-reader-outline--insert-item (index item)
  "Insert outline ITEM with stable INDEX."
  (let* ((start (point))
         (depth (yunge-reader-outline-item-depth item))
         (children (yunge-reader-outline--has-children-p index))
         (collapsed
          (and children
               (gethash index yunge-reader-outline--collapsed)))
         (marker
          (cond (collapsed "+ ")
                (children "- ")
                (t "  "))))
    (insert (make-string (* 2 (min depth 20)) ?\s)
            marker
            (yunge-reader-outline-item-title item)
            "\n")
    (add-text-properties
     start (1- (point))
     (list 'yunge-reader-outline-index index
           'yunge-reader-outline-item item
           'mouse-face 'highlight
           'rear-nonsticky t))))

(defun yunge-reader-outline--render ()
  "Render the current generic outline data."
  (let ((selected (yunge-reader-outline--index-at-point))
        (inhibit-read-only t)
        hidden-depth)
    (erase-buffer)
    (if (zerop (length yunge-reader-outline--items))
        (insert "This document has no outline entries.\n")
      (dotimes (index (length yunge-reader-outline--items))
        (let* ((item (aref yunge-reader-outline--items index))
               (depth (yunge-reader-outline-item-depth item)))
          (when (and hidden-depth (<= depth hidden-depth))
            (setq hidden-depth nil))
          (unless hidden-depth
            (yunge-reader-outline--insert-item index item)
            (when (and (yunge-reader-outline--has-children-p index)
                       (gethash index
                                yunge-reader-outline--collapsed))
              (setq hidden-depth depth))))))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (when selected
      (when-let* ((position
                   (text-property-any
                    (point-min) (point-max)
                    'yunge-reader-outline-index selected)))
        (goto-char position)))))

(defun yunge-reader-outline-set-data (outline)
  "Render generic OUTLINE in the current outline view."
  (unless (yunge-reader--outline-valid-p outline)
    (error "Cannot display an invalid Reader outline"))
  (setq yunge-reader-outline--data outline
        yunge-reader-outline--items
        (vconcat (yunge-reader-outline-data-items outline)))
  (yunge-reader-outline--render))

(defun yunge-reader-outline--buffer-name (reader)
  "Return an outline buffer name for READER."
  (format "*Reader Outline: %s*" (buffer-name reader)))

(defun yunge-reader-outline-create-buffer
    (reader window entry document outline)
  "Create an outline buffer for READER and WINDOW.
ENTRY and DOCUMENT identify the shared resource.  Render OUTLINE."
  (unless (and (buffer-live-p reader)
               (window-live-p window)
               (eq (window-buffer window) reader))
    (error "Cannot create an outline without a live Reader window"))
  (let ((buffer
         (generate-new-buffer
          (yunge-reader-outline--buffer-name reader))))
    (with-current-buffer buffer
      (yunge-reader-outline-mode)
      (setq yunge-reader-outline--reader-buffer reader
            yunge-reader-outline--reader-window window
            yunge-reader-outline--entry entry
            yunge-reader-outline--document document)
      (yunge-reader-outline-set-data outline))
    buffer))

(defun yunge-reader-outline--target ()
  "Return the Reader buffer and window controlled by this outline view."
  (let ((reader yunge-reader-outline--reader-buffer)
        (window yunge-reader-outline--reader-window)
        (entry yunge-reader-outline--entry)
        (document yunge-reader-outline--document))
    (unless
        (and (buffer-live-p reader)
             (window-live-p window)
             (eq (window-buffer window) reader)
             (yunge-reader--entry-current-p entry)
             (eq (yunge-reader--document-entry-state entry) 'ready)
             (eq document
                 (yunge-reader--document-entry-document entry))
             (with-current-buffer reader
               (and (eq entry yunge-reader--document-entry)
                    (eq document yunge-reader-document))))
      (user-error "The Reader view for this outline is no longer live"))
    (list reader window)))

(defun yunge-reader-outline--follow (select-reader)
  "Follow the item at point.
When SELECT-READER is non-nil, leave the Reader window selected."
  (let ((item (yunge-reader-outline--item-at-point))
        (index (yunge-reader-outline--index-at-point)))
    (unless item
      (user-error "There is no outline item at point"))
    (if (not (yunge-reader-outline-item-action item))
        (if (yunge-reader-outline--has-children-p index)
            (yunge-reader-outline-toggle)
          (user-error "This outline item has no destination"))
      (pcase-let ((`(,reader ,window)
                   (yunge-reader-outline--target)))
        (if select-reader
            (progn
              (select-window window)
              (with-current-buffer reader
                (yunge-reader--follow-outline-item item)))
          (save-selected-window
            (select-window window)
            (with-current-buffer reader
              (yunge-reader--follow-outline-item item))))))))

(defun yunge-reader-outline-visit ()
  "Visit the outline item at point and select its Reader window."
  (interactive)
  (yunge-reader-outline--follow t))

(defun yunge-reader-outline-show ()
  "Show the outline item at point without leaving the outline window."
  (interactive)
  (yunge-reader-outline--follow nil))

(defun yunge-reader-outline-next-item (&optional count)
  "Move forward COUNT visible outline items."
  (interactive "p")
  (let ((origin (point)))
    (forward-line (or count 1))
    (unless (yunge-reader-outline--item-at-point)
      (goto-char origin)
      (user-error "There is no further outline item"))
    (beginning-of-line)))

(defun yunge-reader-outline-previous-item (&optional count)
  "Move backward COUNT visible outline items."
  (interactive "p")
  (yunge-reader-outline-next-item (- (or count 1))))

(defun yunge-reader-outline-first-item ()
  "Move to the first visible outline item."
  (interactive)
  (goto-char (point-min))
  (unless (yunge-reader-outline--item-at-point)
    (user-error "This document has no outline items")))

(defun yunge-reader-outline-last-item ()
  "Move to the last visible outline item."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (unless (yunge-reader-outline--item-at-point)
    (user-error "This document has no outline items")))

(defun yunge-reader-outline-toggle ()
  "Toggle the children of the outline item at point."
  (interactive)
  (let ((index (yunge-reader-outline--index-at-point)))
    (unless (yunge-reader-outline--has-children-p index)
      (user-error "This outline item has no children"))
    (if (gethash index yunge-reader-outline--collapsed)
        (remhash index yunge-reader-outline--collapsed)
      (puthash index t yunge-reader-outline--collapsed))
    (yunge-reader-outline--render)))

(provide 'yunge-reader-outline)

;;; yunge-reader-outline.el ends here
