;;; yunge-org-text-object.el --- Org text objects -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'evil)
(require 'org)
(require 'org-element)
(require 'yunge-key)

(defconst yunge-org-text-object-bindings
  '(("ae" yunge-org-a-context "around Org context")
    ("ie" yunge-org-inner-context "inside Org context")
    ("aE" yunge-org-an-element "around Org element")
    ("iE" yunge-org-inner-element "inside Org element")
    ("ac" yunge-org-a-container "around Org container")
    ("ic" yunge-org-inner-container "inside Org container")
    ("ah" yunge-org-a-subtree "around Org subtree")
    ("ih" yunge-org-inner-subtree "inside Org subtree")))

(defun yunge-org--syntax-end (datum)
  "Return the end of DATUM before trailing blank space."
  (let ((end (org-element-end datum))
        (post-blank (or (org-element-post-blank datum) 0)))
    (if (eq (org-element-class datum) 'object)
        (- end post-blank)
      (org-with-point-at end
        (line-end-position (- post-blank))))))

(defun yunge-org--latex-fragment-inner-bounds (datum)
  "Return inner bounds for the LaTeX fragment DATUM."
  (let* ((begin (org-element-begin datum))
         (end (yunge-org--syntax-end datum))
         (syntax (buffer-substring-no-properties begin end)))
    (cond
     ((and (string-prefix-p "$$" syntax)
           (string-suffix-p "$$" syntax))
      (cons (+ begin 2) (- end 2)))
     ((or (and (string-prefix-p "\\(" syntax)
               (string-suffix-p "\\)" syntax))
          (and (string-prefix-p "\\[" syntax)
               (string-suffix-p "\\]" syntax)))
      (cons (+ begin 2) (- end 2)))
     ((and (string-prefix-p "$" syntax)
           (string-suffix-p "$" syntax))
      (cons (1+ begin) (1- end)))
     (t
      (cons begin end)))))

(defun yunge-org--delimited-element-inner-bounds (datum)
  "Return bounds inside the delimiter lines of DATUM."
  (let ((begin
         (org-with-point-at (org-element-post-affiliated datum)
           (line-beginning-position 2)))
        (end
         (org-with-point-at (org-element-end datum)
           (line-beginning-position
            (- (or (org-element-post-blank datum) 0))))))
    (when (< end begin)
      (setq end begin))
    (cons begin end)))

(defun yunge-org--inner-bounds (datum)
  "Return the inner buffer bounds of Org DATUM."
  (let ((type (org-element-type datum))
        (begin (org-element-begin datum))
        (contents-begin (org-element-contents-begin datum))
        (contents-end (org-element-contents-end datum)))
    (cond
     ((and contents-begin contents-end)
      (cons contents-begin contents-end))
     ((or (string-suffix-p "-block" (symbol-name type))
          (eq type 'latex-environment))
      (yunge-org--delimited-element-inner-bounds datum))
     ((memq type '(code verbatim))
      (cons (1+ begin) (1- (yunge-org--syntax-end datum))))
     ((eq type 'latex-fragment)
      (yunge-org--latex-fragment-inner-bounds datum))
     (t
      (cons (or (org-element-post-affiliated datum) begin)
            (yunge-org--syntax-end datum))))))

(defun yunge-org--datum-bounds (datum inner)
  "Return outer or INNER bounds for Org DATUM."
  (unless datum
    (user-error "No Org structure at point"))
  (let ((bounds
         (if inner
             (yunge-org--inner-bounds datum)
           (cons (org-element-begin datum)
                 (org-element-end datum)))))
    (when (> (car bounds) (cdr bounds))
      (user-error "Org structure has no selectable contents"))
    bounds))

(defun yunge-org--next-object (datum)
  "Return the next Org object after DATUM, or nil."
  (let ((position (org-element-end datum))
        next)
    (while (and (< position (point-max)) (not next))
      (goto-char position)
      (let ((candidate (org-element-context)))
        (if (and candidate
                 (eq (org-element-class candidate) 'object)
                 (>= (org-element-begin candidate)
                     (org-element-end datum)))
            (setq next candidate)
          (setq position (1+ position)))))
    next))

(defun yunge-org--next-element (datum)
  "Return the next Org element at the same level as DATUM, or nil."
  (goto-char (org-element-begin datum))
  (condition-case nil
      (progn
        (org-forward-element)
        (let ((next (org-element-at-point)))
          (and (> (org-element-begin next)
                  (org-element-begin datum))
               next)))
    (error nil)))

(defun yunge-org--previous-object (datum)
  "Return the previous Org object before DATUM, or nil."
  (let ((limit (org-element-begin datum))
        (position (1- (org-element-begin datum)))
        previous)
    (while (and (>= position (point-min)) (not previous))
      (goto-char position)
      (let ((candidate (org-element-context)))
        (if (and candidate
                 (eq (org-element-class candidate) 'object)
                 (<= (org-element-end candidate) limit))
            (setq previous candidate)
          (setq position (1- position)))))
    previous))

(defun yunge-org--previous-element (datum)
  "Return the previous Org element at the same level as DATUM, or nil."
  (goto-char (org-element-begin datum))
  (condition-case nil
      (progn
        (org-backward-element)
        (let ((previous (org-element-at-point)))
          (and (< (org-element-begin previous)
                  (org-element-begin datum))
               previous)))
    (error nil)))

(defun yunge-org--next-context (datum)
  "Return the next Org context in the class of DATUM, or nil."
  (if (eq (org-element-class datum) 'object)
      (yunge-org--next-object datum)
    (yunge-org--next-element datum)))

(defun yunge-org--previous-context (datum)
  "Return the previous Org context in the class of DATUM, or nil."
  (if (eq (org-element-class datum) 'object)
      (yunge-org--previous-object datum)
    (yunge-org--previous-element datum)))

(defun yunge-org--sequence-bounds
    (count beginning end selector next previous inner)
  "Return bounds for COUNT Org units selected by SELECTOR and its steps.
BEGINNING and END describe an existing Visual selection.  When INNER
is non-nil, exclude the units' syntax boundaries."
  (save-excursion
    (goto-char (or beginning (point)))
    (let* ((forward (> count 0))
           (step (if forward next previous))
           (first (funcall selector))
           last)
      (when (and beginning end first
                 (<= beginning (org-element-begin first))
                 (>= end (1- (org-element-end first))))
        (setq first (funcall step first)))
      (unless first
        (user-error "No adjacent Org structure"))
      (setq last first)
      (dotimes (_ (1- (abs count)))
        (setq last (funcall step last))
        (unless last
          (user-error "No adjacent Org structure")))
      (let ((first-bounds (yunge-org--datum-bounds first inner))
            (last-bounds (yunge-org--datum-bounds last inner)))
        (list (min (or beginning (car first-bounds))
                   (car last-bounds))
              (max (or end (cdr first-bounds))
                   (cdr last-bounds)))))))

(defun yunge-org--selection-covers-p (datum beginning end)
  "Return non-nil when BEGINNING and END cover DATUM."
  (and beginning end
       (<= beginning (org-element-begin datum))
       (>= end (1- (org-element-end datum)))))

(defun yunge-org--matching-ancestor (datum predicate)
  "Return the nearest ancestor of DATUM satisfying PREDICATE."
  (while (and datum (not (funcall predicate datum)))
    (setq datum (org-element-parent datum)))
  datum)

(defun yunge-org--next-matching-ancestor (datum predicate)
  "Return the first larger parent of DATUM satisfying PREDICATE."
  (let ((begin (org-element-begin datum))
        (end (org-element-end datum))
        (parent (org-element-parent datum)))
    (while (and parent
                (or (not (funcall predicate parent))
                    (and (= (org-element-begin parent) begin)
                         (= (org-element-end parent) end))))
      (setq parent (org-element-parent parent)))
    parent))

(defun yunge-org--enclosing-bounds
    (count beginning end predicate inner)
  "Return bounds for an enclosing Org structure.
PREDICATE selects its kind.  COUNT climbs through matching parents.
BEGINNING and END describe an existing Visual selection."
  (save-excursion
    (goto-char (or beginning (point)))
    (let ((datum
           (yunge-org--matching-ancestor
            (org-element-context) predicate)))
      (when (and datum
                 (yunge-org--selection-covers-p
                  datum beginning end))
        (setq datum
              (yunge-org--next-matching-ancestor datum predicate)))
      (dotimes (_ (1- (abs count)))
        (setq datum
              (and datum
                   (yunge-org--next-matching-ancestor
                    datum predicate))))
      (let ((bounds (yunge-org--datum-bounds datum inner)))
        (list (car bounds) (cdr bounds))))))

(defun yunge-org--container-p (datum)
  "Return non-nil when DATUM is an Org container."
  (memq (org-element-type datum) org-element-greater-elements))

(defun yunge-org--headline-p (datum)
  "Return non-nil when DATUM is an Org headline."
  (eq (org-element-type datum) 'headline))

(evil-define-text-object yunge-org-a-context
    (count &optional beginning end _type)
  "Select COUNT Org contexts with their syntax."
  (yunge-org--sequence-bounds
   count beginning end #'org-element-context
   #'yunge-org--next-context #'yunge-org--previous-context nil))

(evil-define-text-object yunge-org-inner-context
    (count &optional beginning end _type)
  "Select the contents of COUNT Org contexts."
  (yunge-org--sequence-bounds
   count beginning end #'org-element-context
   #'yunge-org--next-context #'yunge-org--previous-context t))

(evil-define-text-object yunge-org-an-element
    (count &optional beginning end _type)
  "Select COUNT Org elements with their syntax."
  (yunge-org--sequence-bounds
   count beginning end #'org-element-at-point
   #'yunge-org--next-element #'yunge-org--previous-element nil))

(evil-define-text-object yunge-org-inner-element
    (count &optional beginning end _type)
  "Select the contents of COUNT Org elements."
  (yunge-org--sequence-bounds
   count beginning end #'org-element-at-point
   #'yunge-org--next-element #'yunge-org--previous-element t))

(evil-define-text-object yunge-org-a-container
    (count &optional beginning end _type)
  "Select an Org container with its syntax."
  :type line
  (yunge-org--enclosing-bounds
   count beginning end #'yunge-org--container-p nil))

(evil-define-text-object yunge-org-inner-container
    (count &optional beginning end _type)
  "Select the contents of an Org container."
  (yunge-org--enclosing-bounds
   count beginning end #'yunge-org--container-p t))

(evil-define-text-object yunge-org-a-subtree
    (count &optional beginning end _type)
  "Select an Org subtree."
  :type line
  (yunge-org--enclosing-bounds
   count beginning end #'yunge-org--headline-p nil))

(evil-define-text-object yunge-org-inner-subtree
    (count &optional beginning end _type)
  "Select the contents of an Org subtree."
  :type line
  (yunge-org--enclosing-bounds
   count beginning end #'yunge-org--headline-p t))

(yunge-key-evil-define '(visual operator) org-mode-map
                       yunge-org-text-object-bindings)

(provide 'yunge-org-text-object)

;;; yunge-org-text-object.el ends here
