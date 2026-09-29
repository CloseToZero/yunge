;;; shuying-org-source.el --- Org formula sources for Shuying -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(cl-defstruct (shuying-org-formula
               (:constructor shuying-org-source--make-formula))
  "One Org formula and its document-wide equation number, when known."
  beginning
  end
  source
  block-math-p
  standalone-p
  equation-number)

(defconst shuying-org-source--single-equation-environments
  '("equation" "multline" "subequations")
  "LaTeX environments containing one automatically numbered equation.")

(defconst shuying-org-source--multi-equation-environments
  '("eqnarray" "align" "alignat" "flalign" "gather" "xalignat"
    "xxalignat")
  "LaTeX environments that may contain several numbered equations.")

(defconst shuying-org-source--equation-token-regexp
  (rx
   (or
    "%"
    (seq
     "\\"
     (or
      (seq "begin{" (group (+ (not "}"))) "}")
      (seq "end{" (group (+ (not "}"))) "}")
      (group
       (seq "\\" (? "*")
            (? (seq "[" (* (not "]")) "]"))))
      (group (or "nonumber" "notag"))
      (group (seq "tag" (? "*") (* space) "{"))))))
  "Regexp matching LaTeX structure relevant to equation numbering.")

(defconst shuying-org-source--math-start-regexp
  (rx string-start
      (* (any " \t\n"))
      (or "$" "\\(" "\\[" "\\begin{"))
  "Regexp matching an explicit Org LaTeX math start.")

(defvar-local shuying-org-source--catalog nil
  "Org formulas found at `shuying-org-source--catalog-tick'.")

(defvar-local shuying-org-source--catalog-tick nil
  "Buffer modification tick of `shuying-org-source--catalog'.")

(defun shuying-org-source-current-p ()
  "Return whether the numbered formula catalog describes the current text."
  (equal shuying-org-source--catalog-tick
         (buffer-chars-modified-tick)))

(defun shuying-org-formula-bounds (formula)
  "Return the source bounds of Org FORMULA."
  (cons (shuying-org-formula-beginning formula)
        (shuying-org-formula-end formula)))

(defun shuying-org-source--block-math-p (element)
  "Return whether Org LaTeX ELEMENT contains block math."
  (or (eq (org-element-type element) 'latex-environment)
      (and (string-match-p
            (rx string-start (* (any " \t\n"))
                (or "$$" "\\[" "\\begin{"))
            (org-element-property :value element))
           t)))

(defun shuying-org-source--standalone-block-math-p (element)
  "Return whether block math ELEMENT occupies its physical lines alone."
  (and
   (shuying-org-source--block-math-p element)
   (let ((beginning (org-element-begin element))
         (end (- (org-element-end element)
                 (or (org-element-property :post-blank element) 0))))
     (save-excursion
       (goto-char beginning)
       (and
        (string-match-p
         (rx string-start (* (any " \t")) string-end)
         (buffer-substring-no-properties
          (line-beginning-position) beginning))
        (progn
          (goto-char end)
          (skip-chars-backward " \t\r\n" beginning)
          (string-match-p
           (rx string-start (* (any " \t")) string-end)
           (buffer-substring-no-properties
            (point) (line-end-position)))))))
   t))

(defun shuying-org-source--blank-latex-fragment-p (value)
  "Return whether LaTeX fragment VALUE contains only delimiters and space."
  (let ((trimmed (string-trim value)))
    (seq-some
     (lambda (delimiters)
       (let ((opening (car delimiters))
             (closing (cdr delimiters)))
         (and (string-prefix-p opening trimmed)
              (string-suffix-p closing trimmed)
              (>= (length trimmed)
                  (+ (length opening) (length closing)))
              (string-empty-p
               (string-trim
                (substring trimmed
                           (length opening)
                           (- (length closing))))))))
     '(("\\(" . "\\)")
       ("\\[" . "\\]")
       ("$$" . "$$")
       ("$" . "$")))))

(defun shuying-org-source--blank-latex-environment-p (value)
  "Return whether LaTeX environment VALUE has a whitespace-only body."
  (let ((trimmed (string-trim value)))
    (when (string-match
           (rx string-start "\\begin{" (group (+ (not "}"))) "}")
           trimmed)
      (let* ((name (match-string 1 trimmed))
             (body-beginning (match-end 0))
             (end-regexp
              (concat "\\\\end{" (regexp-quote name) "}\\'")))
        (when (string-match end-regexp trimmed body-beginning)
          (string-empty-p
           (string-trim
            (substring trimmed body-beginning (match-beginning 0)))))))))

(defun shuying-org-source--blank-math-p (element)
  "Return whether Org LaTeX ELEMENT contains no formula content."
  (pcase (org-element-type element)
    ('latex-fragment
     (shuying-org-source--blank-latex-fragment-p
      (org-element-property :value element)))
    ('latex-environment
     (shuying-org-source--blank-latex-environment-p
      (org-element-property :value element)))))

(defun shuying-org-source--latex-fragment-p (datum)
  "Return whether Org DATUM is previewable LaTeX."
  (and (org-element-type-p datum
                           '(latex-fragment latex-environment))
       (not (shuying-org-source--blank-math-p datum))
       (string-match-p
        shuying-org-source--math-start-regexp
        (org-element-property :value datum))))

(defun shuying-org-source--escaped-p (position)
  "Return whether the character at POSITION is backslash-escaped."
  (let ((slashes 0))
    (while (and (> position (point-min))
                (eq (char-before position) ?\\))
      (cl-incf slashes)
      (cl-decf position))
    (cl-oddp slashes)))

(defun shuying-org-source--count-equation-rows (source multi-row-p)
  "Count automatic equation numbers produced by SOURCE.
When MULTI-ROW-P is non-nil, each outer row may receive a number."
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (let ((depth 0)
          (count 0)
          (row-numbered t))
      (while (re-search-forward shuying-org-source--equation-token-regexp nil t)
        (cond
         ((match-beginning 1)
          (cl-incf depth))
         ((match-beginning 2)
          (when (= depth 1)
            (when row-numbered
              (cl-incf count)))
          (setq depth (max 0 (1- depth))))
         ((and multi-row-p (match-beginning 3) (= depth 1))
          (when row-numbered
            (cl-incf count))
          (setq row-numbered t))
         ((and (= depth 1)
               (or (match-beginning 4) (match-beginning 5)))
          (setq row-numbered nil))
         ((and (eq (char-after (match-beginning 0)) ?%)
               (not (shuying-org-source--escaped-p (match-beginning 0))))
          (goto-char (line-end-position)))))
      count)))

(defun shuying-org-source--equation-count (element)
  "Return automatic equation count for Org ELEMENT, or nil.
Nil means ELEMENT is not an automatically numbered environment."
  (let ((source (org-element-property :value element)))
    (when (and (eq (org-element-type element) 'latex-environment)
               (string-match
                "\\`[ \t\n]*\\\\begin{\\([^}]+\\)}" source))
      (let ((environment (match-string 1 source)))
        (cond
         ((member environment shuying-org-source--single-equation-environments)
          (shuying-org-source--count-equation-rows source nil))
         ((member environment shuying-org-source--multi-equation-environments)
          (shuying-org-source--count-equation-rows source t)))))))

(defun shuying-org-source--formula-from-element
    (element &optional equation-number)
  "Return a Shuying formula described by Org ELEMENT.
EQUATION-NUMBER is the next automatic number at the formula's start."
  (let* ((beginning (org-element-begin element))
         (source (substring-no-properties
                 (org-element-property :value element)))
         ;; Org includes the closing line's newline in an environment's
         ;; value.  Retain it for LaTeX, but leave it outside the display
         ;; overlay so blank lines stay visible.
         (end
          (if (eq (org-element-type element) 'latex-environment)
              (- (+ (org-element-property :post-affiliated element)
                    (length source))
                 (if (string-suffix-p "\n" source) 1 0))
            (- (org-element-end element)
               (or (org-element-property :post-blank element) 0)))))
    (shuying-org-source--make-formula
     :beginning beginning
     :end end
     :source source
     :block-math-p (shuying-org-source--block-math-p element)
     :standalone-p (shuying-org-source--standalone-block-math-p element)
     :equation-number equation-number)))

(defun shuying-org-source-at-position (position &optional include-end)
  "Return the Org formula at POSITION, or nil.
INCLUDE-END also accepts the position immediately after its source.
This local lookup does not compute document-wide equation numbers."
  (when (and position
             (<= (point-min) position)
             (<= position (point-max))
             (or include-end (< position (point-max))))
    (save-excursion
      (goto-char position)
      (when-let* ((element (org-element-context))
                  ((shuying-org-source--latex-fragment-p element))
                  (formula (shuying-org-source--formula-from-element element))
                  (bounds (shuying-org-formula-bounds formula)))
        (when (and (<= (car bounds) position)
                   (if include-end
                       (<= position (cdr bounds))
                     (< position (cdr bounds))))
          formula)))))

(defun shuying-org-source--rebuild-catalog ()
  "Parse and number every Org formula in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let ((equation-number 1))
        (setq shuying-org-source--catalog
              (org-element-map
                  (org-element-parse-buffer)
                  '(latex-fragment latex-environment)
                (lambda (element)
                  (let ((count (shuying-org-source--equation-count element)))
                    (prog1
                        (when (shuying-org-source--latex-fragment-p element)
                          (shuying-org-source--formula-from-element
                           element (and count equation-number)))
                      ;; A source-visible environment can still advance TeX's
                      ;; equation counter for later previews.
                      (when count
                        (cl-incf equation-number count))))))
              shuying-org-source--catalog-tick
              (buffer-chars-modified-tick))))))

(defun shuying-org-source-formulas ()
  "Return all Org formulas with document-wide equation numbers."
  (unless (shuying-org-source-current-p)
    (shuying-org-source--rebuild-catalog))
  shuying-org-source--catalog)

(defun shuying-org-source-in-ranges (ranges)
  "Return Org formulas overlapping any of RANGES.
RANGES and formulas are traversed in buffer order."
  (let ((remaining
         (sort (copy-sequence ranges)
               (lambda (left right)
                 (< (car left) (car right)))))
        formulas)
    (catch 'done
      (dolist (formula (shuying-org-source-formulas))
        (pcase-let ((`(,beginning . ,end)
                     (shuying-org-formula-bounds formula)))
          (while (and remaining
                      (<= (cdar remaining) beginning))
            (setq remaining (cdr remaining)))
          (unless remaining
            (throw 'done nil))
          (when (and (< beginning (cdar remaining))
                     (< (caar remaining) end))
            (push formula formulas)))))
    (nreverse formulas)))

(defun shuying-org-source-reset ()
  "Discard the current buffer's numbered formula catalog."
  (setq shuying-org-source--catalog nil
        shuying-org-source--catalog-tick nil))

(provide 'shuying-org-source)

;;; shuying-org-source.el ends here
