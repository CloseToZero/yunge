;;; shuying-org.el --- Shuying previews in Org -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'shuying-latex)
(require 'subr-x)

(declare-function org-export-get-backend "ox" (name))
(declare-function org-export-get-environment
                  "ox" (&optional backend subtreep ext-plist))
(declare-function org-latex-make-preamble
                  "ox-latex" (info &optional template snippetp))

(cl-defstruct (shuying-org-fragment
               (:constructor shuying-org--make-fragment))
  "The source needed to preview one Org LaTeX fragment."
  beginning
  end
  value
  block-math-p
  standalone-p
  equation-number)

(defconst shuying-org--single-equation-environments
  '("equation" "multline" "subequations")
  "LaTeX environments containing one automatically numbered equation.")

(defconst shuying-org--multi-equation-environments
  '("eqnarray" "align" "alignat" "flalign" "gather" "xalignat"
    "xxalignat")
  "LaTeX environments that may contain several numbered equations.")

(defconst shuying-org--equation-token-regexp
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

(defconst shuying-org--math-start-regexp
  (rx string-start
      (* (any " \t\n"))
      (or "$" "\\(" "\\[" "\\begin{"))
  "Regexp matching an explicit Org LaTeX math start.")

(defvar-local shuying-org--active-start nil
  "Marker at the fragment currently containing point.")

(defvar-local shuying-org--previous-point nil
  "Point before the most recent command.")

(defvar-local shuying-org--previous-tick nil
  "Buffer modification tick before the most recent command.")

(defvar-local shuying-org--visible-window-state nil
  "Last window state populated with Shuying previews.")

(defcustom shuying-org-visible-preview-delay 0.15
  "Idle time before previewing formulas exposed by viewport changes."
  :type 'number
  :group 'shuying)

(defcustom shuying-org-block-math-alignment 'center
  "Horizontal alignment of block math previews.
`center' centers a tightly cropped, standalone block in the window text area.
When the image is too wide to move to the center, it remains at its source
column.  Non-standalone blocks also remain at their source columns.  `source'
always displays block math at its source column.  Inline math is not affected."
  :type '(choice
          (const :tag "Center in the window text area" center)
          (const :tag "Keep the source column" source))
  :group 'shuying)

(defvar-local shuying-org--visible-preview-timer nil
  "Timer for the pending visible preview update.")

(defvar-local shuying-org--pending-window-state nil
  "Window state represented by `shuying-org--visible-preview-timer'.")

(defvar-local shuying-org--changed-overlays nil
  "Preview overlays modified by the current command.")

(defvar-local shuying-org--fragment-catalog nil
  "Org LaTeX fragments found at `shuying-org--catalog-tick'.")

(defvar-local shuying-org--catalog-tick nil
  "Buffer modification tick of `shuying-org--fragment-catalog'.")

(defun shuying-org--viewing-p ()
  "Return whether the current buffer is being read with View mode."
  (bound-and-true-p view-mode))

(defun shuying-org--catalog-current-p ()
  "Return whether the fragment catalog describes the current text."
  (equal shuying-org--catalog-tick
         (buffer-chars-modified-tick)))

(defun shuying-org--fragment-bounds (fragment)
  "Return the source bounds of Org LaTeX FRAGMENT."
  (cons (shuying-org-fragment-beginning fragment)
        (shuying-org-fragment-end fragment)))

(defun shuying-org--block-math-p (element)
  "Return whether Org LaTeX ELEMENT contains block math."
  (or (eq (org-element-type element) 'latex-environment)
      (and (string-match-p
            (rx string-start (* (any " \t\n"))
                (or "$$" "\\[" "\\begin{"))
            (org-element-property :value element))
           t)))

(defun shuying-org--standalone-block-math-p (element)
  "Return whether block math ELEMENT occupies its physical lines alone."
  (and
   (shuying-org--block-math-p element)
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

(defun shuying-org--blank-latex-fragment-p (value)
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

(defun shuying-org--blank-latex-environment-p (value)
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

(defun shuying-org--blank-math-p (element)
  "Return whether Org LaTeX ELEMENT contains no formula content."
  (pcase (org-element-type element)
    ('latex-fragment
     (shuying-org--blank-latex-fragment-p
      (org-element-property :value element)))
    ('latex-environment
     (shuying-org--blank-latex-environment-p
      (org-element-property :value element)))))

(defun shuying-org--latex-fragment-p (datum)
  "Return whether Org DATUM is previewable LaTeX."
  (and (org-element-type-p datum
                           '(latex-fragment latex-environment))
       (not (shuying-org--blank-math-p datum))
       (string-match-p
        shuying-org--math-start-regexp
        (org-element-property :value datum))))

(defun shuying-org--escaped-p (position)
  "Return whether the character at POSITION is backslash-escaped."
  (let ((slashes 0))
    (while (and (> position (point-min))
                (eq (char-before position) ?\\))
      (cl-incf slashes)
      (cl-decf position))
    (cl-oddp slashes)))

(defun shuying-org--count-equation-rows (source multi-row-p)
  "Count automatic equation numbers produced by SOURCE.
When MULTI-ROW-P is non-nil, each outer row may receive a number."
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (let ((depth 0)
          (count 0)
          (row-numbered t))
      (while (re-search-forward shuying-org--equation-token-regexp nil t)
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
               (not (shuying-org--escaped-p (match-beginning 0))))
          (goto-char (line-end-position)))))
      count)))

(defun shuying-org--equation-count (element)
  "Return automatic equation count for Org ELEMENT, or nil.
Nil means ELEMENT is not an automatically numbered environment."
  (let ((source (org-element-property :value element)))
    (when (and (eq (org-element-type element) 'latex-environment)
               (string-match
                "\\`[ \t\n]*\\\\begin{\\([^}]+\\)}" source))
      (let ((environment (match-string 1 source)))
        (cond
         ((member environment shuying-org--single-equation-environments)
          (shuying-org--count-equation-rows source nil))
         ((member environment shuying-org--multi-equation-environments)
          (shuying-org--count-equation-rows source t)))))))

(defun shuying-org--fragment-from-element
    (element &optional equation-number)
  "Return a Shuying fragment described by Org ELEMENT.
EQUATION-NUMBER is the next automatic number at the fragment's start."
  (shuying-org--make-fragment
   :beginning (org-element-begin element)
   :end (- (org-element-end element)
           (or (org-element-property :post-blank element) 0))
   :value (substring-no-properties
           (org-element-property :value element))
   :block-math-p (shuying-org--block-math-p element)
   :standalone-p (shuying-org--standalone-block-math-p element)
   :equation-number equation-number))

(defun shuying-org--fragment-context-at-position (position)
  "Return the previewable Org LaTeX context at POSITION, or nil."
  (when (and position
             (<= (point-min) position)
             (<= position (point-max)))
    (save-excursion
      (goto-char position)
      (let ((fragment (org-element-context)))
        (when (shuying-org--latex-fragment-p fragment)
          (shuying-org--fragment-from-element fragment))))))

(defun shuying-org--fragment-at-position (position)
  "Return the Org LaTeX fragment containing POSITION, or nil."
  (when (and position (< position (point-max)))
    (when-let* ((fragment
                 (shuying-org--fragment-context-at-position position))
                (bounds (shuying-org--fragment-bounds fragment)))
      (when (and (<= (car bounds) position)
                 (< position (cdr bounds)))
        fragment))))

(defun shuying-org--fragment-at-or-ending-at-position (position)
  "Return the Org LaTeX fragment containing or ending at POSITION."
  (when-let* ((fragment
               (shuying-org--fragment-context-at-position position))
              (bounds (shuying-org--fragment-bounds fragment)))
    (when (and (<= (car bounds) position)
               (<= position (cdr bounds)))
      fragment)))

(defun shuying-org--fragment-at-point ()
  "Return the Org LaTeX fragment containing point, or nil."
  (shuying-org--fragment-at-position (point)))

(defun shuying-org--delimited-fragment-p (fragment)
  "Return whether FRAGMENT has explicit LaTeX delimiters."
  (string-match-p
   shuying-org--math-start-regexp
   (shuying-org-fragment-value fragment)))

(defun shuying-org--fragment-at-edit-boundary
    (active-position text-changed)
  "Return the delimited fragment whose end is point, or nil.
ACTIVE-POSITION is the start remembered from the previous command.
TEXT-CHANGED permits a newly closed fragment to become active."
  (when (> (point) (point-min))
    (when-let* ((fragment
                 (shuying-org--fragment-at-position (1- (point))))
                (beginning
                 (shuying-org-fragment-beginning fragment)))
      (when (and (= (shuying-org-fragment-end fragment) (point))
                 (shuying-org--delimited-fragment-p fragment)
                 (or text-changed
                     (equal beginning active-position)))
        fragment))))

(defun shuying-org--fragment-overlays (beginning end)
  "Return Shuying overlays between BEGINNING and END."
  (seq-filter
   (lambda (overlay)
     (overlay-get overlay 'shuying-org))
   (overlays-in beginning end)))

(defun shuying-org-preview-overlay-at (position)
  "Return the displayed Shuying preview overlay containing POSITION.
Return nil when POSITION has no preview or its source is currently visible."
  (seq-find
   (lambda (overlay)
     (and (overlay-get overlay 'shuying-org)
          (overlay-get overlay 'display)))
   (overlays-at position)))

(defun shuying-org--fragment-overlay (fragment)
  "Return the Shuying overlay for FRAGMENT, or nil."
  (pcase-let ((`(,beginning . ,end)
               (shuying-org--fragment-bounds fragment)))
    (seq-find
     (lambda (overlay)
       (and (= (overlay-start overlay) beginning)
            (= (overlay-end overlay) end)))
     (shuying-org--fragment-overlays beginning end))))

(defun shuying-org--fragment-by-beginning (fragments beginning)
  "Return the member of FRAGMENTS starting at BEGINNING, or nil."
  (seq-find
   (lambda (fragment)
     (= (shuying-org-fragment-beginning fragment) beginning))
   fragments))

(defun shuying-org--hide-overlay (overlay)
  "Reveal the source hidden by Shuying OVERLAY."
  (overlay-put overlay 'before-string nil)
  (overlay-put overlay 'display nil))

(defun shuying-org--modified
    (overlay after _beginning _end &optional _length)
  "Mark OVERLAY dirty after its source is modified."
  (unless after
    (unless (overlay-get overlay 'shuying-org-source-beginning)
      (overlay-put overlay 'shuying-org-source-beginning
                   (overlay-start overlay))))
  (shuying-org--hide-overlay overlay)
  (overlay-put overlay 'shuying-org-dirty t)
  (when after
    ;; Invalidate a render that began before this edit.
    (overlay-put
     overlay 'shuying-org-generation
     (1+ (or (overlay-get overlay 'shuying-org-generation) 0)))
    (cl-pushnew overlay shuying-org--changed-overlays)))

(defun shuying-org--sync-overlay-fragment (overlay fragment)
  "Synchronize OVERLAY's source and layout state with FRAGMENT."
  (pcase-let ((`(,beginning . ,end)
               (shuying-org--fragment-bounds fragment)))
    (move-overlay overlay beginning end)
    (overlay-put overlay 'shuying-org-source-beginning beginning)
    (overlay-put overlay 'shuying-org-block-math
                 (shuying-org-fragment-block-math-p fragment))
    (overlay-put overlay 'shuying-org-standalone
                 (shuying-org-fragment-standalone-p fragment)))
  overlay)

(defun shuying-org--layout-context-changed
    (beginning end _old-length)
  "Schedule a layout refresh after text changes from BEGINNING to END."
  (let ((line-beginning
         (save-excursion
           (goto-char beginning)
           (line-beginning-position)))
        (line-end
         (save-excursion
           (goto-char end)
           (line-end-position))))
    (when
        (seq-some
         (lambda (overlay)
           (and (overlay-get overlay 'shuying-org-block-math)
                (not (overlay-get overlay 'shuying-org-dirty))))
         (shuying-org--fragment-overlays line-beginning line-end))
      (setq shuying-org--visible-window-state nil)
      (shuying-org--schedule-visible-preview))))

(defun shuying-org--ensure-overlay (fragment)
  "Return the display overlay for FRAGMENT, creating it if needed."
  (pcase-let* ((`(,beginning . ,end)
                (shuying-org--fragment-bounds fragment))
               (overlay (shuying-org--fragment-overlay fragment))
               (shuying-overlays
                (shuying-org--fragment-overlays beginning end)))
    ;; Org fragments cannot nest.  A differently bounded Shuying overlay is
    ;; stale parser state left behind while delimiters were incomplete.
    (dolist (candidate shuying-overlays)
      (unless (eq candidate overlay)
        (delete-overlay candidate)))
    (dolist (candidate (overlays-in beginning end))
      (when (eq (overlay-get candidate 'org-overlay-type)
                'org-latex-overlay)
        (delete-overlay candidate)))
    (unless overlay
      (setq overlay (make-overlay beginning end nil nil nil))
      (overlay-put overlay 'shuying-org t)
      (overlay-put overlay 'modification-hooks
                   '(shuying-org--modified)))
    (shuying-org--sync-overlay-fragment overlay fragment)))

(defun shuying-org--latex-info ()
  "Return the LaTeX export environment for the current Org buffer."
  (require 'ox-latex)
  (org-export-get-environment (org-export-get-backend 'latex)))

(defun shuying-org--preamble (&optional info)
  "Return the LaTeX preamble for the current Org buffer.
INFO, when non-nil, is an existing LaTeX export environment."
  (org-latex-make-preamble
   (or info (shuying-org--latex-info))
   org-format-latex-header 'snippet))

(defun shuying-org--latex-engine-command (&optional info)
  "Return the preview engine for the current Org buffer.
INFO, when non-nil, is an existing LaTeX export environment."
  (or shuying-latex-engine-command
      (let ((compiler
             (downcase
              (or (plist-get (or info (shuying-org--latex-info))
                             :latex-compiler)
                  ""))))
        (pcase compiler
          ((or "" "latex") '("latex"))
          ("pdflatex" '("pdflatex" "-output-format=dvi"))
          ("xelatex" '("xelatex" "-no-pdf"))
          ("lualatex" '("lualatex" "--output-format=dvi"))
          (_ (error "Unsupported Org LaTeX compiler: %s" compiler))))))

(defun shuying-org--render-spec (fragment preamble &optional engine)
  "Return the render specification for Org FRAGMENT.
PREAMBLE and optional ENGINE describe its LaTeX document context."
  (let ((bounds (shuying-org--fragment-bounds fragment)))
    (save-excursion
      (goto-char (car bounds))
      (make-shuying-render-spec
       :source
       (shuying-org-fragment-value fragment)
       :preamble preamble
       :engine (or engine (shuying-org--latex-engine-command))
       :backend 'shuying-latex
       :backend-options
       (list :converter shuying-latex-converter-command)
       :output-format "svg"
       ;; The SVG uses CSS currentColor, so rendering is theme-independent.
       ;; Emacs supplies the live display face when rasterizing the image.
       :foreground "Black"
       :background "Transparent"
       :equation-number
       (shuying-org-fragment-equation-number fragment)
       ;; Preserve Org's established dvisvgm preview size.  Its process
       ;; definition applies this adjustment before the user scale.
       :scale (* 1.7
                 (or (plist-get org-format-latex-options :scale) 1.0))
       :cache-version shuying-cache-format-version))))

(defun shuying-org--point-inside-overlay-p (overlay)
  "Return whether point is inside OVERLAY."
  (and (<= (overlay-start overlay) (point))
       (< (point) (overlay-end overlay))))

(defun shuying-org--source-visible-p (overlay)
  "Return whether OVERLAY should reveal its source at point."
  (and (not (shuying-org--viewing-p))
       (shuying-org--point-inside-overlay-p overlay)))

(defun shuying-org--alignment-prefix (overlay image)
  "Return the horizontal alignment prefix for OVERLAY displaying IMAGE."
  (when (and (overlay-get overlay 'shuying-org-block-math)
             (overlay-get overlay 'shuying-org-standalone)
             (eq shuying-org-block-math-alignment 'center))
    (propertize
     " " 'display
     `(space :align-to (- center (0.5 . ,image)))
     'face 'default)))

(defun shuying-org--show-overlay (overlay)
  "Show the cached image belonging to OVERLAY."
  (when-let* ((image (overlay-get overlay 'shuying-org-image)))
    (overlay-put overlay 'before-string
                 (shuying-org--alignment-prefix overlay image))
    (unless (equal (overlay-get overlay 'display) image)
      (overlay-put overlay 'display image)
      ;; Replacing source with an image can expose more text at the bottom of
      ;; a window without changing its start or size.  Let the existing
      ;; viewport scheduler discover formulas in the newly visible region.
      (when (bound-and-true-p shuying-org-mode)
        (setq shuying-org--visible-window-state nil)
        (shuying-org--schedule-visible-preview)))))

(defun shuying-org--image (artifact)
  "Return an image display for ARTIFACT aligned to the text baseline.
Return nil when ARTIFACT has no visible geometry."
  (let* ((metadata (shuying-artifact-metadata artifact))
         (width (plist-get metadata :width))
         (height (plist-get metadata :height))
         (depth (plist-get metadata :depth)))
    (if (and (numberp width) (zerop width)
             (numberp height) (zerop height)
             (numberp depth) (zerop depth))
        nil
      (unless (and (numberp height)
                   (> height 0)
                   (numberp depth)
                   (<= 0 depth height))
        (error "Invalid Shuying artifact geometry: %S" metadata))
      (let ((ascent
             (round
              (* 100
                 (- 1 (/ (max 0.0 (min depth height)) height))))))
        (create-image
         (shuying-artifact-path artifact) nil nil
         :height (cons height 'em)
         :ascent ascent)))))

(defun shuying-org--install-artifact (overlay artifact)
  "Install ARTIFACT and its face-relative image on OVERLAY."
  (let ((image (shuying-org--image artifact)))
    (overlay-put overlay 'shuying-org-artifact
                 (shuying-artifact-path artifact))
    (overlay-put overlay 'shuying-org-image image)
    (overlay-put overlay 'shuying-org-empty-artifact (null image))
    image))

(defun shuying-org--record-render-error
    (overlay error-data report-error)
  "Record OVERLAY's ERROR-DATA and call REPORT-ERROR."
  (shuying-org--hide-overlay overlay)
  (overlay-put overlay 'shuying-org-dirty t)
  (overlay-put overlay 'shuying-org-error error-data)
  (overlay-put overlay 'shuying-org-empty-artifact nil)
  (overlay-put overlay 'shuying-org-specification-hash nil)
  (funcall report-error error-data))

(defun shuying-org--record-display-error
    (overlay error-data report-error)
  "Record OVERLAY's display ERROR-DATA and call REPORT-ERROR."
  (shuying-org--hide-overlay overlay)
  (overlay-put overlay 'shuying-org-dirty nil)
  (overlay-put overlay 'shuying-org-error error-data)
  (overlay-put overlay 'shuying-org-empty-artifact nil)
  (funcall report-error error-data))

(defun shuying-org--finish-render
    (buffer overlay generation artifact error-data report-error)
  "Finish rendering OVERLAY in BUFFER for GENERATION.
ARTIFACT is the completed cache file, or nil when ERROR-DATA is non-nil.
REPORT-ERROR reports a current render failure without duplicating its batch."
  (when (and (buffer-live-p buffer)
             (overlay-buffer overlay)
             (= generation
                (overlay-get overlay 'shuying-org-generation)))
    (with-current-buffer buffer
      (if error-data
          (shuying-org--record-render-error
           overlay error-data report-error)
        (condition-case display-error
            (let ((image
                   (shuying-org--install-artifact overlay artifact)))
              (overlay-put overlay 'shuying-org-dirty nil)
              (overlay-put overlay 'shuying-org-error nil)
              (if (or (null image)
                      (shuying-org--source-visible-p overlay))
                  (shuying-org--hide-overlay overlay)
                (shuying-org--show-overlay overlay)))
          (error
           (shuying-org--record-display-error
            overlay display-error report-error)))))))

(defun shuying-org--render-request
    (fragment specification specification-hash report-error)
  "Return a render request for Org FRAGMENT using SPECIFICATION."
  (let* ((overlay (shuying-org--ensure-overlay fragment))
         (generation
          (1+ (or (overlay-get overlay 'shuying-org-generation) 0)))
         (buffer (current-buffer)))
    (shuying-org--hide-overlay overlay)
    (overlay-put overlay 'shuying-org-dirty nil)
    (overlay-put overlay 'shuying-org-artifact nil)
    (overlay-put overlay 'shuying-org-image nil)
    (overlay-put overlay 'shuying-org-empty-artifact nil)
    (overlay-put overlay 'shuying-org-generation generation)
    (overlay-put overlay 'shuying-org-specification-hash
                 specification-hash)
    (cons
     specification
     (lambda (artifact error-data)
       (shuying-org--finish-render
        buffer overlay generation artifact error-data report-error)))))

(defun shuying-org--preview-fragments
    (fragments &optional stale-only automatic)
  "Request previews for Org FRAGMENTS as one render group.
When STALE-ONLY is non-nil, reuse overlays whose render inputs still match.
When AUTOMATIC is non-nil, silently retain unavailable dependency errors."
  (when fragments
    (let* ((info (shuying-org--latex-info))
           (preamble (shuying-org--preamble info))
           (engine (shuying-org--latex-engine-command info))
           error-reported
           requests)
      (dolist (fragment fragments)
        (let* ((specification
                (shuying-org--render-spec fragment preamble engine))
               (specification-hash
                (shuying-render-spec-hash specification))
               (overlay (shuying-org--fragment-overlay fragment)))
          ;; Layout context can change around an otherwise unchanged formula.
          ;; Refresh it even when the rendered artifact remains reusable.
          (when overlay
            (shuying-org--sync-overlay-fragment overlay fragment))
          (if (and stale-only overlay
                   (or (overlay-get overlay 'shuying-org-image)
                       (overlay-get overlay
                                    'shuying-org-empty-artifact))
                   (equal
                    (overlay-get
                     overlay 'shuying-org-specification-hash)
                    specification-hash))
              (progn
                (overlay-put overlay 'shuying-org-dirty nil)
                (overlay-put overlay 'shuying-org-error nil)
                (unless (or (overlay-get overlay
                                         'shuying-org-empty-artifact)
                            (shuying-org--source-visible-p overlay))
                  (shuying-org--show-overlay overlay)))
            (push
             (shuying-org--render-request
              fragment specification specification-hash
              (lambda (error-data)
                (unless error-reported
                  (setq error-reported t)
                  (unless
                      (and automatic
                           (eq (car-safe error-data)
                               'shuying-latex-unavailable))
                    (display-warning
                     'shuying
                     (error-message-string error-data)
                     :error)))))
             requests))))
      (when requests
        (shuying-render-batch (nreverse requests))))))

(defun shuying-org--preview-fragment (fragment &optional automatic)
  "Request a preview for Org FRAGMENT.
When AUTOMATIC is non-nil, silently retain unavailable dependency errors."
  (let* ((beginning (shuying-org-fragment-beginning fragment))
         (catalog-stale (not (shuying-org--catalog-current-p)))
         (old-fragment
          (and catalog-stale
               (shuying-org--fragment-by-beginning
                shuying-org--fragment-catalog beginning)))
         (catalog (shuying-org--fragments))
         (catalog-fragment
          (shuying-org--fragment-by-beginning catalog beginning)))
    ;; Point tracking uses a local Org context so editing does not parse the
    ;; whole buffer on every command.  Rendering resolves that lightweight
    ;; value against the catalog to obtain document-wide numbering context.
    (setq fragment (or catalog-fragment fragment))
    (shuying-org--preview-fragments
     (list fragment) catalog-stale automatic)
    (when (and catalog-stale
               (or (and old-fragment
                        (shuying-org-fragment-equation-number
                         old-fragment))
                   (and catalog-fragment
                        (shuying-org-fragment-equation-number
                         catalog-fragment)))
               (bound-and-true-p shuying-org-mode)
               (shuying-org--window-state))
      ;; A changed environment can renumber every environment after it.
      ;; Rechecking the viewport updates only previews whose hashes changed.
      (setq shuying-org--visible-window-state nil)
      (shuying-org--schedule-visible-preview t))))

(defun shuying-org--leave-fragment (fragment)
  "Show or refresh Org FRAGMENT after point leaves it."
  (if-let* ((overlay (shuying-org--fragment-overlay fragment)))
      (let ((artifact
             (overlay-get overlay 'shuying-org-artifact)))
        (if (and (shuying-org--catalog-current-p)
                 (not (overlay-get overlay 'shuying-org-dirty))
                 artifact
                 (file-exists-p artifact))
            (shuying-org--show-overlay overlay)
          (shuying-org--preview-fragment fragment t)))
    (shuying-org--preview-fragment fragment t)))

(defun shuying-org--enter-fragment (fragment)
  "Reveal the source of Org FRAGMENT."
  (pcase-let ((`(,beginning . ,end)
               (shuying-org--fragment-bounds fragment)))
    (dolist (overlay
             (shuying-org--fragment-overlays beginning end))
      (if (and (= (overlay-start overlay) beginning)
               (= (overlay-end overlay) end))
          (shuying-org--hide-overlay overlay)
        (delete-overlay overlay)))))

(defun shuying-org--set-active-fragment (fragment)
  "Remember FRAGMENT as the one containing point."
  (unless (markerp shuying-org--active-start)
    (setq shuying-org--active-start (make-marker)))
  (set-marker
   shuying-org--active-start
   (shuying-org-fragment-beginning fragment)
   (current-buffer)))

(defun shuying-org--clear-active-fragment ()
  "Forget the fragment previously containing point."
  (when (markerp shuying-org--active-start)
    (set-marker shuying-org--active-start nil))
  (setq shuying-org--active-start nil))

(defun shuying-org--post-command ()
  "Update preview visibility after point moves or source changes."
  (let* ((active-position
          (and (markerp shuying-org--active-start)
               (marker-position shuying-org--active-start)))
         (text-changed
          (not (equal shuying-org--previous-tick
                      (buffer-chars-modified-tick))))
         (current
          (or (shuying-org--fragment-at-point)
              (shuying-org--fragment-at-edit-boundary
               active-position text-changed)))
         (current-start
          (and current (shuying-org-fragment-beginning current)))
         (changed-overlays
          (prog1 shuying-org--changed-overlays
            (setq shuying-org--changed-overlays nil)))
         processed-starts
         refresh-visible)
    (unless (equal current-start active-position)
      (when active-position
        (if-let* ((active
                   (shuying-org--fragment-at-position
                    active-position)))
            (progn
              (push (shuying-org-fragment-beginning active)
                    processed-starts)
              (shuying-org--leave-fragment active))
          (dolist (overlay
                   (shuying-org--fragment-overlays
                    active-position (1+ active-position)))
            (unless (memq overlay changed-overlays)
              (delete-overlay overlay)
              (setq refresh-visible t)))))
      (shuying-org--clear-active-fragment)
      (when current
        (unless (shuying-org--viewing-p)
          (shuying-org--enter-fragment current))
        (shuying-org--set-active-fragment current)))
    (unless current
      (when-let* ((completed
                  (shuying-org--fragment-at-or-ending-at-position
                   shuying-org--previous-point)))
        (unless (or (= (point) (shuying-org-fragment-end completed))
                    (memq (shuying-org-fragment-beginning completed)
                          processed-starts))
          (push (shuying-org-fragment-beginning completed)
                processed-starts)
          (shuying-org--leave-fragment completed))))
    ;; Undo and programmatic edits can modify a preview while point remains
    ;; outside it, so they cannot rely on a later cursor-leave transition.
    (dolist (overlay changed-overlays)
      (when (overlay-buffer overlay)
        (if-let* ((fragment
                   (or
                    (when-let* ((beginning
                                 (overlay-get
                                  overlay
                                  'shuying-org-source-beginning)))
                    (shuying-org--fragment-at-position beginning))
                    (shuying-org--fragment-at-position
                     (overlay-start overlay)))))
            (pcase-let ((`(,beginning . ,end)
                         (shuying-org--fragment-bounds fragment)))
              (move-overlay overlay beginning end)
              (overlay-put overlay 'shuying-org-source-beginning
                           beginning)
              (unless (or (memq beginning processed-starts)
                          (equal beginning current-start))
                (push beginning processed-starts)
                (shuying-org--leave-fragment fragment)))
          (delete-overlay overlay)
          (setq refresh-visible t))))
    (when refresh-visible
      (setq shuying-org--visible-window-state nil)
      (shuying-org--schedule-visible-preview t))
    (setq shuying-org--previous-point (point)
          shuying-org--previous-tick
          (buffer-chars-modified-tick))))

(defun shuying-org--view-mode-changed ()
  "Synchronize the preview at point after View mode changes."
  (when (bound-and-true-p shuying-org-mode)
    (if-let* ((fragment (shuying-org--fragment-at-point)))
        (progn
          (if (shuying-org--viewing-p)
              (shuying-org--leave-fragment fragment)
            (shuying-org--enter-fragment fragment))
          (shuying-org--set-active-fragment fragment))
      (shuying-org--clear-active-fragment))))

(defun shuying-org--rebuild-fragment-catalog ()
  "Parse and remember every Org LaTeX fragment in the current buffer."
  (save-restriction
    (widen)
    (let ((equation-number 1))
      (setq shuying-org--fragment-catalog
            (org-element-map
                (org-element-parse-buffer)
                '(latex-fragment latex-environment)
              (lambda (element)
                (let ((count (shuying-org--equation-count element)))
                  (prog1
                      (when (shuying-org--latex-fragment-p element)
                        (shuying-org--fragment-from-element
                         element (and count equation-number)))
                    ;; A source-visible environment can still advance TeX's
                    ;; equation counter for later previews.
                    (when count
                      (cl-incf equation-number count))))))
            shuying-org--catalog-tick
            (buffer-chars-modified-tick)))))

(defun shuying-org--fragments ()
  "Return the current buffer's catalog of Org LaTeX fragments."
  (unless (shuying-org--catalog-current-p)
    (shuying-org--rebuild-fragment-catalog))
  shuying-org--fragment-catalog)

(defun shuying-org--fragments-in-ranges (ranges)
  "Return Org LaTeX fragments overlapping any of RANGES.
RANGES and the fragment catalog are traversed in buffer order."
  (let ((remaining
         (sort (copy-sequence ranges)
               (lambda (left right)
                 (< (car left) (car right)))))
        fragments)
    (catch 'done
      (dolist (fragment (shuying-org--fragments))
        (pcase-let ((`(,beginning . ,end)
                     (shuying-org--fragment-bounds fragment)))
          (while (and remaining
                      (<= (cdar remaining) beginning))
            (setq remaining (cdr remaining)))
          (unless remaining
            (throw 'done nil))
          (when (and (< beginning (cdar remaining))
                     (< (caar remaining) end))
            (push fragment fragments)))))
    (nreverse fragments)))

(defun shuying-org--fragments-in-region (beginning end)
  "Return Org LaTeX fragments overlapping BEGINNING through END."
  (shuying-org--fragments-in-ranges
   (list (cons beginning end))))

(defun shuying-org--preview-region (beginning end)
  "Preview Org LaTeX fragments between BEGINNING and END."
  (shuying-org--preview-fragments
   (shuying-org--fragments-in-region beginning end)))

(defun shuying-org--visible-ranges ()
  "Return the ranges visible in windows showing the current buffer."
  (mapcar
   (lambda (window)
     (cons (window-start window)
           (or (window-end window t) (point-max))))
   (get-buffer-window-list (current-buffer) nil t)))

(defun shuying-org--window-state ()
  "Return state sufficient to notice changes to visible buffer ranges."
  (mapcar
   (lambda (window)
     (list window
           (window-start window)
           (window-body-width window)
           (window-body-height window)))
   (get-buffer-window-list (current-buffer) nil t)))

(defun shuying-org--preview-visible-windows ()
  "Populate previews when the visible windows change."
  (let ((window-state (shuying-org--window-state)))
    (unless (equal window-state shuying-org--visible-window-state)
      (setq shuying-org--visible-window-state window-state)
      (when (and (bound-and-true-p shuying-org-mode) window-state)
        (shuying-org--preview-fragments
         (shuying-org--fragments-in-ranges
          (shuying-org--visible-ranges))
         t t)))))

(defun shuying-org--run-visible-preview (buffer)
  "Populate visible previews in BUFFER after a window change."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shuying-org--visible-preview-timer nil
            shuying-org--pending-window-state nil)
      (when (bound-and-true-p shuying-org-mode)
        (shuying-org--preview-visible-windows)))))

(defun shuying-org--cancel-visible-preview-timer ()
  "Cancel the pending visible preview update."
  (when (timerp shuying-org--visible-preview-timer)
    (cancel-timer shuying-org--visible-preview-timer))
  (setq shuying-org--visible-preview-timer nil
        shuying-org--pending-window-state nil))

(defun shuying-org--schedule-visible-preview (&optional immediate)
  "Coalesce viewport discovery before submitting render requests.
When IMMEDIATE is non-nil, replace any pending update and run at
the next idle opportunity."
  (let ((window-state (shuying-org--window-state)))
    (unless (or (equal window-state shuying-org--visible-window-state)
                (and (not immediate)
                     (equal window-state
                            shuying-org--pending-window-state)))
      (shuying-org--cancel-visible-preview-timer)
      (setq shuying-org--pending-window-state window-state
            shuying-org--visible-preview-timer
            (run-with-idle-timer
             (if immediate 0 shuying-org-visible-preview-delay)
             nil #'shuying-org--run-visible-preview
             (current-buffer))))))

(defun shuying-org--window-buffer-changed (window)
  "Schedule previews when WINDOW starts displaying the current buffer."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (setq shuying-org--visible-window-state nil)
    (shuying-org--schedule-visible-preview t)))

(defun shuying-org--window-size-changed (window)
  "Schedule previews when WINDOW showing the current buffer is resized."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    ;; Redisplay can change the visible end without moving `window-start',
    ;; especially when a horizontal resize changes line wrapping.  Force the
    ;; idle pass to recompute `window-end' after redisplay has settled.
    (setq shuying-org--visible-window-state nil)
    (shuying-org--schedule-visible-preview t)))

(defun shuying-org--window-scrolled (window _display-start)
  "Schedule previews after WINDOW scrolls to its new display start."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    ;; Commands can move point before redisplay adjusts `window-start'.
    ;; Their post-command pass still sees the old viewport; this hook runs
    ;; after the new start has been installed and catches the actual scroll.
    (shuying-org--schedule-visible-preview)))

(defun shuying-org--buffer-saved ()
  "Recheck visible previews after their Org buffer is saved."
  (when (shuying-org--window-state)
    (setq shuying-org--visible-window-state nil)
    (shuying-org--schedule-visible-preview t)))

(defun shuying-org--buffer-reverted ()
  "Discard stale preview state after reverting the Org buffer."
  (shuying-org--cancel-visible-preview-timer)
  (shuying-org--clear-active-fragment)
  (setq shuying-org--fragment-catalog nil
        shuying-org--catalog-tick nil
        shuying-org--changed-overlays nil
        shuying-org--previous-point (point)
        shuying-org--previous-tick (buffer-chars-modified-tick)
        shuying-org--visible-window-state nil)
  (shuying-org-clear-buffer)
  (when (shuying-org--window-state)
    (shuying-org--schedule-visible-preview t)))

(defun shuying-org--clear-region (beginning end)
  "Remove Shuying overlays between BEGINNING and END."
  (dolist (overlay (shuying-org--fragment-overlays beginning end))
    (delete-overlay overlay)))

(defun shuying-org--section-bounds ()
  "Return the current Org section bounds."
  (cons
   (if (org-before-first-heading-p)
       (point-min)
     (save-excursion
       (org-with-limited-levels
        (org-back-to-heading t)
        (point))))
   (org-with-limited-levels (org-entry-end-position))))

;;;###autoload
(defun shuying-org-preview-buffer ()
  "Preview every Org LaTeX fragment in the current buffer."
  (interactive)
  (shuying-org--preview-region (point-min) (point-max)))

;;;###autoload
(defun shuying-org-clear-buffer ()
  "Remove every Shuying preview from the current buffer."
  (interactive)
  (shuying-org--clear-region (point-min) (point-max)))

;;;###autoload
(defun shuying-org-preview (&optional argument)
  "Preview or clear Org LaTeX in the current context.
With one universal prefix, clear the region or current section.  With two,
preview the whole buffer.  With three, clear the whole buffer."
  (interactive "P")
  (cond
   ((equal argument '(64))
    (shuying-org-clear-buffer))
   ((equal argument '(16))
    (shuying-org-preview-buffer))
   ((equal argument '(4))
    (pcase-let ((`(,beginning . ,end)
                 (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (shuying-org--section-bounds))))
      (shuying-org--clear-region beginning end)))
   ((use-region-p)
    (shuying-org--preview-region
     (region-beginning) (region-end)))
   (t
    (if-let* ((fragment (shuying-org--fragment-at-point)))
        (shuying-org--preview-fragment fragment)
      (pcase-let ((`(,beginning . ,end)
                   (shuying-org--section-bounds)))
        (shuying-org--preview-region beginning end))))))

;;;###autoload
(define-minor-mode shuying-org-mode
  "Reveal Org LaTeX source at point and preview it after leaving."
  :lighter " Shuying"
  (if shuying-org-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq shuying-org-mode nil)
          (user-error "Shuying Org mode requires an Org buffer"))
        (setq shuying-org--previous-point (point)
              shuying-org--previous-tick
              (buffer-chars-modified-tick))
        (add-hook 'post-command-hook
                  #'shuying-org--post-command nil t)
        (setq shuying-org--visible-window-state nil)
        (add-hook 'post-command-hook
                  #'shuying-org--schedule-visible-preview nil t)
        (add-hook 'after-change-functions
                  #'shuying-org--layout-context-changed nil t)
        (add-hook 'window-buffer-change-functions
                  #'shuying-org--window-buffer-changed nil t)
        (add-hook 'window-size-change-functions
                  #'shuying-org--window-size-changed nil t)
        (add-hook 'window-scroll-functions
                  #'shuying-org--window-scrolled nil t)
        (add-hook 'after-save-hook #'shuying-org--buffer-saved nil t)
        (add-hook 'after-revert-hook #'shuying-org--buffer-reverted nil t)
        (add-hook 'view-mode-hook #'shuying-org--view-mode-changed nil t)
        (shuying-org--schedule-visible-preview t))
    (remove-hook 'post-command-hook #'shuying-org--post-command t)
    (remove-hook 'post-command-hook
                 #'shuying-org--schedule-visible-preview t)
    (remove-hook 'after-change-functions
                 #'shuying-org--layout-context-changed t)
    (remove-hook 'window-buffer-change-functions
                 #'shuying-org--window-buffer-changed t)
    (remove-hook 'window-size-change-functions
                 #'shuying-org--window-size-changed t)
    (remove-hook 'window-scroll-functions
                 #'shuying-org--window-scrolled t)
    (remove-hook 'after-save-hook #'shuying-org--buffer-saved t)
    (remove-hook 'after-revert-hook #'shuying-org--buffer-reverted t)
    (remove-hook 'view-mode-hook #'shuying-org--view-mode-changed t)
    (setq shuying-org--visible-window-state nil)
    (shuying-org--cancel-visible-preview-timer)
    (setq shuying-org--changed-overlays nil)
    (shuying-org--clear-active-fragment)
    (shuying-org-clear-buffer)))

(provide 'shuying-org)

;;; shuying-org.el ends here
