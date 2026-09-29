;;; shuying-latex.el --- Async LaTeX rendering -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'shuying)
(require 'shuying-latex-preamble)
(require 'subr-x)

(define-error 'shuying-latex-unavailable
  "Shuying LaTeX dependency unavailable"
  'shuying-latex-error)

(defcustom shuying-latex-engine-command nil
  "Optional command prefix overriding the preview front end's engine.
When nil, a front end such as `shuying-org' chooses the command from its
document context."
  :type '(choice
          (const :tag "Follow the preview front end" nil)
          (repeat :tag "Command prefix" string))
  :group 'shuying)

(defcustom shuying-latex-converter-command '("dvisvgm")
  "Command prefix used to convert Shuying DVI pages to SVG."
  :type '(repeat string)
  :group 'shuying)

(defconst shuying-latex--number-regexp
  "[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)"
  "Regexp matching a decimal number in renderer output.")

(cl-defstruct shuying-latex--batch
  requests
  complete
  directory
  log-buffer
  tex-file
  intermediate-file
  engine
  converter
  format-key
  format-file
  suspect-format-file)

(defun shuying-latex-batch-key (specification)
  "Return the compatibility key for SPECIFICATION.
Sources and equation numbers vary within one TeX document.  The remaining
values affect the document or converter as a whole."
  (list
   (shuying-render-spec-preamble specification)
   (shuying-render-spec-engine specification)
   (shuying-render-spec-backend-options specification)
   (shuying-render-spec-output-format specification)
   (shuying-render-spec-foreground specification)
   (shuying-render-spec-background specification)
   (shuying-render-spec-scale specification)
   (shuying-render-spec-page-width specification)))

(defun shuying-latex--rgb (color)
  "Return COLOR as a comma-separated LaTeX RGB value."
  (when (and color
             (not (string-equal-ignore-case color "Transparent")))
    (or (when-let* ((values (color-values color)))
          (mapconcat
           (lambda (value)
             (format "%.3f" (/ value 65535.0)))
           values ","))
        (error "Unknown color: %s" color))))

(defun shuying-latex--hex-color (color)
  "Return COLOR as a hexadecimal RGB value."
  (when (and color
             (not (string-equal-ignore-case color "Transparent")))
    (or (when-let* ((values (color-values color)))
          (concat
           "#"
           (mapconcat
            (lambda (value)
              (format "%02x" (round (/ value 257.0))))
            values "")))
        (error "Unknown color: %s" color))))

(defun shuying-latex--page-width (width)
  "Return the TeX text-width setup for WIDTH."
  (cond
   ((stringp width)
    (format "\\setlength{\\textwidth}{%s}\n" width))
   ((and (numberp width) (<= 0 width) (<= width 1))
    (format "\\setlength{\\textwidth}{%s\\paperwidth}\n" width))
   (width
    (error "Invalid Shuying page width: %S" width))))

(defun shuying-latex--insert-fragment (specification)
  "Insert one preview page for SPECIFICATION at point."
  (insert "\n\\begin{preview}\n")
  (when-let* ((number
              (shuying-render-spec-equation-number specification)))
    (insert (format "\\setcounter{equation}{%d}\n" (1- number))))
  (insert (shuying-render-spec-source specification))
  (insert "\n\\end{preview}\n"))

(defun shuying-latex--write-document (requests file &optional format-file)
  "Write a batch document for REQUESTS to FILE.
Load FORMAT-FILE instead of writing the full preamble when it is non-nil."
  (let* ((specification
          (shuying-backend-request-specification (car requests)))
         (foreground
          (shuying-latex--rgb
           (shuying-render-spec-foreground specification)))
         (background
          (shuying-latex--rgb
           (shuying-render-spec-background specification)))
         (write-region-inhibit-fsync t)
         (coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (unless format-file
        (shuying-latex-preamble-insert specification))
      (insert "\\begin{document}\n")
      (when-let* ((width
                  (shuying-render-spec-page-width specification)))
        (insert (shuying-latex--page-width width)))
      (when background
        (insert (format "\\pagecolor[rgb]{%s}\n" background)))
      (when foreground
        (insert (format "\\color[rgb]{%s}\n" foreground)))
      (dolist (request requests)
        (shuying-latex--insert-fragment
         (shuying-backend-request-specification request)))
      (insert "\n\\end{document}\n"))))

(defun shuying-latex--command (value name)
  "Validate command VALUE used as NAME and return it."
  (unless (and (consp value)
               (seq-every-p #'stringp value)
               (not (string-empty-p (car value))))
    (error "%s must be a non-empty list of strings" name))
  value)

(defun shuying-latex--resolve-command (value name)
  "Validate command VALUE used as NAME and resolve its executable."
  (let* ((command (shuying-latex--command value name))
         (program (car command))
         (executable (executable-find program)))
    (unless executable
      (signal
       'shuying-latex-unavailable
       (list
        (concat
         (format "%s executable not found: %s" name program)
         (if (eq system-type 'windows-nt)
             "; run M-x shuying-setup"
           "; install it or add it to exec-path")))))
    (cons executable (cdr command))))

(defun shuying-latex--process-error (stage process buffer)
  "Return an error value for STAGE, PROCESS, and log BUFFER."
  (list
   'shuying-latex-error
   (format "%s exited with status %d; see %s"
           stage (process-exit-status process) (buffer-name buffer))))

(defun shuying-latex--font-size (buffer)
  "Return the preview font size reported in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^Preview: Fontsize \\("
                     shuying-latex--number-regexp
                     "\\)pt$")
             nil t)
        (string-to-number (match-string 1))))))

(defun shuying-latex--page-geometries (buffer font-size scale)
  "Return preview page geometry from BUFFER.
FONT-SIZE and SCALE recover the unscaled dimensions in em units.  Prefer
dvisvgm's baseline report.  For XDV input, combine dvisvgm's tight graphic
size with preview.sty's scaled-point baseline report."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((regexp
             (concat
              "^  width=\\(" shuying-latex--number-regexp
              "\\)pt, height=\\(" shuying-latex--number-regexp
              "\\)pt, depth=\\(" shuying-latex--number-regexp
              "\\)pt$"))
            geometries)
        (while (re-search-forward regexp nil t)
          (let* ((divisor (* font-size scale))
                 (width (string-to-number (match-string 1)))
                 (above (string-to-number (match-string 2)))
                 (depth (string-to-number (match-string 3)))
                 (height (+ above depth)))
            (push
             (list :width (/ width divisor)
                   :height (/ height divisor)
                   :depth (/ depth divisor))
             geometries)))
        (setq geometries (nreverse geometries))
        (or geometries
            (progn
              (goto-char (point-min))
              (let ((preview-regexp
                     (concat
                      "^! Preview: Snippet [0-9]+ ended\\.("
                      "\\([0-9]+\\)[+]\\([0-9]+\\)x"
                      "\\([0-9]+\\))\\.$"))
                    (preview-divisor (* 65536.0 font-size))
                    preview-geometries)
                (while (re-search-forward preview-regexp nil t)
                  (let* ((above (string-to-number (match-string 1)))
                         (depth (string-to-number (match-string 2)))
                         (width (string-to-number (match-string 3)))
                         (height (+ above depth)))
                    (push
                     (list :width (/ width preview-divisor)
                           :height (/ height preview-divisor)
                           :depth (/ depth preview-divisor))
                     preview-geometries)))
                (setq preview-geometries
                      (nreverse preview-geometries))
                (goto-char (point-min))
                (let ((graphic-regexp
                       (concat
                        "^  graphic size: \\("
                        shuying-latex--number-regexp
                        "\\)pt x \\("
                        shuying-latex--number-regexp
                        "\\)pt"))
                      (graphic-divisor (* font-size scale))
                      graphic-geometries)
                  (while (re-search-forward graphic-regexp nil t)
                    (push
                     (list
                      :width
                      (/ (string-to-number (match-string 1))
                         graphic-divisor)
                      :height
                      (/ (string-to-number (match-string 2))
                         graphic-divisor))
                     graphic-geometries))
                  (setq graphic-geometries
                        (nreverse graphic-geometries))
                  (if (/= (length graphic-geometries)
                          (length preview-geometries))
                      preview-geometries
                    (cl-mapcar
                     (lambda (graphic preview)
                       (let* ((height (plist-get graphic :height))
                              (preview-height
                               (plist-get preview :height))
                              (depth-ratio
                               (if (zerop preview-height)
                                   0.0
                                 (/ (plist-get preview :depth)
                                    preview-height))))
                         (list :width (plist-get graphic :width)
                               :height height
                               :depth (* height depth-ratio))))
                     graphic-geometries
                     preview-geometries))))))))))

(defun shuying-latex--cleanup (batch keep-log)
  "Clean BATCH files, preserving its log when KEEP-LOG is non-nil."
  (let ((directory (shuying-latex--batch-directory batch))
        (buffer (shuying-latex--batch-log-buffer batch)))
    (when (file-directory-p directory)
      (delete-directory directory t))
    (unless keep-log
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun shuying-latex--complete-all (batch error-data)
  "Complete every request in BATCH with ERROR-DATA."
  (unwind-protect
      (dolist (request (shuying-latex--batch-requests batch))
        (funcall (shuying-latex--batch-complete batch)
                 request error-data))
    (shuying-latex--cleanup batch t)))

(defun shuying-latex--page-file (directory page page-count)
  "Return the dvisvgm output in DIRECTORY for PAGE of PAGE-COUNT.
dvisvgm zero-pads page numbers to the width of the final page number."
  (let* ((page-string (number-to-string page))
         (width (length (number-to-string page-count)))
         (padding (make-string (- width (length page-string)) ?0)))
    (expand-file-name
     (concat "page-" padding page-string ".svg") directory)))

(defun shuying-latex--errored-pages (buffer)
  "Return preview page numbers containing LaTeX errors in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (pages)
        (while (re-search-forward
                "^! Preview: Snippet \\([0-9]+\\) started\\."
                nil t)
          (let* ((page (string-to-number (match-string 1)))
                 (beginning (line-end-position))
                 (end
                  (save-excursion
                    (when (re-search-forward
                           (format
                            "^! Preview: Snippet %d ended\\."
                            page)
                           nil t)
                      (match-beginning 0)))))
            (when (and end
                       (save-excursion
                         (goto-char beginning)
                         (re-search-forward "^! " end t)))
              (push page pages))))
        (nreverse pages)))))

(defun shuying-latex--finish-conversion (batch process)
  "Publish the pages produced for BATCH by PROCESS."
  (let* ((directory (shuying-latex--batch-directory batch))
         (requests (shuying-latex--batch-requests batch))
         (page-count (length requests))
         (complete (shuying-latex--batch-complete batch))
         (log-buffer (shuying-latex--batch-log-buffer batch))
         (specification
          (shuying-backend-request-specification (car requests)))
         (scale (or (shuying-render-spec-scale specification) 1.0))
         (process-error
          (unless (and (eq (process-status process) 'exit)
                       (= (process-exit-status process) 0))
            (shuying-latex--process-error
             "dvisvgm" process
             (shuying-latex--batch-log-buffer batch))))
         (font-size
          (and (not process-error)
               (shuying-latex--font-size log-buffer)))
         (geometries
           (and font-size
                (shuying-latex--page-geometries
                 log-buffer font-size scale)))
         (errored-pages (shuying-latex--errored-pages log-buffer))
         failed)
    (unwind-protect
        (cl-loop
         for request in requests
         for page from 1
         for geometry = (nth (1- page) geometries)
         for page-file = (shuying-latex--page-file
                           directory page page-count)
         for errored-page = (memq page errored-pages)
         do
         (if (and (file-exists-p page-file) geometry
                  (not errored-page))
             (condition-case error-data
                 (progn
                   (copy-file
                    page-file
                    (shuying-backend-request-output-file request) t)
                   (setf (shuying-backend-request-metadata request)
                         geometry)
                   (funcall complete request nil))
               (error
                (setq failed t)
                (funcall complete request error-data)))
           (setq failed t)
           (funcall
            complete request
            (cond
             (process-error)
             (errored-page
              (list
               'shuying-latex-error
               (format
                "LaTeX reported an error on preview page %d; see %s"
                page (buffer-name log-buffer))))
             (t
              (list
               'shuying-latex-error
               (format
                (concat "dvisvgm did not produce page %d with "
                        "geometry; see %s")
                page (buffer-name log-buffer))))))))
      (shuying-latex--cleanup batch (or failed process-error)))))

(defun shuying-latex--conversion-sentinel (batch process _event)
  "Handle completion of the converter PROCESS for BATCH."
  (when (memq (process-status process) '(exit signal))
    (if (eq (process-status process) 'signal)
        (shuying-latex--complete-all
         batch
         (shuying-latex--process-error
          "dvisvgm" process (shuying-latex--batch-log-buffer batch)))
      (shuying-latex--finish-conversion batch process))))

(defun shuying-latex--start-converter (batch specification)
  "Start the SVG converter for BATCH using SPECIFICATION."
  (let* ((converter (shuying-latex--batch-converter batch))
         (directory (shuying-latex--batch-directory batch))
         (output-pattern (expand-file-name "page-%p.svg" directory))
         (scale (or (shuying-render-spec-scale specification) 1.0))
         (current-color
          (shuying-latex--hex-color
           (shuying-render-spec-foreground specification)))
         (command
          (append
           converter
           (when current-color
             (list (concat "--currentcolor=" current-color)))
           (list
            "--page=1-"
            "--bbox=min"
            "--no-fonts"
            "--verbosity=7"
            (format "--scale=%s" scale)
            (concat "--output=" output-pattern)
            (shuying-latex--batch-intermediate-file batch)))))
    (let ((default-directory (file-name-as-directory directory)))
      (make-process
       :name "shuying-dvisvgm"
       :buffer (shuying-latex--batch-log-buffer batch)
       :command command
       :connection-type 'pipe
       :noquery t
       :sentinel
       (lambda (process event)
         (shuying-latex--conversion-sentinel batch process event))))))

(defun shuying-latex--compilation-sentinel
    (batch specification process _event)
  "Continue BATCH after the LaTeX PROCESS for SPECIFICATION exits."
  (when (memq (process-status process) '(exit signal))
    (cond
     ((and (eq (process-status process) 'exit)
           (file-exists-p
            (shuying-latex--batch-intermediate-file batch)))
      (when-let* ((format-file
                   (shuying-latex--batch-suspect-format-file batch)))
        ;; The same document compiled with the complete preamble, so the
        ;; cached format rather than the fragment caused the first failure.
        (shuying-latex-preamble-invalidate-format
         (shuying-latex--batch-format-key batch) format-file))
      (condition-case error-data
          (shuying-latex--start-converter batch specification)
        (error
         (shuying-latex--complete-all batch error-data))))
     ((and (eq (process-status process) 'exit)
           (shuying-latex--batch-format-file batch))
      ;; A format can become invalid after the TeX installation changes.
      ;; Discard it and retry this batch with the complete preamble once.
      (let ((format-file (shuying-latex--batch-format-file batch)))
        (setf (shuying-latex--batch-format-file batch) nil
              (shuying-latex--batch-suspect-format-file batch)
              format-file)
        (with-current-buffer (shuying-latex--batch-log-buffer batch)
          (goto-char (point-max))
          (insert "\nRetrying with the complete LaTeX preamble.\n"))
        (condition-case error-data
            (progn
              (shuying-latex--write-document
               (shuying-latex--batch-requests batch)
               (shuying-latex--batch-tex-file batch))
              (shuying-latex--start-compiler batch specification))
          (error
           (shuying-latex--complete-all batch error-data)))))
     (t
      (shuying-latex--complete-all
       batch
       (shuying-latex--process-error
        "LaTeX" process (shuying-latex--batch-log-buffer batch)))))))

(defun shuying-latex--start-compiler (batch specification)
  "Start the LaTeX compiler for BATCH and SPECIFICATION."
  (let* ((directory (shuying-latex--batch-directory batch))
         (command
          (append
           (shuying-latex--batch-engine batch)
           (shuying-latex-preamble-compiler-options
            (shuying-latex--batch-engine batch) nil)
           (when-let* ((format-file
                        (shuying-latex--batch-format-file batch)))
             (list
              (concat
               "-fmt=" (file-name-base format-file))))
           (list
            "-interaction=nonstopmode"
            (concat "-output-directory=" directory)
            (shuying-latex--batch-tex-file batch)))))
    (let ((default-directory (file-name-as-directory directory)))
      (make-process
       :name "shuying-latex"
       :buffer (shuying-latex--batch-log-buffer batch)
       :command command
       :connection-type 'pipe
       :noquery t
       :sentinel
       (lambda (process event)
         (shuying-latex--compilation-sentinel
          batch specification process event))))))

(defun shuying-latex--start-batch
    (batch specification format-key format-file)
  "Start BATCH for SPECIFICATION, optionally using FORMAT-FILE.
FORMAT-KEY identifies the persistent format for invalidation."
  (condition-case error-data
      (progn
        (setf (shuying-latex--batch-format-key batch) format-key
              (shuying-latex--batch-format-file batch) format-file)
        (when format-file
          ;; Let every TeX distribution find the custom format in the batch
          ;; directory without changing its global format search path.
          (let ((local-format
                 (expand-file-name
                  (file-name-nondirectory format-file)
                  (shuying-latex--batch-directory batch))))
            (condition-case nil
                (add-name-to-file format-file local-format)
              (file-error
               (copy-file format-file local-format t)))))
        (shuying-latex--write-document
         (shuying-latex--batch-requests batch)
         (shuying-latex--batch-tex-file batch)
         format-file)
        (shuying-latex--start-compiler batch specification))
    (error
     (shuying-latex--complete-all batch error-data))))

(defun shuying-latex--intermediate-extension (engine)
  "Return the dvisvgm input extension produced by ENGINE."
  (if (string-equal-ignore-case
       (file-name-base (car engine)) "xelatex")
      "xdv"
    "dvi"))

(defun shuying-latex-render-batch (requests complete)
  "Render compatible REQUESTS asynchronously and call COMPLETE for each."
  (when requests
    (let* ((specification
            (shuying-backend-request-specification (car requests)))
           (engine
            (shuying-latex--resolve-command
             (shuying-render-spec-engine specification) "LaTeX engine"))
           (converter
            (shuying-latex--resolve-command
             (plist-get
              (shuying-render-spec-backend-options specification)
              :converter)
             "LaTeX converter"))
           (_
            (unless (equal
                     (shuying-render-spec-output-format specification)
                     "svg")
              (error "The Shuying LaTeX backend only produces SVG")))
           (_ (make-directory shuying-work-directory t))
           (directory
            (make-temp-file
             (expand-file-name "latex-" shuying-work-directory) t))
           (tex-file (expand-file-name "input.tex" directory))
           (intermediate-file
            (expand-file-name
             (concat "input."
                     (shuying-latex--intermediate-extension engine))
             directory))
           (log-buffer (generate-new-buffer "*Shuying LaTeX*"))
           (batch
            (make-shuying-latex--batch
             :requests requests
             :complete complete
             :directory directory
             :log-buffer log-buffer
             :tex-file tex-file
             :intermediate-file intermediate-file
             :engine engine
             :converter converter)))
      (buffer-disable-undo log-buffer)
      (condition-case error-data
          (shuying-latex-preamble-prepare
           specification engine
           (lambda (format-key format-file preparation-error)
             (if preparation-error
                 (shuying-latex--complete-all batch preparation-error)
               (shuying-latex--start-batch
                batch specification format-key format-file))))
        (error
         (shuying-latex--cleanup batch nil)
         (signal (car error-data) (cdr error-data)))))))

(shuying-register-backend
 'shuying-latex
 #'shuying-latex-render-batch
 #'shuying-latex-batch-key)

(provide 'shuying-latex)

;;; shuying-latex.el ends here
