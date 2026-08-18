;;; shuying-latex.el --- Async LaTeX rendering -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'shuying)
(require 'subr-x)

(define-error 'shuying-latex-error "Shuying LaTeX rendering failed")
(define-error 'shuying-latex-unavailable
  "Shuying LaTeX dependency unavailable"
  'shuying-latex-error)

(defcustom shuying-latex-engine-command '("latex")
  "Command prefix used to compile Shuying preview documents."
  :type '(repeat string)
  :group 'shuying)

(defcustom shuying-latex-converter-command '("dvisvgm")
  "Command prefix used to convert Shuying DVI pages to SVG."
  :type '(repeat string)
  :group 'shuying)

(defcustom shuying-latex-precompile-preamble t
  "Whether to precompile reusable LaTeX preambles.
If precompilation is unavailable or fails, Shuying compiles the complete
preamble with each batch instead."
  :type 'boolean
  :group 'shuying)

(defcustom shuying-latex-format-directory
  (yunge-var-subdirectory "shuying/formats")
  "Directory containing precompiled LaTeX formats."
  :type 'directory
  :group 'shuying)

(defconst shuying-latex--preview-package
  "\\usepackage[active,tightpage,auctex]{preview}\n"
  "LaTeX setup that emits one page and geometry for each preview.")

(defconst shuying-latex--number-regexp
  "[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)"
  "Regexp matching a decimal number in renderer output.")

(cl-defstruct shuying-latex--batch
  requests
  complete
  directory
  log-buffer
  tex-file
  dvi-file
  engine
  converter
  format-key
  format-file
  suspect-format-file)

(cl-defstruct shuying-latex--format-build
  key
  callbacks
  directory
  log-buffer
  built-file
  target-file)

(defvar shuying-latex--format-builds (make-hash-table :test #'equal)
  "LaTeX format builds currently shared by waiting batches.")

(defvar shuying-latex--failed-formats (make-hash-table :test #'equal)
  "LaTeX formats which failed during the current Emacs session.")

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

(defun shuying-latex--write-preamble (specification)
  "Insert the reusable preamble for SPECIFICATION at point."
  (insert (shuying-render-spec-preamble specification))
  (unless (bolp)
    (insert "\n"))
  (insert shuying-latex--preview-package))

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
        (shuying-latex--write-preamble specification))
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
  "Return page geometry from dvisvgm output in BUFFER.
FONT-SIZE and SCALE recover the unscaled dimensions in em units."
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
        (nreverse geometries)))))

(defun shuying-latex--format-key (specification engine)
  "Return the precompiled format key for SPECIFICATION and ENGINE."
  ;; These are precisely the inputs dumped before `\endofdump'.  Fragment
  ;; source, colors, and dimensions remain in the ordinary batch document.
  (secure-hash
   'sha256
   (encode-coding-string
    (prin1-to-string
     (list
      (shuying-render-spec-preamble specification)
      shuying-latex--preview-package
      engine
      (shuying-render-spec-cache-version specification)))
    'utf-8-unix)))

(defun shuying-latex--base-format (engine)
  "Return the dumpable base format for ENGINE, or nil."
  (when (string-equal
         (downcase (file-name-base (car engine))) "latex")
    "latex"))

(defun shuying-latex--complete-format-build (build success)
  "Complete BUILD with SUCCESS and notify its waiting batches."
  (let* ((key (shuying-latex--format-build-key build))
         (built-file (shuying-latex--format-build-built-file build))
         (target-file (shuying-latex--format-build-target-file build))
         (log-buffer (shuying-latex--format-build-log-buffer build)))
    (setq success (and success (file-exists-p built-file)))
    (when success
      (condition-case nil
          (rename-file built-file target-file t)
        (file-error
         (setq success nil))))
    (unless success
      (puthash key t shuying-latex--failed-formats)
      (display-warning
       'shuying
       (concat
        "Could not precompile a LaTeX preamble; using the full preamble.  "
        "See " (buffer-name log-buffer))
       :warning))
    (remhash key shuying-latex--format-builds)
    (when (file-directory-p
           (shuying-latex--format-build-directory build))
      (delete-directory
       (shuying-latex--format-build-directory build) t))
    (when (and success (buffer-live-p log-buffer))
      (kill-buffer log-buffer))
    (dolist (callback
             (nreverse (shuying-latex--format-build-callbacks build)))
      (funcall callback (and success target-file)))))

(defun shuying-latex--format-sentinel (build process _event)
  "Handle completion of the precompiled format PROCESS for BUILD."
  (when (memq (process-status process) '(exit signal))
    (shuying-latex--complete-format-build
     build
     (and (eq (process-status process) 'exit)
          (= (process-exit-status process) 0)))))

(defun shuying-latex--start-format-build
    (key specification engine base-format callback)
  "Build KEY for SPECIFICATION with ENGINE and BASE-FORMAT.
CALLBACK receives the resulting format file, or nil on failure."
  (make-directory shuying-work-directory t)
  (make-directory shuying-latex-format-directory t)
  (let* ((directory
          (make-temp-file
           (expand-file-name "format-" shuying-work-directory) t))
         (source-file (expand-file-name "preamble.tex" directory))
         (built-file
          (expand-file-name (concat key ".fmt") directory))
         (target-file
          (expand-file-name
           (concat key ".fmt") shuying-latex-format-directory))
         (log-buffer
          (generate-new-buffer "*Shuying LaTeX precompile*"))
         (build
          (make-shuying-latex--format-build
           :key key
           :callbacks (list callback)
           :directory directory
           :log-buffer log-buffer
           :built-file built-file
           :target-file target-file))
         (command
          (append
           engine
           (list
            "-interaction=nonstopmode"
            (concat "-output-directory=" directory)
            "-ini"
            (concat "-jobname=" key)
            (concat "&" base-format)
            "mylatexformat.ltx"
            source-file))))
    (buffer-disable-undo log-buffer)
    (let ((write-region-inhibit-fsync t)
          (coding-system-for-write 'utf-8-unix))
      (with-temp-file source-file
        (shuying-latex--write-preamble specification)
        (insert "\\endofdump\n")))
    (puthash key build shuying-latex--format-builds)
    (condition-case error-data
        (let ((default-directory (file-name-as-directory directory)))
          (make-process
           :name "shuying-latex-precompile"
           :buffer log-buffer
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process event)
             (shuying-latex--format-sentinel build process event))))
      (error
       (with-current-buffer log-buffer
         (insert (error-message-string error-data) "\n"))
       (shuying-latex--complete-format-build build nil)))))

(defun shuying-latex--ensure-format
    (specification engine callback)
  "Call CALLBACK with a reusable format for SPECIFICATION and ENGINE.
CALLBACK receives nil when precompilation is disabled or unavailable."
  (let* ((base-format (shuying-latex--base-format engine))
         (key
          (and shuying-latex-precompile-preamble
               base-format
               (shuying-latex--format-key specification engine)))
         (target-file
          (and key
               (expand-file-name
                (concat key ".fmt")
                shuying-latex-format-directory)))
         (build (and key (gethash key shuying-latex--format-builds))))
    (cond
     ((not key)
      (funcall callback nil nil))
     ((gethash key shuying-latex--failed-formats)
      (funcall callback key nil))
     ((file-exists-p target-file)
      (funcall callback key target-file))
     (build
      (push
       (lambda (format-file)
         (funcall callback key format-file))
       (shuying-latex--format-build-callbacks build)))
     (t
      (shuying-latex--start-format-build
       key specification engine base-format
       (lambda (format-file)
         (funcall callback key format-file)))))))

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

(defun shuying-latex--empty-svg-page-p (file)
  "Return whether dvisvgm FILE contains an empty page group."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (re-search-forward
     (concat
      "<g[^>]*\\bid=['\"]page[0-9]+['\"][^>]*"
      "\\(?:/>\\|>[ \t\r\n]*</g>\\)")
     nil t)))

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
         for unusable-page = (and errored-page
                                  (file-exists-p page-file)
                                  (shuying-latex--empty-svg-page-p
                                   page-file))
         do
         (if (and (file-exists-p page-file) geometry
                  (not unusable-page))
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
             (unusable-page
              (list
               'shuying-latex-error
               (format
                "LaTeX produced an empty preview page %d; see %s"
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
         (command
          (append
           converter
           (list
            "--page=1-"
            "--bbox=preview"
            "--no-fonts"
            "--verbosity=7"
            (format "--scale=%s" scale)
            (concat "--output=" output-pattern)
            (shuying-latex--batch-dvi-file batch)))))
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
           (file-exists-p (shuying-latex--batch-dvi-file batch)))
      (when-let* ((format-file
                   (shuying-latex--batch-suspect-format-file batch)))
        ;; The same document compiled with the complete preamble, so the
        ;; cached format rather than the fragment caused the first failure.
        (when (file-exists-p format-file)
          (delete-file format-file))
        (puthash
         (shuying-latex--batch-format-key batch)
         t shuying-latex--failed-formats))
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
           (dvi-file (expand-file-name "input.dvi" directory))
           (log-buffer (generate-new-buffer "*Shuying LaTeX*"))
           (batch
            (make-shuying-latex--batch
             :requests requests
             :complete complete
             :directory directory
             :log-buffer log-buffer
             :tex-file tex-file
             :dvi-file dvi-file
             :engine engine
             :converter converter)))
      (buffer-disable-undo log-buffer)
      (condition-case error-data
          (shuying-latex--ensure-format
           specification engine
           (lambda (format-key format-file)
             (shuying-latex--start-batch
              batch specification format-key format-file)))
        (error
         (shuying-latex--cleanup batch nil)
         (signal (car error-data) (cdr error-data)))))))

(shuying-register-backend
 'shuying-latex
 #'shuying-latex-render-batch
 #'shuying-latex-batch-key)

(provide 'shuying-latex)

;;; shuying-latex.el ends here
