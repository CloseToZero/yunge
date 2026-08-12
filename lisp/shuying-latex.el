;;; shuying-latex.el --- Asynchronous LaTeX rendering -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'shuying)
(require 'subr-x)

(define-error 'shuying-latex-error "Shuying LaTeX rendering failed")

(defcustom shuying-latex-engine-command '("latex")
  "Command prefix used to compile Shuying preview documents."
  :type '(repeat string)
  :group 'shuying)

(defcustom shuying-latex-converter-command '("dvisvgm")
  "Command prefix used to convert Shuying DVI pages to SVG."
  :type '(repeat string)
  :group 'shuying)

(defconst shuying-latex--preview-package
  "\\usepackage[active,tightpage]{preview}\n"
  "LaTeX setup that turns each preview environment into one page.")

(cl-defstruct shuying-latex--batch
  requests
  complete
  directory
  log-buffer
  dvi-file)

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

(defun shuying-latex--write-document (requests file)
  "Write a batch document for REQUESTS to FILE."
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
      (insert (shuying-render-spec-preamble specification))
      (unless (bolp)
        (insert "\n"))
      (insert shuying-latex--preview-package)
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
  (unless (and (consp value) (seq-every-p #'stringp value))
    (error "%s must be a non-empty list of strings" name))
  value)

(defun shuying-latex--process-error (stage process buffer)
  "Return an error value for STAGE, PROCESS, and log BUFFER."
  (list
   'shuying-latex-error
   (format "%s exited with status %d; see %s"
           stage (process-exit-status process) (buffer-name buffer))))

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

(defun shuying-latex--finish-conversion (batch process)
  "Publish the pages produced for BATCH by PROCESS."
  (let ((directory (shuying-latex--batch-directory batch))
        (complete (shuying-latex--batch-complete batch))
        (log-buffer (shuying-latex--batch-log-buffer batch))
        (process-error
         (unless (and (eq (process-status process) 'exit)
                      (= (process-exit-status process) 0))
           (shuying-latex--process-error
            "dvisvgm" process
            (shuying-latex--batch-log-buffer batch))))
        failed)
    (unwind-protect
        (cl-loop
         for request in (shuying-latex--batch-requests batch)
         for page from 1
         for page-file = (expand-file-name
                          (format "page-%d.svg" page) directory)
         do
         (if (file-exists-p page-file)
             (condition-case error-data
                 (progn
                   (copy-file
                    page-file
                    (shuying-backend-request-output-file request) t)
                   (funcall complete request nil))
               (error
                (setq failed t)
                (funcall complete request error-data)))
           (setq failed t)
           (funcall
            complete request
            (or process-error
                (list
                 'shuying-latex-error
                 (format "dvisvgm did not produce page %d; see %s"
                         page (buffer-name log-buffer)))))))
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
  (let* ((options
          (shuying-render-spec-backend-options specification))
         (converter
          (shuying-latex--command
           (plist-get options :converter) "LaTeX converter"))
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
            (format "--scale=%s" scale)
            (concat "--output=" output-pattern)
            (shuying-latex--batch-dvi-file batch)))))
    (make-process
     :name "shuying-dvisvgm"
     :buffer (shuying-latex--batch-log-buffer batch)
     :command command
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (process event)
       (shuying-latex--conversion-sentinel batch process event)))))

(defun shuying-latex--compilation-sentinel
    (batch specification process _event)
  "Continue BATCH after the LaTeX PROCESS for SPECIFICATION exits."
  (when (memq (process-status process) '(exit signal))
    (if (and (eq (process-status process) 'exit)
             (file-exists-p (shuying-latex--batch-dvi-file batch)))
        (condition-case error-data
            (shuying-latex--start-converter batch specification)
          (error
           (shuying-latex--complete-all batch error-data)))
      (shuying-latex--complete-all
       batch
       (shuying-latex--process-error
        "LaTeX" process (shuying-latex--batch-log-buffer batch))))))

(defun shuying-latex-render-batch (requests complete)
  "Render compatible REQUESTS asynchronously and call COMPLETE for each."
  (when requests
    (let* ((specification
            (shuying-backend-request-specification (car requests)))
           (engine
            (shuying-latex--command
             (shuying-render-spec-engine specification) "LaTeX engine"))
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
             :dvi-file dvi-file))
           (command
            (append
             engine
             (list
              "-interaction=nonstopmode"
              (concat "-output-directory=" directory)
              tex-file))))
      (buffer-disable-undo log-buffer)
      (condition-case error-data
          (progn
            (shuying-latex--write-document requests tex-file)
            (make-process
             :name "shuying-latex"
             :buffer log-buffer
             :command command
             :connection-type 'pipe
             :noquery t
             :sentinel
             (lambda (process event)
               (shuying-latex--compilation-sentinel
                batch specification process event))))
        (error
         (shuying-latex--cleanup batch nil)
         (signal (car error-data) (cdr error-data)))))))

(shuying-register-backend
 'shuying-latex
 #'shuying-latex-render-batch
 #'shuying-latex-batch-key)

(provide 'shuying-latex)

;;; shuying-latex.el ends here
