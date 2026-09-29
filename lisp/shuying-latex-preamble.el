;;; shuying-latex-preamble.el --- Reusable LaTeX preambles -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'shuying)
(require 'subr-x)

(define-error 'shuying-latex-error "Shuying LaTeX rendering failed")

(defcustom shuying-latex-precompile-preamble t
  "Whether to precompile reusable LaTeX preambles.
If precompilation is unavailable or fails, Shuying compiles the complete
preamble with each batch instead."
  :type 'boolean
  :group 'shuying)

(defcustom shuying-latex-format-directory
  (expand-file-name "formats/" shuying-state-directory)
  "Directory containing precompiled LaTeX formats."
  :type 'directory
  :group 'shuying)

(defconst shuying-latex-preamble--preview-package
  "\\usepackage[active,tightpage,auctex]{preview}\n"
  "LaTeX setup that emits one page and geometry for each preview.")

(defconst shuying-latex-preamble--dvisvgm-pgf-driver
  "\\def\\pgfsysdriver{pgfsys-dvisvgm.def}\n"
  "Preview-only PGF driver setup for the dvisvgm converter.")

(cl-defstruct shuying-latex-preamble--format-build
  key
  callbacks
  directory
  log-buffer
  built-file
  target-file)

(cl-defstruct shuying-latex-preamble--warmup
  key
  callbacks
  specification
  engine
  directory
  log-buffer
  attempts)

(defvar shuying-latex-preamble--format-builds (make-hash-table :test #'equal)
  "LaTeX format builds currently shared by waiting batches.")

(defvar shuying-latex-preamble--failed-formats (make-hash-table :test #'equal)
  "LaTeX formats which failed during the current Emacs session.")

(defvar shuying-latex-preamble--warmed-preambles (make-hash-table :test #'equal)
  "MiKTeX preambles warmed during the current Emacs session.")

(defvar shuying-latex-preamble--warmups (make-hash-table :test #'equal)
  "MiKTeX preamble warm-ups shared by compatible waiting batches.")

(defvar shuying-latex-preamble--warmup-queue nil
  "MiKTeX preamble warm-ups waiting for the installer lane.")

(defvar shuying-latex-preamble--active-warmup nil
  "MiKTeX preamble warm-up currently owning the installer lane.")

(defun shuying-latex-preamble--uses-dvisvgm-p (specification)
  "Return non-nil when SPECIFICATION uses dvisvgm as its converter."
  (when-let* ((converter
               (plist-get
                (shuying-render-spec-backend-options specification)
                :converter))
              (program (car-safe converter)))
    (string-equal-ignore-case (file-name-base program) "dvisvgm")))

(defun shuying-latex-preamble--preview-preamble (specification)
  "Return the complete preview preamble for SPECIFICATION."
  (let ((preamble (shuying-render-spec-preamble specification)))
    (concat
     (when (shuying-latex-preamble--uses-dvisvgm-p specification)
       shuying-latex-preamble--dvisvgm-pgf-driver)
     preamble
     (unless (or (string-empty-p preamble)
                 (string-suffix-p "\n" preamble))
       "\n")
     shuying-latex-preamble--preview-package)))

(defun shuying-latex-preamble-insert (specification)
  "Insert the reusable preamble for SPECIFICATION at point."
  (insert (shuying-latex-preamble--preview-preamble specification)))

(defun shuying-latex-preamble--format-key (specification engine)
  "Return the precompiled format key for SPECIFICATION and ENGINE."
  ;; These are precisely the inputs dumped before `\endofdump'.  Fragment
  ;; source, colors, and dimensions remain in the ordinary batch document.
  (secure-hash
   'sha256
   (encode-coding-string
    (prin1-to-string
     (list
      (shuying-latex-preamble--preview-preamble specification)
      engine
      (shuying-render-spec-cache-version specification)))
    'utf-8-unix)))

(defun shuying-latex-preamble--base-format (engine)
  "Return the dumpable base format for ENGINE, or nil."
  (when (string-equal
         (downcase (file-name-base (car engine))) "latex")
    "latex"))

(defun shuying-latex-preamble--miktex-engine-p (engine)
  "Return non-nil when resolved ENGINE belongs to MiKTeX."
  (and (eq system-type 'windows-nt)
       (string-match-p
        "[/\\\\]miktex[/\\\\]"
        (downcase (expand-file-name (car engine))))))

(defun shuying-latex-preamble-compiler-options (engine enabled)
  "Return MiKTeX compiler options for ENGINE.
ENABLED permits package installation; otherwise disable it."
  (when (shuying-latex-preamble--miktex-engine-p engine)
    (list (if enabled "-enable-installer" "-disable-installer"))))

(defun shuying-latex-preamble--cleanup-directory (directory)
  "Remove temporary DIRECTORY, reporting cleanup failures without stopping waiters."
  (when (and directory (file-directory-p directory))
    (condition-case error-data
        (delete-directory directory t)
      (error
       (display-warning 'shuying
                        (format "Could not remove LaTeX work directory %s: %s"
                                directory (error-message-string error-data))
                        :warning)))))

(defun shuying-latex-preamble--write-warmup-document (specification file)
  "Write a preamble-only warm-up for SPECIFICATION to FILE."
  (let ((write-region-inhibit-fsync t)
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (shuying-latex-preamble-insert specification)
      (insert "\\begin{document}\n\\end{document}\n"))))

(defun shuying-latex-preamble--complete-warmup (warmup error-data)
  "Complete WARMUP with ERROR-DATA and release its waiting batches."
  (when (eq warmup shuying-latex-preamble--active-warmup)
    (let ((key (shuying-latex-preamble--warmup-key warmup))
          (directory (shuying-latex-preamble--warmup-directory warmup))
          (log-buffer (shuying-latex-preamble--warmup-log-buffer warmup))
          (callbacks
           (nreverse (shuying-latex-preamble--warmup-callbacks warmup))))
      (unless error-data
        (puthash key t shuying-latex-preamble--warmed-preambles))
      (remhash key shuying-latex-preamble--warmups)
      (setq shuying-latex-preamble--active-warmup nil)
      (shuying-latex-preamble--cleanup-directory directory)
      (when (and (not error-data) (buffer-live-p log-buffer))
        (kill-buffer log-buffer))
      ;; Start the next writer before callbacks can enqueue newer warm-ups.
      (shuying-latex-preamble--run-warmup-queue)
      (dolist (callback callbacks)
        (funcall callback error-data)))))

(defun shuying-latex-preamble--warmup-sentinel (warmup process _event)
  "Continue WARMUP after its preamble PROCESS exits."
  (when (and (eq warmup shuying-latex-preamble--active-warmup)
             (memq (process-status process) '(exit signal)))
    (cond
     ((and (eq (process-status process) 'exit)
           (zerop (process-exit-status process)))
      (shuying-latex-preamble--complete-warmup warmup nil))
     ((and (eq (process-status process) 'exit)
           (< (shuying-latex-preamble--warmup-attempts warmup) 2))
      ;; MiKTeX can install a missing package yet fail the process that
      ;; discovered it.  Retry only after that installer has exited.
      (with-current-buffer (shuying-latex-preamble--warmup-log-buffer warmup)
        (goto-char (point-max))
        (insert "\nRetrying the warmed LaTeX preamble.\n"))
      (shuying-latex-preamble--start-warmup-attempt warmup))
     (t
      (shuying-latex-preamble--complete-warmup
       warmup
       (list
        'shuying-latex-error
        (format "LaTeX preamble warm-up exited with status %d; see %s"
                (process-exit-status process)
                (buffer-name
                 (shuying-latex-preamble--warmup-log-buffer warmup)))))))))

(defun shuying-latex-preamble--start-warmup-attempt (warmup)
  "Start one serialized MiKTeX preamble WARMUP attempt."
  (cl-incf (shuying-latex-preamble--warmup-attempts warmup))
  (let* ((directory (shuying-latex-preamble--warmup-directory warmup))
         (engine (shuying-latex-preamble--warmup-engine warmup))
         (source-file (expand-file-name "preamble.tex" directory))
         (command
          (append
           engine
           (shuying-latex-preamble-compiler-options engine t)
           (list
            "-interaction=nonstopmode"
            (concat "-output-directory=" directory)
            source-file))))
    (condition-case error-data
        (let ((default-directory (file-name-as-directory directory)))
          (make-process
           :name "shuying-latex-warmup"
           :buffer (shuying-latex-preamble--warmup-log-buffer warmup)
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process event)
             (shuying-latex-preamble--warmup-sentinel
              warmup process event))))
      (error
       (with-current-buffer (shuying-latex-preamble--warmup-log-buffer warmup)
         (goto-char (point-max))
         (insert (error-message-string error-data) "\n"))
       (shuying-latex-preamble--complete-warmup warmup error-data)))))

(defun shuying-latex-preamble--start-warmup (warmup)
  "Prepare and start the serialized MiKTeX preamble WARMUP."
  (condition-case error-data
      (let* ((directory
              (progn
                (make-directory shuying-work-directory t)
                (make-temp-file
                 (expand-file-name "warmup-" shuying-work-directory) t)))
             (_ (setf (shuying-latex-preamble--warmup-directory warmup)
                      directory))
             (log-buffer
              (generate-new-buffer "*Shuying LaTeX warm-up*"))
             (source-file (expand-file-name "preamble.tex" directory)))
        (setf (shuying-latex-preamble--warmup-log-buffer warmup) log-buffer)
        (buffer-disable-undo log-buffer)
        (shuying-latex-preamble--write-warmup-document
         (shuying-latex-preamble--warmup-specification warmup) source-file)
        (shuying-latex-preamble--start-warmup-attempt warmup))
    (error
     (shuying-latex-preamble--complete-warmup warmup error-data))))

(defun shuying-latex-preamble--run-warmup-queue ()
  "Start the next MiKTeX preamble warm-up when its lane is free."
  (unless shuying-latex-preamble--active-warmup
    (when-let* ((warmup (pop shuying-latex-preamble--warmup-queue)))
      (setq shuying-latex-preamble--active-warmup warmup)
      (shuying-latex-preamble--start-warmup warmup))))

(defun shuying-latex-preamble--ensure-preamble-warm
    (specification engine callback)
  "Warm SPECIFICATION's MiKTeX preamble, then call CALLBACK.
Compatible preambles share one warm-up.  Distinct cold preambles serialize
through one installer lane.  CALLBACK receives nil on success or an error."
  (if (not (shuying-latex-preamble--miktex-engine-p engine))
      (funcall callback nil)
    (let* ((key (shuying-latex-preamble--format-key specification engine))
           (warmup (gethash key shuying-latex-preamble--warmups)))
      (cond
       ((gethash key shuying-latex-preamble--warmed-preambles)
        (funcall callback nil))
       (warmup
        (push callback (shuying-latex-preamble--warmup-callbacks warmup)))
       (t
        (setq warmup
              (make-shuying-latex-preamble--warmup
               :key key
               :callbacks (list callback)
               :specification specification
               :engine engine
               :attempts 0))
        (puthash key warmup shuying-latex-preamble--warmups)
        (setq shuying-latex-preamble--warmup-queue
              (nconc shuying-latex-preamble--warmup-queue (list warmup)))
        (shuying-latex-preamble--run-warmup-queue))))))

(defun shuying-latex-preamble--complete-format-build (build success)
  "Complete BUILD with SUCCESS and notify its waiting batches."
  (let* ((key (shuying-latex-preamble--format-build-key build))
         (built-file (shuying-latex-preamble--format-build-built-file build))
         (target-file (shuying-latex-preamble--format-build-target-file build))
         (log-buffer (shuying-latex-preamble--format-build-log-buffer build)))
    (setq success (and success (file-exists-p built-file)))
    (when success
      (condition-case nil
          (rename-file built-file target-file t)
        (file-error
         (setq success nil))))
    (unless success
      (puthash key t shuying-latex-preamble--failed-formats)
      (display-warning
       'shuying
       (concat
        "Could not precompile a LaTeX preamble; using the full preamble.  "
        "See " (buffer-name log-buffer))
       :warning))
    (remhash key shuying-latex-preamble--format-builds)
    (shuying-latex-preamble--cleanup-directory
     (shuying-latex-preamble--format-build-directory build))
    (when (and success (buffer-live-p log-buffer))
      (kill-buffer log-buffer))
    (dolist (callback
             (nreverse (shuying-latex-preamble--format-build-callbacks build)))
      (funcall callback (and success target-file)))))

(defun shuying-latex-preamble--format-sentinel (build process _event)
  "Handle completion of the precompiled format PROCESS for BUILD."
  (when (memq (process-status process) '(exit signal))
    (shuying-latex-preamble--complete-format-build
     build
     (and (eq (process-status process) 'exit)
          (= (process-exit-status process) 0)))))

(defun shuying-latex-preamble--start-format-build
    (key specification engine base-format callback)
  "Build KEY for SPECIFICATION with ENGINE and BASE-FORMAT.
CALLBACK receives the resulting format file, or nil on failure."
  (make-directory shuying-work-directory t)
  (make-directory shuying-latex-format-directory t)
  (let (directory log-buffer accepted)
    (unwind-protect
        (let* ((source-file
                (progn
                  (setq directory
                        (make-temp-file
                         (expand-file-name "format-" shuying-work-directory) t))
                  (expand-file-name "preamble.tex" directory)))
               (built-file (expand-file-name (concat key ".fmt") directory))
               (target-file
                (expand-file-name
                 (concat key ".fmt") shuying-latex-format-directory)))
          (setq log-buffer (generate-new-buffer "*Shuying LaTeX precompile*"))
          (buffer-disable-undo log-buffer)
          (let ((write-region-inhibit-fsync t)
                (coding-system-for-write 'utf-8-unix))
            (with-temp-file source-file
              (shuying-latex-preamble-insert specification)
              (insert "\\endofdump\n")))
          (let* ((build
                  (make-shuying-latex-preamble--format-build
                   :key key
                   :callbacks (list callback)
                   :directory directory
                   :log-buffer log-buffer
                   :built-file built-file
                   :target-file target-file))
                 (command
                  (append
                   engine
                   (shuying-latex-preamble-compiler-options engine nil)
                   (list
                    "-interaction=nonstopmode"
                    (concat "-output-directory=" directory)
                    "-ini"
                    (concat "-jobname=" key)
                    (concat "&" base-format)
                    "mylatexformat.ltx"
                    source-file))))
            (puthash key build shuying-latex-preamble--format-builds)
            (setq accepted t)
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
                     (shuying-latex-preamble--format-sentinel
                      build process event))))
              (error
               (with-current-buffer log-buffer
                 (insert (error-message-string error-data) "\n"))
               (shuying-latex-preamble--complete-format-build build nil)))))
      (unless accepted
        (shuying-latex-preamble--cleanup-directory directory)
        (when (buffer-live-p log-buffer)
          (kill-buffer log-buffer))))))

(defun shuying-latex-preamble--ensure-format
    (specification engine callback)
  "Call CALLBACK with a reusable format for SPECIFICATION and ENGINE.
CALLBACK receives nil when precompilation is disabled or unavailable."
  (let* ((base-format (shuying-latex-preamble--base-format engine))
         (key
          (and shuying-latex-precompile-preamble
               base-format
               (shuying-latex-preamble--format-key specification engine)))
         (target-file
          (and key
               (expand-file-name
                (concat key ".fmt")
                shuying-latex-format-directory)))
         (build (and key (gethash key shuying-latex-preamble--format-builds))))
    (cond
     ((not key)
      (funcall callback nil nil))
     ((gethash key shuying-latex-preamble--failed-formats)
      (funcall callback key nil))
     ((file-exists-p target-file)
      (funcall callback key target-file))
     (build
      (push
       (lambda (format-file)
         (funcall callback key format-file))
       (shuying-latex-preamble--format-build-callbacks build)))
     (t
      (shuying-latex-preamble--start-format-build
       key specification engine base-format
       (lambda (format-file)
         (funcall callback key format-file)))))))

(defun shuying-latex-preamble--notify (callback key file error-data)
  "Notify CALLBACK of a prepared KEY and FILE, or ERROR-DATA."
  (condition-case callback-error
      (funcall callback key file error-data)
    (error
     (display-warning
      'shuying
      (format "Shuying preamble callback failed: %s"
              (error-message-string callback-error))
      :error))))

(defun shuying-latex-preamble-prepare (specification engine callback)
  "Prepare reusable resources for SPECIFICATION and ENGINE.
Call CALLBACK with (FORMAT-KEY FORMAT-FILE ERROR-DATA).  A nil format file
without an error means the caller should compile the full preamble."
  (condition-case error-data
      (shuying-latex-preamble--ensure-preamble-warm
       specification engine
       (lambda (warmup-error)
         (if warmup-error
             (shuying-latex-preamble--notify callback nil nil warmup-error)
           (condition-case preparation-error
               (shuying-latex-preamble--ensure-format
                specification engine
                (lambda (key file)
                  (shuying-latex-preamble--notify callback key file nil)))
             (error
              (shuying-latex-preamble--notify
               callback nil nil preparation-error))))))
    (error
     (shuying-latex-preamble--notify callback nil nil error-data))))

(defun shuying-latex-preamble-invalidate-format (key file)
  "Stop using cached format KEY and remove FILE after a successful fallback."
  (when key
    (puthash key t shuying-latex-preamble--failed-formats))
  (when (and file (file-exists-p file))
    (condition-case error-data
        (delete-file file)
      (error
       (display-warning
        'shuying
        (format "Could not remove invalid LaTeX format %s: %s"
                file (error-message-string error-data))
        :warning)))))

(provide 'shuying-latex-preamble)

;;; shuying-latex-preamble.el ends here
