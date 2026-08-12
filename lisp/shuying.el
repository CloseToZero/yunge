;;; shuying.el --- LaTeX preview rendering -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'yunge-state)

(defgroup shuying nil
  "Render and cache LaTeX previews."
  :group 'applications)

(defcustom shuying-cache-directory
  (yunge-var-subdirectory "shuying/cache")
  "Directory containing rendered Shuying artifacts."
  :type 'directory
  :group 'shuying)

(defcustom shuying-work-directory
  (yunge-var-subdirectory "shuying/work")
  "Directory containing temporary Shuying render work."
  :type 'directory
  :group 'shuying)

(defconst shuying-cache-format-version 1
  "Version included in Shuying render specification hashes.")

(cl-defstruct shuying-render-spec
  source
  preamble
  engine
  backend
  backend-options
  output-format
  foreground
  background
  scale
  page-width
  equation-number
  cache-version)

(cl-defstruct shuying--job
  temporary-file
  callbacks)

(defvar shuying-backend-functions nil
  "Alist mapping backend names to rendering functions.
Each function receives a render specification, a temporary output file,
and a completion function.  It calls the completion function with nil on
success or an error value on failure.")

(defvar shuying--pending-jobs (make-hash-table :test #'equal)
  "Render jobs indexed by render specification hash.")

(defun shuying-register-backend (name function)
  "Register FUNCTION as rendering backend NAME."
  (setf (alist-get name shuying-backend-functions) function))

(defun shuying-render-spec-hash (specification)
  "Return the cache hash for render SPECIFICATION."
  (secure-hash
   'sha256
   (encode-coding-string
    (prin1-to-string
     (list
      (shuying-render-spec-source specification)
      (shuying-render-spec-preamble specification)
      (shuying-render-spec-engine specification)
      (shuying-render-spec-backend specification)
      (shuying-render-spec-backend-options specification)
      (shuying-render-spec-output-format specification)
      (shuying-render-spec-foreground specification)
      (shuying-render-spec-background specification)
      (shuying-render-spec-scale specification)
      (shuying-render-spec-page-width specification)
      (shuying-render-spec-equation-number specification)
      (shuying-render-spec-cache-version specification)))
    'utf-8-unix)))

(defun shuying-artifact-file (specification)
  "Return the cached artifact file for render SPECIFICATION."
  (expand-file-name
   (format "%s.%s"
           (shuying-render-spec-hash specification)
           (shuying-render-spec-output-format specification))
   shuying-cache-directory))

(defun shuying--notify-callbacks (callbacks artifact error-data)
  "Notify CALLBACKS that ARTIFACT completed with ERROR-DATA."
  (dolist (callback callbacks)
    (funcall callback artifact error-data)))

(defun shuying--complete-job (key artifact job error-data)
  "Complete JOB for KEY and ARTIFACT with ERROR-DATA."
  (let ((temporary-file (shuying--job-temporary-file job)))
    (remhash key shuying--pending-jobs)
    (if error-data
        (when (file-exists-p temporary-file)
          (delete-file temporary-file))
      (rename-file temporary-file artifact t))
    (shuying--notify-callbacks
     (nreverse (shuying--job-callbacks job))
     (and (not error-data) artifact)
     error-data)))

(defun shuying-render (specification callback)
  "Render SPECIFICATION and call CALLBACK with its artifact and error.
Identical pending requests share one backend job.  Cached artifacts call
CALLBACK immediately."
  (let* ((key (shuying-render-spec-hash specification))
         (artifact (shuying-artifact-file specification))
         (pending (gethash key shuying--pending-jobs)))
    (cond
     ((file-exists-p artifact)
      (funcall callback artifact nil))
     (pending
      (push callback (shuying--job-callbacks pending)))
     (t
      (let ((backend
             (alist-get
              (shuying-render-spec-backend specification)
              shuying-backend-functions)))
        (unless backend
          (error "Unknown Shuying backend: %s"
                 (shuying-render-spec-backend specification)))
        (make-directory shuying-cache-directory t)
        (let* ((temporary-file
                (make-temp-file
                 (expand-file-name ".render-" shuying-cache-directory)
                 nil
                 (format ".%s"
                         (shuying-render-spec-output-format
                          specification))))
               (job (make-shuying--job
                     :temporary-file temporary-file
                     :callbacks (list callback)))
               completed)
          (puthash key job shuying--pending-jobs)
          (condition-case error-data
              (funcall
               backend specification temporary-file
               (lambda (backend-error)
                 (setq completed t)
                 (shuying--complete-job
                  key artifact job backend-error)))
            (error
             (if completed
                 (signal (car error-data) (cdr error-data))
               (shuying--complete-job
                key artifact job error-data))))))))))

(provide 'shuying)

;;; shuying.el ends here
