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
  key
  specification
  artifact
  temporary-file
  callbacks)

(cl-defstruct shuying--backend
  function
  batch-key-function)

(cl-defstruct shuying-backend-request
  specification
  output-file)

(defvar shuying-backends nil
  "Alist mapping backend names to registered backend definitions.")

(defvar shuying--pending-jobs (make-hash-table :test #'equal)
  "Render jobs indexed by render specification hash.")

(defun shuying-register-backend
    (name function &optional batch-key-function)
  "Register FUNCTION as rendering backend NAME.
FUNCTION receives a list of `shuying-backend-request' objects and a
completion function.  It calls the completion function with each request
and nil on success, or an error value on failure.

When BATCH-KEY-FUNCTION is non-nil, it receives a render specification.
Only specifications for which it returns equal keys share a backend call."
  (setf (alist-get name shuying-backends)
        (make-shuying--backend
         :function function
         :batch-key-function batch-key-function)))

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

(defun shuying--complete-job (job error-data)
  "Complete JOB with ERROR-DATA."
  (let ((temporary-file (shuying--job-temporary-file job)))
    (remhash (shuying--job-key job) shuying--pending-jobs)
    (if error-data
        (when (file-exists-p temporary-file)
          (delete-file temporary-file))
      (rename-file temporary-file (shuying--job-artifact job) t))
    (shuying--notify-callbacks
     (nreverse (shuying--job-callbacks job))
     (and (not error-data) (shuying--job-artifact job))
     error-data)))

(defun shuying--backend-for (specification)
  "Return the registered backend for SPECIFICATION."
  (or (alist-get
       (shuying-render-spec-backend specification)
       shuying-backends)
      (error "Unknown Shuying backend: %s"
             (shuying-render-spec-backend specification))))

(defun shuying--group-jobs (jobs)
  "Group JOBS by backend and backend-defined compatibility key."
  (let (groups)
    (dolist (job jobs)
      (let* ((specification (shuying--job-specification job))
             (backend-name
              (shuying-render-spec-backend specification))
             (backend (shuying--backend-for specification))
             (key-function
              (shuying--backend-batch-key-function backend))
             (key
              (list backend-name
                    (and key-function
                         (funcall key-function specification))))
             (group (cl-assoc key groups :test #'equal)))
        (if group
            (setcdr group (cons job (cdr group)))
          (push (list key job) groups))))
    (mapcar
     (lambda (group)
       (cons (car group) (nreverse (cdr group))))
     (nreverse groups))))

(defun shuying--dispatch-job-group (jobs)
  "Dispatch compatible JOBS through their registered backend."
  (let* ((backend
          (shuying--backend-for
           (shuying--job-specification (car jobs))))
         (requests
          nil)
         (request-jobs (make-hash-table :test #'eq))
         completion-running)
    (dolist (job jobs)
      (let ((request
             (make-shuying-backend-request
              :specification (shuying--job-specification job)
              :output-file (shuying--job-temporary-file job))))
        (puthash request job request-jobs)
        (push request requests)))
    (setq requests (nreverse requests))
    (condition-case error-data
        (funcall
         (shuying--backend-function backend)
         requests
         (lambda (request backend-error)
           (setq completion-running t)
           (let ((job (gethash request request-jobs)))
             (when (eq job
                       (gethash (shuying--job-key job)
                                shuying--pending-jobs))
               (shuying--complete-job job backend-error)))
           (setq completion-running nil)))
      (error
       (if completion-running
           (signal (car error-data) (cdr error-data))
         (dolist (job jobs)
           (when (eq job
                     (gethash (shuying--job-key job)
                              shuying--pending-jobs))
             (shuying--complete-job job error-data))))))))

(defun shuying-render-batch (requests)
  "Render REQUESTS through compatible backend batches.
Each element of REQUESTS has the form (SPECIFICATION . CALLBACK).
CALLBACK receives the completed artifact and an error value.  Cache hits
complete immediately, while identical pending specifications share a job."
  (let (jobs)
    (dolist (request requests)
      (let* ((specification (car request))
             (callback (cdr request))
             (key (shuying-render-spec-hash specification))
             (artifact (shuying-artifact-file specification))
             (pending (gethash key shuying--pending-jobs)))
        (cond
         ((file-exists-p artifact)
          (funcall callback artifact nil))
         (pending
          (push callback (shuying--job-callbacks pending)))
         (t
          (shuying--backend-for specification)
          (make-directory shuying-cache-directory t)
          (let ((job
                 (make-shuying--job
                  :key key
                  :specification specification
                  :artifact artifact
                  :temporary-file
                  (make-temp-file
                   (expand-file-name ".render-"
                                     shuying-cache-directory)
                   nil
                   (format ".%s"
                           (shuying-render-spec-output-format
                            specification)))
                  :callbacks (list callback))))
            (puthash key job shuying--pending-jobs)
            (push job jobs))))))
    (dolist (group (shuying--group-jobs (nreverse jobs)))
      (shuying--dispatch-job-group (cdr group)))))

(defun shuying-render (specification callback)
  "Render SPECIFICATION and call CALLBACK with its artifact and error.
Identical pending requests share one backend job.  Cached artifacts call
CALLBACK immediately."
  (shuying-render-batch (list (cons specification callback))))

(provide 'shuying)

;;; shuying.el ends here
