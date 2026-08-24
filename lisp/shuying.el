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

(defcustom shuying-max-concurrent-batches 4
  "Maximum number of render batches owned by backends at once.
A batch retains its slot until all of its requests complete."
  :type '(integer :match (lambda (_widget value) (> value 0)))
  :group 'shuying)

(defconst shuying-cache-format-version 6
  "Version included in Shuying render specification hashes.")

(defconst shuying--cache-file-regexp
  (rx string-start (= 64 xdigit) "." (+ anychar) string-end)
  "Regexp matching files owned by the Shuying artifact cache.")

(cl-defstruct shuying-artifact
  "A rendered file and the metadata needed to display it."
  path
  metadata)

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
  artifact-file
  metadata-file
  temporary-file
  temporary-metadata-file
  metadata
  callbacks)

(cl-defstruct shuying--backend
  function
  batch-key-function)

(cl-defstruct shuying--batch
  jobs
  remaining)

(cl-defstruct shuying-backend-request
  specification
  output-file
  metadata)

(defvar shuying-backends nil
  "Alist mapping backend names to registered backend definitions.")

(defvar shuying--pending-jobs (make-hash-table :test #'equal)
  "Render jobs indexed by render specification hash.")

(defvar shuying--waiting-batches nil
  "Render batches waiting for a scheduler slot in FIFO order.")

(defvar shuying--active-batch-count 0
  "Number of render batches currently owned by backends.")

(defvar shuying--scheduler-running nil
  "Whether the scheduler is currently dispatching batches.")

(defun shuying-register-backend
    (name function &optional batch-key-function)
  "Register FUNCTION as rendering backend NAME.
FUNCTION receives a list of `shuying-backend-request' objects and a
completion function.  It calls the completion function exactly once for
each request, with nil on success or an error value on failure.  If FUNCTION
signals, Shuying completes every unfinished request with that error.  A
successful backend may store display metadata in the request before calling
the completion function.

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

(defun shuying--artifact-metadata-file (artifact-file)
  "Return the metadata sidecar belonging to ARTIFACT-FILE."
  (concat artifact-file ".eld"))

;;;###autoload
(defun shuying-clear-cache ()
  "Delete completed Shuying artifacts and metadata sidecars.
Precompiled formats, render work, and diagnostic logs are not affected.
Refuse to clear the cache while render jobs are pending."
  (interactive)
  (when (> (hash-table-count shuying--pending-jobs) 0)
    (user-error "Shuying is rendering; wait before clearing its cache"))
  (let ((deleted 0))
    (when (file-directory-p shuying-cache-directory)
      (dolist (file
               (directory-files
                shuying-cache-directory t shuying--cache-file-regexp t))
        (when (file-regular-p file)
          (delete-file file)
          (cl-incf deleted))))
    (message "Deleted %d Shuying cache file%s"
             deleted (if (= deleted 1) "" "s"))
    deleted))

(defun shuying--read-artifact (artifact-file metadata-file)
  "Return the cached artifact described by ARTIFACT-FILE and METADATA-FILE."
  (when (and (file-exists-p artifact-file)
             (file-exists-p metadata-file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents metadata-file)
          (make-shuying-artifact
           :path artifact-file
           :metadata (read (current-buffer))))
      (error nil))))

(defun shuying--write-job-metadata (job)
  "Write JOB's display metadata to its temporary sidecar."
  (let ((coding-system-for-write 'utf-8-unix)
        (print-circle t)
        (print-length nil)
        (print-level nil))
    (with-temp-file (shuying--job-temporary-metadata-file job)
      (prin1 (shuying--job-metadata job) (current-buffer)))))

(defun shuying--notify-callbacks (callbacks artifact error-data)
  "Notify CALLBACKS that ARTIFACT completed with ERROR-DATA."
  (dolist (callback callbacks)
    (condition-case callback-error
        (funcall callback artifact error-data)
      (error
       (display-warning
        'shuying
        (format "Shuying render callback failed: %s"
                (error-message-string callback-error))
        :error)))))

(defun shuying--complete-job (job error-data)
  "Complete JOB with ERROR-DATA."
  (let ((temporary-file (shuying--job-temporary-file job))
        (temporary-metadata-file
         (shuying--job-temporary-metadata-file job))
        artifact)
    (remhash (shuying--job-key job) shuying--pending-jobs)
    (condition-case publish-error
        (if error-data
            (dolist (file (list temporary-file temporary-metadata-file))
              (when (file-exists-p file)
                (delete-file file)))
          (shuying--write-job-metadata job)
          (rename-file temporary-file (shuying--job-artifact-file job) t)
          (rename-file temporary-metadata-file
                       (shuying--job-metadata-file job) t)
          ;; A cache clear followed by an identical render publishes to the
          ;; same hashed path.  Discard any decoded image for that path before
          ;; consumers create a new display, or Emacs can keep showing the
          ;; pixels from the artifact that was deleted.
          (clear-image-cache (shuying--job-artifact-file job))
          (setq artifact
                (make-shuying-artifact
                 :path (shuying--job-artifact-file job)
                 :metadata (shuying--job-metadata job))))
      (error
       (setq error-data publish-error)
       (dolist (file (list temporary-file temporary-metadata-file
                           (shuying--job-artifact-file job)
                           (shuying--job-metadata-file job)))
         (when (file-exists-p file)
           (delete-file file)))))
    (shuying--notify-callbacks
     (nreverse (shuying--job-callbacks job))
     artifact
     error-data)))

(defun shuying--backend-for (specification)
  "Return the registered backend for SPECIFICATION."
  (or (alist-get
       (shuying-render-spec-backend specification)
       shuying-backends)
      (error "Unknown Shuying backend: %s"
             (shuying-render-spec-backend specification))))

(defun shuying--validate-render-request (request)
  "Validate one render REQUEST before admitting its job."
  (unless (and (consp request)
               (shuying-render-spec-p (car request))
               (functionp (cdr request)))
    (error "Invalid Shuying render request: %S" request))
  (shuying--backend-for (car request)))

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

(defun shuying--finish-batch-job (batch job error-data)
  "Finish JOB in BATCH with ERROR-DATA."
  (when (eq job
            (gethash (shuying--job-key job)
                     shuying--pending-jobs))
    (unwind-protect
        (shuying--complete-job job error-data)
      (cl-decf (shuying--batch-remaining batch))
      (when (zerop (shuying--batch-remaining batch))
        (cl-decf shuying--active-batch-count)
        (shuying--run-scheduler)))))

(defun shuying--dispatch-batch (batch)
  "Dispatch BATCH through its registered backend."
  (let* ((jobs (shuying--batch-jobs batch))
         (backend
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
             (unless backend-error
               (setf (shuying--job-metadata job)
                     (shuying-backend-request-metadata request)))
             (shuying--finish-batch-job
              batch job backend-error))
           (setq completion-running nil)))
      (error
       (if completion-running
           (signal (car error-data) (cdr error-data))
         (dolist (job jobs)
           (shuying--finish-batch-job
            batch job error-data)))))))

(defun shuying--run-scheduler ()
  "Dispatch queued batches while scheduler slots are available."
  (unless shuying--scheduler-running
    (let ((shuying--scheduler-running t))
      (while (and shuying--waiting-batches
                  (< shuying--active-batch-count
                     shuying-max-concurrent-batches))
        (let ((batch (pop shuying--waiting-batches)))
          (cl-incf shuying--active-batch-count)
          (shuying--dispatch-batch batch))))))

(defun shuying--enqueue-job-groups (groups)
  "Append compatible job GROUPS to the render queue."
  (when groups
    (setq shuying--waiting-batches
          (nconc
           shuying--waiting-batches
           (mapcar
            (lambda (group)
              (let ((jobs (cdr group)))
                (make-shuying--batch
                 :jobs jobs
                 :remaining (length jobs))))
            groups)))
    (shuying--run-scheduler)))

(defun shuying-render-batch (requests)
  "Render REQUESTS through compatible backend batches.
Each element of REQUESTS has the form (SPECIFICATION . CALLBACK).
CALLBACK receives a `shuying-artifact' and an error value.  Cache hits complete
immediately, while identical pending specifications share a job."
  (dolist (request requests)
    (shuying--validate-render-request request))
  (let (jobs)
    (dolist (request requests)
      (let* ((specification (car request))
             (callback (cdr request))
             (key (shuying-render-spec-hash specification))
             (artifact-file (shuying-artifact-file specification))
             (metadata-file
              (shuying--artifact-metadata-file artifact-file))
             (artifact
              (shuying--read-artifact artifact-file metadata-file))
             (pending (gethash key shuying--pending-jobs)))
        (cond
         (artifact
          (funcall callback artifact nil))
         (pending
          (push callback (shuying--job-callbacks pending)))
         (t
          (make-directory shuying-cache-directory t)
          (let ((job
                 (make-shuying--job
                  :key key
                  :specification specification
                  :artifact-file artifact-file
                  :metadata-file metadata-file
                  :temporary-file
                  (make-temp-file
                   (expand-file-name ".render-"
                                     shuying-cache-directory)
                   nil
                   (format ".%s"
                           (shuying-render-spec-output-format
                            specification)))
                  :temporary-metadata-file
                  (make-temp-file
                   (expand-file-name ".metadata-"
                                     shuying-cache-directory)
                   nil ".eld")
                  :callbacks (list callback))))
            (puthash key job shuying--pending-jobs)
            (push job jobs))))))
    (shuying--enqueue-job-groups
     (shuying--group-jobs (nreverse jobs)))))

(defun shuying-render (specification callback)
  "Render SPECIFICATION and call CALLBACK with a Shuying artifact and error.
Identical pending requests share one backend job.  Cached artifacts call
CALLBACK immediately."
  (shuying-render-batch (list (cons specification callback))))

(provide 'shuying)

;;; shuying.el ends here
