;;; shuying-setup.el --- Install Shuying dependencies -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'seq)
(require 'subr-x)
(require 'yunge-state)

(defcustom shuying-setup-windows-download-page
  "https://miktex.org/download"
  "Official MiKTeX page used to discover the current Windows installer."
  :type 'string
  :group 'shuying)

(defvar shuying-setup--process nil
  "The active Shuying dependency setup process, or nil.")

(defconst shuying-setup--log-buffer-name "*Shuying setup*"
  "Name of the Shuying dependency setup log buffer.")

(defun shuying-setup--powershell ()
  "Return an available PowerShell executable, or nil."
  (or (executable-find "pwsh.exe")
      (executable-find "powershell.exe")))

(defun shuying-setup--script ()
  "Return the Windows setup script path."
  (expand-file-name
   "script/shuying-setup-windows.ps1" user-emacs-directory))

(defun shuying-setup--work-directory ()
  "Create and return a private directory for one setup run."
  (let ((root (yunge-var-subdirectory "shuying/setup")))
    (make-directory root t)
    (make-temp-file (expand-file-name "run-" root) t)))

(defun shuying-setup--owned-work-directory-p (directory)
  "Return non-nil when DIRECTORY is a setup work directory Shuying owns."
  (when (and directory (file-directory-p directory))
    (let ((root (file-name-as-directory
                 (file-truename
                  (yunge-var-subdirectory "shuying/setup"))))
          (target (file-truename directory)))
      (and (file-in-directory-p target root)
           (string-prefix-p
            "run-" (file-name-nondirectory
                    (directory-file-name target)))))))

(defun shuying-setup--cleanup (directory &optional retries)
  "Delete the temporary setup DIRECTORY.
RETRIES is the number of nonblocking retries after a sharing violation.
Report cleanup failures without hiding the setup result."
  (when (and directory (file-exists-p directory))
    (if (not (shuying-setup--owned-work-directory-p directory))
        (display-warning
         'shuying
         (format "Refusing to remove unowned setup directory: %s"
                 directory)
         :warning)
      (condition-case error-data
          (delete-directory directory t)
        (file-error
         (if (> (or retries 4) 0)
             (run-at-time
              0.5 nil #'shuying-setup--cleanup
              directory (1- (or retries 4)))
           (display-warning
            'shuying
            (format "Could not remove Shuying setup files in %s: %s"
                    directory (error-message-string error-data))
            :warning)))))))

(defun shuying-setup--installed-bin (buffer)
  "Return the MiKTeX binary directory reported in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "^SHUYING_MIKTEX_BIN:\\([[:alnum:]+/=]+\\)\\r?$" nil t)
        (decode-coding-string
         (base64-decode-string (match-string 1)) 'utf-8)))))

(defun shuying-setup--add-exec-directory (directory)
  "Make executables in DIRECTORY visible to the current Emacs process."
  (let* ((directory (directory-file-name (expand-file-name directory)))
         (path (or (getenv "PATH") ""))
         (path-directories (parse-colon-path path)))
    (unless (seq-some
             (lambda (candidate)
               (and candidate
                    (string-equal-ignore-case
                     (directory-file-name (expand-file-name candidate))
                     directory)))
             exec-path)
      (push directory exec-path))
    (unless (seq-some
             (lambda (candidate)
               (and candidate
                    (string-equal-ignore-case
                     (directory-file-name (expand-file-name candidate))
                     directory)))
             path-directories)
      (setenv "PATH"
              (if (string-empty-p path)
                  directory
                (concat directory path-separator path))))))

(defun shuying-setup--sentinel (process _event &optional work-directory)
  "Finish setup after PROCESS exits."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'shuying-setup-finished)))
    (process-put process 'shuying-setup-finished t)
    (let* ((status (process-exit-status process))
           (buffer (process-buffer process))
           (work-directory
            (or work-directory
                (process-get process 'shuying-setup-work-directory))))
      (when (eq process shuying-setup--process)
        (setq shuying-setup--process nil))
      (unwind-protect
          (if (zerop status)
              (progn
                (when-let* ((directory
                             (shuying-setup--installed-bin buffer)))
                  (shuying-setup--add-exec-directory directory))
                (message "Shuying dependencies are ready"))
            (display-buffer buffer)
            (display-warning
             'shuying
             (format
              "Shuying setup failed (exit %d); see %s"
              status (buffer-name buffer))
             :error))
        ;; The PowerShell script cleans up in its `finally' block.  This is
        ;; a fallback for startup failures and externally terminated runs.
        (shuying-setup--cleanup work-directory)))))

(defun shuying-setup--windows ()
  "Set up Shuying's external dependencies on Windows."
  (when (process-live-p shuying-setup--process)
    (user-error "Shuying setup is already running"))
  (let ((powershell (shuying-setup--powershell))
        (script (shuying-setup--script)))
    (unless powershell
      (user-error "PowerShell is required to set up Shuying on Windows"))
    (unless (file-readable-p script)
      (user-error "Shuying setup script is missing: %s" script))
    (unless
        (yes-or-no-p
         (concat
          "Set up Shuying dependencies?  This may download and install "
          "per-user MiKTeX and TeX packages. "))
      (user-error "Shuying setup cancelled"))
    (let* ((buffer (get-buffer-create shuying-setup--log-buffer-name))
           (work-directory (shuying-setup--work-directory))
           process)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Shuying dependency setup\n\n"))
        (setq default-directory user-emacs-directory)
        (compilation-mode))
      (condition-case error-data
          (setq process
                (make-process
                 :name "shuying-setup"
                 :buffer buffer
                 :command
                 (list
                  powershell
                  "-NoLogo"
                  "-NoProfile"
                  "-NonInteractive"
                  "-ExecutionPolicy" "Bypass"
                  "-File" script
                  "-WorkDirectory" work-directory
                  "-WorkRoot" (yunge-var-subdirectory "shuying/setup")
                  "-DownloadPage" shuying-setup-windows-download-page)
                 :connection-type 'pipe
                 :coding 'utf-8-dos
                 :sentinel
                 (lambda (child event)
                   (shuying-setup--sentinel
                    child event work-directory))))
        (error
         (shuying-setup--cleanup work-directory)
         (signal (car error-data) (cdr error-data))))
      (process-put process 'shuying-setup-work-directory work-directory)
      (unless (process-get process 'shuying-setup-finished)
        (setq shuying-setup--process process))
      ;; A very short-lived PowerShell process can exit before Emacs returns
      ;; from `make-process'.  Complete it here if its sentinel has not run.
      (when (memq (process-status process) '(exit signal))
        (shuying-setup--sentinel process "finished" work-directory))
      (display-buffer buffer)
      process)))

;;;###autoload
(defun shuying-setup ()
  "Install and verify external dependencies used by Shuying.
Setup only runs when this command is invoked explicitly.  Windows is the
only currently supported platform."
  (interactive)
  (pcase system-type
    ('windows-nt (shuying-setup--windows))
    (_ (user-error
        "Shuying setup is not yet supported on %s" system-type))))

(provide 'shuying-setup)

;;; shuying-setup.el ends here
