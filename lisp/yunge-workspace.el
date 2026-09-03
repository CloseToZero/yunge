;;; yunge-workspace.el --- Persistent editing workspaces -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'frameset)
(require 'seq)
(require 'subr-x)
(require 'tab-bar)
(require 'yunge-state)

(defgroup yunge-workspace nil
  "Save and restore named editing workspaces."
  :group 'yunge)

(defvar yunge-workspace-auto-save-mode)

(defvar yunge-workspace--auto-save-timer nil
  "Timer used to save the default workspace.")

(defun yunge-workspace--set-auto-save-interval (symbol value)
  "Set SYMBOL to VALUE and update the workspace timer."
  (set-default symbol value)
  (when (and (bound-and-true-p yunge-workspace-auto-save-mode)
             (fboundp 'yunge-workspace--reset-auto-save-timer))
    (yunge-workspace--reset-auto-save-timer)))

(defcustom yunge-workspace-auto-save-interval (* 5 60)
  "Seconds between saves of the default workspace."
  :type '(integer :match (lambda (_widget value) (> value 0)))
  :set #'yunge-workspace--set-auto-save-interval
  :group 'yunge-workspace)

(defcustom yunge-workspace-default-name "default"
  "Name of the workspace maintained by automatic saves."
  :type 'string
  :group 'yunge-workspace)

(defconst yunge-workspace-format-version 1
  "Version of the persistent workspace data format.")

(defconst yunge-workspace-file-extension ".eld"
  "Extension used by persistent workspace files.")

(defun yunge-workspace--validate-name (name)
  "Return NAME when it is a portable workspace name.
Signal a user error otherwise."
  (unless (and (stringp name)
               (not (string-empty-p name))
               (not (member name '("." "..")))
               (not (string-match-p "[/\\\\]" name))
               (not (string-match-p "[[:cntrl:]]" name))
               (not (string-match-p "[ .]\\'" name))
               (not (string-match-p file-name-invalid-regexp name)))
    (user-error "Invalid workspace name: %S" name))
  name)

(defun yunge-workspace--file (name)
  "Return the workspace file for NAME."
  (expand-file-name
   (concat (yunge-workspace--validate-name name)
           yunge-workspace-file-extension)
   yunge-workspace-directory))

(defun yunge-workspace-names ()
  "Return the names of saved workspaces."
  (when (file-directory-p yunge-workspace-directory)
    (sort
     (delq
      nil
      (mapcar
       (lambda (file)
         (when (file-regular-p file)
           (string-remove-suffix
            yunge-workspace-file-extension
            (file-name-nondirectory file))))
       (directory-files
        yunge-workspace-directory t
        (concat (regexp-quote yunge-workspace-file-extension) "\\'") t)))
     #'string-lessp)))

(defun yunge-workspace--persistent-frame-p (frame)
  "Return non-nil when FRAME belongs in a workspace."
  (and (frame-live-p frame)
       (not (frame-parameter frame 'parent-frame))
       (not (and (daemonp) (frame-initial-p frame)))))

(defun yunge-workspace--file-buffer-record (buffer)
  "Return a persistent record for file-visiting BUFFER, or nil."
  (with-current-buffer buffer
    (when (and buffer-file-name
               (not (buffer-base-buffer))
               (not (file-remote-p buffer-file-name)))
      (list :kind 'file
            :name (buffer-name)
            :file (expand-file-name buffer-file-name)))))

(defun yunge-workspace--snapshot (name)
  "Return a persistent snapshot named NAME."
  (let ((frames (seq-filter #'yunge-workspace--persistent-frame-p
                            (frame-list))))
    (unless frames
      (user-error "There is no frame to save in the workspace"))
    ;; Ensure every frame has current tab metadata before Frameset reads it.
    (dolist (frame frames)
      (tab-bar-tabs frame))
    (list
     :version yunge-workspace-format-version
     :name name
     :saved-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
     ;; Tagged records leave room for mode-owned special-buffer serializers in
     ;; a later format without coupling them to frame or tab persistence.
     :buffers (delq nil
                    (mapcar #'yunge-workspace--file-buffer-record
                            (buffer-list)))
     :frameset (frameset-save
                frames
                :app `(yunge-workspace . ,yunge-workspace-format-version)
                :name name))))

(defun yunge-workspace--write (file data)
  "Atomically write workspace DATA to FILE."
  (make-directory (file-name-directory file) t)
  (let ((temporary
         (make-temp-file
          (expand-file-name ".yunge-workspace-"
                            (file-name-directory file))
          nil ".tmp")))
    (unwind-protect
        (let ((print-circle t)
              (print-length nil)
              (print-level nil))
          (let ((serialized (readablep data)))
            (unless serialized
              (error "Workspace contains state that cannot be serialized"))
            (let ((coding-system-for-write 'utf-8-emacs))
              (with-temp-file temporary
                (insert ";; Yunge workspace -- generated file\n"
                        serialized "\n")))
            (rename-file temporary file t)))
      (when (file-exists-p temporary)
        (delete-file temporary))))
  file)

;;;###autoload
(defun yunge-workspace-save (name)
  "Save the current editing workspace as NAME."
  (interactive
   (list
    (completing-read "Save workspace as: "
                     (yunge-workspace-names) nil nil nil nil
                     yunge-workspace-default-name)))
  (setq name (yunge-workspace--validate-name name))
  (let ((file (yunge-workspace--file name)))
    (yunge-workspace--write file (yunge-workspace--snapshot name))
    (when (called-interactively-p 'interactive)
      (message "Saved workspace '%s'" name))
    file))

(defun yunge-workspace--read (name)
  "Read and validate the saved workspace NAME."
  (let ((file (yunge-workspace--file name)))
    (unless (file-regular-p file)
      (user-error "Workspace does not exist: %s" name))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (unless (and (equal (plist-get data :version)
                                yunge-workspace-format-version)
                         (equal (plist-get data :name) name)
                         (listp (plist-get data :buffers))
                         (frameset-p (plist-get data :frameset)))
              (user-error "Workspace has an unsupported or invalid format: %s"
                          name))
            data))
      (user-error (signal (car err) (cdr err)))
      (error
       (user-error "Could not read workspace '%s': %s"
                   name (error-message-string err))))))

(defun yunge-workspace--restore-file-buffer (record)
  "Restore the file buffer described by RECORD, or return nil."
  (when (eq (plist-get record :kind) 'file)
    (let ((file (plist-get record :file))
          (name (plist-get record :name)))
      (when (and (stringp file)
                 (stringp name)
                 (file-readable-p file))
        (condition-case err
            (let ((buffer (find-file-noselect file)))
              (when (and (not (equal (buffer-name buffer) name))
                         (not (get-buffer name)))
                (with-current-buffer buffer
                  (rename-buffer name)))
              buffer)
          (error
           (display-warning
            'yunge-workspace
            (format "Could not restore %s: %s"
                    file (error-message-string err))
            :warning)
           nil))))))

;;;###autoload
(defun yunge-workspace-restore (name)
  "Restore the saved workspace NAME.
Existing buffers are kept when they are not part of the workspace."
  (interactive
   (let ((names (yunge-workspace-names)))
     (unless names
       (user-error "There are no saved workspaces"))
     (list
      (completing-read "Restore workspace: " names nil t nil nil
                       (and (member yunge-workspace-default-name names)
                            yunge-workspace-default-name)))))
  (let* ((data (yunge-workspace--read name))
         (records (plist-get data :buffers))
         (restored 0))
    ;; Frameset only restores windows; materialize their buffers first.
    (dolist (record records)
      (when (yunge-workspace--restore-file-buffer record)
        (setq restored (1+ restored))))
    (let ((inhibit-redisplay t))
      (frameset-restore
       (plist-get data :frameset)
       :reuse-frames t
       :cleanup-frames t
       :force-display t
       :force-onscreen (display-graphic-p)))
    (message "Restored workspace '%s' (%d file buffer%s)"
             name restored (if (= restored 1) "" "s"))
    name))

;;;###autoload
(defun yunge-workspace-save-default ()
  "Save the current state to the default workspace.
Errors are reported without preventing Emacs from exiting."
  (interactive)
  (let ((interactivep (called-interactively-p 'interactive)))
    (condition-case err
        (progn
          (yunge-workspace-save yunge-workspace-default-name)
          (when interactivep
            (message "Saved default workspace")))
      (error
       (if interactivep
           (user-error "Could not save default workspace: %s"
                       (error-message-string err))
         (display-warning
          'yunge-workspace
          (format "Could not save default workspace: %s"
                  (error-message-string err))
          :warning)))))
  t)

(defun yunge-workspace--auto-save-default ()
  "Save the default workspace when automatic saving is appropriate."
  (when (and yunge-workspace-auto-save-mode
             (not noninteractive)
             (not (active-minibuffer-window)))
    (yunge-workspace-save-default))
  t)

(defun yunge-workspace--reset-auto-save-timer ()
  "Reset the default workspace auto-save timer."
  (when (timerp yunge-workspace--auto-save-timer)
    (cancel-timer yunge-workspace--auto-save-timer)
    (setq yunge-workspace--auto-save-timer nil))
  (when (and yunge-workspace-auto-save-mode
             (not noninteractive))
    (setq yunge-workspace--auto-save-timer
          (run-with-timer yunge-workspace-auto-save-interval
                          yunge-workspace-auto-save-interval
                          #'yunge-workspace--auto-save-default))))

;;;###autoload
(define-minor-mode yunge-workspace-auto-save-mode
  "Save the default workspace periodically and before a normal exit."
  :global t
  :init-value t
  :lighter nil
  :group 'yunge-workspace
  (if yunge-workspace-auto-save-mode
      (progn
        (yunge-workspace--reset-auto-save-timer)
        (add-hook 'kill-emacs-hook
                  #'yunge-workspace--auto-save-default))
    (remove-hook 'kill-emacs-hook
                 #'yunge-workspace--auto-save-default)
    (yunge-workspace--reset-auto-save-timer)))

;; Keep a crash-recovery snapshot available, but never restore it implicitly.
(yunge-workspace-auto-save-mode
 (if yunge-workspace-auto-save-mode 1 -1))

(provide 'yunge-workspace)

;;; yunge-workspace.el ends here
