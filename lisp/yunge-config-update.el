;;; yunge-config-update.el --- Configuration update reminders -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'subr-x)
(require 'filenotify nil t)
(require 'yunge-state)

(declare-function magit-status "magit-status" (&optional directory cache))
(declare-function vc-dir "vc-dir" (directory))

(defvar global-mode-string)

(defgroup yunge-config-update nil
  "Check the tracked Emacs configuration for Git updates."
  :group 'convenience)

(defcustom yunge-config-update-interval (* 60 60)
  "Seconds between periodic configuration update checks."
  :type 'number
  :group 'yunge-config-update)

(defcustom yunge-config-update-idle-delay 5
  "Idle seconds to wait before a periodic update check."
  :type 'number
  :group 'yunge-config-update)

(defcustom yunge-config-update-timeout 60
  "Seconds before an individual Git process is stopped."
  :type 'number
  :group 'yunge-config-update)

(defcustom yunge-config-update-watch-delay 1
  "Seconds to debounce configuration Git metadata changes."
  :type 'number
  :group 'yunge-config-update)

(defcustom yunge-config-update-remote "origin"
  "Remote whose branch matching the current branch is checked."
  :type 'string
  :group 'yunge-config-update)

(defface yunge-config-update-mode-line
  '((t :inherit warning :weight bold))
  "Face for an out-of-sync configuration in the mode line."
  :group 'yunge-config-update)

(defvar yunge-config-update--process nil)
(defvar yunge-config-update--periodic-timer nil)
(defvar yunge-config-update--idle-timer nil)
(defvar yunge-config-update--watch-timer nil)
(defvar yunge-config-update--watch-descriptors nil)
(defvar yunge-config-update--watched-paths nil)
(defvar yunge-config-update--watching-p nil)
(defvar yunge-config-update--local-check-pending-p nil)
(defvar yunge-config-update--state nil)
(defvar yunge-config-update--last-error nil)

(defvar-keymap yunge-config-update-mode-line-map
  :doc "Keymap for the configuration update mode-line indicator."
  "<mode-line> <mouse-1>" #'yunge-config-update-open)

(defconst yunge-config-update-mode-line-format
  '(:eval (yunge-config-update--mode-line))
  "Mode-line construct showing configuration Git divergence.")

(defun yunge-config-update--commit-count (count)
  "Format COUNT as a number of commits."
  (format "%d commit%s" count (if (= count 1) "" "s")))

(defun yunge-config-update--description (state)
  "Return a human-readable description of update STATE."
  (let ((branch (plist-get state :branch))
        (ahead (plist-get state :ahead))
        (behind (plist-get state :behind)))
    (cond
     ((and (> ahead 0) (> behind 0))
      (format "Config and %s/%s diverged: %s local, %s remote"
              yunge-config-update-remote branch
              (yunge-config-update--commit-count ahead)
              (yunge-config-update--commit-count behind)))
     ((> behind 0)
      (format "Config update available: %s/%s is ahead by %s"
              yunge-config-update-remote branch
              (yunge-config-update--commit-count behind)))
     (t
      (format "Config has %s not on %s/%s"
              (yunge-config-update--commit-count ahead)
              yunge-config-update-remote branch)))))

(defun yunge-config-update--mode-line ()
  "Return the configuration update indicator for the selected window."
  (when (and yunge-config-update--state
             (mode-line-window-selected-p))
    (let* ((ahead (plist-get yunge-config-update--state :ahead))
           (behind (plist-get yunge-config-update--state :behind))
           (label
            (cond
             ((and (> ahead 0) (> behind 0))
              (format " Config↕%d/%d" ahead behind))
             ((> behind 0) (format " Config↓%d" behind))
             (t (format " Config↑%d" ahead)))))
      (propertize
       label
       'face 'yunge-config-update-mode-line
       'mouse-face 'mode-line-highlight
       'help-echo
       (concat (yunge-config-update--description
                yunge-config-update--state)
               "; mouse-1: open Magit status")
       'local-map yunge-config-update-mode-line-map))))

(defun yunge-config-update-open ()
  "Open the configuration repository in Magit or VC Directory mode."
  (interactive)
  (if (require 'magit-status nil t)
      (magit-status yunge-config-directory)
    (vc-dir yunge-config-directory)))

(defun yunge-config-update--git-output (git &rest arguments)
  "Run local GIT with ARGUMENTS and return its trimmed output on success."
  (let ((coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (with-temp-buffer
      (when (zerop
             (apply #'process-file git nil t nil
                    "-C" yunge-config-directory arguments))
        (string-trim (buffer-string))))))

(defun yunge-config-update--current-branch (git)
  "Return the current configuration branch using GIT."
  (yunge-config-update--git-output
   git "symbolic-ref" "--quiet" "--short" "HEAD"))

(defun yunge-config-update--git-path (git path)
  "Return the absolute repository path for Git PATH using GIT."
  (when-let* ((value
               (yunge-config-update--git-output
                git "rev-parse" "--git-path" path))
              ((not (string-empty-p value))))
    (expand-file-name value yunge-config-directory)))

(defun yunge-config-update--fetch-command (git branch)
  "Return the GIT command that fetches the remote copy of BRANCH."
  (list
   git "-C" yunge-config-directory "fetch" "--quiet" "--no-tags"
   yunge-config-update-remote
   (format "+refs/heads/%s:refs/remotes/%s/%s"
           branch yunge-config-update-remote branch)))

(defun yunge-config-update--compare-command (git branch)
  "Return the GIT command that compares HEAD with remote BRANCH."
  (list
   git "-C" yunge-config-directory "rev-list"
   "--left-right" "--count"
   (format "HEAD...refs/remotes/%s/%s"
           yunge-config-update-remote branch)))

(defun yunge-config-update--process-output (process)
  "Return the trimmed output collected for PROCESS."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (string-trim (buffer-string)))))

(defun yunge-config-update--finish-process (process)
  "Release timers and buffers owned by PROCESS."
  (when-let* ((timer (process-get process 'yunge-config-update-timeout))
              ((timerp timer)))
    (cancel-timer timer))
  (when (eq process yunge-config-update--process)
    (setq yunge-config-update--process nil))
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (kill-buffer buffer))
  (yunge-config-update--schedule-pending-local-check))

(defun yunge-config-update--record-error (interactive format-string
                                                      &rest arguments)
  "Record a check error described by FORMAT-STRING and ARGUMENTS.
When INTERACTIVE is non-nil, also show it in the echo area."
  (setq yunge-config-update--last-error
        (apply #'format format-string arguments))
  (when interactive
    (message "Config update check failed: %s"
             yunge-config-update--last-error)))

(defun yunge-config-update--process-failure (process stage output)
  "Record PROCESS failure during STAGE, including its OUTPUT."
  (let ((interactive
         (process-get process 'yunge-config-update-interactive)))
    (unless (process-get process 'yunge-config-update-stopping)
      (cond
       ((process-get process 'yunge-config-update-timed-out)
        (yunge-config-update--record-error
         interactive "%s timed out" stage))
       ((not (string-empty-p output))
        (yunge-config-update--record-error
         interactive "%s: %s" stage output))
       (t
        (yunge-config-update--record-error
         interactive "%s exited with status %d"
         stage (process-exit-status process)))))))

(defun yunge-config-update--timeout (process)
  "Stop PROCESS after `yunge-config-update-timeout'."
  (when (process-live-p process)
    (process-put process 'yunge-config-update-timed-out t)
    (delete-process process)))

(defun yunge-config-update--start-process
    (name command sentinel branch interactive stage)
  "Start an asynchronous Git process named NAME running COMMAND.
SENTINEL handles completion.  BRANCH, INTERACTIVE, and STAGE are saved as
process metadata."
  (let ((buffer (generate-new-buffer (format " *%s*" name)))
        (process-environment (copy-sequence process-environment))
        process)
    (setenv "GIT_TERMINAL_PROMPT" "0")
    (setenv "GCM_INTERACTIVE" "Never")
    (condition-case error-data
        (setq process
              (make-process
               :name name
               :buffer buffer
               :command command
               :coding '(utf-8-unix . utf-8-unix)
               :connection-type 'pipe
               :noquery t
               :sentinel sentinel))
      (error
       (kill-buffer buffer)
       (yunge-config-update--record-error
        interactive "%s: %s" stage
        (error-message-string error-data))))
    (when process
      (process-put process 'yunge-config-update-branch branch)
      (process-put process 'yunge-config-update-interactive interactive)
      (process-put process 'yunge-config-update-stage stage)
      (when (> yunge-config-update-timeout 0)
        (process-put
         process 'yunge-config-update-timeout
         (run-at-time yunge-config-update-timeout nil
                      #'yunge-config-update--timeout process)))
      (setq yunge-config-update--process process))
    process))

(defun yunge-config-update--start-comparison (git branch interactive)
  "Asynchronously compare HEAD with remote BRANCH using GIT."
  (yunge-config-update--start-process
   "yunge-config-update-compare"
   (yunge-config-update--compare-command git branch)
   #'yunge-config-update--compare-sentinel
   branch interactive "git rev-list"))

(defun yunge-config-update--check-local ()
  "Asynchronously compare HEAD with the cached remote-tracking branch."
  (cond
   ((process-live-p yunge-config-update--process)
    (setq yunge-config-update--local-check-pending-p t))
   ((not (executable-find "git"))
    (yunge-config-update--record-error nil "Git is unavailable"))
   (t
    (let* ((git (executable-find "git"))
           (branch (yunge-config-update--current-branch git)))
      (if branch
          (yunge-config-update--start-comparison git branch nil)
        (yunge-config-update--record-error
         nil "Config HEAD is detached or is not a Git repository"))))))

(defun yunge-config-update--fetch-sentinel (process _event)
  "Handle completion of the asynchronous Git fetch PROCESS."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'yunge-config-update-finished)))
    (process-put process 'yunge-config-update-finished t)
    (let ((git (car (process-command process)))
          (branch (process-get process 'yunge-config-update-branch))
          (interactive
           (process-get process 'yunge-config-update-interactive))
          (output (or (yunge-config-update--process-output process) ""))
          (success (and (eq (process-status process) 'exit)
                        (zerop (process-exit-status process)))))
      (yunge-config-update--finish-process process)
      (if success
          (if (equal branch (yunge-config-update--current-branch git))
              (yunge-config-update--start-comparison
               git branch interactive)
            (yunge-config-update-check interactive))
        (yunge-config-update--process-failure
         process "git fetch" output)))))

(defun yunge-config-update--set-state (branch ahead behind)
  "Record that BRANCH is AHEAD and BEHIND its remote counterpart."
  (yunge-config-update--refresh-watches)
  (let* ((old-state yunge-config-update--state)
         (new-state
          (unless (and (zerop ahead) (zerop behind))
            (list :branch branch :ahead ahead :behind behind))))
    (setq yunge-config-update--state new-state
          yunge-config-update--last-error nil)
    (force-mode-line-update t)
    (unless (equal old-state new-state)
      (cond
       (new-state
        (message "%s" (yunge-config-update--description new-state)))
       (old-state
        (message "Config is now in sync with %s/%s"
                 yunge-config-update-remote
                 (plist-get old-state :branch)))))))

(defun yunge-config-update--compare-sentinel (process _event)
  "Handle completion of the asynchronous Git comparison PROCESS."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'yunge-config-update-finished)))
    (process-put process 'yunge-config-update-finished t)
    (let ((git (car (process-command process)))
          (branch (process-get process 'yunge-config-update-branch))
          (interactive
           (process-get process 'yunge-config-update-interactive))
          (output (or (yunge-config-update--process-output process) ""))
          (success (and (eq (process-status process) 'exit)
                        (zerop (process-exit-status process)))))
      (yunge-config-update--finish-process process)
      (cond
       ((not success)
        (yunge-config-update--process-failure
         process "git rev-list" output))
       ((not (equal branch (yunge-config-update--current-branch git)))
        (yunge-config-update-check interactive))
       ((string-match
         "\\`[[:space:]]*\\([0-9]+\\)[[:space:]]+\\([0-9]+\\)[[:space:]]*\\'"
         output)
        (yunge-config-update--set-state
         branch
         (string-to-number (match-string 1 output))
         (string-to-number (match-string 2 output))))
       (t
        (yunge-config-update--record-error
         interactive "Unexpected git rev-list output: %s" output))))))

(defun yunge-config-update-check (&optional interactive)
  "Asynchronously compare the config HEAD with its matching remote branch.
When INTERACTIVE is non-nil, report failures that scheduled checks keep quiet."
  (interactive (list t))
  (cond
   ((process-live-p yunge-config-update--process)
    (when interactive
      (message "A config update check is already running")))
   ((not (executable-find "git"))
    (yunge-config-update--record-error interactive "Git is unavailable"))
   (t
    (let* ((git (executable-find "git"))
           (branch (yunge-config-update--current-branch git)))
      (if (not branch)
          (yunge-config-update--record-error
           interactive "Config HEAD is detached or is not a Git repository")
        (yunge-config-update--start-process
         "yunge-config-update-fetch"
         (yunge-config-update--fetch-command git branch)
         #'yunge-config-update--fetch-sentinel
         branch interactive "git fetch"))))))

(defun yunge-config-update--watch-paths ()
  "Return existing Git metadata paths that should be watched."
  (when-let* ((git (executable-find "git"))
              (branch (yunge-config-update--current-branch git)))
    (delete-dups
     (delq
      nil
      (mapcar
       (lambda (path)
         (when-let* ((file (yunge-config-update--git-path git path))
                     ((file-exists-p file)))
           file))
       (list "logs/HEAD"
             (format "logs/refs/remotes/%s/%s"
                     yunge-config-update-remote branch)))))))

(defun yunge-config-update--remove-watches ()
  "Remove all configuration Git metadata watches."
  (when (fboundp 'file-notify-rm-watch)
    (dolist (descriptor yunge-config-update--watch-descriptors)
      (ignore-errors (file-notify-rm-watch descriptor))))
  (setq yunge-config-update--watch-descriptors nil
        yunge-config-update--watched-paths nil))

(defun yunge-config-update--refresh-watches ()
  "Watch Git metadata for the current local and remote branches."
  (when (and yunge-config-update--watching-p
             (fboundp 'file-notify-add-watch))
    (let ((paths (yunge-config-update--watch-paths)))
      (unless (equal paths yunge-config-update--watched-paths)
        (yunge-config-update--remove-watches)
        (dolist (path paths)
          (condition-case nil
              (push
               (file-notify-add-watch
                path '(change) #'yunge-config-update--watch-callback)
               yunge-config-update--watch-descriptors)
            (file-notify-error nil)))
        (setq yunge-config-update--watch-descriptors
              (nreverse yunge-config-update--watch-descriptors)
              yunge-config-update--watched-paths paths)))))

(defun yunge-config-update--schedule-pending-local-check ()
  "Schedule a pending local comparison when no debounce timer exists."
  (when (and yunge-config-update--watching-p
             yunge-config-update--local-check-pending-p
             (not (timerp yunge-config-update--watch-timer)))
    (setq yunge-config-update--watch-timer
          (run-at-time yunge-config-update-watch-delay nil
                       #'yunge-config-update--run-local-check))))

(defun yunge-config-update--queue-local-check ()
  "Debounce a local comparison after Git metadata changes."
  (when yunge-config-update--watching-p
    (setq yunge-config-update--local-check-pending-p t)
    (when (timerp yunge-config-update--watch-timer)
      (cancel-timer yunge-config-update--watch-timer)
      (setq yunge-config-update--watch-timer nil))
    (yunge-config-update--schedule-pending-local-check)))

(defun yunge-config-update--watch-callback (event)
  "Queue a local comparison for a Git file notification EVENT."
  (when (and yunge-config-update--watching-p
             (memq (car event) yunge-config-update--watch-descriptors))
    (when (eq (cadr event) 'stopped)
      (yunge-config-update--remove-watches))
    (yunge-config-update--queue-local-check)))

(defun yunge-config-update--run-local-check ()
  "Run a pending local comparison when no other check is active."
  (setq yunge-config-update--watch-timer nil)
  (when (and yunge-config-update--watching-p
             yunge-config-update--local-check-pending-p
             (not (process-live-p yunge-config-update--process)))
    (setq yunge-config-update--local-check-pending-p nil)
    (yunge-config-update--refresh-watches)
    (yunge-config-update--check-local)))

(defun yunge-config-update--run-idle-check ()
  "Run a periodic update check after its idle delay."
  (setq yunge-config-update--idle-timer nil)
  (yunge-config-update-check))

(defun yunge-config-update--schedule-idle-check ()
  "Schedule one update check after Emacs next becomes idle."
  (unless (or (timerp yunge-config-update--idle-timer)
              (process-live-p yunge-config-update--process))
    (setq yunge-config-update--idle-timer
          (run-with-idle-timer
           yunge-config-update-idle-delay nil
           #'yunge-config-update--run-idle-check))))

(defun yunge-config-update-stop ()
  "Stop scheduled configuration update checks."
  (interactive)
  (setq yunge-config-update--watching-p nil
        yunge-config-update--local-check-pending-p nil)
  (dolist (timer (list yunge-config-update--periodic-timer
                       yunge-config-update--idle-timer
                       yunge-config-update--watch-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (setq yunge-config-update--periodic-timer nil
        yunge-config-update--idle-timer nil
        yunge-config-update--watch-timer nil)
  (yunge-config-update--remove-watches)
  (when (process-live-p yunge-config-update--process)
    (process-put yunge-config-update--process
                 'yunge-config-update-stopping t)
    (delete-process yunge-config-update--process)))

(defun yunge-config-update-start ()
  "Start immediate and periodic configuration update checks."
  (interactive)
  (yunge-config-update-stop)
  (setq yunge-config-update--watching-p t)
  (yunge-config-update--refresh-watches)
  (yunge-config-update-check)
  (setq yunge-config-update--periodic-timer
        (run-at-time
         yunge-config-update-interval yunge-config-update-interval
         #'yunge-config-update--schedule-idle-check)))

(add-to-list 'global-mode-string
             yunge-config-update-mode-line-format t)
(add-hook 'emacs-startup-hook #'yunge-config-update-start t)
(add-hook 'kill-emacs-hook #'yunge-config-update-stop)

(provide 'yunge-config-update)

;;; yunge-config-update.el ends here
