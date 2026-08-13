;;; yunge-state.el --- Mutable state locations -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defvar transient-history-file)
(defvar transient-levels-file)
(defvar transient-values-file)
(defvar server-auth-dir)
(defvar org-clock-persist-file)
(defvar org-id-locations-file)
(defvar org-persist-directory)
(defvar org-publish-timestamp-directory)

(defvar yunge-config-directory
  (file-name-as-directory user-emacs-directory)
  "Directory containing this configuration's tracked files.")

(defvar yunge-var-directory
  (file-name-as-directory
   (expand-file-name "var/" user-emacs-directory))
  "Directory containing mutable state for this configuration.")

(defun yunge-var-subdirectory (name)
  "Return the NAME directory below `yunge-var-directory'."
  (file-name-as-directory
   (expand-file-name name yunge-var-directory)))

(defun yunge-var-file (owner name)
  "Return NAME in OWNER's directory below `yunge-var-directory'."
  (expand-file-name name (yunge-var-subdirectory owner)))

(let* ((auto-save-directory
        (yunge-var-subdirectory "auto-save"))
       (tramp-auto-save-directory-path
        (yunge-var-subdirectory "tramp/auto-save")))
  ;; These writers require their destination directories to exist.
  (dolist (directory
           (list auto-save-directory
                 tramp-auto-save-directory-path
                 (yunge-var-subdirectory "bookmark")
                 (yunge-var-subdirectory "custom")
                 (yunge-var-subdirectory "org-clock")
                 (yunge-var-subdirectory "org-id")
                 (yunge-var-subdirectory "org-persist")
                 (yunge-var-subdirectory "org-publish/timestamps")
                 (yunge-var-subdirectory "project")
                 (yunge-var-subdirectory "recentf")
                 (yunge-var-subdirectory "save-place")
                 (yunge-var-subdirectory "savehist")))
    (make-directory directory t))

  ;; Keep state produced by common editing features out of source trees.
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-directory t))
        auto-save-list-file-prefix
        (expand-file-name "session-" auto-save-directory)
        backup-directory-alist
        `(("." . ,(yunge-var-subdirectory "backup")))
        bookmark-default-file
        (yunge-var-file "bookmark" "bookmarks.eld")
        custom-file
        (yunge-var-file "custom" "custom.el")
        org-clock-persist-file
        (yunge-var-file "org-clock" "clock-save.el")
        org-id-locations-file
        (yunge-var-file "org-id" "locations.eld")
        org-persist-directory
        (yunge-var-subdirectory "org-persist")
        org-publish-timestamp-directory
        (yunge-var-subdirectory "org-publish/timestamps")
        project-list-file
        (yunge-var-file "project" "projects.eld")
        recentf-save-file
        (yunge-var-file "recentf" "recentf.eld")
        save-place-file
        (yunge-var-file "save-place" "places.eld")
        savehist-file
        (yunge-var-file "savehist" "savehist.eld")
        server-auth-dir
        (yunge-var-subdirectory "server")
        tramp-auto-save-directory
        tramp-auto-save-directory-path
        tramp-persistency-file-name
        (yunge-var-file "tramp" "persistency.eld")
        transient-history-file
        (yunge-var-file "transient" "history.el")
        transient-levels-file
        (yunge-var-file "transient" "levels.el")
        transient-values-file
        (yunge-var-file "transient" "values.el"))

  ;; We almost never edit one file from multiple Emacs instances.  Lock-file
  ;; churn is more likely to trigger pointless project-watcher automation.
  (setq create-lockfiles nil))

(provide 'yunge-state)

;;; yunge-state.el ends here
