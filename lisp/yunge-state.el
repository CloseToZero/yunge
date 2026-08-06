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

(defvar yunge-var-directory
  (file-name-as-directory
   (expand-file-name "var/" user-emacs-directory))
  "Directory containing mutable state for this configuration.")

(let* ((auto-save-directory
        (expand-file-name "auto-save/" yunge-var-directory))
       (tramp-auto-save-directory-path
        (expand-file-name "tramp/auto-save/" yunge-var-directory)))
  ;; Unlike the backup writer, auto-save does not create its destination.
  (dolist (path (list auto-save-directory
                      tramp-auto-save-directory-path))
    (make-directory path t))

  ;; Keep state produced by common editing features out of source trees.
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-directory t))
        auto-save-list-file-prefix
        (expand-file-name "session-" auto-save-directory)
        backup-directory-alist
        `(("." . ,(expand-file-name "backup/" yunge-var-directory)))
        bookmark-default-file
        (expand-file-name "bookmark.eld" yunge-var-directory)
        custom-file
        (expand-file-name "custom.el" yunge-var-directory)
        org-clock-persist-file
        (expand-file-name "org-clock-save.el" yunge-var-directory)
        org-id-locations-file
        (expand-file-name "org-id-locations.eld" yunge-var-directory)
        org-persist-directory
        (expand-file-name "org-persist/" yunge-var-directory)
        org-publish-timestamp-directory
        (expand-file-name "org-publish-timestamps/" yunge-var-directory)
        project-list-file
        (expand-file-name "project-list.eld" yunge-var-directory)
        recentf-save-file
        (expand-file-name "recentf.eld" yunge-var-directory)
        save-place-file
        (expand-file-name "save-place.eld" yunge-var-directory)
        savehist-file
        (expand-file-name "savehist.eld" yunge-var-directory)
        server-auth-dir
        (expand-file-name "server/" yunge-var-directory)
        tramp-auto-save-directory
        tramp-auto-save-directory-path
        tramp-persistency-file-name
        (expand-file-name "tramp/persistency.eld" yunge-var-directory)
        transient-history-file
        (expand-file-name "transient/history.el" yunge-var-directory)
        transient-levels-file
        (expand-file-name "transient/levels.el" yunge-var-directory)
        transient-values-file
        (expand-file-name "transient/values.el" yunge-var-directory))

  ;; We almost never edit one file from multiple Emacs instances.  Lock-file
  ;; churn is more likely to trigger pointless project-watcher automation.
  (setq create-lockfiles nil))

(provide 'yunge-state)

;;; yunge-state.el ends here
