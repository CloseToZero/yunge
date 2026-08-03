;;; yunge-state.el --- Mutable state locations -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defvar transient-history-file)
(defvar transient-levels-file)
(defvar transient-values-file)
(defvar server-auth-dir)

(let* ((directory (expand-file-name "var/" user-emacs-directory))
       (auto-save-directory
        (expand-file-name "auto-save/" directory))
       (tramp-auto-save-directory-path
        (expand-file-name "tramp/auto-save/" directory)))
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
        `(("." . ,(expand-file-name "backup/" directory)))
        bookmark-default-file
        (expand-file-name "bookmark.eld" directory)
        custom-file
        (expand-file-name "custom.el" directory)
        project-list-file
        (expand-file-name "project-list.eld" directory)
        recentf-save-file
        (expand-file-name "recentf.eld" directory)
        save-place-file
        (expand-file-name "save-place.eld" directory)
        savehist-file
        (expand-file-name "savehist.eld" directory)
        server-auth-dir
        (expand-file-name "server/" directory)
        tramp-auto-save-directory
        tramp-auto-save-directory-path
        tramp-persistency-file-name
        (expand-file-name "tramp/persistency.eld" directory)
        transient-history-file
        (expand-file-name "transient/history.el" directory)
        transient-levels-file
        (expand-file-name "transient/levels.el" directory)
        transient-values-file
        (expand-file-name "transient/values.el" directory))

  ;; We almost never edit one file from multiple Emacs instances.  Lock-file
  ;; churn is more likely to trigger pointless project-watcher automation.
  (setq create-lockfiles nil))

(provide 'yunge-state)

;;; yunge-state.el ends here
