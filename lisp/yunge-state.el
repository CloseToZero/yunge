;;; yunge-state.el --- Mutable state locations -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(let ((directory (expand-file-name "var/" user-emacs-directory)))
  ;; Keep state produced by common editing features out of source trees.
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name "auto-save/" directory) t))
        auto-save-list-file-prefix
        (expand-file-name "auto-save/session-" directory)
        backup-directory-alist
        `(("." . ,(expand-file-name "backup/" directory)))
        project-list-file
        (expand-file-name "project-list.eld" directory)
        recentf-save-file
        (expand-file-name "recentf.eld" directory)
        save-place-file
        (expand-file-name "save-place.eld" directory)
        savehist-file
        (expand-file-name "savehist.eld" directory)
        tramp-auto-save-directory
        (expand-file-name "tramp/auto-save/" directory)
        tramp-persistency-file-name
        (expand-file-name "tramp/persistency.eld" directory))

  ;; We almost never edit one file from multiple Emacs instances.  Lock-file
  ;; churn is more likely to trigger pointless project-watcher automation.
  (setq create-lockfiles nil))

(provide 'yunge-state)

;;; yunge-state.el ends here
