;;; yunge-autorevert.el --- Buffer synchronization -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'autorevert)

(declare-function dired-directory-changed-p "dired" (dirname))

(defvar dired-auto-revert-buffer)

(setopt auto-revert-use-notify t
        auto-revert-avoid-polling t
        global-auto-revert-non-file-buffers t)

;; Exclude the Buffer Menu, which would otherwise rebuild every interval.
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

(global-auto-revert-mode 1)

(with-eval-after-load 'dired
  (setq dired-auto-revert-buffer #'dired-directory-changed-p))

(provide 'yunge-autorevert)

;;; yunge-autorevert.el ends here
