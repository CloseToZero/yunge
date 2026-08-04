;;; yunge-autorevert-test.el --- Auto Revert tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-autorevert-configures-buffer-synchronization ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'yunge-autorevert)
       (unless auto-revert-use-notify
         (error "File notifications are disabled"))
       (unless auto-revert-avoid-polling
         (error "Files with notifications are still polled"))
       (unless global-auto-revert-non-file-buffers
         (error "Non-file buffers are not synchronized"))
       (unless (memq 'Buffer-menu-mode global-auto-revert-ignore-modes)
         (error "The Buffer Menu is not excluded"))
       (unless global-auto-revert-mode
         (error "Global Auto Revert mode is disabled"))
       (when (featurep 'dired)
         (error "Loading Auto Revert eagerly loaded Dired"))
       (require 'dired)
       (unless (eq dired-auto-revert-buffer
                   #'dired-directory-changed-p)
         (error "Dired does not check for changes when revisited"))))))

;;; yunge-autorevert-test.el ends here
