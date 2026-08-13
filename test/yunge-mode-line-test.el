;;; yunge-mode-line-test.el --- Mode line tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-early-init-discovers-the-tracked-config-root ()
  (yunge-test-run-emacs
   "--eval" "(makunbound 'yunge-config-directory)"
   "-l" (expand-file-name "early-init.el" yunge-test-root)
   "--eval"
   (prin1-to-string
    `(unless
         (and
          (equal yunge-config-directory
                 ,(file-name-as-directory yunge-test-root))
          (not (equal yunge-config-directory user-emacs-directory))
          (equal yunge-var-directory
                 (expand-file-name "var/" user-emacs-directory)))
       (error
        "Config, user, and state roots were not separated: %S"
        (list yunge-config-directory user-emacs-directory
              yunge-var-directory))))))

(ert-deftest yunge-mode-line-shows-one-based-columns ()
  (yunge-test-run-emacs
   "-l" (expand-file-name "early-init.el" yunge-test-root)
   "--eval"
   (prin1-to-string
    '(unless
         (and column-number-mode
              (equal mode-line-position-column-format '(" C%C"))
              (equal mode-line-position-column-line-format
                     '(" (%l,%C)")))
       (error "Mode-line columns are not one-based")))))

;;; yunge-mode-line-test.el ends here
