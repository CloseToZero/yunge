;;; yunge-which-key-test.el --- Which-Key tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-which-key
  (which-key))

(ert-deftest yunge-which-key-enables-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-which-key 'which-key
   :before-ready
   '(when (or (featurep 'which-key)
              (bound-and-true-p which-key-mode))
      (error "Which-Key was enabled before its Elpaca body ran"))
   :after-ready
   '(unless (and (featurep 'which-key) which-key-mode)
      (error "Which-Key was not enabled after package readiness"))))

;;; yunge-which-key-test.el ends here
