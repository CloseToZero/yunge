;;; yunge-marginalia-test.el --- Marginalia tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-marginalia
  (marginalia))

(ert-deftest yunge-marginalia-enables-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-marginalia 'marginalia
   :before-ready
   '(when (or (featurep 'marginalia)
              (bound-and-true-p marginalia-mode))
      (error "Marginalia was enabled before its Elpaca body ran"))
   :after-ready
   '(unless (and (featurep 'marginalia) marginalia-mode)
      (error "Marginalia was not enabled after package readiness"))))

;;; yunge-marginalia-test.el ends here
