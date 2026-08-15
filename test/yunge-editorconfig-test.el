;;; yunge-editorconfig-test.el --- EditorConfig tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-editorconfig
  (editorconfig))

(ert-deftest yunge-editorconfig-enables-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-editorconfig 'editorconfig
   :before-ready
   '(when (or (featurep 'editorconfig)
              (bound-and-true-p editorconfig-mode))
      (error "EditorConfig was enabled before its Elpaca body ran"))
   :after-ready
   '(unless (and (featurep 'editorconfig) editorconfig-mode)
      (error "EditorConfig was not enabled after package readiness"))))

;;; yunge-editorconfig-test.el ends here
