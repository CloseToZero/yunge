;;; yunge-project-test.el --- Project tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-project)

(yunge-test-deftest-lazy-load yunge-project
  (project))

(ert-deftest yunge-project-keeps-submodules-separate ()
  (should-not project-vc-merge-submodules))

(provide 'yunge-project-test)

;;; yunge-project-test.el ends here
