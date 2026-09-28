;;; yunge-edit-test.el --- Editing default tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-edit)

(ert-deftest yunge-edit-defaults-to-100-columns ()
  (should (= (default-value 'fill-column) 100)))

;;; yunge-edit-test.el ends here
