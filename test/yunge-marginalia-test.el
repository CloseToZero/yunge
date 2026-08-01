;;; yunge-marginalia-test.el --- Marginalia tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-marginalia-loads-lazily ()
  (yunge-test-assert-lazy-load
   'yunge-marginalia '(marginalia)))

;;; yunge-marginalia-test.el ends here
