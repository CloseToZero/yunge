;;; yunge-key-control-test.el --- Key control tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-key-control)

(ert-deftest yunge-key-control-styles-hints ()
  (let* ((message
          (yunge-key-control-message
           "Window" '(("h/j/k/l" "select") ("SPC" "exit"))))
         (plain (substring-no-properties message))
         (key-position (string-match "h/j/k/l" plain))
         (description-position (string-match "select" plain)))
    (should (equal plain "Window: h/j/k/l select  SPC exit"))
    (should (eq (get-text-property 0 'face message)
                'yunge-key-control-title))
    (should (eq (get-text-property key-position 'face message)
                'yunge-key-control-key))
    (should (eq (get-text-property description-position 'face message)
                'yunge-key-control-description))))

;;; yunge-key-control-test.el ends here
