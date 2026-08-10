;;; yunge-encoding-test.el --- Encoding tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-encoding-configures-utf-8 ()
  (yunge-test-run-emacs
   "-l" "yunge-encoding"
   "--eval"
   (prin1-to-string
    '(unless
         (and (eq (default-value 'buffer-file-coding-system)
                  'utf-8-unix)
              (equal (find-operation-coding-system
                      'call-process "rg" nil nil nil)
                     '(utf-8 . utf-8-unix)))
       (error "UTF-8 encoding was not configured")))))

(provide 'yunge-encoding-test)

;;; yunge-encoding-test.el ends here
