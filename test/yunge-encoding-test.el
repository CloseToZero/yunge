;;; yunge-encoding-test.el --- Encoding tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-encoding-configures-utf-8 ()
  (yunge-test-run-emacs
   "-l" "yunge-encoding"
   "--eval"
   (prin1-to-string
    '(let ((input-coding
            (if (eq system-type 'windows-nt)
                (intern (format "cp%d" w32-ansi-code-page))
              'utf-8-unix)))
       (unless
           (and (eq (default-value 'buffer-file-coding-system)
                    'utf-8-unix)
                (eq yunge-encoding-process-input-coding-system
                    input-coding)
                (equal (find-operation-coding-system
                        'call-process "rg" nil nil nil)
                       `(utf-8 . ,input-coding)))
         (error "UTF-8 encoding was not configured"))))))

(ert-deftest yunge-encoding-preserves-ripgrep-nonascii-arguments ()
  (skip-unless (executable-find "rg"))
  (yunge-test-run-emacs
   "-l" "yunge-encoding"
   "--eval"
   (prin1-to-string
    '(let* ((query (string #x91cd #x8981 #x7684 #x4f8b #x5b50))
            (directory (make-temp-file "yunge-rg-encoding-" t))
            (default-directory (file-name-as-directory directory)))
       (unwind-protect
           (progn
             (with-temp-file (expand-file-name "match.txt" directory)
               (insert query))
             (with-temp-buffer
               (let ((status
                      (process-file
                       "rg" nil t nil "--fixed-strings" query ".")))
                 (unless
                     (and (zerop status)
                          (string-match-p
                           (regexp-quote query) (buffer-string)))
                   (error "Ripgrep lost a non-ASCII argument: %S"
                          (buffer-string))))))
         (delete-directory directory t))))))

(provide 'yunge-encoding-test)

;;; yunge-encoding-test.el ends here
