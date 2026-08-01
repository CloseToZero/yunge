;;; yunge-byte-compile-test.el --- Compiler warnings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-add-package-path
 'compat 'elpaca 'evil 'goto-chg 'marginalia 'vertico 'which-key)

(defun yunge-byte-compile-test--source-files ()
  "Return configuration source files that should compile without warnings."
  (append
   (mapcar (lambda (name) (expand-file-name name yunge-test-root))
           '("early-init.el" "init.el"))
   (directory-files (expand-file-name "lisp" yunge-test-root)
                    t "\\.el\\'")
   (directory-files (expand-file-name "test" yunge-test-root)
                    t "\\.el\\'")))

(defun yunge-byte-compile-test--format-diagnostic (file diagnostic)
  "Format a byte compiler DIAGNOSTIC reported for FILE."
  (pcase-let ((`(,position ,level ,message) diagnostic))
    (let ((line
           (when position
             (with-temp-buffer
               (insert-file-contents file)
               (line-number-at-pos position))))
          (relative (file-relative-name file yunge-test-root)))
      (format "%s%s: %s: %s"
              relative
              (if line (format ":%d" line) "")
              (or level :warning)
              message))))

(ert-deftest yunge-configuration-byte-compiles-without-warnings ()
  (require 'elpaca)
  (let (diagnostics)
    (dolist (file (yunge-byte-compile-test--source-files))
      (dolist (diagnostic (yunge-test-byte-compile-diagnostics file))
        (push (yunge-byte-compile-test--format-diagnostic file diagnostic)
              diagnostics)))
    (when diagnostics
      (ert-fail (mapconcat #'identity (nreverse diagnostics) "\n")))))

;;; yunge-byte-compile-test.el ends here
