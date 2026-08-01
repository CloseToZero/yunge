;;; yunge-byte-compile-test.el --- Compiler warnings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-add-package-path
 'compat 'consult 'elpaca 'evil 'goto-chg 'marginalia 'orderless
 'vertico 'which-key)

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

(defun yunge-byte-compile-test--run ()
  "Compile configuration files and signal any diagnostics."
  (require 'elpaca)
  (let (diagnostics)
    (dolist (file (yunge-byte-compile-test--source-files))
      (dolist (diagnostic (yunge-test-byte-compile-diagnostics file))
        (push (yunge-byte-compile-test--format-diagnostic file diagnostic)
              diagnostics)))
    (when diagnostics
      (error "%s" (mapconcat #'identity
                             (nreverse diagnostics) "\n")))))

(ert-deftest yunge-configuration-byte-compiles-without-warnings ()
  (apply
   #'yunge-test-run-emacs
   (append
    (yunge-test-package-arguments
     '(compat consult elpaca evil goto-chg marginalia orderless
              vertico which-key))
    (list "-L" (expand-file-name "test" yunge-test-root)
          "-l" "yunge-test-helper"
          "-l" "yunge-byte-compile-test"
          "--eval" "(yunge-byte-compile-test--run)"))))

;;; yunge-byte-compile-test.el ends here
