;;; yunge-test-runner.el --- Repository checks -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'subr-x)

(defun yunge-test-runner--external-checks (suite &optional module)
  "Return external check commands for SUITE and optional native MODULE."
  (let ((root yunge-test-root))
    (pcase suite
      ('native
       (let ((checks
              `(("fangcun" "Fangcun Watch Rust tests"
                 "cargo" "test" "--manifest-path"
                 ,(expand-file-name "native/fangcun-watch/Cargo.toml" root))
                ("mcp" "Yunge MCP Rust tests"
                 "cargo" "test" "--manifest-path"
                 ,(expand-file-name "native/yunge-mcp/Cargo.toml" root))
                ("reader" "Yunge Reader Rust tests"
                 "cargo" "test" "--manifest-path"
                 ,(expand-file-name "native/yunge-reader/Cargo.toml" root)))))
         (if module
             (cl-remove-if-not (lambda (check) (equal (car check) module))
                               checks)
           checks)))
      ('renderer
       `(("renderer" "Yunge Reader renderer syntax"
          "node" "--check"
          ,(expand-file-name
            "native/yunge-reader/renderer/yunge-reader.js" root))
         ("renderer" "Yunge Reader renderer tests"
          "node" "--test"
          ,(expand-file-name
            (concat "native/yunge-reader/renderer-test/"
                    "yunge-reader-core.test.mjs")
            root)))))))

(defun yunge-test-runner--run-command (name program arguments)
  "Run required check NAME using PROGRAM with ARGUMENTS."
  (princ (format "\n==> %s\n" name))
  (if-let* ((executable (executable-find program)))
      (with-temp-buffer
        (let ((status
               (apply #'call-process executable nil (current-buffer) nil
                      arguments)))
          (princ (buffer-string))
          (if (and (integerp status) (zerop status))
              (progn
                (princ (format "%s passed\n" name))
                t)
            (princ (format "%s failed (exit %S)\n" name status))
            nil)))
    (princ (format "Required program is unavailable: %s\n" program))
    nil))

(defun yunge-test-runner--external (suite &optional module)
  "Run SUITE's external checks for optional MODULE and count failures."
  (let ((failures 0))
    (dolist (check (yunge-test-runner--external-checks suite module))
      (unless (yunge-test-runner--run-command
               (cadr check) (caddr check) (cdddr check))
        (cl-incf failures)))
    failures))

(defun yunge-test-runner--options (arguments)
  "Parse and validate check ARGUMENTS after the Emacs `--' marker."
  (let ((args arguments)
        suite module)
    (when args
      (unless (equal (pop args) "--")
        (error "Pass check options after --")))
    (while args
      (let ((option (pop args)))
        (unless (member option '("--suite" "--module"))
          (error "Unknown check option: %s" option))
        (unless (and args (not (string-prefix-p "--" (car args)))
                     (not (string-empty-p (car args))))
          (error "Missing value for %s" option))
        (pcase option
          ("--suite"
           (when suite (error "--suite may only be given once"))
           (setq suite (intern (pop args))))
          ("--module"
           (when module (error "--module may only be given once"))
           (setq module (pop args))))))
    (setq suite (or suite 'all))
    (unless (memq suite '(all ert static native renderer))
      (error "Unknown check suite: %s" suite))
    (when module
      (unless (member suite '(ert native))
        (error "--module requires --suite ert or native"))
      (unless (string-match-p "\\`[[:alnum:]-]+\\'" module)
        (error "Invalid module name: %s" module))
      (when (and (eq suite 'native)
                 (not (member module '("fangcun" "mcp" "reader"))))
        (error "Unknown native module: %s" module)))
    (cons suite module)))

(defun yunge-test-runner--ert-files (&optional module)
  "Return ERT files for optional MODULE file family."
  (let* ((directory (expand-file-name "test/" yunge-test-root))
         (files
          (directory-files
           directory t
           "\\`\\(?:fangcun\\(?:-.+\\)?\\|shuying\\(?:-.+\\)?\\|yunge-.+\\)-test\\.el\\'"))
         (selected
          (cl-remove-if-not
           (lambda (file)
             (let* ((name (file-name-base file))
                    (name (string-remove-suffix "-test" name))
                    (name (string-remove-prefix "yunge-" name)))
               (and (not (equal name "byte-compile"))
                    (or (null module)
                        (equal name module)
                        (string-prefix-p (concat module "-") name)))))
           files)))
    (when (null selected)
      (error "No ERT files match module: %s" module))
    selected))

(defun yunge-test-runner--ert (module)
  "Run ERT tests for MODULE and return failure count."
  (dolist (file (yunge-test-runner--ert-files module))
    (load file nil nil t))
  (let ((statistics (ert-run-tests-batch t)))
    (when (zerop (ert-stats-total statistics))
      (error "Selected ERT files defined no tests"))
    (princ (format "ERT: %d test(s), %d skipped, %d unexpected\n"
                   (ert-stats-total statistics)
                   (ert-stats-skipped statistics)
                   (ert-stats-completed-unexpected statistics)))
    (ert-stats-completed-unexpected statistics)))

(defun yunge-test-runner--static ()
  "Run byte compilation in a child Emacs and return failure count."
  (condition-case error-data
      (progn
        (yunge-test-run-emacs
         "-L" (expand-file-name "script" yunge-test-root)
         "-L" (expand-file-name "test" yunge-test-root)
         "-l" "yunge-test-helper"
         "-l" "yunge-byte-compile-test"
         "--eval" "(yunge-byte-compile-test--run)")
        (princ "Static byte compilation passed\n")
        0)
    (error
     (princ (format "Static byte compilation failed: %s\n"
                    (error-message-string error-data)))
     1)))

(defun yunge-test-runner--run (suite module)
  "Run SUITE for optional MODULE and return total failures."
  (let ((failures 0))
    (when (memq suite '(all ert))
      (cl-incf failures (yunge-test-runner--ert module)))
    (when (memq suite '(all static))
      (cl-incf failures (yunge-test-runner--static)))
    (when (memq suite '(all native))
      (cl-incf failures (yunge-test-runner--external 'native module)))
    (when (memq suite '(all renderer))
      (cl-incf failures (yunge-test-runner--external 'renderer)))
    (princ (format "\nRepository checks (%s%s): %d failure(s)\n"
                   suite (if module (format "/%s" module) "") failures))
    failures))

(when noninteractive
  (condition-case error-data
      (let* ((options (yunge-test-runner--options command-line-args-left))
             (suite (car options))
             (module (cdr options)))
        (setq command-line-args-left nil)
        (kill-emacs (if (zerop (yunge-test-runner--run suite module)) 0 1)))
    (error
     (princ (format "Repository checks failed: %s\n"
                    (error-message-string error-data)))
     (kill-emacs 1))))

;;; yunge-test-runner.el ends here
