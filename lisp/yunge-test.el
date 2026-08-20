;;; yunge-test.el --- Test command -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'compile)
(require 'subr-x)
(require 'yunge-state)

(defvar yunge-test-external-checks-running-separately nil
  "Non-nil when the batch runner executes external checks itself.")

(defun yunge-test--external-checks ()
  "Return required native and renderer test commands."
  (let ((root yunge-config-directory))
    `(("Fangcun Watch Rust tests"
       "cargo" "test" "--manifest-path"
       ,(expand-file-name "native/fangcun-watch/Cargo.toml" root))
      ("Yunge MCP Rust tests"
       "cargo" "test" "--manifest-path"
       ,(expand-file-name "native/yunge-mcp/Cargo.toml" root))
      ("Yunge Reader Rust tests"
       "cargo" "test" "--manifest-path"
       ,(expand-file-name "native/yunge-reader/Cargo.toml" root))
      ("Yunge Reader renderer syntax"
       "node" "--check"
       ,(expand-file-name
         "native/yunge-reader/renderer/yunge-reader.js" root))
      ("Yunge Reader renderer tests"
       "node" "--test"
       ,(expand-file-name
         (concat "native/yunge-reader/renderer-test/"
                 "yunge-reader-core.test.mjs")
         root)))))

(defun yunge-test--run-command (name program arguments)
  "Run required check NAME using PROGRAM with ARGUMENTS.
Return non-nil on success and print its output to `standard-output'."
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

(defun yunge-test--run-external-checks ()
  "Run every required external check and return the failure count."
  (let ((failures 0))
    (dolist (check (yunge-test--external-checks))
      (unless (yunge-test--run-command
               (car check) (cadr check) (cddr check))
        (cl-incf failures)))
    failures))

(defun yunge-test--sentinel (process _event)
  "Report the result when test PROCESS exits."
  (when (memq (process-status process) '(exit signal))
    (let* ((status (process-exit-status process))
           (result (if (zerop status) "passed" "failed")))
      (with-current-buffer (process-buffer process)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "\nTests %s (exit %d)\n" result status))))
      (message "Tests %s" result))))

;;;###autoload
(defun yunge-test ()
  "Run all configuration tests in a clean Emacs process."
  (interactive)
  (let* ((buffer (get-buffer-create "*yunge-test*"))
         (running (get-buffer-process buffer))
         (test-directory
          (expand-file-name "test/" yunge-config-directory)))
    (when (process-live-p running)
      (user-error "Tests are already running"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq default-directory yunge-config-directory)
      (compilation-mode))
    (let ((process-environment (copy-sequence process-environment))
          (test-config-home
           (yunge-var-subdirectory "test")))
      ;; Keep the clean process's default native-comp cache under `var'.
      ;; Emacs only selects XDG_CONFIG_HOME when its emacs directory exists.
      (make-directory (expand-file-name "emacs/" test-config-home) t)
      (setenv "XDG_CONFIG_HOME" test-config-home)
      (make-process
       :name "yunge-test"
       :buffer buffer
       :command
       (list (expand-file-name invocation-name invocation-directory)
             "--batch" "-Q"
             "-L" test-directory
             "-l" "yunge-test-runner")
       :noquery t
       :sentinel #'yunge-test--sentinel))
    (display-buffer buffer)))

(provide 'yunge-test)

;;; yunge-test.el ends here
