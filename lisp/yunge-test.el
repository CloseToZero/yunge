;;; yunge-test.el --- Test command -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'compile)
(require 'yunge-state)

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
(defun yunge-test (&optional suite module)
  "Run configuration checks in a clean Emacs process.
SUITE defaults to all.  MODULE selects an ERT file family or native crate."
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
    (let* ((process-environment (copy-sequence process-environment))
           (test-state-parent (yunge-var-subdirectory "test/runs"))
           (test-state-root
            (progn
              (make-directory test-state-parent t)
              (make-temp-file (expand-file-name "run-" test-state-parent)
                              t))))
      (make-directory (expand-file-name "xdg-config/emacs" test-state-root) t)
      (setenv "YUNGE_TEST_STATE_ROOT" test-state-root)
      (setenv "XDG_CONFIG_HOME"
              (expand-file-name "xdg-config" test-state-root))
      (setenv "XDG_CACHE_HOME"
              (expand-file-name "xdg-cache" test-state-root))
      (setenv "XDG_DATA_HOME"
              (expand-file-name "xdg-data" test-state-root))
      (make-process
       :name "yunge-test"
       :buffer buffer
       :command
       (append
        (list (expand-file-name invocation-name invocation-directory)
              "--batch" "-Q"
              "-L" test-directory
              "-l" "yunge-test-runner")
        (when (or suite module)
          (append (list "--")
                  (when suite (list "--suite" (symbol-name suite)))
                  (when module (list "--module" module)))))
       :noquery t
       :sentinel #'yunge-test--sentinel))
    (display-buffer buffer)))

(provide 'yunge-test)

;;; yunge-test.el ends here
