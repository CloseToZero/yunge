;;; yunge-test.el --- Test command -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'compile)

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
          (expand-file-name "test/" user-emacs-directory)))
    (when (process-live-p running)
      (user-error "Tests are already running"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq default-directory user-emacs-directory)
      (compilation-mode))
    (let ((process-environment (copy-sequence process-environment)))
      ;; Keep the clean process's default native-comp cache under `var'.
      (setenv "XDG_CONFIG_HOME"
              (expand-file-name "var/test/" user-emacs-directory))
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
