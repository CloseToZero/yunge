;;; yunge-test-test.el --- Test command tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-test)

(ert-deftest yunge-test-separates-source-and-isolated-state-roots ()
  (let* ((root (make-temp-file "yunge-test-command-" t))
         (yunge-config-directory
          (file-name-as-directory (expand-file-name "source" root)))
         (yunge-var-directory
          (file-name-as-directory (expand-file-name "state" root)))
         (buffer (get-buffer-create "*yunge-test*"))
         command
         xdg-config-home
         xdg-cache-home
         state-root)
    (unwind-protect
        (progn
          (make-directory yunge-config-directory t)
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest arguments)
                       (setq command (plist-get arguments :command)
                             xdg-config-home (getenv "XDG_CONFIG_HOME")
                             xdg-cache-home (getenv "XDG_CACHE_HOME")
                             state-root (getenv "YUNGE_TEST_STATE_ROOT"))
                       'yunge-test-process))
                    ((symbol-function 'display-buffer) #'ignore))
            (yunge-test))
          (should (member "-Q" command))
          (should (member "yunge-test-runner" command))
          (should (file-in-directory-p state-root yunge-var-directory))
          (should (file-in-directory-p xdg-config-home state-root))
          (should (file-in-directory-p xdg-cache-home state-root))
          (should-not (equal state-root yunge-var-directory))
          (with-current-buffer buffer
            (should (equal default-directory yunge-config-directory))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(defun yunge-test-test--run (&rest arguments)
  "Return the exit code and output of a runner with ARGUMENTS."
  (with-temp-buffer
    (let ((status
           (apply #'call-process
                  (expand-file-name invocation-name invocation-directory)
                  nil t nil "--batch" "-Q"
                  "-L" (expand-file-name "test" yunge-test-root)
                  "-l" "yunge-test-runner" "--" arguments)))
      (cons status (buffer-string)))))

(ert-deftest yunge-test-focuses-ert-on-one-file-family ()
  (pcase-let ((`(,status . ,output)
               (yunge-test-test--run "--suite" "ert"
                                     "--module" "theme")))
    (should (equal status 0))
    (should (string-match-p "yunge-theme-test.el" output))
    (should (string-match-p "passed.*yunge-theme-" output))
    (should-not (string-match-p "yunge-consult-test.el" output))
    (should-not (string-match-p "Fangcun Watch Rust tests" output))))

(ert-deftest yunge-test-rejects-unmatched-or-invalid-selection ()
  (dolist (arguments '(("--suite" "ert" "--module" "absent")
                       ("--suite" "unknown")
                       ("--suite" "static" "--module" "theme")
                       ("--suite" "native" "--module" "absent")))
    (pcase-let ((`(,status . ,output)
                 (apply #'yunge-test-test--run arguments)))
      (should-not (equal status 0))
      (should (string-match-p "Repository checks failed:" output)))))

(ert-deftest yunge-test-required-native-tool-fails-the-suite ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "PATH" "")
    (pcase-let ((`(,status . ,output)
                 (yunge-test-test--run "--suite" "native"
                                       "--module" "fangcun")))
      (should-not (equal status 0))
      (should (string-match-p "Required program is unavailable: cargo"
                              output)))))

(provide 'yunge-test-test)

;;; yunge-test-test.el ends here
