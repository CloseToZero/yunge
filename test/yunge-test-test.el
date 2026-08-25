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
         xdg-config-home)
    (unwind-protect
        (progn
          (make-directory yunge-config-directory t)
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest arguments)
                       (setq command (plist-get arguments :command)
                             xdg-config-home (getenv "XDG_CONFIG_HOME"))
                       'yunge-test-process))
                    ((symbol-function 'display-buffer) #'ignore))
            (yunge-test))
          (should
           (equal
            command
            (list
             (expand-file-name invocation-name invocation-directory)
             "--batch" "-Q"
             "-L" (expand-file-name "test/" yunge-config-directory)
             "-l" "yunge-test-runner")))
          (should
           (equal
            xdg-config-home
            (yunge-var-subdirectory "test")))
          (with-current-buffer buffer
            (should (equal default-directory yunge-config-directory))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest yunge-test-external-checks-cover-native-and-renderer-suites ()
  (let* ((yunge-config-directory
          (file-name-as-directory
           (expand-file-name "source" temporary-file-directory)))
         (commands
          (mapcar #'cdr (yunge-test--external-checks))))
    (dolist
        (expected
         `(("cargo" "test" "--manifest-path"
            ,(concat yunge-config-directory
                     "native/fangcun-watch/Cargo.toml"))
           ("cargo" "test" "--manifest-path"
            ,(concat yunge-config-directory
                     "native/yunge-mcp/Cargo.toml"))
           ("cargo" "test" "--manifest-path"
            ,(concat yunge-config-directory
                     "native/yunge-reader/Cargo.toml"))
           ("node" "--check"
            ,(concat yunge-config-directory
                     "native/yunge-reader/renderer/yunge-reader.js"))
           ("node" "--test"
            ,(concat yunge-config-directory
                     "native/yunge-reader/renderer-test/"
                     "yunge-reader-core.test.mjs"))))
      (should (member expected commands)))))

(ert-deftest yunge-test-required-command-reports-its-result ()
  (dolist (case '((0 . t) (7 . nil)))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/bin/check"))
                  ((symbol-function 'call-process)
                   (lambda (_program _input destination _display
                            &rest arguments)
                     (should (equal arguments '("--verify")))
                     (with-current-buffer destination
                       (insert "check output\n"))
                     (car case))))
          (should (eq (yunge-test--run-command
                       "Native check" "check" '("--verify"))
                      (cdr case))))
        (should (string-match-p "check output" (buffer-string)))
        (should (string-match-p
                 (if (cdr case) "passed" "failed")
                 (buffer-string)))))))

(ert-deftest yunge-test-required-command-does-not-skip-a-missing-tool ()
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (cl-letf (((symbol-function 'executable-find) #'ignore))
        (should-not
         (yunge-test--run-command "Native check" "missing" nil)))
      (should
       (string-match-p
        "Required program is unavailable: missing"
        (buffer-string))))))

(provide 'yunge-test-test)

;;; yunge-test-test.el ends here
