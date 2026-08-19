;;; yunge-reader-graphical-smoke-test.el --- Tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(add-to-list 'load-path (expand-file-name "script" yunge-test-root))
(require 'yunge-reader-graphical-smoke)

(defun yunge-reader-graphical-smoke-test--context ()
  "Return an isolated graphical smoke test context."
  (yunge-reader-graphical-smoke-create
   (expand-file-name "script" yunge-test-root)
   "Graphical smoke test"
   "yunge-reader-graphical-test-"
   "YUNGE_READER_GRAPHICAL_TEST_LOG"))

(defun yunge-reader-graphical-smoke-test--emacs ()
  "Return the running Emacs executable."
  (expand-file-name invocation-name invocation-directory))

(ert-deftest yunge-reader-graphical-smoke-creates-and-cleans-contexts ()
  (let* ((process-environment (copy-sequence process-environment))
         (log-file (expand-file-name "graphical-smoke.log"
                                     temporary-file-directory))
         context
         temporary)
    (setenv "YUNGE_READER_GRAPHICAL_TEST_LOG" log-file)
    (unwind-protect
        (progn
          (setq context (yunge-reader-graphical-smoke-test--context)
                temporary
                (yunge-reader-graphical-smoke-context-temporary-root
                 context))
          (should (equal
                   (yunge-reader-graphical-smoke-context-root context)
                   (file-name-as-directory yunge-test-root)))
          (should (equal
                   (yunge-reader-graphical-smoke-context-manifest context)
                   (expand-file-name
                    "native/yunge-reader/Cargo.toml" yunge-test-root)))
          (should (equal
                   (yunge-reader-graphical-smoke-context-helper context)
                   (expand-file-name
                    (concat
                     "var/yunge-reader/cargo-target/release/yunge-reader"
                     (if (eq system-type 'windows-nt) ".exe" ""))
                    yunge-test-root)))
          (should (equal
                   (yunge-reader-graphical-smoke-context-module context)
                   (expand-file-name
                    (pcase system-type
                      ('windows-nt
                       (concat "var/yunge-reader/cargo-target/release/"
                               "yunge_reader_module.dll"))
                      ('darwin
                       (concat "var/yunge-reader/cargo-target/release/"
                               "libyunge_reader_module.dylib"))
                      (_
                       (concat "var/yunge-reader/cargo-target/release/"
                               "libyunge_reader_module.so")))
                    yunge-test-root)))
          (should (equal
                   (yunge-reader-graphical-smoke-context-log-file context)
                   log-file))
          (should (file-directory-p temporary))
          (yunge-reader-graphical-smoke-cleanup context)
          (should-not (file-exists-p temporary))
          (should-not
           (yunge-reader-graphical-smoke-context-temporary-root context))
          (yunge-reader-graphical-smoke-cleanup context))
      (when (and context
                 (yunge-reader-graphical-smoke-context-temporary-root
                  context))
        (yunge-reader-graphical-smoke-cleanup context)))))

(ert-deftest yunge-reader-graphical-smoke-captures-process-output ()
  (let* ((context (yunge-reader-graphical-smoke-test--context))
         (temporary
          (yunge-reader-graphical-smoke-context-temporary-root context))
         (log-file (expand-file-name "process.log" temporary))
         output)
    (setf (yunge-reader-graphical-smoke-context-log-file context)
          log-file)
    (unwind-protect
        (progn
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (yunge-reader-graphical-smoke-run-process
               context
               (yunge-reader-graphical-smoke-test--emacs)
               '("--batch" "-Q" "--eval"
                 "(progn (princ \"stdout-marker\\n\")
                         (message \"stderr-marker\"))")))
            (setq output (buffer-string)))
          (should (string-match-p "stdout-marker" output))
          (should (string-match-p "stderr-marker" output))
          (should-not (string-match-p "Process .* finished" output))
          (with-temp-buffer
            (insert-file-contents log-file)
            (should (equal output (buffer-string)))))
      (yunge-reader-graphical-smoke-cleanup context))))

(ert-deftest yunge-reader-graphical-smoke-reports-process-failures ()
  (let* ((context (yunge-reader-graphical-smoke-test--context))
         (temporary
          (yunge-reader-graphical-smoke-context-temporary-root context))
         (log-file (expand-file-name "failure.log" temporary))
         error-data)
    (setf (yunge-reader-graphical-smoke-context-log-file context)
          log-file)
    (unwind-protect
        (progn
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (setq error-data
                    (should-error
                     (yunge-reader-graphical-smoke-run-process
                      context
                      (yunge-reader-graphical-smoke-test--emacs)
                      '("--batch" "-Q" "--eval"
                        "(progn (princ \"failure-marker\\n\")
                                (kill-emacs 7))"))
                     :type 'error))))
          (should (string-match-p
                   "status 7" (error-message-string error-data)))
          (with-temp-buffer
            (insert-file-contents log-file)
            (should (string-match-p "failure-marker" (buffer-string)))
            (should-not
             (string-match-p "Process .* finished" (buffer-string)))))
      (yunge-reader-graphical-smoke-cleanup context))))

(ert-deftest yunge-reader-graphical-smoke-refuses-foreign-cleanup ()
  (let* ((context (yunge-reader-graphical-smoke-test--context))
         (temporary
          (yunge-reader-graphical-smoke-context-temporary-root context))
         (foreign
          (expand-file-name
           "nonexistent-graphical-smoke-cleanup-target" yunge-test-root)))
    (unwind-protect
        (progn
          (should-not (file-exists-p foreign))
          (setf
           (yunge-reader-graphical-smoke-context-temporary-root context)
           foreign)
          (should-error
           (yunge-reader-graphical-smoke-cleanup context)
           :type 'error)
          (should (equal
                   (yunge-reader-graphical-smoke-context-temporary-root
                    context)
                   foreign)))
      (setf
       (yunge-reader-graphical-smoke-context-temporary-root context)
       temporary)
      (yunge-reader-graphical-smoke-cleanup context))))

;;; yunge-reader-graphical-smoke-test.el ends here
