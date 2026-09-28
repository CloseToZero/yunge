;;; shuying-setup-test.el --- Shuying setup tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'shuying-setup)

(ert-deftest shuying-setup-rejects-unsupported-platforms ()
  (let ((system-type 'gnu/linux))
    (should-error (shuying-setup) :type 'user-error)))

(ert-deftest shuying-setup-loads-without-yunge-state ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'shuying-setup)
       (when (featurep 'yunge-state)
         (error "Shuying setup loaded Yunge state"))
       (unless (file-readable-p shuying-setup--windows-script)
         (error "Shuying setup did not find its bundled script"))))))

(ert-deftest shuying-setup-windows-script-passes-behavior-contracts ()
  (skip-unless (eq system-type 'windows-nt))
  (let ((powershell (shuying-setup--powershell))
        (script shuying-setup--windows-script)
        (contract
         (expand-file-name
          "test/shuying-setup-windows-integration.ps1"
          yunge-config-directory))
        (coding-system-for-read 'utf-8-unix))
    (should powershell)
    (with-temp-buffer
      (let ((status
             (call-process
              powershell nil (current-buffer) nil
              "-NoLogo" "-NoProfile" "-NonInteractive"
              "-ExecutionPolicy" "Bypass"
              "-File" contract "-SetupScript" script)))
        (unless (and (integerp status) (zerop status))
          (ert-fail
           (format "Windows setup behavior test failed (%S):\n%s"
                   status (buffer-string))))
        (should
         (string-match-p
          "Windows setup behavior tests passed: 中文"
          (buffer-string)))))))

(ert-deftest shuying-setup-starts-and-finishes-in-its-own-state-directory ()
  (let* ((root (make-temp-file "shuying-setup-start-" t))
         (shuying-state-directory (expand-file-name "state/" root))
         (shuying-setup--process nil)
         (system-type 'windows-nt)
         (process-environment (copy-sequence process-environment))
         (exec-path (copy-sequence exec-path))
         (installed (expand-file-name "bin" root))
         (status 'run)
         command sentinel buffer work-directory work-root)
    (unwind-protect
        (cl-letf (((symbol-function 'process-live-p) #'ignore)
                  ((symbol-function 'executable-find)
                   (lambda (name)
                     (and (equal name "pwsh.exe") "pwsh.exe")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest options)
                     (setq command (plist-get options :command)
                           sentinel (plist-get options :sentinel)
                           buffer (plist-get options :buffer))
                     'setup-process))
                  ((symbol-function 'process-status)
                   (lambda (_process) status))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 0))
                  ((symbol-function 'process-buffer)
                   (lambda (_process) buffer))
                  ((symbol-function 'process-get) #'ignore)
                  ((symbol-function 'process-put) #'ignore)
                  ((symbol-function 'display-buffer) #'ignore))
          (shuying-setup)
          (setq work-root (cadr (member "-WorkRoot" command))
                work-directory (cadr (member "-WorkDirectory" command)))
          (should (member "-NonInteractive" command))
          (should (equal (cadr (member "-File" command))
                         shuying-setup--windows-script))
          (should (equal work-root
                         (expand-file-name "setup/" shuying-state-directory)))
          (should (file-in-directory-p work-directory work-root))
          (should (file-directory-p work-directory))
          (should (equal (cadr (member "-DownloadPage" command))
                         shuying-setup-windows-download-page))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "SHUYING_MIKTEX_BIN:"
                      (base64-encode-string
                       (encode-coding-string installed 'utf-8) t)
                      "\n")))
          ;; Cleanup still owns this run if a user changes the option meanwhile.
          (setq shuying-state-directory (expand-file-name "changed/" root)
                status 'exit)
          (funcall sentinel 'setup-process "finished\n")
          (should-not (file-exists-p work-directory))
          (should (member installed exec-path))
          (should (member (file-name-as-directory installed)
                          (parse-colon-path (getenv "PATH")))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest shuying-setup-cleans-work-after-process-start-fails ()
  (let* ((root (make-temp-file "shuying-setup-start-failure-" t))
         (shuying-state-directory (expand-file-name "state/" root))
         (shuying-setup--process nil)
         (system-type 'windows-nt)
         (setup-root (expand-file-name "setup/" shuying-state-directory))
         (buffer (get-buffer-create shuying-setup--log-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-live-p) #'ignore)
                  ((symbol-function 'executable-find)
                   (lambda (name)
                     (and (equal name "pwsh.exe") "pwsh.exe")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest _options) (error "could not start"))))
          (should-error (shuying-setup))
          (should-not (directory-files setup-root nil "\\`run-")))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest shuying-setup-cleans-only-owned-work-directories ()
  (let* ((root (make-temp-file "shuying-setup-cleanup-" t))
         (shuying-state-directory (file-name-as-directory root))
         (setup-root (shuying-setup--work-root))
         (owned (expand-file-name "run-owned" setup-root))
         (outside (expand-file-name "other" root)))
    (unwind-protect
        (progn
          (make-directory owned t)
          (make-directory outside t)
          (shuying-setup--cleanup owned setup-root)
          (should-not (file-exists-p owned))
          (shuying-setup--cleanup outside setup-root)
          (should (file-directory-p outside)))
      (delete-directory root t))))

(ert-deftest shuying-setup-retries-a-sharing-violation-without-blocking ()
  (let* ((root (make-temp-file "shuying-setup-retry-" t))
         (shuying-state-directory (file-name-as-directory root))
         (setup-root (shuying-setup--work-root))
         (directory
          (expand-file-name
           "run-locked" setup-root))
         (real-delete (symbol-function 'delete-directory))
         (attempts 0)
         scheduled)
    (unwind-protect
        (progn
          (make-directory directory t)
          (cl-letf (((symbol-function 'delete-directory)
                     (lambda (&rest arguments)
                       (cl-incf attempts)
                       (when (< attempts 3)
                         (signal 'file-error '("sharing violation")))
                       (apply real-delete arguments)))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest arguments)
                       (setq scheduled (cons function arguments))
                       'cleanup-timer)))
            (shuying-setup--cleanup directory setup-root)
            (should (= attempts 1))
            (should scheduled)
            (while scheduled
              (let ((callback scheduled))
                (setq scheduled nil)
                (apply (car callback) (cdr callback)))))
          (should-not (file-exists-p directory))
          (should-not scheduled))
      (delete-directory root t))))

(ert-deftest shuying-setup-bounds-sharing-violation-retries ()
  (let* ((root (make-temp-file "shuying-setup-retry-limit-" t))
         (shuying-state-directory (file-name-as-directory root))
         (setup-root (shuying-setup--work-root))
         (directory
          (expand-file-name
           "run-locked" setup-root))
         (attempts 0)
         scheduled
         warning)
    (unwind-protect
        (progn
          (make-directory directory t)
          (cl-letf (((symbol-function 'delete-directory)
                     (lambda (&rest _arguments)
                       (cl-incf attempts)
                       (signal 'file-error '("sharing violation"))))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest arguments)
                       (setq scheduled (cons function arguments))
                       'cleanup-timer))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &rest _arguments)
                       (setq warning message))))
            (shuying-setup--cleanup directory setup-root)
            (while scheduled
              (let ((callback scheduled))
                (setq scheduled nil)
                (apply (car callback) (cdr callback)))))
          (should (> attempts 1))
          (should (< attempts 100))
          (should (string-match-p "sharing violation" warning))
          (should-not scheduled))
      (delete-directory root t))))

(ert-deftest shuying-setup-keeps-the-log-and-cleans-work-after-failure ()
  (let* ((root (make-temp-file "shuying-setup-failure-" t))
         (shuying-state-directory (expand-file-name "state/" root))
         (shuying-setup--process nil)
         (system-type 'windows-nt)
         (status 'run)
         command sentinel buffer displayed warning)
    (unwind-protect
        (cl-letf (((symbol-function 'process-live-p) #'ignore)
                  ((symbol-function 'executable-find)
                   (lambda (name)
                     (and (equal name "pwsh.exe") "pwsh.exe")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest options)
                     (setq command (plist-get options :command)
                           sentinel (plist-get options :sentinel)
                           buffer (plist-get options :buffer))
                     'setup-process))
                  ((symbol-function 'process-status)
                   (lambda (_process) status))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 1))
                  ((symbol-function 'process-buffer)
                   (lambda (_process) buffer))
                  ((symbol-function 'process-get) #'ignore)
                  ((symbol-function 'process-put) #'ignore)
                  ((symbol-function 'display-buffer)
                   (lambda (candidate) (setq displayed candidate)))
                  ((symbol-function 'display-warning)
                   (lambda (_type message &rest _arguments)
                     (setq warning message))))
          (shuying-setup)
          (let ((work-directory (cadr (member "-WorkDirectory" command))))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "installer failed\n")))
            (setq displayed nil
                  status 'exit)
            (funcall sentinel 'setup-process "failed\n")
            (should-not (file-exists-p work-directory))
            (should (eq displayed buffer))
            (should (buffer-live-p buffer))
            (with-current-buffer buffer
              (should (string-match-p "installer failed" (buffer-string))))
            (should (string-match-p "exit 1" warning))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(provide 'shuying-setup-test)

;;; shuying-setup-test.el ends here
