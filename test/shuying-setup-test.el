;;; shuying-setup-test.el --- Shuying setup tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'shuying-setup)

(ert-deftest shuying-setup-rejects-untested-platforms ()
  (let ((system-type 'gnu/linux))
    (should-error (shuying-setup) :type 'user-error)))

(ert-deftest shuying-setup-windows-script-passes-behavior-contracts ()
  (skip-unless (eq system-type 'windows-nt))
  (let ((powershell (shuying-setup--powershell))
        (script (shuying-setup--script))
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

(ert-deftest shuying-setup-windows-starts-an-explicit-process ()
  (let ((shuying-setup--process nil)
        arguments
        process-property
        (buffer (get-buffer-create shuying-setup--log-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-live-p) #'ignore)
                  ((symbol-function 'shuying-setup--powershell)
                   (lambda () "C:/Windows/powershell.exe"))
                  ((symbol-function 'shuying-setup--script)
                   (lambda () "C:/config/script/setup.ps1"))
                  ((symbol-function 'file-readable-p)
                   (lambda (_file) t))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'shuying-setup--work-directory)
                   (lambda () "C:/temp/shuying-run"))
                  ((symbol-function 'make-process)
                   (lambda (&rest plist)
                     (setq arguments plist)
                     'setup-process))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'run))
                  ((symbol-function 'process-get)
                   (lambda (_process _key) nil))
                  ((symbol-function 'process-put)
                   (lambda (process key value)
                     (setq process-property (list process key value))))
                  ((symbol-function 'display-buffer) #'ignore))
          (should (eq (shuying-setup--windows) 'setup-process))
          (should (eq shuying-setup--process 'setup-process))
          (should
           (equal
            (plist-get arguments :command)
            `("C:/Windows/powershell.exe"
              "-NoLogo" "-NoProfile" "-NonInteractive"
              "-ExecutionPolicy" "Bypass"
              "-File" "C:/config/script/setup.ps1"
              "-WorkDirectory" "C:/temp/shuying-run"
              "-WorkRoot"
              ,(yunge-var-subdirectory "shuying/setup")
              "-DownloadPage" "https://miktex.org/download")))
          (should
           (equal process-property
                  '(setup-process shuying-setup-work-directory
                                  "C:/temp/shuying-run"))))
      (kill-buffer buffer))))

(ert-deftest shuying-setup-windows-cleans-up-process-start-failure ()
  (let ((shuying-setup--process nil)
        cleaned
        (buffer (get-buffer-create shuying-setup--log-buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'process-live-p) #'ignore)
                  ((symbol-function 'shuying-setup--powershell)
                   (lambda () "powershell.exe"))
                  ((symbol-function 'shuying-setup--script)
                   (lambda () "setup.ps1"))
                  ((symbol-function 'file-readable-p)
                   (lambda (_file) t))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'shuying-setup--work-directory)
                   (lambda () "C:/temp/failed-run"))
                  ((symbol-function 'make-process)
                   (lambda (&rest _plist) (error "could not start")))
                  ((symbol-function 'shuying-setup--cleanup)
                   (lambda (directory) (setq cleaned directory))))
          (should-error (shuying-setup--windows))
          (should (equal cleaned "C:/temp/failed-run")))
      (kill-buffer buffer))))

(ert-deftest shuying-setup-cleans-only-owned-work-directories ()
  (let* ((root (make-temp-file "shuying-setup-cleanup-" t))
         (yunge-var-directory (file-name-as-directory root))
         (setup-root (yunge-var-subdirectory "shuying/setup"))
         (owned (expand-file-name "run-owned" setup-root))
         (outside (expand-file-name "other" root)))
    (unwind-protect
        (progn
          (make-directory owned t)
          (make-directory outside t)
          (should (shuying-setup--owned-work-directory-p owned))
          (should-not (shuying-setup--owned-work-directory-p outside))
          (shuying-setup--cleanup owned)
          (should-not (file-exists-p owned))
          (should (file-directory-p outside)))
      (delete-directory root t))))

(ert-deftest shuying-setup-retries-a-sharing-violation-without-blocking ()
  (let* ((root (make-temp-file "shuying-setup-retry-" t))
         (yunge-var-directory (file-name-as-directory root))
         (directory
          (expand-file-name
           "run-locked" (yunge-var-subdirectory "shuying/setup")))
         (attempts 0)
         scheduled)
    (unwind-protect
        (progn
          (make-directory directory t)
          (cl-letf (((symbol-function 'delete-directory)
                     (lambda (&rest _arguments)
                       (cl-incf attempts)
                       (when (< attempts 3)
                         (signal 'file-error '("sharing violation")))))
                    ((symbol-function 'run-at-time)
                     (lambda (_delay _repeat function &rest arguments)
                       (setq scheduled (cons function arguments))
                       'cleanup-timer)))
            (shuying-setup--cleanup directory)
            (should (= attempts 1))
            (should scheduled)
            (while scheduled
              (let ((callback scheduled))
                (setq scheduled nil)
                (apply (car callback) (cdr callback)))))
          (should (= attempts 3))
          (should-not scheduled))
      (delete-directory root t))))

(ert-deftest shuying-setup-bounds-sharing-violation-retries ()
  (let* ((root (make-temp-file "shuying-setup-retry-limit-" t))
         (yunge-var-directory (file-name-as-directory root))
         (directory
          (expand-file-name
           "run-locked" (yunge-var-subdirectory "shuying/setup")))
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
            (shuying-setup--cleanup directory)
            (while scheduled
              (let ((callback scheduled))
                (setq scheduled nil)
                (apply (car callback) (cdr callback)))))
          (should (> attempts 1))
          (should (< attempts 100))
          (should (string-match-p "sharing violation" warning))
          (should-not scheduled))
      (delete-directory root t))))

(ert-deftest shuying-setup-sentinel-refreshes-path-and-cleans-up ()
  (let ((buffer (get-buffer-create " *shuying-setup-sentinel-test*"))
        (shuying-setup--process 'setup-process)
        installed
        cleaned)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert
             "SHUYING_MIKTEX_BIN:QzovTWlLVGVYL21pa3RleC9iaW4veDY0\n"))
          (cl-letf (((symbol-function 'process-status)
                     (lambda (_process) 'exit))
                    ((symbol-function 'process-exit-status)
                     (lambda (_process) 0))
                    ((symbol-function 'process-buffer)
                     (lambda (_process) buffer))
                    ((symbol-function 'process-get)
                     (lambda (_process key)
                       (and (eq key 'shuying-setup-work-directory)
                            "C:/temp/setup-run")))
                    ((symbol-function 'process-put) #'ignore)
                    ((symbol-function 'shuying-setup--add-exec-directory)
                     (lambda (directory) (setq installed directory)))
                    ((symbol-function 'shuying-setup--cleanup)
                     (lambda (directory) (setq cleaned directory))))
            (shuying-setup--sentinel 'setup-process "finished\n"))
          (should-not shuying-setup--process)
          (should (equal installed "C:/MiKTeX/miktex/bin/x64"))
          (should (equal cleaned "C:/temp/setup-run")))
      (kill-buffer buffer))))

(ert-deftest shuying-setup-sentinel-preserves-log-on-failure ()
  (let ((buffer (get-buffer-create " *shuying-setup-log-test*"))
        (shuying-setup--process 'setup-process)
        displayed
        cleaned)
    (unwind-protect
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 1))
                  ((symbol-function 'process-buffer)
                   (lambda (_process) buffer))
                  ((symbol-function 'process-get)
                   (lambda (_process key)
                     (and (eq key 'shuying-setup-work-directory)
                          "C:/temp/setup-run")))
                  ((symbol-function 'process-put) #'ignore)
                  ((symbol-function 'display-buffer)
                   (lambda (candidate) (setq displayed candidate)))
                  ((symbol-function 'shuying-setup--cleanup)
                   (lambda (directory) (setq cleaned directory))))
          (shuying-setup--sentinel 'setup-process "failed\n")
          (should (eq displayed buffer))
          (should (equal cleaned "C:/temp/setup-run")))
      (kill-buffer buffer))))

(provide 'shuying-setup-test)

;;; shuying-setup-test.el ends here
