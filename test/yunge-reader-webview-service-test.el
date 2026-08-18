;;; yunge-reader-webview-service-test.el --- Tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-webview-service)

(ert-deftest yunge-reader-webview-service-notifies-inactive-stop ()
  (let ((yunge-reader-webview--process nil)
        (yunge-reader-webview-service-stopped-hook nil)
        (notifications 0))
    (add-hook 'yunge-reader-webview-service-stopped-hook
              (lambda () (cl-incf notifications)))
    (cl-letf (((symbol-function 'process-live-p) #'ignore))
      (should-not (yunge-reader-webview-stop)))
    (should (= notifications 1))))

(ert-deftest yunge-reader-webview-service-notifies-current-exit ()
  (let ((yunge-reader-webview--process 'current-webview)
        (yunge-reader-webview--transport nil)
        (yunge-reader-webview--force-stop-timer nil)
        (yunge-reader-webview-service-stopped-hook nil)
        (notifications 0)
        warning)
    (add-hook 'yunge-reader-webview-service-stopped-hook
              (lambda () (cl-incf notifications)))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_process) 'exit))
              ((symbol-function 'process-get)
               (lambda (_process _property) nil))
              ((symbol-function 'display-warning)
               (lambda (&rest arguments)
                 (setq warning arguments))))
      (yunge-reader-webview--sentinel 'current-webview "finished"))
    (should-not yunge-reader-webview--process)
    (should (= notifications 1))
    (should (equal (car warning) 'yunge-reader))))

(ert-deftest yunge-reader-webview-service-ignores-obsolete-exit ()
  (let ((yunge-reader-webview--process 'current-webview)
        (yunge-reader-webview-service-stopped-hook nil)
        (notifications 0))
    (add-hook 'yunge-reader-webview-service-stopped-hook
              (lambda () (cl-incf notifications)))
    (cl-letf (((symbol-function 'process-status)
               (lambda (_process) 'exit)))
      (yunge-reader-webview--sentinel 'obsolete-webview "finished"))
    (should (eq yunge-reader-webview--process 'current-webview))
    (should (zerop notifications))))

(ert-deftest yunge-reader-webview-service-waits-for-graceful-exit ()
  (let ((yunge-reader-webview--process 'current-webview)
        (yunge-reader-webview--force-stop-timer nil)
        (yunge-reader-webview-service-stopped-hook nil)
        (notifications 0)
        intentional
        request
        timer)
    (add-hook 'yunge-reader-webview-service-stopped-hook
              (lambda () (cl-incf notifications)))
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (_process) t))
              ((symbol-function 'process-put)
               (lambda (_process property value)
                 (when (eq property
                           'yunge-reader-webview-intentional-stop)
                   (setq intentional value))))
              ((symbol-function 'yunge-reader-webview--request)
               (lambda (operation parameters _complete)
                 (setq request (list operation parameters))))
              ((symbol-function 'run-at-time)
               (lambda (&rest arguments)
                 (setq timer arguments)
                 'force-stop-timer)))
      (should
       (eq (yunge-reader-webview-stop) 'current-webview)))
    (should intentional)
    (should (equal request '("shutdown" nil)))
    (should timer)
    (should (zerop notifications))))

(provide 'yunge-reader-webview-service-test)

;;; yunge-reader-webview-service-test.el ends here
