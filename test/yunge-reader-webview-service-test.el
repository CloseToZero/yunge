;;; yunge-reader-webview-service-test.el --- WebView lifecycle tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-webview-service)

(defmacro yunge-reader-webview-service-test--with-module (&rest body)
  "Run BODY with real pipes and a test implementation of the native module API."
  (declare (indent 0))
  `(let ((system-type 'windows-nt)
         (yunge-reader-webview--process nil)
         (yunge-reader-webview--transport nil)
         (yunge-reader-webview--force-stop-timer nil)
         (yunge-reader-webview-stop-timeout 0.02)
         (yunge-reader-webview-service-stopped-hook nil)
         (module-file (make-temp-file "yunge-webview-module-"))
         (start-error nil) (stop-error nil) (send-error nil) (reply nil)
         pipe running shutdown stopped warnings)
     (cl-letf (((symbol-function 'yunge-reader-native-module-file) (lambda () module-file))
               ((symbol-function 'yunge-reader-native-build-id-file)
                (lambda () (expand-file-name "native/yunge-reader/source.sha256" yunge-test-root)))
               ((symbol-function 'yunge-reader-module-start)
                (lambda (process)
                  (setq pipe process running t)
                  (when (eq start-error 'error) (error "Module startup failed"))
                  (not (eq start-error 'unattached))))
               ((symbol-function 'yunge-reader-module-running-p)
                (lambda ()
                  (when stop-error (error "Module state unavailable"))
                  running))
               ((symbol-function 'yunge-reader-module-pump) #'ignore)
               ((symbol-function 'yunge-reader-module-stop)
                (lambda ()
                  (when stop-error (error "Module cleanup failed"))
                  (setq running nil)))
               ((symbol-function 'yunge-reader-module-request)
                (lambda (line)
                  (when send-error (error "Module send failed"))
                  (let ((request (json-parse-string line :object-type 'alist)))
                    (when (equal (alist-get 'op request) "shutdown")
                      (setq shutdown t)
                      (when reply
                        (yunge-reader-webview-service-test--receive
                         pipe `((id . ,(alist-get 'id request)) (ok . t)
                                (result . ((stopped . t))))))))))
               ((symbol-function 'display-warning)
                (lambda (_type message &rest _) (push message warnings))))
       (add-hook 'yunge-reader-webview-service-stopped-hook (lambda () (setq stopped t)))
       (unwind-protect (progn ,@body)
         (ignore shutdown)
         (when (process-live-p pipe)
           (ignore-errors (yunge-reader-webview-stop t)))
         (delete-file module-file)))))

(defun yunge-reader-webview-service-test--receive (process message)
  "Deliver a native MESSAGE through PROCESS's installed filter."
  (funcall (process-filter process) process (concat (json-serialize message) "\n")))

(defun yunge-reader-webview-service-test--ready (process)
  "Complete PROCESS's native module handshake."
  (yunge-reader-webview-service-test--receive
   process
   `((kind . "webview-ready") (protocol . 2)
     (build-id . ,(yunge-reader-native--build-id))
     (platform . "windows") (engine . "webview2") (available . t)
     (accelerators . ,(vconcat yunge-reader-webview--accelerators))
     (capabilities
      . ["view-appearance" "view-bounds" "view-clear-selection" "view-create"
         "view-destroy" "view-events" "view-focus" "view-focus-parent" "view-info"
         "view-navigate" "view-open-publication" "view-search" "view-search-result"
         "view-current-selection" "view-selection-text" "view-set-selection"
         "view-scroll-bars" "view-status" "view-style" "view-visible" "view-zoom"]))))

(defun yunge-reader-webview-service-test--wait-for-exit (process)
  "Wait briefly for PROCESS to close through its shutdown deadline."
  (let ((deadline (+ (float-time) 2)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (should-not (process-live-p process)))

(ert-deftest yunge-reader-webview-service-recovers-from-startup-failure ()
  (dolist (failure '(error unattached))
    (yunge-reader-webview-service-test--with-module
      (setq start-error failure)
      (should-error (yunge-reader-webview-start))
      (should-not (process-live-p pipe))
      (should-not running)
      (should stopped)
      (should-not warnings)
      (setq start-error nil)
      (should (process-live-p (yunge-reader-webview-start))))))

(ert-deftest yunge-reader-webview-service-notifies-inactive-stop ()
  (yunge-reader-webview-service-test--with-module
    (should-not (yunge-reader-webview-stop))
    (should stopped)))

(ert-deftest yunge-reader-webview-service-closes-pipe-when-module-stop-fails ()
  (yunge-reader-webview-service-test--with-module
    (let (completed)
      (yunge-reader-webview--request
       "view-info" nil (lambda (value failure) (push (list value failure) completed)))
      (setq stop-error t)
      (should-error (yunge-reader-webview-stop t))
      (should-not (process-live-p pipe))
      (should stopped)
      (should (= (length completed) 1))
      (should (cadar completed))
      (should-not warnings))))

(ert-deftest yunge-reader-webview-service-forces-exit-without-a-handshake ()
  (yunge-reader-webview-service-test--with-module
    (let (failure)
      (yunge-reader-webview--request "view-info" nil (lambda (_value error) (setq failure error)))
      (yunge-reader-webview-stop)
      (yunge-reader-webview-service-test--wait-for-exit pipe)
      (should failure)
      (should stopped)
      (should-not running)
      (should-not warnings))))

(ert-deftest yunge-reader-webview-service-handles-synchronous-shutdown ()
  (yunge-reader-webview-service-test--with-module
    (yunge-reader-webview-service-test--ready (yunge-reader-webview-start))
    (setq reply t)
    (yunge-reader-webview-stop)
    (should shutdown)
    (should-not (process-live-p pipe))
    (should stopped)
    (should-not warnings)
    (let ((replacement (yunge-reader-webview-start)))
      (accept-process-output nil 0.05)
      (should (process-live-p replacement)))))

(ert-deftest yunge-reader-webview-service-cleans-up-after-shutdown-send-fails ()
  (yunge-reader-webview-service-test--with-module
    (yunge-reader-webview-service-test--ready (yunge-reader-webview-start))
    (let (failure)
      (yunge-reader-webview--request "view-info" nil (lambda (_value error) (setq failure error)))
      (setq send-error t)
      (should-error (yunge-reader-webview-stop))
      (should-not (process-live-p pipe))
      (should failure)
      (should stopped)
      (should-not warnings))))

(ert-deftest yunge-reader-webview-service-reports-unexpected-pipe-exit ()
  (yunge-reader-webview-service-test--with-module
    (let (failure)
      (yunge-reader-webview--request "view-info" nil (lambda (_value error) (setq failure error)))
      (delete-process pipe)
      (should failure)
      (should stopped)
      (should-not running)
      (should (string-match-p "stopped unexpectedly" (car warnings)))
      (should (process-live-p (yunge-reader-webview-start))))))

;;; yunge-reader-webview-service-test.el ends here
