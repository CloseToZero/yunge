;;; yunge-reader-native-lifecycle-test.el --- Helper lifetime tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-native)

(defmacro yunge-reader-native-lifecycle-test--with-helper (&rest body)
  "Run BODY with child processes that implement helper startup and shutdown."
  (declare (indent 0))
  `(let* ((yunge-reader-native--process nil)
          (yunge-reader-native--build-process nil)
          (yunge-reader-native--transport nil)
          (yunge-reader-native--client-count 0)
          (yunge-reader-native--idle-timer nil)
          (yunge-reader-native--force-stop-timer nil)
          (yunge-reader-native--restart-timer nil)
          (yunge-reader-native--restart-after-stop nil)
          (yunge-reader-native--build-after-stop nil)
          (yunge-reader-native--restart-count 0)
          (yunge-reader-native-idle-seconds nil)
          (yunge-reader-native-stop-timeout 0.1)
          (emacs (expand-file-name invocation-name invocation-directory))
          (spawn (symbol-function 'make-process))
          children warnings)
     (cl-letf (((symbol-function 'yunge-reader-native-program) (lambda () emacs))
               ((symbol-function 'yunge-reader-native-module-file) (lambda () emacs))
               ((symbol-function 'yunge-reader-native-pdfium-library) (lambda () emacs))
               ((symbol-function 'yunge-reader-native-build-id-file)
                (lambda () (expand-file-name "native/yunge-reader/source.sha256" yunge-test-root)))
               ((symbol-function 'make-process)
                (lambda (&rest arguments)
                  (let ((child
                         (apply spawn
                                (plist-put arguments :command
                                           (list emacs "--batch" "-Q" "--eval"
                                                 (yunge-reader-native-lifecycle-test--program))))))
                    (push child children)
                    child)))
               ((symbol-function 'display-warning)
                (lambda (_type message &rest _) (push message warnings))))
       (unwind-protect (progn ,@body)
         (ignore warnings)
         (yunge-reader-native-stop t)
         (dolist (child children)
           (when (process-live-p child) (delete-process child)))))))

(defun yunge-reader-native-lifecycle-test--program ()
  "Return Lisp for a helper which handshakes and exits on a shutdown request."
  (prin1-to-string
   `(progn
      (require 'json)
      (princ ,(concat
               (json-serialize
                `((kind . "ready") (protocol . 2)
                  (build-id . ,(yunge-reader-native--build-id)) (pdfium-api . "7881")
                  (capabilities . ["cache-maintenance" "epub-publications" "epub-renderer"
                                   "epub-resources" "lifecycle" "pdf-links" "pdf-outline"
                                   "pdf-render" "pdf-search" "pdf-text"])))
               "\n"))
      (condition-case nil
          (while t
            (let ((request (json-parse-string (read-from-minibuffer "") :object-type 'alist)))
              (when (equal (alist-get 'op request) "shutdown") (kill-emacs 0))))
        (end-of-file nil)))))

(defun yunge-reader-native-lifecycle-test--wait (predicate)
  "Wait briefly for observable helper PREDICATE to hold."
  (let ((deadline (+ (float-time) 3)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (should (funcall predicate)))

(defun yunge-reader-native-lifecycle-test--ready-p ()
  "Return whether the current helper has completed its handshake."
  (eq (plist-get (yunge-reader-native-status) :state) 'ready))

(ert-deftest yunge-reader-native-restarts-without-open-documents ()
  (yunge-reader-native-lifecycle-test--with-helper
    (let ((original (yunge-reader-native-start))
          (session (yunge-reader-native-current-session))
          failure)
      (yunge-reader-native-lifecycle-test--wait #'yunge-reader-native-lifecycle-test--ready-p)
      (yunge-reader-native-request "wait" nil (lambda (_value error) (setq failure error)))
      (yunge-reader-native-restart)
      (yunge-reader-native-lifecycle-test--wait
       (lambda () (and (not (process-live-p original))
                       (not (equal session (yunge-reader-native-current-session)))
                       (yunge-reader-native-lifecycle-test--ready-p))))
      (should (eq (car failure) 'yunge-reader-native-session-stopped))
      (should-not (yunge-reader-native-session-live-p session))
      (should-not warnings))))

(ert-deftest yunge-reader-native-stop-cancels-an-explicit-restart ()
  (yunge-reader-native-lifecycle-test--with-helper
    (yunge-reader-native-start)
    (yunge-reader-native-restart)
    (yunge-reader-native-stop t)
    (accept-process-output nil 0.15)
    (should-not (yunge-reader-native-live-p))
    (should-not (seq-some #'process-live-p children))))

(ert-deftest yunge-reader-native-recovers-once-after-a-crash ()
  (yunge-reader-native-lifecycle-test--with-helper
    (let ((session (yunge-reader-native-acquire))
          failure)
      (yunge-reader-native-request "wait" nil (lambda (_value error) (setq failure error)))
      (delete-process (yunge-reader-native-start))
      (yunge-reader-native-lifecycle-test--wait
       (lambda () (and (not (equal session (yunge-reader-native-current-session)))
                       (yunge-reader-native-lifecycle-test--ready-p))))
      (should (eq (car failure) 'yunge-reader-native-session-lost))
      (delete-process (yunge-reader-native-start))
      (accept-process-output nil 0.15)
      (should-not (yunge-reader-native-live-p))
      (should-not (seq-some #'process-live-p children))
      (should (string-match-p "stopped unexpectedly" (car warnings)))
      (yunge-reader-native-release))))

(ert-deftest yunge-reader-native-stop-cancels-queued-crash-recovery ()
  (yunge-reader-native-lifecycle-test--with-helper
    (yunge-reader-native-acquire)
    (delete-process (yunge-reader-native-start))
    (yunge-reader-native-stop)
    (accept-process-output nil 0.15)
    (should-not (yunge-reader-native-live-p))
    (should-not (seq-some #'process-live-p children))
    (yunge-reader-native-release)))

(ert-deftest yunge-reader-native-exit-cancels-pending-restarts ()
  (dolist (phase '(stopping queued))
    (yunge-reader-native-lifecycle-test--with-helper
      (let ((original (yunge-reader-native-start)))
        (yunge-reader-native-restart)
        (when (eq phase 'queued) (delete-process original))
        (let ((kill-emacs-hook '(yunge-reader-native--shutdown-for-emacs-exit)))
          (run-hooks 'kill-emacs-hook))
        (when (process-live-p original) (delete-process original))
        (accept-process-output nil 0.15)
        (should-not (yunge-reader-native-live-p))
        (should-not (seq-some #'process-live-p children))))))

;;; yunge-reader-native-lifecycle-test.el ends here
