;;; yunge-server-test.el --- Emacs server tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function yunge-server-start "yunge-server")

(defvar server-process)

(yunge-test-deftest-lazy-load yunge-server
  (server))

(ert-deftest yunge-server-registers-only-for-interactive-startup ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'yunge-server)
       (when (memq #'yunge-server-start emacs-startup-hook)
         (error "Batch Emacs registered the server startup hook"))
       (let ((noninteractive nil)
             (emacs-startup-hook nil))
         (load "yunge-server" nil nil nil t)
         (unless (memq #'yunge-server-start emacs-startup-hook)
           (error "Interactive Emacs did not register the startup hook")))
       (when (featurep 'server)
         (error "Registering the startup hook loaded server.el"))))))

(ert-deftest yunge-server-starts-unless-current-process-is-live ()
  (require 'server)
  (require 'yunge-server)
  (let ((server-process 'live)
        (starts 0))
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (process)
                 (eq process 'live)))
              ((symbol-function 'server-start)
               (lambda (&rest _arguments)
                 (cl-incf starts))))
      (yunge-server-start)
      (setq server-process 'dead)
      (yunge-server-start)
      (setq server-process nil)
      (yunge-server-start))
    (should (= starts 2))))

;;; yunge-server-test.el ends here
