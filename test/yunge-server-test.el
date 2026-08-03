;;; yunge-server-test.el --- Emacs server tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function yunge-server-start "yunge-server")

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

(ert-deftest yunge-server-starts-only-when-no-server-is-detected ()
  (require 'server)
  (require 'yunge-server)
  (let ((results '(t unknown nil))
        (starts 0))
    (cl-letf (((symbol-function 'server-running-p)
               (lambda (&optional _name)
                 (pop results)))
              ((symbol-function 'server-start)
               (lambda (&rest _arguments)
                 (cl-incf starts))))
      (dotimes (_ 3)
        (yunge-server-start)))
    (should (= starts 1))))

;;; yunge-server-test.el ends here
