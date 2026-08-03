;;; yunge-input-source-test.el --- Input source guard tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-input-source)

(ert-deftest yunge-input-source-does-nothing-without-a-backend ()
  (cl-letf (((symbol-function 'yunge-input-source--detect-backend)
             #'ignore))
    (should (eq (yunge-input-source-call-with-ascii
                 (lambda () 'result))
                'result))))

(ert-deftest yunge-input-source-switches-and-restores ()
  (let ((current 'other)
        changes)
    (cl-letf
        (((symbol-function 'yunge-input-source--detect-backend)
          (lambda ()
            (yunge-input-source--make-backend
             :get (lambda () current)
             :set (lambda (source)
                    (setq current source)
                    (push source changes))
             :ascii 'english))))
      (should (eq (yunge-input-source-call-with-ascii
                   (lambda ()
                     (should (eq current 'english))
                     'result))
                  'result))
      (should (eq current 'other))
      (should (equal (nreverse changes) '(english other))))))

(ert-deftest yunge-input-source-restores-after-an-error ()
  (let ((current 'other))
    (cl-letf
        (((symbol-function 'yunge-input-source--detect-backend)
          (lambda ()
            (yunge-input-source--make-backend
             :get (lambda () current)
             :set (lambda (source) (setq current source))
             :ascii 'english))))
      (should-error
       (yunge-input-source-call-with-ascii
        (lambda () (error "stop"))))
      (should (eq current 'other)))))

(ert-deftest yunge-input-source-detects-the-windows-api ()
  (let ((system-type 'windows-nt))
    (cl-letf (((symbol-function 'w32-get-ime-open-status)
               (lambda () t))
              ((symbol-function 'w32-set-ime-open-status)
               #'ignore))
      (let ((backend (yunge-input-source--detect-backend)))
        (should backend)
        (should (eq (funcall (yunge-input-source--backend-get backend))
                    t))
        (should-not (yunge-input-source--backend-ascii backend))))))

(ert-deftest yunge-input-source-detects-the-mac-port-api ()
  (let ((system-type 'darwin)
        selected)
    (cl-letf (((symbol-function 'mac-input-source)
               (lambda () "com.apple.inputmethod.SCIM.ITABC"))
              ((symbol-function 'mac-select-input-source)
               (lambda (source) (setq selected source))))
      (let ((backend (yunge-input-source--detect-backend)))
        (should backend)
        (should
         (equal (funcall (yunge-input-source--backend-get backend))
                "com.apple.inputmethod.SCIM.ITABC"))
        (should
         (equal (yunge-input-source--backend-ascii backend)
                yunge-input-source-macos-ascii-source))
        (funcall (yunge-input-source--backend-set backend)
                 "com.apple.keylayout.ABC")
        (should (equal selected "com.apple.keylayout.ABC"))))))

(ert-deftest yunge-input-source-detects-macism-on-stock-macos ()
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'mac-input-source) nil)
              ((symbol-function 'mac-select-input-source) nil)
              ((symbol-function 'executable-find)
               (lambda (program)
                 (when (equal program "macism")
                   "/usr/local/bin/macism"))))
      (let ((backend (yunge-input-source--detect-backend)))
        (should backend)
        (should
         (equal (yunge-input-source--backend-ascii backend)
                yunge-input-source-macos-ascii-source))
        (cl-letf (((symbol-function
                    'yunge-input-source--program-output)
                   (lambda (program &rest arguments)
                     (should (equal program "/usr/local/bin/macism"))
                     (should-not arguments)
                     "com.apple.inputmethod.SCIM.ITABC")))
          (should
           (equal (funcall (yunge-input-source--backend-get backend))
                  "com.apple.inputmethod.SCIM.ITABC")))))))

(ert-deftest yunge-input-source-detects-fcitx5-on-linux ()
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (when (equal program "fcitx5-remote")
                   "/usr/bin/fcitx5-remote"))))
      (let ((backend (yunge-input-source--detect-backend)))
        (should backend)
        (should (= (yunge-input-source--backend-ascii backend) 1))
        (cl-letf (((symbol-function
                    'yunge-input-source--program-output)
                   (lambda (program &rest arguments)
                     (should (equal program "/usr/bin/fcitx5-remote"))
                     (should-not arguments)
                     "2")))
          (should (= (funcall (yunge-input-source--backend-get backend))
                     2)))))))

(ert-deftest yunge-input-source-detects-ibus-as-a-linux-fallback ()
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (when (equal program "ibus") "/usr/bin/ibus"))))
      (let ((backend (yunge-input-source--detect-backend)))
        (should backend)
        (should
         (equal (yunge-input-source--backend-ascii backend)
                yunge-input-source-ibus-ascii-source))
        (cl-letf (((symbol-function
                    'yunge-input-source--program-output)
                   (lambda (program &rest arguments)
                     (should (equal program "/usr/bin/ibus"))
                     (should (equal arguments '("engine")))
                     "libpinyin")))
          (should
           (equal (funcall (yunge-input-source--backend-get backend))
                  "libpinyin")))))))

(ert-deftest yunge-input-source-reads-fcitx-state-from-output ()
  (let ((backend (yunge-input-source--fcitx-backend "fcitx5-remote")))
    (cl-letf (((symbol-function 'yunge-input-source--program-output)
               (lambda (program &rest arguments)
                 (should (equal program "fcitx5-remote"))
                 (should-not arguments)
                 "2")))
      (should (= (funcall (yunge-input-source--backend-get backend)) 2)))))

;;; yunge-input-source-test.el ends here
