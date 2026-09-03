;;; yunge-slime-test.el --- Common Lisp tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-normal-state "evil-states")
(declare-function slime-inspector-mode "slime")
(declare-function slime-popup-buffer-mode "slime")
(declare-function slime-repl-mode "slime-repl")
(declare-function slime-setup "slime")
(declare-function slime-thread-control-mode "slime")
(declare-function slime-trace-dialog-mode "slime-trace-dialog")
(declare-function sldb-mode "slime")
(declare-function yunge-slime--default-implementation "yunge-slime")

(defvar slime-completion-at-point-functions)
(defvar slime-default-lisp)
(defvar slime-lisp-implementations)
(defvar slime-repl-history-file)
(defvar slime-repl-history-size)

(yunge-test-deftest-lazy-load yunge-slime
  (slime slime-fancy slime-repl))

(ert-deftest yunge-slime-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-slime 'slime
   :setup '(setq lisp-mode-hook nil
                 slime-completion-at-point-functions nil
                 slime-default-lisp nil
                 slime-lisp-implementations nil
                 slime-repl-history-file nil
                 slime-repl-history-size 200)
   :before-ready
   '(when (or slime-completion-at-point-functions
              (memq 'slime-lisp-mode-hook lisp-mode-hook)
              slime-default-lisp
              slime-lisp-implementations
              slime-repl-history-file
              (/= slime-repl-history-size 200))
      (error "SLIME was configured before its package became ready"))
   :after-ready
   '(unless
        (and (equal slime-completion-at-point-functions
                    '(yunge-slime-completion-at-point
                      slime-filename-completion))
             (eq slime-default-lisp
                 (yunge-slime--default-implementation))
             (equal slime-lisp-implementations
                    '((roswell ("ros" "run"))
                      (sbcl ("sbcl"))))
             (equal slime-repl-history-file
                    (expand-file-name "slime/repl-history.eld"
                                      yunge-var-directory))
             (= slime-repl-history-size 1000)
             (memq 'slime-lisp-mode-hook lisp-mode-hook)
             (autoloadp (symbol-function 'slime-lisp-mode-hook))
             (file-directory-p
              (expand-file-name "slime/" yunge-var-directory)))
      (error "SLIME configuration was not applied"))))

(ert-deftest yunge-slime-enables-existing-buffers-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-slime 'slime
   :setup
   '(progn
      (setq lisp-mode-hook nil
            yunge-slime-test-buffer
            (generate-new-buffer " *yunge-slime-existing*"))
      (with-current-buffer yunge-slime-test-buffer
        (lisp-mode)))
   :before-ready
   '(with-current-buffer yunge-slime-test-buffer
      (when (bound-and-true-p slime-mode)
        (error "SLIME was enabled before package readiness")))
   :after-ready
   '(with-current-buffer yunge-slime-test-buffer
      (unless (bound-and-true-p slime-mode)
        (error "Existing Lisp buffer did not enable SLIME")))))

(ert-deftest yunge-slime-selects-an-available-default-lisp ()
  (yunge-test-load-package-config 'yunge-slime)
  (let (available)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (and (member program available) program))))
      (setq available '("ros" "sbcl"))
      (should (eq (yunge-slime--default-implementation) 'roswell))
      (setq available '("sbcl"))
      (should (eq (yunge-slime--default-implementation) 'sbcl)))))

(ert-deftest yunge-slime-skips-remote-completion-while-disconnected ()
  (require 'slime-autoloads)
  (yunge-test-load-package-config 'yunge-slime)
  (require 'slime)
  (slime-setup)
  (with-temp-buffer
    (lisp-mode)
    (insert "defun")
    (should-not
     (run-hook-with-args-until-success
      'completion-at-point-functions))))

(ert-deftest yunge-slime-keeps-representative-evil-bindings ()
  (yunge-test-enable-evil)
  (require 'slime-autoloads)
  (yunge-test-load-package-config 'yunge-slime)
  (require 'slime)
  (require 'slime-repl)
  (require 'which-key)

  ;; Sample each configured surface instead of snapshotting every upstream
  ;; SLIME keymap and every declarative binding.
  (let ((lisp-mode-hook nil))
    (yunge-test-evil-normal-keys
     'lisp-mode
     '(("SPC m s" . slime)
       ("SPC m r" . yunge-slime-repl)
       ("gd" . slime-edit-definition))))

  (with-temp-buffer
    (slime-repl-mode)
    (yunge-test-evil-keys
     'insert
     '(("RET" . slime-repl-return)
       ("M-p" . slime-repl-previous-input)))
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("gd" . slime-edit-definition)
       ("SPC m c b" . slime-repl-clear-buffer))))

  ;; Mode-specific cleanup must win over the generic popup bindings.
  (with-temp-buffer
    (slime-inspector-mode)
    (slime-popup-buffer-mode 1)
    (yunge-test-evil-keys
     'normal
     '(("RET" . slime-inspector-operate-on-point)
       ("q" . slime-inspector-quit)
       ("K" . slime-inspector-describe))))

  (cl-letf (((symbol-function 'slime-connection) #'ignore))
    (yunge-test-evil-normal-keys
     'sldb-mode
     '(("RET" . sldb-default-action)
       ("q" . sldb-quit)
       ("gf" . sldb-show-source))))

  (yunge-test-evil-normal-keys
   'slime-thread-control-mode
   '(("q" . slime-quit-threads-buffer)
     ("x" . yunge-slime-thread-kill)))

  (slime-setup)
  (let (fetch-all)
    (cl-letf (((symbol-function 'slime-trace-dialog-fetch-traces)
               (lambda (&optional all) (setq fetch-all all))))
      (with-temp-buffer
        (slime-trace-dialog-mode)
        (yunge-test-evil-keys
         'normal
         '(("F" . yunge-slime-trace-dialog-fetch-all)
           ("q" . quit-window)))
        (call-interactively (key-binding (kbd "F")))))
    (should fetch-all)))

(ert-deftest yunge-slime-thread-kill-respects-visual-line-selection ()
  (yunge-test-enable-evil)
  (require 'slime-autoloads)
  (yunge-test-load-package-config 'yunge-slime)
  (require 'slime)

  (with-temp-buffer
    (slime-thread-control-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "one\n" 'thread-index 0)
              (propertize "two\n" 'thread-index 1)
              (propertize "three\n" 'thread-index 2)))
    (goto-char (point-min))
    (evil-normal-state)
    (let (request)
      (cl-letf (((symbol-function 'slime-eval)
                 (lambda (form &optional _package)
                   (setq request form)))
                ((symbol-function 'slime-update-threads-buffer) #'ignore))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd "V j x"))))
      (should
       (equal request
              '(cl:mapc 'swank:kill-nth-thread '(0 1)))))))

;;; yunge-slime-test.el ends here
