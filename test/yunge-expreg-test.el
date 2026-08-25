;;; yunge-expreg-test.el --- Expreg tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-get-command-property "evil-common"
                  (command property &optional default))

(yunge-test-deftest-lazy-load yunge-expreg
  (evil expreg))

(ert-deftest yunge-expreg-configures-after-package-is-ready ()
  (yunge-test-run-package-config
   'yunge-expreg 'expreg
   :setup '(progn
             (setq evil-want-keybinding nil)
             (require 'evil))
   :before-ready
   '(when (lookup-key
           (evil-get-auxiliary-keymap global-map 'normal t)
           (kbd "C-="))
      (error "Expreg key was bound before package readiness"))
   :after-ready
   '(progn
      (unless (eq (lookup-key
                   (evil-get-auxiliary-keymap global-map 'normal t)
                   (kbd "C-="))
                  'yunge-expreg-expand)
        (error "Expreg key was not bound after package readiness"))
      (when (featurep 'expreg)
        (error "Expreg was loaded by its configuration")))))

(ert-deftest yunge-expreg-expands-and-contracts-an-evil-selection ()
  (yunge-test-enable-evil)
  (require 'expreg-autoloads)
  (yunge-test-load-package-config 'yunge-expreg)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar baz))")
    (goto-char 9)
    (evil-local-mode 1)
    (evil-normal-state)
    (yunge-test-keys
     '(("C-=" . yunge-expreg-expand)
       ("C--" . yunge-expreg-contract)))
    (yunge-expreg-expand)
    (should (eq evil-state 'visual))
    (should (equal (buffer-substring-no-properties
                    (region-beginning) (region-end))
                   "bar"))
    (yunge-expreg-expand)
    (should (equal (buffer-substring-no-properties
                    (region-beginning) (region-end))
                   "bar baz"))
    (yunge-expreg-contract)
    (should (equal (buffer-substring-no-properties
                    (region-beginning) (region-end))
                   "bar"))
    (yunge-test-keys
     '(("C-=" . yunge-expreg-expand)
       ("C--" . yunge-expreg-contract)))
    (dolist (command '(yunge-expreg-expand yunge-expreg-contract))
      (should (evil-get-command-property command :keep-visual)))))

;;; yunge-expreg-test.el ends here
