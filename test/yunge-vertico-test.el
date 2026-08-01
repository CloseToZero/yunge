;;; yunge-vertico-test.el --- Vertico tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-add-package-path
 'compat 'elpaca 'evil 'goto-chg 'vertico 'which-key)

(declare-function evil-insert-state "evil-states")
(declare-function evil-local-mode "evil-core")
(declare-function evil-normal-state "evil-states")

(defvar evil-local-mode)
(defvar evil-state)
(defvar vertico-count)
(defvar vertico-map)

(ert-deftest yunge-vertico-loads-lazily ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string '(defmacro elpaca (&rest _body) nil))
   "-L" (expand-file-name "lisp" yunge-test-root)
   "-l" "yunge-vertico"
   "--eval"
   (prin1-to-string
    '(when (featurep 'vertico)
       (error "yunge-vertico eagerly loaded Vertico")))))

(ert-deftest yunge-vertico-binds-keys ()
  (require 'yunge-minibuffer)
  (require 'yunge-vertico)
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'vertico)

  (let ((vertico-count 10)
        movement)
    (cl-letf (((symbol-function 'vertico-next)
               (lambda (count) (setq movement count))))
      (yunge-vertico-next-half-page 1)
      (should (= movement 5)))
    (cl-letf (((symbol-function 'vertico-previous)
               (lambda (count) (setq movement count))))
      (yunge-vertico-previous-half-page 1)
      (should (= movement 5))))

  (with-current-buffer (window-buffer (minibuffer-window))
    (let ((original-map (current-local-map)))
      (unwind-protect
          (progn
            (use-local-map vertico-map)
            (evil-local-mode 1)
            (yunge-minibuffer--setup)

            (evil-insert-state)
            (should (eq evil-state 'insert))
            (dolist (binding
                     '(("C-n" . vertico-next)
                       ("C-p" . vertico-previous)
                       ("TAB" . vertico-insert)
                       ("<tab>" . vertico-insert)
                       ("RET" . vertico-exit)))
              (yunge-test-key (car binding) (cdr binding)))

            (evil-normal-state)

            (dolist (binding
                     '(("j" . vertico-next)
                       ("k" . vertico-previous)
                       ("<down>" . vertico-next)
                       ("<up>" . vertico-previous)
                       ("gg" . vertico-first)
                       ("G" . vertico-last)
                       ("C-d" . yunge-vertico-next-half-page)
                       ("C-u" . yunge-vertico-previous-half-page)
                       ("C-f" . vertico-scroll-up)
                       ("C-b" . vertico-scroll-down)
                       ("TAB" . vertico-insert)
                       ("<tab>" . vertico-insert)
                       ("RET" . yunge-minibuffer--return)
                       ("C-g" . abort-minibuffers)
                       ("d" . evil-delete)))
              (yunge-test-key (car binding) (cdr binding)))

            (let (called)
              (cl-letf (((symbol-function 'call-interactively)
                         (lambda (command &rest _arguments)
                           (setq called command))))
                (yunge-minibuffer--return ?\r))
              (should (eq called 'vertico-exit)))

            (yunge-test-which-key-prefix
             "g" '(("g" vertico-first
                     "first candidate or prompt"))))
        (evil-local-mode -1)
        (use-local-map original-map)))))

;;; yunge-vertico-test.el ends here
