;;; yunge-vertico-test.el --- Vertico tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-add-package-path
 'compat 'elpaca 'evil 'goto-chg 'vertico 'which-key)

(require 'elpaca-autoloads)

(declare-function evil-local-mode "evil-core")
(declare-function evil-normal-state "evil-states")
(declare-function vertico--advice "vertico")

(defvar evil-local-mode)
(defvar evil-state)
(defvar vertico-count)
(defvar vertico-map)

(yunge-test-deftest-lazy-load yunge-vertico
  (vertico))

(ert-deftest yunge-vertico-enables-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-vertico 'vertico
   :dependencies '(compat)
   :before-ready
   '(when (or (featurep 'vertico)
              (bound-and-true-p vertico-mode))
      (error "Vertico was enabled before its Elpaca body ran"))
   :after-ready
   '(unless (and (featurep 'vertico) vertico-mode)
      (error "Vertico was not enabled after package readiness"))))

(ert-deftest yunge-vertico-moves-by-half-pages ()
  (require 'yunge-vertico)
  (let ((vertico-count 10)
        movement)
    (cl-letf (((symbol-function 'vertico-next)
               (lambda (count) (setq movement count))))
      (yunge-vertico-next-half-page 1)
      (should (= movement 5)))
    (cl-letf (((symbol-function 'vertico-previous)
               (lambda (count) (setq movement count))))
      (yunge-vertico-previous-half-page 1)
      (should (= movement 5)))))

(ert-deftest yunge-vertico-integrates-with-minibuffer ()
  (require 'yunge-minibuffer)
  (yunge-test-enable-evil)
  (require 'vertico-autoloads)
  (yunge-test-load-package-config 'yunge-vertico)
  (require 'which-key)

  (let (setup-hook)
    ;; Batch Emacs reads stdin instead of opening a live minibuffer.  Capture
    ;; the hook assembled by Vertico's real completion advice and run it in
    ;; the minibuffer buffer.
    (vertico--advice
     (lambda ()
       (setq setup-hook (copy-sequence minibuffer-setup-hook))))

    (yunge-test-with-evil-minibuffer
      (use-local-map minibuffer-local-completion-map)
      (let ((minibuffer-setup-hook setup-hook))
        (run-hooks 'minibuffer-setup-hook))

      (should (eq (current-local-map) vertico-map))
      (yunge-test-evil-keys
       'insert
       '(("C-n" . vertico-next)
         ("C-p" . vertico-previous)
         ("TAB" . vertico-insert)
         ("<tab>" . vertico-insert)
         ("RET" . vertico-exit)))

      (evil-normal-state)
      (yunge-test-evil-keys
       'normal
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

      (yunge-test-assert-calls-interactively
       #'yunge-minibuffer--return 'vertico-exit ?\r)
      (yunge-test-which-key-prefix
       "g" '(("g" vertico-first "first candidate or prompt"))))))

;;; yunge-vertico-test.el ends here
