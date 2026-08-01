;;; yunge-minibuffer-test.el --- Minibuffer tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-local-mode "evil-core")
(declare-function evil-normal-state "evil-states")

(defvar evil-local-mode)
(defvar evil-echo-state)
(defvar evil-state)

(yunge-test-deftest-lazy-load yunge-minibuffer
  (evil))

(ert-deftest yunge-minibuffer-preserves-prompt-actions ()
  (require 'yunge-minibuffer)
  (yunge-test-enable-evil)

  (let ((evil-position
         (seq-position minibuffer-setup-hook 'evil-initialize))
        (setup-position
         (seq-position minibuffer-setup-hook #'yunge-minibuffer--setup)))
    (should evil-position)
    (should setup-position)
    (should (< evil-position setup-position)))

  (yunge-test-with-evil-minibuffer
    (cl-labels
        ((verify (map expected)
           (use-local-map map)
           (let ((minibuffer-setup-hook
                  (seq-filter
                   (lambda (function)
                     (memq function
                           '(evil-initialize yunge-minibuffer--setup)))
                   minibuffer-setup-hook)))
             ;; Simulate a reused minibuffer whose local value was reset.
             (setq-local evil-echo-state t)
             (run-hooks 'minibuffer-setup-hook))

           (should evil-local-mode)
           (should-not evil-echo-state)
           (should (eq evil-state 'insert))
           (evil-normal-state)
           (yunge-minibuffer--setup)
           (yunge-test-evil-keys
            'insert
            '(("<escape>" . evil-normal-state)
              ("C-g" . abort-minibuffers)))
           (evil-normal-state)
           (yunge-test-evil-keys
            'normal
            '(("<escape>" . evil-force-normal-state)
              ("RET" . yunge-minibuffer--return)
              ("<return>" . yunge-minibuffer--return)
              ("C-g" . abort-minibuffers)))

           (yunge-test-assert-calls-interactively
            #'yunge-minibuffer--return expected ?\r)))
      (verify minibuffer-local-map 'exit-minibuffer)
      (verify minibuffer-local-must-match-map
              'minibuffer-complete-and-exit))))

;;; yunge-minibuffer-test.el ends here
