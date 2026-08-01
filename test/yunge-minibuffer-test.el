;;; yunge-minibuffer-test.el --- Minibuffer tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-local-mode "evil-core")
(declare-function evil-normal-state "evil-states")

(defvar evil-local-mode)
(defvar evil-state)

(ert-deftest yunge-minibuffer-loads-lazily ()
  (yunge-test-assert-lazy-load
   'yunge-minibuffer '(evil)))

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

  (with-current-buffer (window-buffer (minibuffer-window))
    (let ((original-map (current-local-map)))
      (unwind-protect
          (cl-labels
              ((verify (map expected)
                 (use-local-map map)
                 (let ((minibuffer-setup-hook
                        (seq-filter
                         (lambda (function)
                           (memq function
                                 '(evil-initialize
                                   yunge-minibuffer--setup)))
                         minibuffer-setup-hook)))
                   (run-hooks 'minibuffer-setup-hook))

                 (should evil-local-mode)
                 (should (eq evil-state 'insert))
                 (yunge-test-key "<escape>" 'evil-normal-state)
                 (yunge-test-key "C-g" 'abort-minibuffers)

                 (evil-normal-state)
                 (yunge-test-key "<escape>" 'evil-force-normal-state)
                 (yunge-test-key "RET" 'yunge-minibuffer--return)
                 (yunge-test-key "<return>" 'yunge-minibuffer--return)
                 (yunge-test-key "C-g" 'abort-minibuffers)

                 (let (called)
                   (cl-letf (((symbol-function 'call-interactively)
                              (lambda (command &rest _arguments)
                                (setq called command))))
                     (yunge-minibuffer--return ?\r))
                   (should (eq called expected)))))
            (verify minibuffer-local-map 'exit-minibuffer)
            (verify minibuffer-local-must-match-map
                    'minibuffer-complete-and-exit))
        (evil-local-mode -1)
        (use-local-map original-map)))))

;;; yunge-minibuffer-test.el ends here
