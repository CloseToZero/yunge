;;; yunge-minibuffer-test.el --- Minibuffer tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar evil-echo-state)
(defvar evil-state)
(defvar last-command-event)

(yunge-test-deftest-lazy-load yunge-minibuffer
  (evil))

(ert-deftest yunge-minibuffer-return-follows-current-prompt ()
  (require 'yunge-minibuffer)
  (yunge-test-enable-evil)
  (yunge-test-with-evil-minibuffer
    (let ((original-input (minibuffer-contents-no-properties))
          (original-echo-state evil-echo-state)
          (echo-local (local-variable-p 'evil-echo-state)))
      (unwind-protect
          (progn
            (use-local-map minibuffer-local-map)
            (run-hooks 'minibuffer-setup-hook)
            (should (eq evil-state 'insert))
            (call-interactively (key-binding (kbd "<escape>")))
            (should (eq evil-state 'normal))
            ;; Batch Emacs has no recursive minibuffer edit. Simulate its
            ;; active loop so the real Return commands can exit.
            (cl-letf (((symbol-function 'minibuffer-innermost-command-loop-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'innermost-minibuffer-p)
                       (lambda (&rest _) t)))
              (cl-labels
                  ((press (key event)
                     (let ((last-command-event event))
                       (catch 'exit
                         (call-interactively (key-binding (kbd key)))
                         'not-exited))))
                (delete-minibuffer-contents)
                (insert "probe")
                (should-not (press "RET" ?\r))
                (should (equal (minibuffer-contents-no-properties) "probe"))

                (use-local-map minibuffer-local-must-match-map)
                (let ((minibuffer-completion-table '("alpha" "beta"))
                      (minibuffer-completion-predicate nil)
                      (minibuffer-completion-confirm nil))
                  (delete-minibuffer-contents)
                  (insert "zz")
                  (should (eq (press "<return>" 'return) 'not-exited))
                  (should (equal (minibuffer-contents-no-properties) "zz"))
                  (delete-minibuffer-contents)
                  (insert "alpha")
                  (should-not (press "<return>" 'return))
                  (should (equal (minibuffer-contents-no-properties) "alpha")))

                (let ((map (copy-keymap minibuffer-local-map)))
                  (define-key map (kbd "RET") #'delete-backward-char)
                  (use-local-map map)
                  (dolist (key '("RET" "<return>"))
                    (delete-minibuffer-contents)
                    (insert "probe")
                    (should (eq (press key (if (equal key "RET") ?\r 'return))
                                'not-exited))
                    (should (equal (minibuffer-contents-no-properties) "prob")))))))
        (delete-minibuffer-contents)
        (insert original-input)
        (if echo-local
            (setq-local evil-echo-state original-echo-state)
          (kill-local-variable 'evil-echo-state))))))

;;; yunge-minibuffer-test.el ends here
