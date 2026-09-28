;;; yunge-corfu-test.el --- Corfu tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function completion-at-point "minibuffer")
(declare-function corfu-mode "corfu")
(declare-function evil-define-minor-mode-key "evil-core")
(declare-function evil-insert-state "evil-states")
(declare-function evil-local-mode "evil-core")

(defvar completion-in-region-mode)
(defvar corfu-mode)

(define-minor-mode yunge-corfu-test-input-mode
  "Simulate an input mode that owns RET while no completion is active.")

(defun yunge-corfu-test-return ()
  "Insert a marker through the surrounding input mode."
  (interactive)
  (insert "<return>"))

(yunge-test-deftest-lazy-load yunge-corfu
  (corfu))

(ert-deftest yunge-corfu-enables-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-corfu 'corfu
   :setup '(setq text-mode-ispell-word-completion t)
   :before-ready
   '(when (or (featurep 'corfu)
              (bound-and-true-p global-corfu-mode)
              text-mode-ispell-word-completion)
      (error "Corfu's early configuration was not applied"))
   :after-ready
   '(unless (bound-and-true-p global-corfu-mode)
      (error "Corfu was not enabled after package readiness"))))

(ert-deftest yunge-corfu-keeps-pcomplete-git-help-non-interactive ()
  (yunge-test-run-emacs
   "--eval" "(defmacro elpaca (&rest _body) nil)"
   "-l" "yunge-corfu"
   "--eval"
   (prin1-to-string
    '(progn
       (require 'pcmpl-git)
       (let ((vc-git-program "yunge-test-git")
             invoked)
         (cl-letf (((symbol-function 'call-process)
                    (lambda (&rest arguments)
                      (setq invoked arguments)
                      (insert "    --amend  amend previous commit\n")
                      0)))
           (unless
               (member
                "--amend"
                (pcomplete-from-help
                 '("yunge-test-git" "help" "commit")))
             (error "Git option completion was not produced")))
         (unless
             (equal invoked
                    '("yunge-test-git" nil t nil "commit" "-h"))
           (error "Git completion invoked interactive help: %S"
                  invoked)))))))

(ert-deftest yunge-corfu-popup-keys-follow-session-lifetime ()
  (yunge-test-enable-evil)
  (require 'corfu-autoloads)
  (yunge-test-load-package-config 'yunge-corfu)
  (evil-define-minor-mode-key 'insert 'yunge-corfu-test-input-mode
    (kbd "RET") #'yunge-corfu-test-return
    (kbd "<return>") #'yunge-corfu-test-return)
  (let ((buffer (generate-new-buffer " *yunge-corfu-test*"))
        (window (selected-window))
        (original-buffer (window-buffer)))
    (unwind-protect
        (progn
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (fundamental-mode)
            ;; Global Corfu intentionally skips noninteractive Emacs.
            (corfu-mode 1)
            (setq-local completion-at-point-functions
                        (list
                         (lambda ()
                           (list (- (point) 2) (point)
                                 '("alpha" "alpine")))))
            (evil-local-mode 1)
            (evil-insert-state)
            (yunge-corfu-test-input-mode 1)
            (let ((next-command (key-binding (kbd "C-j")))
                  (tab-command (key-binding (kbd "TAB"))))
              (cl-labels
                  ((start ()
                     (erase-buffer)
                     (insert "al")
                     (cl-letf (((symbol-function 'corfu--popup-support-p)
                                (lambda () t)))
                       (completion-at-point))
                     (should completion-in-region-mode))
                   (press (key)
                     (let ((this-command (key-binding (kbd key))))
                       (run-hooks 'pre-command-hook)
                       (call-interactively this-command))))
                (should-not (eq next-command 'corfu-next))
                (should (eq (key-binding (kbd "RET")) #'yunge-corfu-test-return))
                (start)
                (yunge-test-keys
                 '(("C-j" . corfu-next)
                   ("C-k" . corfu-previous)
                   ("TAB" . corfu-complete)
                   ("<tab>" . corfu-complete)))
                (press "C-j")
                (press "TAB")
                (should (equal (buffer-string) "alpine"))
                (should-not completion-in-region-mode)
                (should (eq (key-binding (kbd "C-j")) next-command))
                (should (eq (key-binding (kbd "TAB")) tab-command))

                (start)
                (press "C-j")
                (let ((before-return (buffer-string)))
                  (press "RET")
                  (should (equal (buffer-string)
                                 (concat before-return "<return>"))))
                ;; Completion may be dismissed while another buffer is current.
                (with-temp-buffer
                  (completion-in-region-mode -1))
                (should-not completion-in-region-mode)
                (should (eq (key-binding (kbd "C-j")) next-command))

                (start)
                (corfu-mode -1)
                (should-not completion-in-region-mode)
                (should (eq (key-binding (kbd "C-j")) next-command))
                (should (eq (key-binding (kbd "TAB")) tab-command))
                (should (eq (key-binding (kbd "RET"))
                            #'yunge-corfu-test-return))))))
      (when completion-in-region-mode
        (completion-in-region-mode -1))
      (set-window-buffer window original-buffer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-corfu-does-not-own-fallback-completion-keys ()
  (yunge-test-enable-evil)
  (require 'corfu-autoloads)
  (yunge-test-load-package-config 'yunge-corfu)
  (let ((buffer (generate-new-buffer " *yunge-corfu-fallback-test*"))
        (window (selected-window))
        (original-buffer (window-buffer)))
    (unwind-protect
        (progn
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (fundamental-mode)
            (corfu-mode 1)
            (insert "al")
            (setq-local completion-at-point-functions
                        (list (lambda ()
                                (list (- (point) 2) (point)
                                      '("algebra" "alpine" "alpha")))))
            (evil-local-mode 1)
            (evil-insert-state)
            (cl-letf (((symbol-function 'corfu--popup-support-p)
                       (lambda () nil)))
              (completion-at-point)
              (should completion-in-region-mode)
              (should-not (eq (key-binding (kbd "C-j")) 'corfu-next))
              (should-not (eq (key-binding (kbd "TAB")) 'corfu-complete)))))
      (when completion-in-region-mode
        (completion-in-region-mode -1))
      (set-window-buffer window original-buffer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; yunge-corfu-test.el ends here
