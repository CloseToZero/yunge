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
(defvar corfu-map)
(defvar corfu-mode)

(define-minor-mode yunge-corfu-test-input-mode
  "Simulate an input mode that owns RET while no completion is active.")

(yunge-test-deftest-lazy-load yunge-corfu
  (corfu))

(ert-deftest yunge-corfu-enables-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-corfu 'corfu
   :setup '(setq corfu-auto nil
                 corfu-auto-delay 0
                 corfu-auto-prefix 3
                 corfu-cycle nil
                 tab-always-indent nil
                 text-mode-ispell-word-completion t)
   :before-ready
   '(when (or (featurep 'corfu)
              (bound-and-true-p global-corfu-mode)
              text-mode-ispell-word-completion)
      (error "Corfu's early configuration was not applied"))
   :after-ready
   '(unless (and (bound-and-true-p global-corfu-mode)
                 corfu-auto
                 (= corfu-auto-delay 0.1)
                 (= corfu-auto-prefix 2)
                 corfu-cycle
                 (null tab-always-indent)
                 (null corfu-preview-current))
      (error "Corfu configuration was not applied"))))

(ert-deftest yunge-corfu-owns-popup-keys ()
  (yunge-test-enable-evil)
  (require 'corfu-autoloads)
  (yunge-test-load-package-config 'yunge-corfu)
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
            (insert "al")
            (setq-local completion-at-point-functions
                        (list
                         (lambda ()
                           (list (- (point) 2) (point)
                                 '("alpha" "alpine")))))
            (evil-local-mode 1)
            (evil-insert-state)
            (let ((next-command (key-binding (kbd "C-j")))
                  (previous-command (key-binding (kbd "C-k")))
                  (return-command (key-binding (kbd "RET"))))
              (should-not (eq next-command 'corfu-next))
              (should-not (eq previous-command 'corfu-previous))
              (completion-at-point)
              (should completion-in-region-mode)
              (yunge-test-keys
               '(("C-j" . corfu-next)
                 ("C-k" . corfu-previous)
                 ("TAB" . corfu-complete)
                 ("<tab>" . corfu-complete)
                 ("RET" . corfu-insert)
                 ("<return>" . corfu-insert)))
              (completion-in-region-mode -1)
              (should (eq (key-binding (kbd "C-j"))
                          next-command))
              (should (eq (key-binding (kbd "C-k"))
                          previous-command))
              (should (eq (key-binding (kbd "RET"))
                          return-command)))))
      (set-window-buffer window original-buffer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-corfu-accepts-before-a-surrounding-input-mode ()
  (yunge-test-enable-evil)
  (require 'corfu-autoloads)
  (yunge-test-load-package-config 'yunge-corfu)
  (evil-define-minor-mode-key 'insert 'yunge-corfu-test-input-mode
    (kbd "RET") #'ignore
    (kbd "<return>") #'ignore)
  (let ((buffer (generate-new-buffer " *yunge-corfu-input-test*"))
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
                        (list
                         (lambda ()
                           (list (- (point) 2) (point)
                                 '("alpha" "alpine")))))
            (evil-local-mode 1)
            (evil-insert-state)
            (yunge-corfu-test-input-mode 1)
            (should (eq (key-binding (kbd "RET")) #'ignore))
            (completion-at-point)
            (should completion-in-region-mode)
            (yunge-test-keys
             '(("RET" . corfu-insert)
               ("<return>" . corfu-insert)))
            (completion-in-region-mode -1)
            (should (eq (key-binding (kbd "RET")) #'ignore))))
      (set-window-buffer window original-buffer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; yunge-corfu-test.el ends here
