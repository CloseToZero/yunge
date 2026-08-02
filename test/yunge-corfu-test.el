;;; yunge-corfu-test.el --- Corfu tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function completion-at-point "minibuffer")
(declare-function corfu-mode "corfu")
(declare-function evil-insert-state "evil-states")
(declare-function evil-local-mode "evil-core")

(defvar completion-in-region-mode)
(defvar corfu-map)
(defvar corfu-mode)

(yunge-test-deftest-lazy-load yunge-corfu
  (corfu))

(ert-deftest yunge-corfu-enables-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-corfu 'corfu
   :setup '(setq tab-always-indent nil)
   :before-ready
   '(when (or (featurep 'corfu)
              (bound-and-true-p global-corfu-mode))
      (error "Corfu was enabled before its package was ready"))
   :after-ready
   '(unless (and (bound-and-true-p global-corfu-mode)
                 (eq tab-always-indent 'complete)
                 (null corfu-preview-current))
      (error "Corfu configuration was not applied"))))

(ert-deftest yunge-corfu-preserves-editing-return ()
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
            (let ((next-command (key-binding (kbd "C-n")))
                  (return-command (key-binding (kbd "RET"))))
              (should-not (eq next-command 'corfu-next))
              (completion-at-point)
              (should completion-in-region-mode)
              (yunge-test-keys
               '(("C-n" . corfu-next)
                 ("C-p" . corfu-previous)
                 ("TAB" . corfu-complete)
                 ("<tab>" . corfu-complete)))
              (should (eq (key-binding (kbd "RET"))
                          return-command))
              (completion-in-region-mode -1)
              (should (eq (key-binding (kbd "C-n"))
                          next-command)))))
      (set-window-buffer window original-buffer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; yunge-corfu-test.el ends here
