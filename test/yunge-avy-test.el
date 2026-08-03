;;; yunge-avy-test.el --- IME-friendly Avy tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar avy-all-windows)
(defvar avy-keys)
(defvar avy-single-candidate-jump)

(declare-function yunge-avy--query-regexp "yunge-avy" (text))

(defun yunge-avy-test--load-config ()
  "Load the Avy configuration synchronously for command tests."
  (require 'evil-autoloads)
  (yunge-test-load-package-config 'yunge-avy))

(yunge-test-deftest-lazy-load yunge-avy
  (avy pyim pyim-cregexp))

(ert-deftest yunge-avy-configures-after-package-is-ready ()
  (yunge-test-run-package-config
   'yunge-avy 'avy
   :before-ready
   '(when (keymap-lookup yunge-jump-map "j")
      (error "Avy key was bound before package readiness"))
   :after-ready
   '(progn
      (unless (eq (keymap-lookup yunge-jump-map "j")
                  'yunge-avy-jump-to-text)
        (error "Avy key was not bound after package readiness"))
      (when (featurep 'avy)
        (error "Avy was loaded by its configuration")))))

(ert-deftest yunge-avy-jump-to-text-uses-a-literal-ime-query ()
  (yunge-avy-test--load-config)
  (let (guarded regex)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _arguments)
                 (should (eq feature 'avy))
                 t))
              ((symbol-function 'avy-jump)
               (lambda (value &rest _arguments)
                 (setq regex value)))
              ((symbol-function 'yunge-input-source-call-with-ascii)
               (lambda (function)
                 (setq guarded t)
                 (funcall function))))
      (yunge-avy-jump-to-text "中.*文")
      (should guarded)
      (should (equal regex "中\\.\\*文")))))

(ert-deftest yunge-avy-jump-to-text-rejects-an-empty-query ()
  (yunge-avy-test--load-config)
  (should-error (yunge-avy-jump-to-text "") :type 'user-error))

(ert-deftest yunge-avy-pinyin-query-matches-full-and-abbreviated-input ()
  (yunge-avy-test--load-config)
  (dolist (query '("baoliu" "baol" "bl"))
    (let ((regexp (yunge-avy--query-regexp query)))
      (should (string-match-p regexp "保留"))
      (should (string-match-p regexp query)))))

(ert-deftest yunge-avy-jump-to-text-selects-visible-non-ascii-text ()
  (yunge-avy-test--load-config)
  (require 'avy)
  (let ((buffer (generate-new-buffer " *yunge-avy*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "中文 x 中文")
          (goto-char (point-min))
          (let ((avy-all-windows nil)
                (avy-keys '(?a ?s))
                (avy-single-candidate-jump nil)
                (unread-command-events '(?s)))
            (cl-letf
                (((symbol-function
                   'yunge-input-source-call-with-ascii)
                  (lambda (function) (funcall function))))
              (yunge-avy-jump-to-text "中文")))
          (should (= (point) 6)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-avy-pinyin-query-jumps-across-windows ()
  (yunge-avy-test--load-config)
  (require 'avy)
  (let ((origin (generate-new-buffer " *yunge-avy-origin*"))
        (target (generate-new-buffer " *yunge-avy-target*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer origin)
          (insert "source")
          (let ((target-window (split-window-right)))
            (with-current-buffer target
              (insert "保留"))
            (set-window-buffer target-window target)
            (let ((avy-all-windows t)
                  (avy-single-candidate-jump t))
              (cl-letf
                  (((symbol-function
                     'yunge-input-source-call-with-ascii)
                    (lambda (function) (funcall function))))
                (yunge-avy-jump-to-text "bl")))
            (should (eq (selected-window) target-window))
            (should (eq (current-buffer) target))
            (should (= (point) 1))))
      (dolist (buffer (list origin target))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

;;; yunge-avy-test.el ends here
