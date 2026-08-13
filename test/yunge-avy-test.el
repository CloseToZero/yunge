;;; yunge-avy-test.el --- IME-friendly Avy tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar avy-action)
(defvar avy-all-windows)
(defvar avy-keys)
(defvar avy-last-candidates)
(defvar avy-single-candidate-jump)
(defvar yunge-avy-candidate-project-functions)

(declare-function yunge-avy--jump "yunge-avy" (regexp))
(declare-function yunge-avy--query-regexp "yunge-avy" (text))
(declare-function yunge-avy-make-projection
                  "yunge-avy" (&rest arguments))

(defun yunge-avy-test--overlay-projection (beginning end _window)
  "Project a test display overlay containing BEGINNING through END."
  (when-let* ((overlay
               (seq-find
                (lambda (candidate)
                  (and
                   (overlay-get candidate 'yunge-avy-test-projection)
                   (overlay-get candidate 'display)))
                (overlays-at beginning)))
              ((<= end (overlay-end overlay))))
    (let ((anchor (overlay-start overlay)))
      (yunge-avy-make-projection
       :identity overlay
       :beginning anchor
       :end (min (1+ anchor) (overlay-end overlay))
       :target beginning))))

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

(ert-deftest yunge-avy-jump-to-text-projects-rendered-source ()
  (yunge-avy-test--load-config)
  (require 'avy)
  (let ((buffer (generate-new-buffer " *yunge-avy-projection*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "$alpha + alpha$ outside alpha $alpha$")
          (goto-char (point-min))
          (let* ((first-beginning (point-min))
                 (first-end (1- (search-forward "$ ")))
                 (normal-target (progn (search-forward "alpha")
                                       (match-beginning 0)))
                 (second-beginning (progn (search-forward "$")
                                          (1- (point))))
                 (second-target (progn (search-forward "alpha")
                                       (match-beginning 0)))
                 (second-end (progn (search-forward "$") (point)))
                 (first (make-overlay first-beginning first-end))
                 (second (make-overlay second-beginning second-end)))
            (dolist (overlay (list first second))
              (overlay-put overlay 'yunge-avy-test-projection t)
              (overlay-put overlay 'display "[SVG]"))
            (goto-char (point-min))
            (let ((avy-all-windows nil)
                  (avy-keys '(?a ?s ?d))
                  (avy-single-candidate-jump nil)
                  (yunge-avy-candidate-project-functions
                   '(yunge-avy-test--overlay-projection))
                  (unread-command-events '(?d)))
              (cl-letf
                  (((symbol-function
                     'yunge-input-source-call-with-ascii)
                    (lambda (function) (funcall function))))
                (yunge-avy-jump-to-text "alpha")))
            (should (= (point) second-target))
            (should (equal (mapcar #'caar avy-last-candidates)
                           (list first-beginning normal-target
                                 second-beginning)))
            (should (equal (overlay-get first 'display) "[SVG]"))
            (should (equal (overlay-get second 'display) "[SVG]"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-avy-projected-source-dispatches-at-the-real-match ()
  (yunge-avy-test--load-config)
  (require 'avy)
  (let ((buffer (generate-new-buffer " *yunge-avy-projection-action*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "$alpha + alpha$")
          (let ((overlay (make-overlay (point-min) (point-max))))
            (overlay-put overlay 'yunge-avy-test-projection t)
            (overlay-put overlay 'display "[SVG]")
            (goto-char (point-max))
            (let ((avy-all-windows nil)
                  (avy-keys '(?a ?s))
                  (avy-single-candidate-jump nil)
                  (yunge-avy-candidate-project-functions
                   '(yunge-avy-test--overlay-projection))
                  (unread-command-events '(?m ?a)))
              (cl-letf
                  (((symbol-function
                     'yunge-input-source-call-with-ascii)
                    (lambda (function) (funcall function))))
                (yunge-avy-jump-to-text "alpha")))
            (should (= (mark) 2))
            (should (equal (overlay-get overlay 'display) "[SVG]"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-avy-projected-source-preserves-a-preselected-action ()
  (yunge-avy-test--load-config)
  (require 'avy)
  (let ((buffer (generate-new-buffer " *yunge-avy-projection-action*"))
        target)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "$alpha$")
          (let ((overlay (make-overlay (point-min) (point-max))))
            (overlay-put overlay 'yunge-avy-test-projection t)
            (overlay-put overlay 'display "[SVG]")
            (goto-char (point-max))
            (let ((avy-action (lambda (position) (setq target position)))
                  (avy-all-windows nil)
                  (avy-single-candidate-jump t)
                  (yunge-avy-candidate-project-functions
                   '(yunge-avy-test--overlay-projection)))
              (yunge-avy--jump "alpha"))
            (should (= target 2))
            (should (equal (overlay-get overlay 'display) "[SVG]"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-avy-cancel-keeps-projected-displays-intact ()
  (yunge-avy-test--load-config)
  (require 'avy)
  (let ((buffer (generate-new-buffer " *yunge-avy-projection-cancel*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "$alpha$ and $alpha$")
          (let ((first (make-overlay 1 8))
                (second (make-overlay 13 20)))
            (dolist (overlay (list first second))
              (overlay-put overlay 'yunge-avy-test-projection t)
              (overlay-put overlay 'display "[SVG]"))
            (goto-char (point-max))
            (let ((origin (point))
                  (avy-all-windows nil)
                  (avy-keys '(?a ?s))
                  (avy-single-candidate-jump nil)
                  (yunge-avy-candidate-project-functions
                   '(yunge-avy-test--overlay-projection))
                  (unread-command-events '(?\e)))
              (cl-letf
                  (((symbol-function
                     'yunge-input-source-call-with-ascii)
                    (lambda (function) (funcall function))))
                (yunge-avy-jump-to-text "alpha"))
              (should (= (point) origin)))
            (should (equal (overlay-get first 'display) "[SVG]"))
            (should (equal (overlay-get second 'display) "[SVG]"))))
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
