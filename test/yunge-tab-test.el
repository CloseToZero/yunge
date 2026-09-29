;;; yunge-tab-test.el --- Tab tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar evil-state)

(ert-deftest yunge-tab-names-and-switches-task-layouts ()
  (yunge-test-enable-evil)
  (require 'yunge-tab)
  (let ((initial-count (length (tab-bar-tabs)))
        (code (generate-new-buffer " *tab-code*"))
        (notes (generate-new-buffer " *tab-notes*")))
    (should-not tab-bar-show)
    (should-not tab-bar-mode)
    (should (equal (alist-get 'name (assq 'current-tab (tab-bar-tabs)))
                   "default"))
    (unwind-protect
        (progn
          (yunge-tab-new "  code  ")
          (switch-to-buffer code)
          (delete-other-windows)
          (split-window-right)
          (yunge-tab-new "notes")
          (delete-other-windows)
          (switch-to-buffer notes)
          (yunge-tab-rename "  writing  ")
          (yunge-tab-rename "writing")
          (should (= (length (tab-bar-tabs)) (+ initial-count 2)))
          (yunge-tab-switch "code")
          (should (= (length (window-list)) 2))
          (should (eq (window-buffer) code))
          (yunge-tab-switch "writing")
          (should (one-window-p))
          (should (eq (window-buffer) notes))
          (should-not tab-bar-mode))
      (while (> (length (tab-bar-tabs)) initial-count)
        (tab-close))
      (kill-buffer code)
      (kill-buffer notes))))

(ert-deftest yunge-tab-switch-offers-other-tabs-and-defaults-to-the-most-recent ()
  (yunge-test-enable-evil)
  (require 'yunge-tab)
  (let ((initial-count (length (tab-bar-tabs))))
    (unwind-protect
        (progn
          (yunge-tab-new "zzz-recent")
          (yunge-tab-new "aaa-current")
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection _predicate _require-match
                              _initial _history default &rest _arguments)
                       (should-not (member "aaa-current" (all-completions "" collection)))
                       (should (equal (car default) "zzz-recent"))
                       (car default))))
            (call-interactively #'yunge-tab-switch))
          (should (equal (alist-get 'name (assq 'current-tab (tab-bar-tabs)))
                         "zzz-recent")))
      (while (> (length (tab-bar-tabs)) initial-count)
        (tab-close)))))

(ert-deftest yunge-tab-rejects-ambiguous-or-missing-names-without-changing-tabs ()
  (yunge-test-enable-evil)
  (require 'yunge-tab)
  (let ((initial-count (length (tab-bar-tabs))))
    (unwind-protect
        (progn
          (yunge-tab-new "code")
          (yunge-tab-new "notes")
          (dolist (command '(yunge-tab-new yunge-tab-rename))
            (dolist (name '("" "   " " code "))
              (should-error (funcall command name) :type 'user-error)))
          (should-error (yunge-tab-switch "missing") :type 'user-error)
          (should (= (length (tab-bar-tabs)) (+ initial-count 2)))
          (should (equal (alist-get 'name (assq 'current-tab (tab-bar-tabs)))
                         "notes"))
          (yunge-tab-switch "code")
          (should (equal (alist-get 'name (assq 'current-tab (tab-bar-tabs)))
                         "code")))
      (while (> (length (tab-bar-tabs)) initial-count)
        (tab-close)))))

(ert-deftest yunge-tab-shows-the-current-tab-in-the-selected-mode-line ()
  (yunge-test-enable-evil)
  (require 'yunge-tab)
  (let ((initial-count (length (tab-bar-tabs))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mode-line-window-selected-p)
                     (lambda () t)))
            (should-not (yunge-tab--mode-line)))
          (yunge-tab-new "yunge-tab-mode-line-test")
          (cl-letf (((symbol-function 'mode-line-window-selected-p)
                     (lambda () t)))
            (should
             (equal (substring-no-properties (yunge-tab--mode-line))
                    "[yunge-tab-mode-line-test] ")))
          (cl-letf (((symbol-function 'mode-line-window-selected-p)
                     (lambda () nil)))
            (should-not (yunge-tab--mode-line))))
      (while (> (length (tab-bar-tabs)) initial-count)
        (tab-close))))
  (let ((tail (member yunge-tab-mode-line-format
                      (default-value 'mode-line-format))))
    (should (eq (cadr tail) 'mode-line-buffer-identification))))

(ert-deftest yunge-tab-routes-leader-bindings ()
  (yunge-test-enable-evil)
  (require 'yunge-tab)
  (require 'which-key)

  (with-temp-buffer
    (fundamental-mode)
    (should (eq evil-state 'normal))
    (yunge-test-keys
     '(("C-i" . yunge-jump-history-forward)
       ("SPC TAB TAB" . yunge-tab-switch)
       ("SPC <tab> <tab>" . yunge-tab-switch)
       ("SPC TAB l" . yunge-workspace-restore)
       ("SPC TAB n" . yunge-tab-new)
       ("SPC TAB q" . tab-close)
       ("SPC TAB r" . yunge-tab-rename)
       ("SPC TAB s" . yunge-workspace-save)
       ("SPC TAB u" . tab-undo)))))

;;; yunge-tab-test.el ends here
