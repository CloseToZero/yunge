;;; yunge-grep-test.el --- Grep tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function grep-change-to-grep-edit-mode "grep" ())
(declare-function grep-edit-save-changes "grep" ())
(declare-function compilation--ensure-parse "compile" (limit))

(yunge-test-deftest-lazy-load yunge-grep
  (evil grep))

(ert-deftest yunge-grep-integrates-results-with-evil ()
  (require 'yunge-grep)
  (yunge-test-enable-evil)
  (require 'grep)
  (yunge-test-evil-normal-keys
   'grep-mode
   '(("j" . evil-next-line)
     ("k" . evil-previous-line)
     ("C-j" . compilation-next-error)
     ("C-k" . compilation-previous-error)
     ("RET" . compile-goto-error)
     ("q" . quit-window)
     ("gf" . compilation-display-error)
     ("gr" . recompile)
     ("i" . grep-change-to-grep-edit-mode)
     ("]]" . compilation-next-file)
     ("[[" . compilation-previous-file))))

(ert-deftest yunge-grep-edit-uses-the-result-edit-lifecycle ()
  (require 'yunge-grep)
  (yunge-test-enable-evil)
  (require 'grep)
  (with-temp-buffer
    (grep-mode)
    (grep-change-to-grep-edit-mode)
    (should (eq major-mode 'grep-edit-mode))
    (yunge-test-evil-keys
     'normal
     '(("C-c C-c" . yunge-edit-finish-result-session)
       ("ZZ" . yunge-edit-finish-result-session)
       ("ZQ" . yunge-edit-refuse-result-abort)))
    (should
     (eq (command-remapping #'grep-edit-save-changes)
         #'yunge-edit-finish-result-session))
    (should
     (eq (command-remapping #'evil-save-and-close)
         #'yunge-edit-finish-result-session))
    (should
     (eq (command-remapping #'evil-quit)
         #'yunge-edit-refuse-result-abort))
    (yunge-edit-finish-result-session)
    (should (eq major-mode 'grep-mode))))

(ert-deftest yunge-grep-edit-saves-an-edited-match ()
  (require 'yunge-grep)
  (require 'grep)
  (let* ((directory (make-temp-file "yunge-grep-" t))
         (file (expand-file-name "source.txt" directory))
         (result (generate-new-buffer " *yunge-grep-result*"))
         source)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "before\n"))
          (with-current-buffer result
            (setq default-directory directory)
            (grep-mode)
            (let ((inhibit-read-only t))
              (insert "source.txt:1:before\n"))
            (compilation--ensure-parse (point-max))
            (grep-change-to-grep-edit-mode)
            (goto-char (point-min))
            (search-forward "before")
            (replace-match "after")
            (setq source (marker-buffer
                          (occur--targets-start
                           (get-text-property (point) 'occur-target))))
            (yunge-edit-finish-result-session))
          (should-not (buffer-modified-p source))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "after\n"))))
      (when (buffer-live-p result)
        (kill-buffer result))
      (when (buffer-live-p source)
        (kill-buffer source))
      (delete-directory directory t))))

;;; yunge-grep-test.el ends here
