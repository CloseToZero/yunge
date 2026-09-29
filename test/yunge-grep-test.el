;;; yunge-grep-test.el --- Grep tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function grep-change-to-grep-edit-mode "grep" ())
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
     ("C-j" . yunge-grep-next-match)
     ("C-k" . yunge-grep-previous-match)
     ("RET" . compile-goto-error)
     ("q" . quit-window)
     ("gf" . compilation-display-error)
     ("gr" . recompile)
     ("i" . grep-change-to-grep-edit-mode)
     ("]]" . compilation-next-file)
     ("[[" . compilation-previous-file))))

(ert-deftest yunge-grep-previews-matches-before-visiting-source ()
  (require 'yunge-grep)
  (yunge-test-enable-evil)
  (require 'grep)
  (let* ((directory (make-temp-file "yunge-grep-navigation-" t))
         (file (expand-file-name "source.txt" directory))
         (result (generate-new-buffer " *yunge-grep-result*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-temp-file file
            (insert "first match\ncontext\nlast match\n"))
          (switch-to-buffer result)
          (setq default-directory directory)
          (grep-mode)
          (let ((inhibit-read-only t))
            (insert "Matches:\n\nsource.txt:1:first match\n"
                    "source.txt:3:last match\n"))
          (goto-char (point-min))
          (let ((result-window (selected-window)))
            (dolist (step '(("C-j" . 1) ("C-j" . 3) ("C-k" . 1)
                            ("j" . 1) ("gf" . 3)))
              (execute-kbd-macro (kbd (car step)))
              (should (eq (selected-window) result-window))
              (should (eq (current-buffer) result))
              (let ((source-window (get-buffer-window (get-file-buffer file))))
                (should (window-live-p source-window))
                (with-selected-window source-window
                  (should (= (line-number-at-pos) (cdr step)))))))
          (execute-kbd-macro (kbd "RET"))
          (should (equal (buffer-file-name) file))
          (should (= (line-number-at-pos) 3)))
      (dolist (buffer (list result (get-file-buffer file)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory directory t))))

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
            (setq source (get-file-buffer file))
            (should source)
            (call-interactively (key-binding (kbd "C-c C-c")))
            (should (eq major-mode 'grep-mode)))
          (should-not (buffer-modified-p source))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "after\n"))))
      (when (buffer-live-p result)
        (kill-buffer result))
      (dolist (buffer (list source (get-file-buffer file)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

;;; yunge-grep-test.el ends here
