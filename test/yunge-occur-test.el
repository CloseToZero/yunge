;;; yunge-occur-test.el --- Occur tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-occur
  (evil))

(ert-deftest yunge-occur-integrates-results-with-evil ()
  (require 'yunge-occur)
  (yunge-test-enable-evil)
  (require 'replace)
  (yunge-test-evil-normal-keys
   'occur-mode
   '(("j" . evil-next-line)
     ("k" . evil-previous-line)
     ("C-j" . occur-next)
     ("C-k" . occur-prev)
     ("RET" . occur-mode-goto-occurrence)
     ("q" . quit-window)
     ("gf" . occur-mode-display-occurrence)
     ("gr" . revert-buffer)
     ("i" . occur-edit-mode))))

(ert-deftest yunge-occur-edit-saves-only-the-edited-source ()
  (require 'yunge-occur)
  (yunge-test-enable-evil)
  (require 'replace)
  (let* ((directory (make-temp-file "yunge-occur-" t))
         (edited-file (expand-file-name "edited.txt" directory))
         (untouched-file (expand-file-name "untouched.txt" directory))
         (edited nil)
         (untouched nil)
         (result nil))
    (unwind-protect
        (save-window-excursion
          (with-temp-file edited-file (insert "first-before\n"))
          (with-temp-file untouched-file (insert "second-before\n"))
          (setq edited (find-file-noselect edited-file)
                untouched (find-file-noselect untouched-file))
          (with-current-buffer edited
            (goto-char (point-max))
            (insert "draft\n"))
          (with-current-buffer untouched
            (goto-char (point-max))
            (insert "unsaved\n"))
          (multi-occur (list edited untouched) "before")
          (setq result (get-buffer "*Occur*"))
          (switch-to-buffer result)
          (with-current-buffer result
            (occur-edit-mode)
            (goto-char (point-min))
            (search-forward "first-before")
            (replace-match "first-after")
            (should-error (call-interactively (key-binding (kbd "ZQ")))
                          :type 'user-error)
            (should (eq major-mode 'occur-edit-mode))
            (with-temp-buffer
              (insert-file-contents edited-file)
              (should (equal (buffer-string) "first-before\n")))
            (call-interactively (key-binding (kbd "C-c C-c")))
            (should (eq major-mode 'occur-mode)))
          (with-temp-buffer
            (insert-file-contents edited-file)
            (should (equal (buffer-string) "first-after\ndraft\n")))
          (with-temp-buffer
            (insert-file-contents untouched-file)
            (should (equal (buffer-string) "second-before\n")))
          (should-not (buffer-modified-p edited))
          (should (buffer-modified-p untouched))
          (with-current-buffer untouched
            (should (equal (buffer-string) "second-before\nunsaved\n"))))
      (dolist (buffer (list result edited untouched))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

;;; yunge-occur-test.el ends here
