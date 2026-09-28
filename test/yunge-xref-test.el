;;; yunge-xref-test.el --- Xref tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function xref-change-to-xref-edit-mode "xref" ())
(declare-function xref-make-file-location "xref" (file line column))
(declare-function xref-make-match "xref" (summary location length))

(defvar xref-auto-jump-to-first-xref)
(defvar xref-file-name-display)

(yunge-test-deftest-lazy-load yunge-xref
  (evil which-key xref))

(ert-deftest yunge-xref-integrates-result-buffers-with-evil ()
  (require 'yunge-xref)
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'xref)

  (yunge-test-evil-normal-keys
   'xref--xref-buffer-mode
   '(("j" . evil-next-line)
     ("k" . evil-previous-line)
     ("C-j" . xref-next-line)
     ("C-k" . xref-prev-line)
     ("RET" . xref-goto-xref)
     ("q" . quit-window)
     ("gf" . xref-show-location-at-point)
     ("gr" . xref-revert-buffer)
     ("i" . xref-change-to-xref-edit-mode)
     ("]]" . xref-next-group)
     ("[[" . xref-prev-group)
     ("SPC m r" . xref-query-replace-in-results)))

  (yunge-test-evil-normal-keys
   'xref--transient-buffer-mode
   '(("RET" . xref-quit-and-goto-xref)
     ("q" . quit-window)
     ("C-j" . xref-next-line)
     ("C-k" . xref-prev-line))))

(ert-deftest yunge-xref-edit-saves-a-reference-from-an-unopened-file ()
  (require 'yunge-xref)
  (yunge-test-enable-evil)
  (require 'xref)
  (let* ((directory (make-temp-file "yunge-xref-" t))
         (file (expand-file-name "source.txt" directory))
         (xref-auto-jump-to-first-xref nil)
         (xref-file-name-display 'abs)
         (result nil)
         (source nil))
    (unwind-protect
        (save-window-excursion
          (with-temp-file file (insert "before\n"))
          (setq result
                (xref-show-xrefs
                 (lambda ()
                   (list (xref-make-match
                          "before" (xref-make-file-location file 1 0) 6)))
                 nil))
          (should-not (get-file-buffer file))
          (with-current-buffer result
            (xref-change-to-xref-edit-mode)
            (goto-char (point-min))
            (search-forward "before")
            (replace-match "after")
            (call-interactively (key-binding (kbd "ZZ")))
            (should (eq major-mode 'xref--xref-buffer-mode)))
          (setq source (get-file-buffer file))
          (should source)
          (should-not (buffer-modified-p source))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "after\n"))))
      (dolist (buffer (list result source (get-file-buffer file)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

;;; yunge-xref-test.el ends here
