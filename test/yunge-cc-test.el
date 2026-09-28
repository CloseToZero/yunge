;;; yunge-cc-test.el --- C/C++ tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-cc)

(yunge-test-deftest-lazy-load yunge-cc
  (cc-mode
   files-x
   project))

(ert-deftest yunge-cc-h-files-default-to-c++-mode ()
  (with-temp-buffer
    (let ((buffer-file-name "example.h")
          (enable-local-variables nil)
          (major-mode-remap-alist nil))
      (set-auto-mode)
      (should (eq major-mode 'c++-mode)))))

(ert-deftest yunge-cc-project-can-use-c-mode-for-h-files ()
  (let* ((root (make-temp-file "yunge-c-project-" t))
         (directory (expand-file-name "src/" root))
         (locals-file (expand-file-name dir-locals-file root))
         (dir-locals-class-alist nil)
         (dir-locals-directory-cache nil)
         (major-mode-remap-alist nil)
         buffers saved-locals)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git/" root))
          (make-directory directory)
          (dolist (name '("example.h" "example.inc" "settings.el"))
            (with-temp-file (expand-file-name name directory)))
          (with-temp-file locals-file
            (prin1
             '((auto-mode-alist . (("\\.inc\\'" . c-mode)))
               (nil . ((fill-column . 79)))
               (emacs-lisp-mode . ((tab-width . 3))))
             (current-buffer)))
          (let ((default-directory directory))
            (call-interactively #'yunge-cc-use-c-headers)
            (with-temp-buffer
              (insert-file-contents locals-file)
              (setq saved-locals (buffer-string)))
            (call-interactively #'yunge-cc-use-c-headers))
          (with-temp-buffer
            (insert-file-contents locals-file)
            (should (equal (buffer-string) saved-locals)))
          (dolist (name '("example.h" "example.inc" "settings.el"))
            (let ((buffer (find-file-noselect (expand-file-name name directory))))
              (push buffer buffers)
              (with-current-buffer buffer
                (should (= fill-column 79))
                (if (equal name "settings.el")
                    (progn
                      (should (eq major-mode 'emacs-lisp-mode))
                      (should (= tab-width 3)))
                  (should (eq major-mode 'c-mode)))))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (when-let* ((buffer (find-buffer-visiting locals-file)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(provide 'yunge-cc-test)

;;; yunge-cc-test.el ends here
