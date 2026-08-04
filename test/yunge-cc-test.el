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
  (should (eq (cdr (assoc yunge-cc-header-regexp auto-mode-alist))
              'c++-mode))
  (with-temp-buffer
    (let ((buffer-file-name "example.h")
          (enable-local-variables nil)
          (major-mode-remap-alist nil))
      (set-auto-mode)
      (should (eq major-mode 'c++-mode)))))

(ert-deftest yunge-cc-project-can-use-c-mode-for-h-files ()
  (let* ((root (make-temp-file "yunge-c-project-" t))
         (locals-file (expand-file-name dir-locals-file root))
         (dir-locals-class-alist nil)
         (dir-locals-directory-cache nil)
         project-current-arguments)
    (unwind-protect
        (progn
          (with-temp-file locals-file
            (prin1
             '((auto-mode-alist . (("\\.inc\\'" . c-mode)))
               (nil . ((fill-column . 79)))
               (emacs-lisp-mode . ((indent-tabs-mode . nil))))
             (current-buffer)))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional maybe-prompt directory)
                       (setq project-current-arguments
                             (list maybe-prompt directory))
                       'test-project))
                    ((symbol-function 'project-root)
                     (lambda (project)
                       (should (eq project 'test-project))
                       root)))
            (yunge-project-use-c-mode-for-headers)
            ;; Repeating the command must replace, not duplicate, the rule.
            (yunge-project-use-c-mode-for-headers))
          (should (equal project-current-arguments '(t nil)))
          (with-current-buffer (find-file-noselect locals-file)
            (save-excursion
              (goto-char (point-min))
              (let* ((variables (read (current-buffer)))
                     (all-modes (alist-get nil variables))
                     (mode-alist (alist-get 'auto-mode-alist variables)))
                (should (equal (alist-get 'fill-column all-modes) 79))
                (should (equal mode-alist
                               `((,yunge-cc-header-regexp . c-mode)
                                 ("\\.inc\\'" . c-mode))))
                (should (equal
                         (alist-get 'emacs-lisp-mode variables)
                         '((indent-tabs-mode . nil)))))))
          (with-temp-buffer
            (let ((buffer-file-name (expand-file-name "example.h" root))
                  (default-directory root)
                  (major-mode-remap-alist nil))
              (set-auto-mode)
              (should (eq major-mode 'c-mode)))))
      (when-let* ((buffer (find-buffer-visiting locals-file)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(provide 'yunge-cc-test)

;;; yunge-cc-test.el ends here
