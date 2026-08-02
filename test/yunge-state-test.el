;;; yunge-state-test.el --- Mutable state tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-state-provisions-auto-save-storage ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(let* ((root (make-temp-file "yunge-state-" t))
            (user-emacs-directory (file-name-as-directory root))
            (file (expand-file-name "file.txt" root)))
       (unwind-protect
           (progn
             (require 'yunge-state)
             (with-temp-file file
               (insert "saved"))
             (with-current-buffer (find-file-noselect file)
               (auto-save-mode 1)
               (goto-char (point-max))
               (insert " recovery")
               (do-auto-save t t)
               (unless (and buffer-auto-save-file-name
                            (file-exists-p
                             buffer-auto-save-file-name))
                 (error "Auto-save recovery file was not written"))
               (set-buffer-modified-p nil)
               (kill-buffer))
             (unless (file-directory-p tramp-auto-save-directory)
               (error "TRAMP auto-save directory was not created")))
         (delete-directory root t))))))

(provide 'yunge-state-test)

;;; yunge-state-test.el ends here
