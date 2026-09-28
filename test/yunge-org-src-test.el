;;; yunge-org-src-test.el --- Org source editing tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-org-source-edit-applies-or-discards-changes ()
  (yunge-test-enable-evil)
  (require 'yunge-org)
  (require 'org-src)
  (dolist (finish '(t nil))
    (let ((source (generate-new-buffer " *yunge-org-source*"))
          edit)
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer source)
            (insert "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n")
            (org-mode)
            (forward-line -2)
            (call-interactively #'org-edit-special)
            (setq edit (window-buffer (selected-window)))
            (should-not (eq edit source))
            (with-current-buffer edit
              (goto-char (point-min))
              (delete-region (point-min) (point-max))
              (insert "(+ 2 3)\n")
              (evil-normal-state)
              (call-interactively (key-binding (kbd (if finish "ZZ" "ZQ")))))
            (should (eq (window-buffer (selected-window)) source))
            (should-not (buffer-live-p edit))
            (with-current-buffer source
              (should (equal (buffer-string)
                             (concat "#+begin_src emacs-lisp\n"
                                     (if finish "  (+ 2 3)\n" "(+ 1 2)\n")
                                     "#+end_src\n")))))
        (when (buffer-live-p edit) (kill-buffer edit))
        (when (buffer-live-p source) (kill-buffer source))))))

;;; yunge-org-src-test.el ends here
