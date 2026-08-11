;;; yunge-org-src-test.el --- Tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defun yunge-org-src-test--load-config ()
  "Load the Org source editing configuration."
  (yunge-test-enable-evil)
  (require 'yunge-org)
  (require 'org-src))

(ert-deftest yunge-org-configures-source-edit-lifecycle ()
  (yunge-org-src-test--load-config)
  (should
   (eq (lookup-key org-src-mode-map
                   [remap evil-save-and-close])
       #'org-edit-src-exit))
  (should
   (eq (lookup-key org-src-mode-map
                   [remap evil-save-modified-and-close])
       #'org-edit-src-exit))
  (should
   (eq (lookup-key org-src-mode-map [remap evil-quit])
       #'org-edit-src-abort)))

(ert-deftest yunge-org-source-edit-commands-resolve-through-evil ()
  (yunge-org-src-test--load-config)
  (with-temp-buffer
    (org-mode)
    (org-src-mode)
    (evil-normal-state)
    (should (eq (key-binding (kbd "ZZ")) #'org-edit-src-exit))
    (should (eq (key-binding (kbd "ZQ")) #'org-edit-src-abort))))

;;; yunge-org-src-test.el ends here
