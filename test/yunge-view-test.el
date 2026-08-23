;;; yunge-view-test.el --- View mode tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defun yunge-view-test--load-config ()
  "Load the View configuration after enabling Evil."
  (yunge-test-enable-evil)
  (require 'yunge-view)
  (require 'view))

(yunge-test-deftest-lazy-load yunge-view
  (evil view which-key))

(ert-deftest yunge-view-activates-evil-bindings-on-the-first-toggle ()
  (yunge-test-run-emacs
   "-L" (expand-file-name "test" yunge-test-root)
   "-l" "yunge-test-helper"
   "--eval"
   (prin1-to-string
    '(progn
       (yunge-test-enable-evil)
       (require 'yunge-view)
       (when (featurep 'view)
         (error "View loaded before its first toggle"))
       (with-temp-buffer
         (fundamental-mode)
         (evil-normal-state)
         (call-interactively #'view-mode)
         (unless (eq (key-binding (kbd "i")) #'yunge-view-edit)
           (error "First View toggle kept stale Evil bindings")))))))

(ert-deftest yunge-view-integrates-with-global-toggle-map ()
  (yunge-view-test--load-config)
  (require 'which-key)
  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC t v" . view-mode)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC t"
   '(("v" nil "view mode"))))

(ert-deftest yunge-view-keeps-evil-navigation-and-view-actions ()
  (yunge-view-test--load-config)
  (with-temp-buffer
    (view-mode 1)
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("h" . evil-backward-char)
       ("j" . evil-next-line)
       ("q" . View-quit)
       ("i" . yunge-view-edit)
       ("SPC t v" . view-mode)))))

(ert-deftest yunge-view-edit-exits-view-before-inserting ()
  (yunge-view-test--load-config)
  (with-temp-buffer
    (view-mode 1)
    (evil-normal-state)
    (call-interactively #'yunge-view-edit)
    (should-not view-mode)
    (should-not buffer-read-only)
    (should (eq evil-state 'insert))))

(provide 'yunge-view-test)

;;; yunge-view-test.el ends here
