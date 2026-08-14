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

(ert-deftest yunge-occur-edit-uses-the-result-edit-lifecycle ()
  (require 'yunge-occur)
  (yunge-test-enable-evil)
  (require 'replace)
  (with-temp-buffer
    (occur-mode)
    (occur-edit-mode)
    (should (eq major-mode 'occur-edit-mode))
    (yunge-test-evil-keys
     'normal
     '(("C-c C-c" . yunge-edit-finish-result-session)
       ("ZZ" . yunge-edit-finish-result-session)
       ("ZQ" . yunge-edit-refuse-result-abort)))
    (should
     (eq (command-remapping #'occur-cease-edit)
         #'yunge-edit-finish-result-session))
    (should
     (eq (command-remapping #'evil-save-and-close)
         #'yunge-edit-finish-result-session))
    (should
     (eq (command-remapping #'evil-quit)
         #'yunge-edit-refuse-result-abort))
    (yunge-edit-finish-result-session)
    (should (eq major-mode 'occur-mode))))

;;; yunge-occur-test.el ends here
