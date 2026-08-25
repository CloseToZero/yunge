;;; yunge-xref-test.el --- Xref tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function xref--xref-buffer-mode "xref" ())
(declare-function xref-change-to-xref-edit-mode "xref" ())
(declare-function xref-edit-save-changes "xref" ())

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

(ert-deftest yunge-xref-edit-uses-the-result-edit-lifecycle ()
  (require 'yunge-xref)
  (yunge-test-enable-evil)
  (require 'xref)
  (with-temp-buffer
    (xref--xref-buffer-mode)
    (xref-change-to-xref-edit-mode)
    (should (eq major-mode 'xref-edit-mode))
    (yunge-test-evil-keys
     'normal
     '(("C-c C-c" . yunge-edit-finish-result-session)
       ("ZZ" . yunge-edit-finish-result-session)
       ("ZQ" . yunge-edit-refuse-result-abort)))
    (should
     (eq (command-remapping #'xref-edit-save-changes)
         #'yunge-edit-finish-result-session))
    (should
     (eq (command-remapping #'evil-save-and-close)
         #'yunge-edit-finish-result-session))
    (should
     (eq (command-remapping #'evil-save-modified-and-close)
         #'yunge-edit-finish-result-session))
    (should
     (eq (command-remapping #'evil-quit)
         #'yunge-edit-refuse-result-abort))
    (yunge-edit-finish-result-session)
    (should (eq major-mode 'xref--xref-buffer-mode))))

;;; yunge-xref-test.el ends here
