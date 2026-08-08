;;; yunge-xref-test.el --- Xref tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

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
     ("]]" . xref-next-group)
     ("[[" . xref-prev-group)
     ("SPC m e" . xref-change-to-xref-edit-mode)
     ("SPC m r" . xref-query-replace-in-results)))

  (yunge-test-evil-normal-keys
   'xref--transient-buffer-mode
   '(("RET" . xref-quit-and-goto-xref)
     ("q" . quit-window)
     ("C-j" . xref-next-line)
     ("C-k" . xref-prev-line)))

  (yunge-test-which-key-prefix-bindings
   'xref--xref-buffer-mode "SPC m"
   yunge-xref-command-bindings))

;;; yunge-xref-test.el ends here
