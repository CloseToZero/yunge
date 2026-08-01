;;; yunge-help-test.el --- Help tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-help
  (evil help-mode which-key))

(ert-deftest yunge-help-binds-navigation-keys ()
  (require 'yunge-help)
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'help-mode)

  (should (eq help-window-select t))

  (yunge-test-evil-normal-keys
   'help-mode
   '(("RET" . push-button)
     ("q" . quit-window)
     ("gr" . revert-buffer)
     ("gh" . help-go-back)
     ("gl" . help-go-forward)
     ("gf" . help-view-source)
     ("g]" . forward-button)
     ("g[" . backward-button)
     ("<tab>" . forward-button)
     ("S-TAB" . backward-button)
     ("C-o" . yunge-jump-backward)
     ("C-i" . yunge-jump-forward)))

  (yunge-test-which-key-bindings
   'help-mode
   '(("RET" nil "activate button")
     ("q" nil "quit")
     ("gr" nil "refresh")
     ("gh" nil "history back")
     ("gl" nil "history forward")
     ("gf" nil "visit source")
     ("g]" nil "next button")
     ("g[" nil "previous button")
     ("<tab>" nil "next button")
     ("S-TAB" nil "previous button"))))

;;; yunge-help-test.el ends here
