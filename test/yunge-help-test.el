;;; yunge-help-test.el --- Help tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-visual-state "evil-states")

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

  (with-temp-buffer
    (help-mode)
    (yunge-test-which-key-prefix "g"
                                 '(("f" nil "visit source")
                                   ("h" nil "history back")
                                   ("l" nil "history forward")
                                   ("r" nil "refresh")))
    (evil-visual-state)
    (should-not
     (equal (yunge-test-which-key-prefix-description "g" "f")
            "visit source"))))

;;; yunge-help-test.el ends here
