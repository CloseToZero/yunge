;;; yunge-help-test.el --- Help tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-visual-state "evil-states")

(defvar evil-state)

(yunge-test-deftest-lazy-load yunge-help
  (evil help-mode which-key))

(ert-deftest yunge-help-binds-navigation-keys ()
  (require 'yunge-help)
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'help-mode)

  (should (eq help-window-select t))

  (with-temp-buffer
    (fundamental-mode)
    (should (eq evil-state 'normal))
    (yunge-test-keys
     '(("SPC h b" . describe-bindings)
       ("SPC h c" . describe-key-briefly)
       ("SPC h f" . describe-function)
       ("SPC h i" . info)
       ("SPC h k" . describe-key)
       ("SPC h m" . describe-mode)
       ("SPC h o" . describe-symbol)
       ("SPC h v" . describe-variable)))

    (yunge-test-which-key-prefix "SPC"
                                 '(("h" nil "+help")))
    (yunge-test-which-key-prefix "SPC h"
                                 yunge-help-command-bindings))

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
