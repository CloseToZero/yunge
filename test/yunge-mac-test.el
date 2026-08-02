;;; yunge-mac-test.el --- macOS modifier tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function yunge-mac-toggle-modifiers "yunge-mac")

(defvar ns-command-modifier)
(defvar ns-option-modifier)

(ert-deftest yunge-mac-toggles-modifiers ()
  (let (ns-command-modifier ns-option-modifier)
    (load "yunge-mac" nil t)
    (should (eq ns-command-modifier 'meta))
    (should (eq ns-option-modifier 'super))

    (yunge-mac-toggle-modifiers)
    (should (eq ns-command-modifier 'super))
    (should (eq ns-option-modifier 'meta))

    (yunge-mac-toggle-modifiers)
    (should (eq ns-command-modifier 'meta))
    (should (eq ns-option-modifier 'super))))

;;; yunge-mac-test.el ends here
