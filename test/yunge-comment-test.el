;;; yunge-comment-test.el --- Comment operator tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function c-ts-base-mode "c-ts-mode")
(declare-function evil-normal-state "evil-states")

(defvar evil-state)

(ert-deftest yunge-comment-binds-the-evil-operator ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (evil-normal-state)
    (yunge-test-key "g c" 'yunge-comment-toggle)))

(ert-deftest yunge-comment-toggles-c++-lines ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (c++-mode)
    (insert "int one;\nint two;\n")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "g c c"))
      (should (equal (buffer-string)
                     "// int one;\nint two;\n"))
      (execute-kbd-macro (kbd "g c c"))
      (should (equal (buffer-string)
                     "int one;\nint two;\n")))))

(ert-deftest yunge-comment-honors-line-counts ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (c++-mode)
    (insert "int one;\nint two;\nint three;\n")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "2 g c c"))
      (should (equal (buffer-string)
                     "// int one;\n// int two;\nint three;\n")))))

(ert-deftest yunge-comment-preserves-c++-character-ranges ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (c++-mode)
    (insert "int value = foo + bar;\n")
    (goto-char (point-min))
    (search-forward "foo")
    (backward-word)
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "g c i w"))
      (should (equal (buffer-string)
                     "int value = /* foo */ + bar;\n"))
      (execute-kbd-macro (kbd "g c i w"))
      (should (equal (buffer-string)
                     "int value = foo + bar;\n")))))

(ert-deftest yunge-comment-preserves-c++-visual-character-ranges ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (c++-mode)
    (insert "foo + bar;\n")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "v l l g c"))
      (should (equal (buffer-string)
                     "/* foo */ + bar;\n")))))

(ert-deftest yunge-comment-preserves-c++-block-ranges ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (c++-mode)
    (insert "foo one\nfoo two\n")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "C-v j l l g c"))
      (should (equal (buffer-string)
                     "/* foo */ one\n/* foo */ two\n"))
      (goto-char (point-min))
      (execute-kbd-macro (kbd "C-v j 8 l g c"))
      (should (equal (buffer-string)
                     "foo one\nfoo two\n")))))

(ert-deftest yunge-comment-uses-c-tree-sitter-comment-styles ()
  (yunge-test-enable-evil)
  (require 'c-ts-mode)
  (with-temp-buffer
    (c-ts-base-mode)
    (insert "int value = foo;\n")
    (goto-char (point-min))
    (search-forward "foo")
    (backward-word)
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "g c i w"))
      (should (equal (buffer-string)
                     "int value = /* foo */;\n")))))

(ert-deftest yunge-comment-repeats-line-operators ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (c++-mode)
    (insert "int one;\nint two;\n")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "g c c j ."))
      (should (equal (buffer-string)
                     "// int one;\n// int two;\n")))))

(ert-deftest yunge-comment-uses-org-comment-handling ()
  (yunge-test-enable-evil)
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nText\n")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "g c c"))
      (should (equal (buffer-string)
                     "# * Heading\nText\n")))))

;;; yunge-comment-test.el ends here
