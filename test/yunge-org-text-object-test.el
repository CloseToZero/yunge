;;; yunge-org-text-object-test.el --- Tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-range-beginning "evil-common")
(declare-function evil-range-end "evil-common")

(defun yunge-org-text-object-test--load-config ()
  "Load the Org text object configuration."
  (yunge-test-enable-evil)
  (require 'yunge-org)
  (require 'org)
  (require 'yunge-org-text-object))

(defun yunge-org-text-object-test--contents (command &optional count)
  "Return the text selected by COMMAND with COUNT."
  (let* ((range (funcall command (or count 1)))
         (beginning (evil-range-beginning range))
         (end (evil-range-end range)))
    (should beginning)
    (buffer-substring-no-properties beginning end)))

(ert-deftest yunge-org-loads-text-objects-after-org-and-evil ()
  (yunge-test-run-emacs
   "-L" (expand-file-name "test" yunge-test-root)
   "-l" "yunge-test-helper"
   "--eval" "(defmacro elpaca (&rest _body) nil)"
   "-l" "yunge-org"
   "--eval"
   (prin1-to-string
    '(progn
       (require 'org)
       (when (featurep 'yunge-org-text-object)
         (error "Org text objects loaded before Evil"))
       (require 'evil)
       (unless (featurep 'yunge-org-text-object)
         (error "Org text objects did not load"))))))

(ert-deftest yunge-org-configures-structural-text-objects ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (dolist (state '(operator visual))
      (yunge-test-keymap-keys
       (evil-get-auxiliary-keymap org-mode-map state t)
       '(("ae" . yunge-org-a-context)
         ("ie" . yunge-org-inner-context)
         ("aE" . yunge-org-an-element)
         ("iE" . yunge-org-inner-element)
         ("ac" . yunge-org-a-container)
         ("ic" . yunge-org-inner-container)
         ("ah" . yunge-org-a-subtree)
         ("ih" . yunge-org-inner-subtree))))))

(ert-deftest yunge-org-selects-link-context-and-paragraph-element ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "Before [[id:theorem][theorem]] after.\n")
    (goto-char (point-min))
    (search-forward "theorem]")
    (backward-char 2)
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-context)
      "[[id:theorem][theorem]] "))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-inner-context)
      "theorem"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-an-element)
      "Before [[id:theorem][theorem]] after.\n"))))

(ert-deftest yunge-org-selects-source-block-syntax-and-body ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n")
    (goto-char (point-min))
    (search-forward "+ 1")
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-an-element)
      "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-inner-element)
      "(+ 1 2)\n"))))

(ert-deftest yunge-org-selects-latex-environment-body ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\nx+y\n\\end{equation}\n")
    (goto-char (point-min))
    (search-forward "x+y")
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-inner-element)
      "x+y\n"))))

(ert-deftest yunge-org-prunes-blank-lines-from-inner-elements ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "#+caption: Example\n\nBody\n")
    (goto-char (point-min))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-inner-element)
      "#+caption: Example"))))

(ert-deftest yunge-org-selects-list-item-and-list-container ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "- Parent\n  - Child\n- Sibling\n")
    (goto-char (point-min))
    (search-forward "Child")
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-container)
      "  - Child\n"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-container 2)
      "- Parent\n  - Child\n"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-container 3)
      "- Parent\n  - Child\n- Sibling\n"))))

(ert-deftest yunge-org-selects-table-cell-row-and-table ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "| Name | Value |\n| One  | Two   |\n")
    (goto-char (point-min))
    (search-forward "Two")
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-context)
      " Two   |"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-inner-context)
      "Two"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-an-element)
      "| One  | Two   |\n"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-container)
      "| Name | Value |\n| One  | Two   |\n"))))

(ert-deftest yunge-org-selects-adjacent-contexts-with-count ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "[[id:one][One]] then [[id:two][Two]]")
    (goto-char (+ (point-min) 4))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-context 2)
      "[[id:one][One]] then [[id:two][Two]]"))))

(ert-deftest yunge-org-selects-heading-body-and-subtree ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nIntro\n** Child\nBody\n* Next\nAfter\n")
    (goto-char (point-min))
    (search-forward "Body")
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-subtree)
      "** Child\nBody\n"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-inner-subtree)
      "Body\n"))
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-subtree 2)
      "* Parent\nIntro\n** Child\nBody\n"))))

(ert-deftest yunge-org-selects-latex-with-and-without-delimiters ()
  (yunge-org-text-object-test--load-config)
  (dolist (case '(("$x+y$" . "x+y")
                  ("$$x+y$$" . "x+y")
                  ("\\(x+y\\)" . "x+y")
                  ("\\[x+y\\]" . "x+y")))
    (with-temp-buffer
      (org-mode)
      (insert (car case))
      (goto-char (+ (point-min) 2))
      (should
       (equal
        (yunge-org-text-object-test--contents
         #'yunge-org-a-context)
        (car case)))
      (should
       (equal
        (yunge-org-text-object-test--contents
         #'yunge-org-inner-context)
        (cdr case))))))

(ert-deftest yunge-org-subtree-text-object-works-while-folded ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody\n* Next\n")
    (goto-char (point-min))
    (org-fold-hide-subtree)
    (should
     (equal
      (yunge-org-text-object-test--contents
       #'yunge-org-a-subtree)
      "* Heading\nBody\n"))))

(ert-deftest yunge-org-text-objects-drive-evil-operators ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* First\nBody\n* Second\nKeep\n")
    (goto-char (point-min))
    (forward-line 1)
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "d a h")))
    (should (equal (buffer-string) "* Second\nKeep\n"))))

(ert-deftest yunge-org-container-expands-in-visual-state ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "- Parent\n  - Child\n- Sibling\n")
    (goto-char (point-min))
    (search-forward "Child")
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "v a c"))
      (execute-kbd-macro (kbd "a c"))
      (execute-kbd-macro (kbd "d")))
    (should (equal (buffer-string) "- Sibling\n"))))

(ert-deftest yunge-org-context-expands-backward-in-visual-state ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "[[id:one][One]] [[id:two][Two]] [[id:three][Three]]")
    (goto-char (point-min))
    (search-forward "Two")
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "v a e"))
      (execute-kbd-macro (kbd "o"))
      (execute-kbd-macro (kbd "a e"))
      (execute-kbd-macro (kbd "d")))
    (should
     (equal
      (buffer-string)
      "[[id:three][Three]]"))))

(ert-deftest yunge-org-keeps-standard-evil-text-objects ()
  (yunge-org-text-object-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "one two")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "d a w")))
    (should (equal (buffer-string) "two"))))

;;; yunge-org-text-object-test.el ends here
