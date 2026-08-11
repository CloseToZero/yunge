;;; yunge-org-delete-test.el --- Tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-get-register "evil-common")
(declare-function evil-set-register "evil-common")

(defun yunge-org-delete-test--load-config ()
  "Load the Org deletion configuration."
  (yunge-test-enable-evil)
  (require 'yunge-org)
  (require 'org)
  (require 'yunge-org-delete))

(defmacro yunge-org-delete-test--with-keys (keys &rest body)
  "Execute KEYS in the current buffer, then evaluate BODY."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (switch-to-buffer (current-buffer))
     (execute-kbd-macro (kbd ,keys))
     ,@body))

(ert-deftest yunge-org-loads-delete-integration-after-org-and-evil ()
  (yunge-test-run-emacs
   "-L" (expand-file-name "test" yunge-test-root)
   "-l" "yunge-test-helper"
   "--eval" "(defmacro elpaca (&rest _body) nil)"
   "-l" "yunge-org"
   "--eval"
   (prin1-to-string
    '(progn
       (require 'org)
       (when (featurep 'yunge-org-delete)
         (error "Org deletion loaded before Evil"))
       (require 'evil)
       (unless (featurep 'yunge-org-delete)
         (error "Org deletion did not load"))))))

(ert-deftest yunge-org-binds-structure-aware-delete-commands ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("d" . yunge-org-delete)
       ("x" . yunge-org-delete-char)
       ("X" . yunge-org-delete-backward-char)))
    (evil-visual-state)
    (should (eq (key-binding (kbd "d")) #'yunge-org-delete))
    (should (eq (key-binding (kbd "x")) #'evil-delete-char))
    (should (eq (key-binding (kbd "X")) #'evil-delete-line))))

(ert-deftest yunge-org-delete-renumbers-an-ordered-list ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "1. One\n2. Two\n3. Three\n")
    (goto-char (point-min))
    (forward-line 1)
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "d d")
    (should (equal (buffer-string) "1. One\n2. Three\n"))))

(ert-deftest yunge-org-delete-repairs-a-nested-ordered-list ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "1. Parent\n   1. One\n   2. Two\n2. Sibling\n")
    (goto-char (point-min))
    (forward-line 1)
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "d d")
    (should
     (equal
      (buffer-string)
      "1. Parent\n   1. Two\n2. Sibling\n"))))

(ert-deftest yunge-org-visual-delete-renumbers-an-ordered-list ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "1. One\n2. Two\n3. Three\n")
    (goto-char (point-min))
    (forward-line 1)
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "V d")
    (should (equal (buffer-string) "1. One\n2. Three\n"))))

(ert-deftest yunge-org-delete-keeps-heading-tags-aligned ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (let ((org-tags-column 30))
      (insert "* Funny heading :tag:\n")
      (goto-char (point-min))
      (org-align-tags)
      (let ((tag-column
             (save-excursion
               (search-forward ":tag:")
               (current-column))))
        (search-forward "Funny")
        (backward-char)
        (evil-normal-state)
        (yunge-org-delete-test--with-keys "d a w")
        (goto-char (point-min))
        (search-forward ":tag:")
        (should (= (current-column) tag-column))))))

(ert-deftest yunge-org-character-delete-preserves-table-width ()
  (yunge-org-delete-test--load-config)
  (dolist (case '(("x" "b")
                  ("X" "c")))
    (with-temp-buffer
      (org-mode)
      (insert "| abc | def |\n")
      (goto-char (point-min))
      (org-table-align)
      (let ((width (line-end-position)))
        (search-forward (cadr case))
        (backward-char)
        (evil-normal-state)
        (yunge-org-delete-test--with-keys (car case))
        (should (= (line-end-position) width))
        (should-not (string-match-p "b" (buffer-string)))))))

(ert-deftest yunge-org-character-delete-populates-small-register ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "| abc |\n")
    (goto-char (point-min))
    (search-forward "b")
    (backward-char)
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "x")
    (should (equal (evil-get-register ?-) "b"))))

(ert-deftest yunge-org-character-delete-honors-an-explicit-register ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "| abc |\n")
    (goto-char (point-min))
    (search-forward "b")
    (backward-char)
    (evil-set-register ?- "previous")
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "\" a x")
    (should (equal (evil-get-register ?a) "b"))
    (should (equal (evil-get-register ?-) "previous"))))

(ert-deftest yunge-org-delete-honors-an-explicit-register ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "1. One\n2. Two\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "\" a d d")
    (should
     (equal
      (substring-no-properties (evil-get-register ?a))
      "1. One\n"))))

(ert-deftest yunge-org-delete-repeats-with-list-repair ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "1. One\n2. Two\n3. Three\n4. Four\n")
    (goto-char (point-min))
    (forward-line 1)
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "d d .")
    (should (equal (buffer-string) "1. One\n2. Four\n"))))

(ert-deftest yunge-org-character-delete-repeats ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* abc\n")
    (goto-char (point-min))
    (search-forward "a")
    (backward-char)
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "x .")
    (should (equal (buffer-string) "* c\n"))))

(ert-deftest yunge-org-delete-keeps-ordinary-evil-behavior ()
  (yunge-org-delete-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "One two\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-delete-test--with-keys "d a w")
    (should (equal (buffer-string) "two\n"))))

;;; yunge-org-delete-test.el ends here
