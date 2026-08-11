;;; yunge-org-shift-test.el --- Tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defun yunge-org-shift-test--load-config ()
  "Load the Org shifting configuration."
  (yunge-test-enable-evil)
  (require 'yunge-org)
  (require 'org)
  (require 'yunge-org-shift))

(defmacro yunge-org-shift-test--with-keys (keys &rest body)
  "Execute KEYS in the current buffer, then evaluate BODY."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (switch-to-buffer (current-buffer))
     (execute-kbd-macro (kbd ,keys))
     ,@body))

(ert-deftest yunge-org-loads-shift-integration-after-org-and-evil ()
  (yunge-test-run-emacs
   "-L" (expand-file-name "test" yunge-test-root)
   "-l" "yunge-test-helper"
   "--eval" "(defmacro elpaca (&rest _body) nil)"
   "-l" "yunge-org"
   "--eval"
   (prin1-to-string
    '(progn
       (require 'org)
       (when (featurep 'yunge-org-shift)
         (error "Org shifting loaded before Evil"))
       (require 'evil)
       (unless (featurep 'yunge-org-shift)
         (error "Org shifting did not load"))))))

(ert-deftest yunge-org-binds-structure-aware-shift-operators ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("<" . yunge-org-shift-left)
       (">" . yunge-org-shift-right)))
    (evil-visual-state)
    (should (eq (key-binding (kbd "<")) #'yunge-org-shift-left))
    (should (eq (key-binding (kbd ">")) #'yunge-org-shift-right))))

(ert-deftest yunge-org-shift-operators-remain-linewise ()
  (yunge-org-shift-test--load-config)
  (should (eq (evil-get-command-property
               'yunge-org-shift-left :type)
              'line))
  (should (eq (evil-get-command-property
               'yunge-org-shift-right :type)
              'line)))

(ert-deftest yunge-org-shift-demotes-and-promotes-a-heading ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "> >")
    (should (equal (buffer-string) "** Heading\nBody\n"))
    (yunge-org-shift-test--with-keys "< <")
    (should (equal (buffer-string) "* Heading\nBody\n"))))

(ert-deftest yunge-org-shift-handles-heading-ranges ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Child\n* Next\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "2 > >")
    (should
     (equal
      (buffer-string)
      "** Parent\n*** Child\n* Next\n"))))

(ert-deftest yunge-org-shift-composes-with-org-text-objects ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Child\n* Next\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "> a h")
    (should
     (equal
      (buffer-string)
      "** Parent\n*** Child\n* Next\n"))))

(ert-deftest yunge-org-visual-count-shifts-multiple-heading-levels ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* One\n* Two\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "V j 2 >")
    (should (equal (buffer-string) "*** One\n*** Two\n"))))

(ert-deftest yunge-org-shift-indents-a-list-item-with-children ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "- First\n- Parent\n  - Child\n- Last\n")
    (goto-char (point-min))
    (forward-line 1)
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "> >")
    (should
     (equal
      (buffer-string)
      "- First\n  - Parent\n    - Child\n- Last\n"))
    (yunge-org-shift-test--with-keys "< <")
    (should
     (equal
      (buffer-string)
      "- First\n- Parent\n  - Child\n- Last\n"))))

(ert-deftest yunge-org-shift-indents-selected-list-items ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "- Parent\n- One\n- Two\n- Last\n")
    (goto-char (point-min))
    (forward-line 1)
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "V j >")
    (should
     (equal
      (buffer-string)
      "- Parent\n  - One\n  - Two\n- Last\n"))))

(ert-deftest yunge-org-shift-indents-a-counted-list-range ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "- Parent\n- One\n- Two\n- Last\n")
    (goto-char (point-min))
    (forward-line 1)
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "2 > >")
    (should
     (equal
      (buffer-string)
      "- Parent\n  - One\n  - Two\n- Last\n"))))

(ert-deftest yunge-org-shift-repeats-structural-heading-changes ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "> > .")
    (should (equal (buffer-string) "*** Heading\n"))))

(ert-deftest yunge-org-shift-keeps-ordinary-evil-behavior ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "Body\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "> >")
    (should (> (current-indentation) 0))
    (yunge-org-shift-test--with-keys "< <")
    (should (equal (buffer-string) "Body\n"))))

(ert-deftest yunge-org-shift-does-not-reorder-table-columns ()
  (yunge-org-shift-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "| A | B |\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-shift-test--with-keys "> >")
    (should (string-match-p "| A | B |" (buffer-string)))
    (should (> (current-indentation) 0))))

;;; yunge-org-shift-test.el ends here
