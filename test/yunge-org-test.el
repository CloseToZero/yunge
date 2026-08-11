;;; yunge-org-test.el --- Org integration tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defun yunge-org-test--load-config ()
  "Load the Org configuration after enabling Evil."
  (yunge-test-enable-evil)
  (require 'yunge-org))

(yunge-test-deftest-lazy-load yunge-org
  (org ob-core ol org-id which-key))

(ert-deftest yunge-org-configures-structural-evil-bindings ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("0" . yunge-org-beginning-of-line)
       ("$" . yunge-org-end-of-line)
       ("RET" . org-open-at-point)
       ("I" . yunge-org-insert-line)
       ("A" . yunge-org-append-line)
       ("o" . yunge-org-open-below)
       ("O" . yunge-org-open-above)
       ("C-j" . org-next-visible-heading)
       ("C-k" . org-previous-visible-heading)
       ("gf" . org-open-at-point)
       ("gj" . evil-next-visual-line)
       ("gk" . evil-previous-visual-line)
       ("[h" . org-backward-heading-same-level)
       ("]h" . org-forward-heading-same-level)
       ("[l" . org-previous-link)
       ("]l" . org-next-link)
       ("[c" . org-babel-previous-src-block)
       ("]c" . org-babel-next-src-block)
       ("m" . evil-set-marker)
       ("q" . evil-record-macro)
       ("<tab>" . org-cycle)
       ("S-TAB" . org-shifttab)
       ("M-h" . org-metaleft)
       ("M-j" . org-metadown)
       ("M-k" . org-metaup)
       ("M-l" . org-metaright)
       ("M-H" . org-shiftmetaleft)
       ("M-L" . org-shiftmetaright)
       ("za" . org-cycle)
       ("zA" . org-shifttab)
       ("zc" . org-fold-hide-subtree)
       ("zC" . yunge-org-close-child-folds)
       ("zo" . yunge-org-open-fold)
       ("zO" . org-fold-show-subtree)))
    (should (eq (key-binding (kbd "C-i"))
                #'yunge-jump-history-forward))
    (evil-visual-state)
    (yunge-test-keys
     '(("0" . yunge-org-beginning-of-line)
       ("$" . yunge-org-end-of-line)
       ("M-h" . org-metaleft)
       ("M-j" . org-metadown)
       ("M-k" . org-metaup)
       ("M-l" . org-metaright)
       ("M-H" . org-shiftmetaleft)
       ("M-L" . org-shiftmetaright)
       ("[h" . org-backward-heading-same-level)
       ("]h" . org-forward-heading-same-level)
       ("[l" . org-previous-link)
       ("]l" . org-next-link)
       ("[c" . org-babel-previous-src-block)
       ("]c" . org-babel-next-src-block)))))

(ert-deftest yunge-org-registers-structural-navigation-as-motion ()
  (yunge-org-test--load-config)
  (dolist (binding yunge-org-motion-bindings)
    (let ((command (nth 1 binding)))
      (should (evil-get-command-property command :keep-visual))
      (should (eq (evil-get-command-property command :repeat)
                  'motion)))))

(ert-deftest yunge-org-configures-line-boundaries-as-evil-motions ()
  (yunge-org-test--load-config)
  (should (eq (evil-get-command-property
               'yunge-org-beginning-of-line :type)
              'exclusive))
  (should (eq (evil-get-command-property
               'yunge-org-end-of-line :type)
              'inclusive)))

(ert-deftest yunge-org-moves-between-structural-line-boundaries ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO [#A] Heading :tag:\n- [ ] Item\nBody\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-beginning-of-line)
    (should (looking-at-p "Heading"))
    (yunge-org-beginning-of-line)
    (should (= (point) (line-beginning-position)))
    (forward-line 1)
    (end-of-line)
    (backward-char)
    (yunge-org-beginning-of-line)
    (should (looking-at-p "Item"))
    (yunge-org-beginning-of-line)
    (should (= (point) (line-beginning-position)))
    (forward-line 1)
    (end-of-line)
    (backward-char)
    (yunge-org-beginning-of-line)
    (should (= (point) (line-beginning-position)))))

(ert-deftest yunge-org-moves-before-and-after-heading-tags ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading :tag:\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-end-of-line 1)
    (should (eq (char-after) ?g))
    (yunge-org-end-of-line 1)
    (should (eq (char-after) ?:))
    (should (= (1+ (point)) (line-end-position)))))

(ert-deftest yunge-org-keeps-heading-syntax-outside-evil-operators ()
  (yunge-org-test--load-config)
  (dolist (case '(("d $" beginning "")
                  ("d 0" end "g")))
    (with-temp-buffer
      (org-mode)
      (insert "* Heading :tag:\n")
      (goto-char (point-min))
      (search-forward "Heading")
      (goto-char
       (if (eq (nth 1 case) 'beginning)
           (match-beginning 0)
         (1- (match-end 0))))
      (evil-normal-state)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro (kbd (car case)))
        (goto-char (point-min))
        (should (equal (org-get-heading t t t t) (nth 2 case)))
        (should (equal (org-get-tags nil t) '("tag")))))))

(ert-deftest yunge-org-stops-at-a-folded-heading-end ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody\n")
    (goto-char (point-min))
    (org-fold-hide-subtree)
    (evil-normal-state)
    (yunge-org-end-of-line 1)
    (should (eq (char-after) ?g))
    (should
     (org-invisible-p
      (save-excursion
        (forward-line 1)
        (point))))))

(ert-deftest yunge-org-keeps-ordinary-end-of-line-semantics ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "Body\n\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-end-of-line 1)
    (should (eq (char-after) ?y))
    (forward-line 1)
    (yunge-org-end-of-line 1)
    (should (eq evil-this-type 'exclusive))))

(ert-deftest yunge-org-inserts-within-heading-structure ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO [#A] Heading :tag:\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-insert-line 1)
    (should (eq evil-state 'insert))
    (should (looking-at-p "Heading"))
    (evil-normal-state)
    (yunge-org-append-line 1)
    (should (eq evil-state 'insert))
    (should (looking-at-p " :tag:"))))

(ert-deftest yunge-org-inserts-after-a-list-item-prefix ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "- [ ] Item\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-insert-line 1)
    (should (eq evil-state 'insert))
    (should (looking-at-p "Item"))))

(ert-deftest yunge-org-appends-before-a-folded-subtree ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody\n")
    (goto-char (point-min))
    (org-fold-hide-subtree)
    (evil-normal-state)
    (yunge-org-append-line 1)
    (should (eq evil-state 'insert))
    (should (= (point) (line-end-position)))
    (insert " tail")
    (should (equal (buffer-string) "* Heading tail\nBody\n"))
    (should (org-invisible-p
             (save-excursion
               (forward-line 1)
               (point))))))

(ert-deftest yunge-org-opens-checkbox-items-structurally ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "- [ ] First\n- [ ] Last\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-open-below 1)
    (should (eq evil-state 'insert))
    (should
     (equal (buffer-string)
            "- [ ] First\n- [ ] \n- [ ] Last\n"))
    (evil-normal-state)
    (goto-char (point-min))
    (yunge-org-open-above 1)
    (should
     (equal (buffer-string)
            "- [ ] \n- [ ] First\n- [ ] \n- [ ] Last\n"))))

(ert-deftest yunge-org-opens-after-the-current-list-item-tree ()
  (yunge-org-test--load-config)
  (dolist (case
           '(("- Parent\n  - Child\n- Last\n"
              "- Parent\n  - Child\n- \n- Last\n")
             ("- Term :: description\n  - Child\n- Last :: final\n"
              "- Term :: description\n  - Child\n- :: \n- Last :: final\n")))
    (with-temp-buffer
      (org-mode)
      (insert (car case))
      (goto-char (point-min))
      (evil-normal-state)
      (yunge-org-open-below 1)
      (should (eq evil-state 'insert))
      (should (equal (buffer-string) (cadr case))))))

(ert-deftest yunge-org-opens-table-rows-structurally ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "| First |\n| Last  |\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-open-below 1)
    (should (eq evil-state 'insert))
    (should
     (equal (buffer-string)
            "| First |\n|       |\n| Last  |\n"))
    (evil-normal-state)
    (goto-char (point-min))
    (yunge-org-open-above 1)
    (should
     (equal (buffer-string)
            "|       |\n| First |\n|       |\n| Last  |\n"))))

(ert-deftest yunge-org-keeps-evil-open-outside-structures ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "Body")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-open-below 1)
    (should (eq evil-state 'insert))
    (should (equal (buffer-string) "Body\n"))))

(ert-deftest yunge-org-controls-fold-depth-explicitly ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Root\nBody\n** Child\nChild body\n*** Grandchild\n")
    (insert "Grandchild body\n* Next\n")
    (goto-char (point-min))
    (let ((body
           (save-excursion
             (search-forward "Body")
             (match-beginning 0)))
          (child
           (save-excursion
             (search-forward "** Child")
             (match-beginning 0)))
          (child-body
           (save-excursion
             (search-forward "Child body")
             (match-beginning 0)))
          (grandchild
           (save-excursion
             (search-forward "*** Grandchild")
             (match-beginning 0))))
      (org-fold-hide-subtree)
      (yunge-org-open-fold)
      (should-not (org-invisible-p body))
      (should-not (org-invisible-p child))
      (should (org-invisible-p child-body))
      (should (org-invisible-p grandchild))
      (org-fold-show-subtree)
      (yunge-org-close-child-folds)
      (should-not (org-invisible-p body))
      (should-not (org-invisible-p child))
      (should (org-invisible-p child-body))
      (should (org-invisible-p grandchild)))))

(ert-deftest yunge-org-configures-precise-link-workflow ()
  (yunge-org-test--load-config)
  (require 'which-key)

  (should org-id-link-consider-parent-id)
  (should (eq org-id-link-to-org-use-id 'create-if-interactive))
  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC n l" . org-insert-link)
     ("SPC n s" . org-store-link)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC n"
   '(("l" nil "insert link")
     ("s" nil "store link")))
  (should
   (advice-member-p #'yunge-org--insert-link-at-normal-state-eol
                    'org-insert-link)))

(ert-deftest yunge-org-inserts-after-the-normal-state-eol-character ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "Theorem:")
    (backward-char)
    (evil-normal-state)
    (org-insert-link nil "id:theorem" "A theorem")
    (should
     (equal (buffer-string)
            "Theorem:[[id:theorem][A theorem]]"))))

(ert-deftest yunge-org-edits-a-link-at-the-normal-state-eol ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "[[id:theorem][A theorem]]")
    (backward-char)
    (evil-normal-state)
    (let ((origin (point))
          called-at)
      (yunge-org--insert-link-at-normal-state-eol
       (lambda (&rest _arguments)
         (setq called-at (point))))
      (should (= called-at origin)))))

(ert-deftest yunge-org-restores-point-when-link-insertion-is-cancelled ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "Theorem:")
    (backward-char)
    (evil-normal-state)
    (let ((origin (point)))
      (condition-case nil
          (progn
            (yunge-org--insert-link-at-normal-state-eol
             (lambda (&rest _arguments)
               (signal 'quit nil)))
            (ert-fail "Link insertion did not quit"))
        (quit nil))
      (should (= (point) origin)))))

;;; yunge-org-test.el ends here
