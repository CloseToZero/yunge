;;; yunge-org-test.el --- Org integration tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar yunge-avy-candidate-project-functions)

(declare-function yunge-avy-projection-beginning
                  "yunge-avy" (projection))
(declare-function yunge-avy-projection-end
                  "yunge-avy" (projection))
(declare-function yunge-avy-projection-identity
                  "yunge-avy" (projection))
(declare-function yunge-avy-projection-target
                  "yunge-avy" (projection))

(defun yunge-org-test--load-config ()
  "Load the Org configuration after enabling Evil."
  (yunge-test-enable-evil)
  (require 'yunge-org))

(yunge-test-deftest-lazy-load yunge-org
  (org ob-core ol org-id shuying shuying-org which-key))

(ert-deftest yunge-org-registers-shuying-as-an-avy-projection-provider ()
  (yunge-org-test--load-config)
  (yunge-test-load-package-config 'yunge-avy)
  (require 'shuying-org)
  (should (memq #'yunge-org--avy-shuying-projection
                yunge-avy-candidate-project-functions))
  (with-temp-buffer
    (insert "before $x$ after")
    (let ((overlay (make-overlay 8 11)))
      (overlay-put overlay 'shuying-org t)
      (overlay-put overlay 'display 'image)
      (let ((projection
             (yunge-org--avy-shuying-projection 9 10 nil)))
        (should (eq (yunge-avy-projection-identity projection)
                    overlay))
        (should (= (yunge-avy-projection-beginning projection) 8))
        (should (= (yunge-avy-projection-end projection) 9))
        (should (= (yunge-avy-projection-target projection) 9))))))

(ert-deftest yunge-org-uses-text-first-alignment-settings ()
  (yunge-org-test--load-config)
  (should-not org-auto-align-tags)
  (should (zerop org-tags-column)))

(ert-deftest yunge-org-keeps-table-navigation-without-realignment ()
  (yunge-org-test--load-config)
  (should-not org-table-automatic-realign)
  (with-temp-buffer
    (org-mode)
    (insert "| a|b |\n| longer|c |\n")
    (goto-char (point-min))
    (search-forward "a")
    (let ((contents (buffer-string))
          (org-table-may-need-update t))
      (org-table-next-field)
      (should (equal (buffer-string) contents))
      (should (= (org-table-current-column) 2)))))

(ert-deftest yunge-org-configures-structural-evil-bindings ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (evil-normal-state)
    (should shuying-org-mode)
    (should
     (memq #'shuying-org--schedule-visible-preview
           post-command-hook))
    (should
     (memq #'shuying-org--window-buffer-changed
           window-buffer-change-functions))
    (shuying-org-mode -1)
    (should-not
     (memq #'shuying-org--schedule-visible-preview
           post-command-hook))
    (should-not
     (memq #'shuying-org--window-buffer-changed
           window-buffer-change-functions))
    (shuying-org-mode 1)
    (should (eq (command-remapping 'org-latex-preview)
                #'shuying-org-preview))
    (yunge-test-evil-keys
     'normal
     '(("0" . yunge-org-beginning-of-line)
       ("$" . yunge-org-end-of-line)
       ("RET" . org-open-at-point)
       ("<C-return>" . yunge-org-insert-heading-below)
       ("<C-S-return>" . yunge-org-insert-todo-heading-below)
       ("I" . yunge-org-insert-line)
       ("A" . yunge-org-append-line)
       ("o" . yunge-org-open-below)
       ("O" . yunge-org-open-above)
       ("<" . yunge-org-shift-left)
       (">" . yunge-org-shift-right)
       ("C-j" . org-next-visible-heading)
       ("C-k" . org-previous-visible-heading)
       ("gf" . org-open-at-point)
       ("gh" . org-up-element)
       ("gl" . org-down-element)
       ("gH" . yunge-org-top-heading)
       ("gj" . evil-next-visual-line)
       ("gk" . evil-previous-visual-line)
       ("[E" . org-backward-element)
       ("]E" . org-forward-element)
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
       ("<" . yunge-org-shift-left)
       (">" . yunge-org-shift-right)
       ("gh" . org-up-element)
       ("gl" . org-down-element)
       ("gH" . yunge-org-top-heading)
       ("[E" . org-backward-element)
       ("]E" . org-forward-element)
       ("[h" . org-backward-heading-same-level)
       ("]h" . org-forward-heading-same-level)
       ("[l" . org-previous-link)
       ("]l" . org-next-link)
       ("[c" . org-babel-previous-src-block)
       ("]c" . org-babel-next-src-block)))
    (evil-insert-state)
    (yunge-test-keys
     '(("C-d" . yunge-org-shift-left-line)
       ("C-t" . yunge-org-shift-right-line)))))

(ert-deftest yunge-org-configures-local-commands ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("SPC m p" . shuying-org-preview)
       ("SPC m P" . shuying-org-preview-buffer)
       ("SPC m t" . org-todo)))))

(ert-deftest yunge-org-moves-to-the-outermost-heading ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Child\n*** Grandchild\nBody\n")
    (goto-char (point-max))
    (forward-line -1)
    (yunge-org-top-heading)
    (should (= (line-number-at-pos) 1))
    (should (looking-at-p "\\* Parent"))))

(ert-deftest yunge-org-registers-structural-navigation-as-motion ()
  (yunge-org-test--load-config)
  (dolist (binding yunge-org-motion-bindings)
    (let ((command (nth 1 binding)))
      (should (evil-get-command-property command :keep-visual))
      (should (eq (evil-get-command-property command :repeat)
                  'motion))))
  (should (evil-get-command-property 'yunge-org-top-heading :jump)))

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

(ert-deftest yunge-org-moves-between-adjacent-elements ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nFirst paragraph.\n\nSecond paragraph.\n")
    (goto-char (point-min))
    (search-forward "First")
    (beginning-of-line)
    (org-forward-element)
    (should (looking-at-p "Second paragraph"))
    (org-backward-element)
    (should (looking-at-p "First paragraph"))))

(ert-deftest yunge-org-moves-between-parent-and-inner-elements ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nParagraph.\n")
    (goto-char (point-min))
    (org-down-element)
    (should (looking-at-p "Paragraph"))
    (org-up-element)
    (should (= (point) (point-min)))))

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

(ert-deftest yunge-org-inserts-headings-after-the-current-contents ()
  (yunge-org-test--load-config)
  (dolist (case
           '((yunge-org-insert-heading-below . "* ")
             (yunge-org-insert-todo-heading-below . "* TODO ")))
    (with-temp-buffer
      (org-mode)
      (insert "* Current\nBody\n** Child\nChild body\n* Next\n")
      (goto-char (point-min))
      (search-forward "Curr")
      (evil-normal-state)
      (call-interactively (car case))
      (should (eq evil-state 'insert))
      (should (looking-back (regexp-quote (cdr case))
                            (line-beginning-position)))
      (should
       (equal
        (buffer-substring-no-properties
         (line-beginning-position)
         (point-max))
        (concat (cdr case) "\n* Next\n"))))))

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
    (insert "| First |x|\n| Last  |long |\n")
    (goto-char (point-min))
    (evil-normal-state)
    (yunge-org-open-below 1)
    (should (eq evil-state 'insert))
    (should
     (equal (buffer-string)
            "| First |x|\n|  |  |\n| Last  |long |\n"))
    (evil-normal-state)
    (goto-char (point-min))
    (yunge-org-open-above 1)
    (should
     (equal (buffer-string)
            "|  |  |\n| First |x|\n|  |  |\n| Last  |long |\n"))))

(ert-deftest yunge-org-inserts-minimum-width-table-columns ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "| a|long |\n|--+-----|\n| wider |b |\n")
    (goto-char (point-min))
    (search-forward "a")
    (org-table-insert-column)
    (should
     (equal (buffer-string)
            "|  | a|long |\n|--+--+-----|\n|  | wider |b |\n"))))

(ert-deftest yunge-org-deletes-table-columns-without-realignment ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "|  | a|long |\n|--+--+-----|\n|  | wider |b |\n")
    (goto-char (point-min))
    (search-forward "  ")
    (org-table-delete-column)
    (should
     (equal (buffer-string)
            "| a|long |\n|--+-----|\n| wider |b |\n"))))

(ert-deftest yunge-org-moves-table-columns-without-realignment ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "| a|long |\n|--+-----|\n| wider |b |\n")
    (goto-char (point-min))
    (search-forward "a")
    (org-table-move-column)
    (should
     (equal (buffer-string)
            "|long | a|\n|-----+--|\n|b | wider |\n"))))

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
