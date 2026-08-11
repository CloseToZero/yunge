;;; yunge-org-test.el --- Org integration tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defun yunge-org-test--load-config ()
  "Load the Org configuration after enabling Evil."
  (yunge-test-enable-evil)
  (require 'yunge-org))

(yunge-test-deftest-lazy-load yunge-org
  (org ol org-id which-key))

(ert-deftest yunge-org-configures-structural-evil-bindings ()
  (yunge-org-test--load-config)
  (with-temp-buffer
    (org-mode)
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("RET" . org-open-at-point)
       ("C-j" . org-next-visible-heading)
       ("C-k" . org-previous-visible-heading)
       ("gf" . org-open-at-point)
       ("gj" . evil-next-visual-line)
       ("gk" . evil-previous-visual-line)
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
     '(("M-h" . org-metaleft)
       ("M-j" . org-metadown)
       ("M-k" . org-metaup)
       ("M-l" . org-metaright)
       ("M-H" . org-shiftmetaleft)
       ("M-L" . org-shiftmetaright)))))

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
