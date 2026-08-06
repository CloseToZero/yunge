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
