;;; yunge-tempel-test.el --- Tempel tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function org-cycle "org-cycle" (&optional arg))
(declare-function tempel-expand "tempel" (&optional interactive))
(declare-function tempel-insert "tempel" (template-or-name))

(defvar org-cycle-tab-first-hook)
(defvar tempel-path)

(yunge-test-deftest-lazy-load yunge-tempel
  (org tempel which-key))

(defun yunge-tempel-test--load-config ()
  "Load the Tempel configuration synchronously for a test."
  (yunge-test-enable-evil)
  (require 'tempel-autoloads)
  (yunge-test-load-package-config 'yunge-tempel))

(ert-deftest yunge-tempel-catches-up-existing-org-buffers ()
  (yunge-tempel-test--load-config)
  (let ((buffer (generate-new-buffer " *yunge-tempel-existing*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local completion-at-point-functions nil)
          (yunge-tempel--setup-existing-org-buffers)
          (should (equal completion-at-point-functions
                         '(tempel-expand))))
      (kill-buffer buffer))))

(ert-deftest yunge-tempel-configures-org-integration ()
  (yunge-tempel-test--load-config)
  (should
   (equal tempel-path
          (expand-file-name "template/*.eld"
                            yunge-config-directory)))
  (with-temp-buffer
    (org-mode)
    (should (eq (car completion-at-point-functions)
                #'tempel-expand))
    (should (memq #'yunge-tempel--org-tab
                  org-cycle-tab-first-hook))
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal '(("SPC m i" . tempel-insert)))
    (evil-visual-state)
    (yunge-test-evil-keys
     'visual '(("SPC m i" . tempel-insert)))))

(ert-deftest yunge-tempel-expands-an-org-definition ()
  (yunge-tempel-test--load-config)
  (with-temp-buffer
    (org-mode)
    (tempel-insert 'def)
    (should
     (equal (buffer-string)
            "#+begin_definition\n\n#+end_definition"))
    (should (= (point) (1+ (line-end-position 0))))))

(ert-deftest yunge-tempel-tab-expands-an-exact-org-trigger ()
  (yunge-tempel-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "def")
    (org-cycle)
    (should
     (equal (buffer-string)
            "#+begin_definition\n\n#+end_definition"))))

(ert-deftest yunge-tempel-tab-retains-org-cycle-without-a-trigger ()
  (yunge-tempel-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "ordinary text")
    (should-not (yunge-tempel--org-tab))
    (should (equal (buffer-string) "ordinary text"))))

(ert-deftest yunge-tempel-wraps-an-org-definition-around-a-region ()
  (yunge-tempel-test--load-config)
  (with-temp-buffer
    (org-mode)
    (insert "A statement.")
    (set-mark (point-min))
    (activate-mark)
    (tempel-insert 'def)
    (should
     (equal (buffer-string)
            (concat "#+begin_definition\n"
                    "A statement.\n"
                    "#+end_definition")))))

;;; yunge-tempel-test.el ends here
