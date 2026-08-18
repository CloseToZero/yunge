;;; yunge-pair-test.el --- Paired delimiter tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-insert-state "evil-states" ())
(declare-function evil-normal-state "evil-states" ())
(declare-function evil-replace-state "evil-states" ())
(declare-function org-mode "org" ())
(declare-function smartparens-mode "smartparens" (&optional arg))

(defvar smartparens-mode)

(yunge-test-deftest-lazy-load yunge-pair
  (evil org smartparens smartparens-config smartparens-org))

(defun yunge-pair-test--load-config ()
  "Load the paired delimiter configuration synchronously for a test."
  (yunge-test-enable-evil)
  (require 'org)
  (require 'smartparens-autoloads)
  (yunge-test-load-package-config 'yunge-pair))

(defmacro yunge-pair-test--with-org-buffer (&rest body)
  "Evaluate BODY in a displayed Org buffer in Evil Insert state."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (save-window-excursion
       (switch-to-buffer (current-buffer))
       (org-mode)
       (evil-insert-state)
       ,@body)))

(ert-deftest yunge-pair-enables-supported-modes ()
  (yunge-pair-test--load-config)
  (with-temp-buffer
    (org-mode)
    (should smartparens-mode))
  (with-temp-buffer
    (emacs-lisp-mode)
    (should smartparens-mode))
  (with-temp-buffer
    (text-mode)
    (should-not smartparens-mode)))

(ert-deftest yunge-pair-inserts-latex-delimiters-in-org ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (execute-kbd-macro "\\(")
    (should (equal (buffer-string) "\\(\\)"))
    (should (= (point) 3))
    (erase-buffer)
    (execute-kbd-macro "\\[")
    (should (equal (buffer-string) "\\[\\]"))
    (should (= (point) 3))))

(ert-deftest yunge-pair-space-expands-latex-delimiters ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (execute-kbd-macro "\\( ")
    (should (equal (buffer-string) "\\(  \\)"))
    (should (= (point) 4))
    (erase-buffer)
    (execute-kbd-macro "\\[ ")
    (should (equal (buffer-string) "\\[  \\]"))
    (should (= (point) 4))))

(ert-deftest yunge-pair-space-keeps-ordinary-spacing ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (insert "ordinary")
    (execute-kbd-macro " ")
    (should (equal (buffer-string) "ordinary "))))

(ert-deftest yunge-pair-inserts-ordinary-delimiters-in-org ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (execute-kbd-macro "(A)")
    (should (equal (buffer-string) "(A)"))
    (should (= (point) (point-max)))))

(ert-deftest yunge-pair-closing-key-skips-latex-delimiter ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (execute-kbd-macro "\\( A)")
    (should (equal (buffer-string) "\\( A \\)"))
    (should (= (point) (point-max)))
    (erase-buffer)
    (execute-kbd-macro "\\[ A]")
    (should (equal (buffer-string) "\\[ A \\]"))
    (should (= (point) (point-max)))))

(ert-deftest yunge-pair-backspace-deletes-empty-latex-pair ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (execute-kbd-macro "\\(")
    (execute-kbd-macro (kbd "DEL"))
    (should (equal (buffer-string) ""))
    (execute-kbd-macro "\\( ")
    (execute-kbd-macro (kbd "DEL"))
    (should (equal (buffer-string) ""))
    (insert "ordinary")
    (execute-kbd-macro (kbd "DEL"))
    (should (equal (buffer-string) "ordinar"))))

(ert-deftest yunge-pair-adds-selected-org-markup-pairs ()
  (yunge-pair-test--load-config)
  (dolist (delimiter '("*" "_" "/" "~" "="))
    (yunge-pair-test--with-org-buffer
      (insert "text ")
      (execute-kbd-macro delimiter)
      (should
       (equal (buffer-string)
              (concat "text " delimiter delimiter)))
      (should (= (point) (1- (point-max)))))))

(ert-deftest yunge-pair-space-abandons-org-markup-pair ()
  (yunge-pair-test--load-config)
  (dolist (delimiter '("*" "_" "/" "~" "="))
    (yunge-pair-test--with-org-buffer
      (insert "text ")
      (execute-kbd-macro (concat delimiter " "))
      (should
       (equal (buffer-string)
              (concat "text " delimiter " "))))))

(ert-deftest yunge-pair-leaves-org-link-slash-unpaired ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (insert "[")
    (execute-kbd-macro "/")
    (should (equal (buffer-string) "[/"))))

(ert-deftest yunge-pair-leaves-org-heading-marker-unpaired ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (execute-kbd-macro "*")
    (should (equal (buffer-string) "*"))))

(ert-deftest yunge-pair-leaves-org-literal-contexts-unpaired ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (insert "#+begin_src emacs-lisp\n\n#+end_src")
    (goto-char (point-min))
    (forward-line 1)
    (execute-kbd-macro "=")
    (should (equal (buffer-string)
                   (concat "#+begin_src emacs-lisp\n"
                           "=\n"
                           "#+end_src"))))
  (yunge-pair-test--with-org-buffer
    (execute-kbd-macro "\\( =")
    (should (equal (buffer-string) "\\( = \\)"))))

(ert-deftest yunge-pair-disables-itself-in-replace-state ()
  (yunge-pair-test--load-config)
  (yunge-pair-test--with-org-buffer
    (should smartparens-mode)
    (evil-replace-state)
    (should-not smartparens-mode)
    (evil-normal-state)
    (should smartparens-mode)))

(ert-deftest yunge-pair-does-not-load-broad-default-config ()
  (yunge-pair-test--load-config)
  (should-not (featurep 'smartparens-config))
  (should-not (featurep 'smartparens-org)))

;;; yunge-pair-test.el ends here
