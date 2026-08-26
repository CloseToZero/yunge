;;; yunge-org-reveal-test.el --- Org reveal tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'org)
(require 'view)
(require 'yunge-org-reveal)

(defmacro yunge-org-reveal-test--with-buffer (contents &rest body)
  "Create a fontified Org buffer containing CONTENTS and evaluate BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((org-mode-hook nil))
       (org-mode))
     (setq-local org-hidden-keywords '(title)
                 org-hide-emphasis-markers t
                 org-link-descriptive t
                 org-pretty-entities t
                 org-pretty-entities-include-sub-superscripts t)
     (insert ,contents)
     (font-lock-ensure)
     ,@body))

(defun yunge-org-reveal-test--position (text)
  "Return the beginning position of the first occurrence of TEXT."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (- (point) (length text))))

(defun yunge-org-reveal-test--finish-command ()
  "Notify buffer-local modes after a test command moves point."
  (let ((this-command 'forward-char))
    (run-hooks 'post-command-hook)))

(ert-deftest yunge-org-reveal-shows-and-restores-emphasis-markers ()
  (yunge-org-reveal-test--with-buffer "before *bold* after"
    (let ((marker (yunge-org-reveal-test--position "*bold*")))
      (should (invisible-p marker))
      (goto-char (+ marker 2))
      (yunge-org-reveal-mode 1)
      (should-not (invisible-p marker))
      (goto-char (point-max))
      (yunge-org-reveal-test--finish-command)
      (should (invisible-p marker)))))

(ert-deftest yunge-org-reveal-shows-and-restores-link-source ()
  (yunge-org-reveal-test--with-buffer
      "before [[https://example.test][description]] after"
    (let ((beginning (yunge-org-reveal-test--position "[[")))
      (should (invisible-p beginning))
      (goto-char (yunge-org-reveal-test--position "description"))
      (yunge-org-reveal-mode 1)
      (should-not (invisible-p beginning))
      (goto-char (point-max))
      (yunge-org-reveal-test--finish-command)
      (should (invisible-p beginning)))))

(ert-deftest yunge-org-reveal-shows-entity-and-script-source ()
  (yunge-org-reveal-test--with-buffer "\\alpha and x_{12}"
    (let ((entity (yunge-org-reveal-test--position "\\alpha"))
          (script (yunge-org-reveal-test--position "_{12}")))
      (should (find-composition entity))
      (goto-char entity)
      (yunge-org-reveal-mode 1)
      (should-not (find-composition entity))
      (goto-char (+ script 2))
      (yunge-org-reveal-test--finish-command)
      (should (find-composition entity))
      (should-not (invisible-p script))
      (should-not (get-char-property (+ script 2) 'display))
      (goto-char (point-max))
      (yunge-org-reveal-test--finish-command)
      (should (invisible-p script))
      (should (get-char-property (+ script 2) 'display)))))

(ert-deftest yunge-org-reveal-shows-and-restores-hidden-keywords ()
  (yunge-org-reveal-test--with-buffer "#+title: Example\nbody"
    (let ((beginning (point-min)))
      (should (invisible-p beginning))
      (goto-char beginning)
      (yunge-org-reveal-mode 1)
      (should-not (invisible-p beginning))
      (goto-char (point-max))
      (yunge-org-reveal-test--finish-command)
      (should (invisible-p beginning)))))

(ert-deftest yunge-org-reveal-keeps-presentation-at-point-in-view-mode ()
  (yunge-org-reveal-test--with-buffer
      "[[https://example.test][description]]"
    (let ((beginning (point-min)))
      (goto-char (yunge-org-reveal-test--position "description"))
      (yunge-org-reveal-mode 1)
      (should-not (invisible-p beginning))
      (view-mode 1)
      (should buffer-read-only)
      (should (invisible-p beginning))
      (goto-char (yunge-org-reveal-test--position "description"))
      (yunge-org-reveal-test--finish-command)
      (should (invisible-p beginning))
      (view-mode -1)
      (should-not buffer-read-only)
      (should-not (invisible-p beginning))
      (yunge-org-reveal-mode -1)
      (goto-char (point-max))
      (yunge-org-reveal-test--finish-command)
      (goto-char (yunge-org-reveal-test--position "description"))
      (yunge-org-reveal-test--finish-command)
      (should (invisible-p beginning)))))

(ert-deftest yunge-org-reveal-shows-entity-source-inside-latex ()
  (yunge-org-reveal-test--with-buffer "before \\( x \\in R \\) after"
    (let ((entity (yunge-org-reveal-test--position "\\in")))
      (should (find-composition entity))
      (goto-char (1+ entity))
      (yunge-org-reveal-mode 1)
      (should-not (find-composition entity))
      (yunge-org-reveal-test--finish-command)
      (should-not (find-composition entity))
      (goto-char (point-max))
      (yunge-org-reveal-test--finish-command)
      (should (find-composition entity)))))

(ert-deftest yunge-org-reveal-updates-after-editing-in-place ()
  (yunge-org-reveal-test--with-buffer "[[target][description]]"
    (let ((beginning (point-min)))
      (goto-char (yunge-org-reveal-test--position "description"))
      (yunge-org-reveal-mode 1)
      (insert "new-")
      (font-lock-flush (point-min) (point-max))
      (font-lock-ensure)
      (yunge-org-reveal-test--finish-command)
      (should-not (invisible-p beginning))
      (goto-char (point-max))
      (yunge-org-reveal-test--finish-command)
      (should (invisible-p beginning))
      (should (invisible-p (1- (point-max)))))))

(ert-deftest yunge-org-reveal-works-across-evil-editing-states ()
  (yunge-test-enable-evil)
  (yunge-org-reveal-test--with-buffer "[[target][description]]"
    (let ((beginning (point-min)))
      (goto-char (yunge-org-reveal-test--position "description"))
      (evil-normal-state)
      (yunge-org-reveal-mode 1)
      (should (eq evil-state 'normal))
      (should-not (invisible-p beginning))
      (evil-insert-state)
      (yunge-org-reveal-test--finish-command)
      (should (eq evil-state 'insert))
      (should-not (invisible-p beginning)))))

(provide 'yunge-org-reveal-test)

;;; yunge-org-reveal-test.el ends here
