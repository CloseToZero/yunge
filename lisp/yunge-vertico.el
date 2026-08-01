;;; yunge-vertico.el --- Vertical completion -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function evil-get-auxiliary-keymap "evil-core")
(declare-function vertico-mode "vertico")
(declare-function vertico-next "vertico")
(declare-function vertico-previous "vertico")

(defvar vertico-count)
(defvar vertico-map)

(defconst yunge-vertico-normal-bindings
  '(("j" vertico-next "next candidate")
    ("k" vertico-previous "previous candidate")
    ("<down>" vertico-next nil)
    ("<up>" vertico-previous nil)
    ("gg" vertico-first "first candidate or prompt")
    ("G" vertico-last "last candidate")
    ("C-d" yunge-vertico-next-half-page "next half-page")
    ("C-u" yunge-vertico-previous-half-page "previous half-page")
    ("C-f" vertico-scroll-up "next page")
    ("C-b" vertico-scroll-down "previous page")
    ("TAB" vertico-insert "insert candidate")
    ("<tab>" vertico-insert nil)))

(defconst yunge-vertico-insert-bindings
  '(("C-n" vertico-next "next candidate")
    ("C-p" vertico-previous "previous candidate")
    ("TAB" vertico-insert "insert candidate")
    ("<tab>" vertico-insert nil)))

(defun yunge-vertico-next-half-page (&optional count)
  "Move forward COUNT half-pages through Vertico candidates."
  (interactive "p")
  (vertico-next (* (or count 1) (max 1 (/ vertico-count 2)))))

(defun yunge-vertico-previous-half-page (&optional count)
  "Move backward COUNT half-pages through Vertico candidates."
  (interactive "p")
  (vertico-previous (* (or count 1) (max 1 (/ vertico-count 2)))))

(defun yunge-vertico--setup-keys ()
  "Set up Evil bindings for Vertico."
  (yunge-key-evil-define 'normal vertico-map
                         yunge-vertico-normal-bindings)
  (yunge-key-evil-define 'insert vertico-map
                         yunge-vertico-insert-bindings))

(with-eval-after-load 'evil
  (with-eval-after-load 'vertico
    (yunge-vertico--setup-keys)))

(with-eval-after-load 'which-key
  (with-eval-after-load 'evil
    (with-eval-after-load 'vertico
      (dolist (entry `((normal ,yunge-vertico-normal-bindings)
                       (insert ,yunge-vertico-insert-bindings)))
        (yunge-key-which-key-describe-map
         (evil-get-auxiliary-keymap vertico-map (car entry) t t)
         (cadr entry))))))

(elpaca vertico
  (vertico-mode 1))

(provide 'yunge-vertico)

;;; yunge-vertico.el ends here
