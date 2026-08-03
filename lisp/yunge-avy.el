;;; yunge-avy.el --- IME-friendly visible text jumps -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'subr-x)
(require 'yunge-evil)
(require 'yunge-input-source)
(require 'yunge-key)

(declare-function avy-jump "avy" (regex &rest arguments))
(declare-function evil-add-command-properties "evil-common")

(defvar avy-action)
(defvar avy-command)

(defconst yunge-avy-bindings
  '(("j" yunge-avy-jump-to-text "jump to text")))

(defun yunge-avy-jump-to-text (text)
  "Jump to visible literal TEXT after reading it in the minibuffer."
  (interactive (list (read-from-minibuffer "Jump to text: ")))
  (when (string-empty-p text)
    (user-error "Jump text cannot be empty"))
  (require 'avy)
  (let ((avy-action nil)
        (avy-command 'yunge-avy-jump-to-text))
    (yunge-input-source-call-with-ascii
     (lambda ()
       (avy-jump (regexp-quote text))))))

(defun yunge-avy--setup ()
  "Expose the IME-friendly Avy command after Avy is ready."
  (yunge-key-define yunge-jump-map yunge-avy-bindings)
  (with-eval-after-load 'evil
    (evil-add-command-properties 'yunge-avy-jump-to-text
                                 :jump nil :repeat nil)
    (yunge-jump-history-track-command 'yunge-avy-jump-to-text))
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-jump-map yunge-avy-bindings)))

(elpaca avy
  (yunge-avy--setup))

(provide 'yunge-avy)

;;; yunge-avy.el ends here
