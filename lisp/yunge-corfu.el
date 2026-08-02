;;; yunge-corfu.el --- In-buffer completion -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function global-corfu-mode "corfu")

(defvar completion-in-region-mode)
(defvar corfu-auto)
(defvar corfu-auto-delay)
(defvar corfu-auto-prefix)
(defvar corfu-cycle)
(defvar corfu-map)
(defvar corfu-mode)
(defvar corfu-preview-current)
(defvar text-mode-ispell-word-completion)

;; Ispell launches an external dictionary search for prose completion, which
;; is too costly to run automatically while typing.
(setq text-mode-ispell-word-completion nil)

(defconst yunge-corfu-insert-bindings
  '(("C-j" corfu-next "next candidate")
    ("C-k" corfu-previous "previous candidate")
    ("TAB" corfu-complete "complete candidate")
    ("<tab>" corfu-complete nil)))

(define-minor-mode yunge-corfu--completion-mode
  "Give Corfu's active completion session precedence over Evil."
  :init-value nil
  :lighter nil)

(defun yunge-corfu--sync-completion-mode ()
  "Track whether Corfu owns the active completion session."
  (yunge-corfu--completion-mode
   (if (and completion-in-region-mode corfu-mode) 1 -1)))

(defun yunge-corfu--setup-keys ()
  "Set up bindings for Corfu."
  (add-hook 'completion-in-region-mode-hook
            #'yunge-corfu--sync-completion-mode))

(with-eval-after-load 'corfu
  (yunge-corfu--setup-keys))

(with-eval-after-load 'evil
  (with-eval-after-load 'corfu
    ;; This adapter follows Corfu's popup, so these bindings become active
    ;; without refreshing all of Evil's keymaps for every completion session.
    (yunge-key-evil-define-minor-mode
     'insert 'yunge-corfu--completion-mode
     yunge-corfu-insert-bindings)))

(elpaca corfu
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preview-current nil)
  (global-corfu-mode 1))

(provide 'yunge-corfu)

;;; yunge-corfu.el ends here
