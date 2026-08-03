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

(defconst yunge-corfu-popup-bindings
  '(("C-j" corfu-next "next candidate")
    ("C-k" corfu-previous "previous candidate")
    ("RET" corfu-insert "accept candidate")
    ("<return>" corfu-insert nil)
    ("TAB" corfu-complete "complete candidate")
    ("<tab>" corfu-complete nil)))

(defvar-keymap yunge-corfu--completion-mode-map
  :doc "Keymap active while Corfu owns a completion session.")

(yunge-key-define yunge-corfu--completion-mode-map
                  yunge-corfu-popup-bindings)

(defvar yunge-corfu--emulation-map-alist
  `((yunge-corfu--completion-mode
     . ,yunge-corfu--completion-mode-map)))

(define-minor-mode yunge-corfu--completion-mode
  "Give Corfu's active completion session precedence over Evil."
  :init-value nil
  :lighter nil
  :keymap yunge-corfu--completion-mode-map)

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
    ;; An active completion interface owns acceptance even when the surrounding
    ;; mode, such as a REPL, binds RET in its own Evil minor-mode map.
    (add-to-list 'emulation-mode-map-alists
                 'yunge-corfu--emulation-map-alist)))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-corfu--completion-mode-map yunge-corfu-popup-bindings))

(elpaca corfu
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preview-current nil)
  (global-corfu-mode 1))

(provide 'yunge-corfu)

;;; yunge-corfu.el ends here
