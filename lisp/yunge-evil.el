;;; yunge-evil.el --- Evil modal editing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function evil-make-intercept-map "evil-core")

(defvar-keymap yunge-leader-map
  :doc "Global leader map.")

(defvar-keymap yunge-evil--empty-localleader-map
  :doc "Fallback map when the current mode has no local leader.")

(defun yunge-evil--localleader-binding (_binding)
  "Return the current mode's labelled local leader binding."
  (cons "mode"
        (or (key-binding [localleader])
            yunge-evil--empty-localleader-map)))

(keymap-set
 yunge-leader-map "m"
 '(menu-item "mode" nil :filter yunge-evil--localleader-binding))

(defvar-keymap yunge-leader-mode-map)

(define-minor-mode yunge-leader-mode
  "Keep the global leader above ordinary mode-specific Evil maps."
  :global t
  :keymap yunge-leader-mode-map)

(defconst yunge-evil-leader-bindings
  `(("SPC" ,yunge-leader-map nil)))

(defun yunge-evil--setup-leader ()
  "Set up the leader after Evil has loaded."
  (yunge-key-evil-define '(normal visual)
                         yunge-leader-mode-map
                         yunge-evil-leader-bindings)
  ;; Mode-specific Evil maps must not replace the global leader.
  (evil-make-intercept-map yunge-leader-mode-map 'normal t)
  (evil-make-intercept-map yunge-leader-mode-map 'visual t)
  (yunge-leader-mode 1))

(with-eval-after-load 'evil
  (yunge-evil--setup-leader))

(elpaca evil
  ;; Keep Evil's command semantics, but own all mode-specific bindings.
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-search-module 'evil-search
        evil-symbol-word-search t
        evil-undo-system 'undo-redo
        evil-want-C-u-delete t
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil)
  (evil-mode 1))

(provide 'yunge-evil)

;;; yunge-evil.el ends here
