;;; yunge-evil.el --- Evil modal editing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(elpaca evil
  ;; Keep Evil's command semantics, but own all mode-specific bindings.
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-search-module 'evil-search
        evil-symbol-word-search t
        evil-want-C-u-delete t
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil)
  (require 'evil)
  (setopt evil-undo-system 'undo-redo)
  (evil-mode 1))

(provide 'yunge-evil)

;;; yunge-evil.el ends here
