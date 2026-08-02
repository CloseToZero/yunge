;;; yunge-help.el --- Help navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(defvar help-map)
(defvar help-mode-map)
(defvar help-window-select)

(setq help-window-select t)

(defconst yunge-help-command-bindings
  '(("b" describe-bindings "bindings")
    ("c" describe-key-briefly "key briefly")
    ("f" describe-function "function")
    ("i" info "info")
    ("k" describe-key "key")
    ("m" describe-mode "mode")
    ("o" describe-symbol "symbol")
    ("v" describe-variable "variable")))

(defconst yunge-help-normal-bindings
  `(("RET" push-button "activate button")
    ("q" quit-window "quit")
    ("gr" revert-buffer "refresh")
    ("gh" help-go-back "history back")
    ("gl" help-go-forward "history forward")
    ("gf" help-view-source "visit source")
    ,@yunge-key-button-navigation-bindings))

(with-eval-after-load 'evil
  (with-eval-after-load 'help-mode
    (yunge-key-evil-define 'normal help-mode-map
                           yunge-help-normal-bindings)))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   help-map yunge-help-command-bindings))

(provide 'yunge-help)

;;; yunge-help.el ends here
