;;; yunge-window.el --- Window management -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)
(require 'yunge-key-control)

(defconst yunge-window-action-bindings
  '(("+" enlarge-window "increase height")
    ("-" shrink-window "decrease height")
    ("<" shrink-window-horizontally "decrease width")
    ("=" balance-windows "balance")
    (">" enlarge-window-horizontally "increase width")
    ("h" windmove-left "select left")
    ("j" windmove-down "select below")
    ("k" windmove-up "select above")
    ("l" windmove-right "select right")
    ("o" delete-other-windows "keep only this window")
    ("q" delete-window "close window")
    ("s" yunge-window-split-below "split below")
    ("v" yunge-window-split-right "split right")
    ("w" other-window "next window")))

(defconst yunge-window-bindings
  `(("SPC" yunge-window-control "window control")
    ,@yunge-window-action-bindings))

(defconst yunge-window-control-bindings
  `(,@yunge-key-control-exit-bindings
    ,@yunge-window-action-bindings))

(defconst yunge-window-control-hints
  '(("h/j/k/l" "select")
    ("s/v" "split")
    ("q" "close")
    ("o" "only")
    ("=" "balance")
    ("+/-" "height")
    ("</>" "width")
    ("SPC/RET/ESC" "exit")))

(defvar-keymap yunge-window-control-map
  :doc "Keymap active during persistent window control.")

(defun yunge-window--split-and-select (side)
  "Split the selected window on SIDE and select the new window."
  (select-window (split-window nil nil side)))

(defun yunge-window-split-below ()
  "Split the selected window below and select the new window."
  (interactive)
  (yunge-window--split-and-select 'below))

(defun yunge-window-split-right ()
  "Split the selected window to the right and select the new window."
  (interactive)
  (yunge-window--split-and-select 'right))

(defun yunge-window-control ()
  "Enter persistent window control."
  (interactive)
  (yunge-key-control-start
   yunge-window-control-map "Window" yunge-window-control-hints))

(yunge-key-define yunge-window-map yunge-window-bindings)
(yunge-key-define yunge-window-control-map
                  yunge-window-control-bindings)

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-window-map yunge-window-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-window-control-map yunge-window-control-bindings))

(provide 'yunge-window)

;;; yunge-window.el ends here
