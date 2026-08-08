;;; yunge-xref.el --- Xref navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function evil-set-initial-state "evil-core" (mode state))

(defvar xref--transient-buffer-mode-map)
(defvar xref--xref-buffer-mode-map)

(defconst yunge-xref-command-bindings
  '(("e" xref-change-to-xref-edit-mode "edit results")
    ("r" xref-query-replace-in-results "query replace")))

(defvar-keymap yunge-xref-command-map
  :doc "Xref result commands.")

(yunge-key-define yunge-xref-command-map
                  yunge-xref-command-bindings)

(defconst yunge-xref-normal-bindings
  `(("j" evil-next-line "next line")
    ("k" evil-previous-line "previous line")
    ("C-j" xref-next-line "next reference")
    ("C-k" xref-prev-line "previous reference")
    ("RET" xref-goto-xref "visit")
    ("q" quit-window "quit")
    ("gf" xref-show-location-at-point "show source")
    ("gr" xref-revert-buffer "refresh")
    ("]]" xref-next-group "next group")
    ("[[" xref-prev-group "previous group")
    ([localleader] ,yunge-xref-command-map nil)))

(defconst yunge-xref-transient-normal-bindings
  '(("RET" xref-quit-and-goto-xref "visit and quit")))

(with-eval-after-load 'evil
  (with-eval-after-load 'xref
    (evil-set-initial-state 'xref--xref-buffer-mode 'normal)
    (evil-set-initial-state 'xref--transient-buffer-mode 'normal)
    (yunge-key-evil-define 'normal xref--xref-buffer-mode-map
                           yunge-xref-normal-bindings)
    (yunge-key-evil-define 'normal xref--transient-buffer-mode-map
                           yunge-xref-transient-normal-bindings)))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-xref-command-map yunge-xref-command-bindings))

(provide 'yunge-xref)

;;; yunge-xref.el ends here
