;;; yunge-magit.el --- Git interface -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-evil)

(defvar magit-diff-section-map)
(defvar magit-mode-map)
(defvar magit-module-section-map)
(defvar magit-status-mode-map)

(defconst yunge-magit-go-bindings
  '(("g" magit-status "Git status")))

(defconst yunge-magit-section-normal-bindings
  '(("C-j" magit-section-forward "next section")
    ("C-k" magit-section-backward "previous section")
    ("M-h" magit-section-up "parent section")
    ("M-j" magit-section-forward-sibling "next sibling")
    ("M-k" magit-section-backward-sibling "previous sibling")))

(defconst yunge-magit-status-normal-bindings
  '(("RET" magit-visit-thing "visit")
    ("<tab>" magit-section-toggle "toggle section")
    ("q" magit-mode-bury-buffer "quit")
    ("gr" magit-refresh "refresh")))

(elpaca magit
  (yunge-key-define yunge-go-map yunge-magit-go-bindings)
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-go-map yunge-magit-go-bindings))
  ;; Section keymaps at point take precedence over Evil's mode bindings.
  ;; Remove their C-j bindings so navigation also works on file and module
  ;; rows.  C-RET retains the original visit commands.
  (with-eval-after-load 'magit-diff
    (keymap-unset magit-diff-section-map "C-j" t))
  (with-eval-after-load 'magit-submodule
    (keymap-unset magit-module-section-map "C-j" t))
  (with-eval-after-load 'evil
    (with-eval-after-load 'magit
      (yunge-key-evil-define
       'normal magit-mode-map
       yunge-magit-section-normal-bindings)
      (yunge-key-evil-define
       'normal magit-status-mode-map
       (append yunge-magit-section-normal-bindings
               yunge-magit-status-normal-bindings)))))

(provide 'yunge-magit)

;;; yunge-magit.el ends here
