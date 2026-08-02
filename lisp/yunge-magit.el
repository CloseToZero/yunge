;;; yunge-magit.el --- Git interface -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-evil)

(defvar magit-status-mode-map)

(defconst yunge-magit-go-bindings
  '(("g" magit-status "Git status")))

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
  (with-eval-after-load 'evil
    (with-eval-after-load 'magit
      (yunge-key-evil-define
       'normal magit-status-mode-map
       yunge-magit-status-normal-bindings))))

(provide 'yunge-magit)

;;; yunge-magit.el ends here
