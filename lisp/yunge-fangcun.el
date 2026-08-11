;;; yunge-fangcun.el --- Fangcun integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'fangcun-loader)
(require 'yunge-key)

(defvar fangcun-backlinks-mode-map)

(defconst yunge-fangcun-note-bindings
  '(("b" fangcun-backlink-find "find backlink")
    ("f" fangcun-node-find "find node")
    ("i" fangcun-node-insert "insert node link")
    ("n" fangcun-file-node-create "new file node")
    ("t" fangcun-node-set-tags "set node tags")
    ("v" fangcun-backlinks "view backlinks")))

(defconst yunge-fangcun-backlinks-normal-bindings
  `(("RET" fangcun-backlink-visit "visit")
    ("C-j" forward-button "next backlink")
    ("C-k" backward-button "previous backlink")
    ("q" quit-window "quit")
    ("gr" revert-buffer "refresh")
    ,@yunge-key-button-navigation-bindings))

(advice-add 'fangcun-node-insert :around
            #'yunge-evil-call-after-normal-state-eol)

(with-eval-after-load 'evil
  (with-eval-after-load 'fangcun
    (yunge-key-evil-define
     'normal fangcun-backlinks-mode-map
     yunge-fangcun-backlinks-normal-bindings)))

(yunge-key-define yunge-note-map yunge-fangcun-note-bindings)

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-note-map yunge-fangcun-note-bindings))

(provide 'yunge-fangcun)

;;; yunge-fangcun.el ends here
