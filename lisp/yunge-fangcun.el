;;; yunge-fangcun.el --- Fangcun integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

(defconst yunge-fangcun-note-bindings
  '(("b" fangcun-backlink-find "find backlink")
    ("f" fangcun-node-find "find node")
    ("i" fangcun-node-insert "insert node link")
    ("n" fangcun-file-node-create "new file node")))

(advice-add 'fangcun-node-insert :around
            #'yunge-evil-call-after-normal-state-eol)

(yunge-key-define yunge-note-map yunge-fangcun-note-bindings)

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-note-map yunge-fangcun-note-bindings))

(provide 'yunge-fangcun)

;;; yunge-fangcun.el ends here
