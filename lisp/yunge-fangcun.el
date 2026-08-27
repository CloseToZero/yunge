;;; yunge-fangcun.el --- Fangcun integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'fangcun-loader)
(require 'yunge-key)

(declare-function org-in-regexp "org")
(declare-function org-region-active-p "org")

(defvar fangcun-backlinks-mode-map)
(defvar fangcun-check-mode-map)
(defvar org-link-bracket-re)

(defvar-keymap yunge-fangcun-backlink-map
  :doc "Fangcun backlink commands.")

(defconst yunge-fangcun-backlink-bindings
  '(("f" fangcun-backlink-find "find backlink")
    ("v" fangcun-backlinks "view backlinks")))

(defconst yunge-fangcun-note-bindings
  `(("b" ,yunge-fangcun-backlink-map "backlink")
    ("C" fangcun-check "check notes")
    ("c" fangcun-heading-node-create "create heading node")
    ("f" fangcun-node-find "find node")
    ("i" fangcun-node-insert "insert node link")
    ("n" fangcun-file-node-create "new file node")
    ("t" fangcun-node-set-tags "set node tags")))

(defconst yunge-fangcun-backlinks-normal-bindings
  `(("RET" fangcun-backlink-visit "visit")
    ("C-j" forward-button "next backlink")
    ("C-k" backward-button "previous backlink")
    ("q" quit-window "quit")
    ("gr" revert-buffer "refresh")
    ,@yunge-key-button-navigation-bindings))

(defconst yunge-fangcun-check-normal-bindings
  `(("RET" fangcun-check-visit "visit")
    ("C-j" forward-button "next issue")
    ("C-k" backward-button "previous issue")
    ("q" quit-window "quit")
    ("gr" revert-buffer "refresh")
    ,@yunge-key-button-navigation-bindings))

(defun yunge-fangcun--insert-node-link-at-normal-state-eol
    (function &rest arguments)
  "Call FUNCTION at the insertion side of a Normal-state EOL.
Keep point on an active region or existing ID link so Fangcun can replace that
context."
  (if (or (org-region-active-p)
          (and (org-in-regexp org-link-bracket-re 1)
               (string-prefix-p
                "id:" (match-string-no-properties 1))))
      (apply function arguments)
    (apply #'yunge-evil-call-after-normal-state-eol
           function arguments)))

(advice-add 'fangcun-node-insert :around
            #'yunge-fangcun--insert-node-link-at-normal-state-eol)

(yunge-key-define yunge-fangcun-backlink-map
                  yunge-fangcun-backlink-bindings)

(with-eval-after-load 'evil
  (with-eval-after-load 'fangcun
    (yunge-key-evil-define
     'normal fangcun-backlinks-mode-map
     yunge-fangcun-backlinks-normal-bindings)
    (yunge-key-evil-define
     'normal fangcun-check-mode-map
     yunge-fangcun-check-normal-bindings)))

(yunge-key-define yunge-note-map yunge-fangcun-note-bindings)

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-note-map yunge-fangcun-note-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-fangcun-backlink-map yunge-fangcun-backlink-bindings))

(provide 'yunge-fangcun)

;;; yunge-fangcun.el ends here
