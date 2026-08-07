;;; yunge-bookmark.el --- Persistent named locations -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-state)

(defvar bookmark-bmenu-mode-map)
(defvar bookmark-save-flag)

(defconst yunge-bookmark-bindings
  '(("l" bookmark-bmenu-list "list bookmarks")
    ("s" bookmark-set "set bookmark")))

(defvar-keymap yunge-bookmark-map
  :doc "Global bookmark command map.")

(yunge-key-define yunge-bookmark-map yunge-bookmark-bindings)

(defconst yunge-bookmark-jump-bindings
  `(("b" ,yunge-bookmark-map "bookmark")))

(defconst yunge-bookmark-bmenu-normal-bindings
  '(("RET" bookmark-bmenu-this-window "jump")
    ("d" bookmark-bmenu-delete "mark delete")
    ("gr" revert-buffer "refresh")
    ("j" next-line nil)
    ("k" previous-line nil)
    ("m" bookmark-bmenu-mark "mark")
    ("M" bookmark-bmenu-select "open marked")
    ("q" quit-window "quit")
    ("r" bookmark-bmenu-rename "rename")
    ("R" bookmark-bmenu-relocate "relocate")
    ("u" bookmark-bmenu-unmark "unmark")
    ("x" bookmark-bmenu-execute-deletions "execute deletions")))

;; Bookmarks express deliberate user intent, so persist each change at once.
(setq bookmark-save-flag 1)

(yunge-key-define yunge-jump-map
                  yunge-bookmark-jump-bindings)

(defun yunge-bookmark--setup-bmenu-keys ()
  "Set up Evil bindings for the bookmark list."
  (yunge-key-evil-define 'normal bookmark-bmenu-mode-map
                         yunge-bookmark-bmenu-normal-bindings))

(with-eval-after-load 'evil
  (with-eval-after-load 'bookmark
    (yunge-bookmark--setup-bmenu-keys)))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-jump-map yunge-bookmark-jump-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-bookmark-map yunge-bookmark-bindings))

(provide 'yunge-bookmark)

;;; yunge-bookmark.el ends here
