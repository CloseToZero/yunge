;;; yunge-bookmark.el --- Persistent named locations -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-state)

(defvar bookmark-save-flag)

(defconst yunge-bookmark-jump-bindings
  '(("B" bookmark-bmenu-list "manage bookmarks")
    ("m" bookmark-set "set bookmark")))

;; Bookmarks express deliberate user intent, so persist each change at once.
(setq bookmark-save-flag 1)

(yunge-key-define yunge-jump-map
                  yunge-bookmark-jump-bindings)

(with-eval-after-load 'which-key
  (yunge-key-which-key-describe-map
   yunge-jump-map yunge-bookmark-jump-bindings))

(provide 'yunge-bookmark)

;;; yunge-bookmark.el ends here
