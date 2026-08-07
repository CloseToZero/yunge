;;; yunge-orderless.el --- Completion matching -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-pinyin)

(defvar completion-pcm-leading-wildcard)
(defvar orderless-matching-styles)
(defvar orderless-kwd-alist)
(defvar orderless-style-dispatchers)

(defun yunge-orderless-pinyin (component)
  "Return a literal and Pinyin-aware regexp for Orderless COMPONENT."
  (yunge-pinyin-regexp component))

(elpaca orderless
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(yunge-orderless-pinyin)
        ;; Preserve native path and TRAMP completion semantics.
        completion-category-overrides
        '((file (styles partial-completion)))
        ;; Let each path component match as a substring on Emacs 31.
        completion-pcm-leading-wildcard t)
  (with-eval-after-load 'orderless
    (require 'orderless-kwd)
    ;; Keep explicit regexp queries consistent with Evil's `:re:' syntax.
    (add-to-list 'orderless-kwd-alist '(re orderless-regexp))
    (add-to-list 'orderless-style-dispatchers
                 #'orderless-kwd-dispatch)))

(provide 'yunge-orderless)

;;; yunge-orderless.el ends here
