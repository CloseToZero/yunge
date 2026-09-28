;;; yunge-orderless.el --- Completion matching -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-pinyin)

(defvar completion-pcm-leading-wildcard)
(defvar orderless-matching-styles)
(defvar orderless-kwd-alist)
(defvar orderless-style-dispatchers)

(elpaca orderless
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(yunge-pinyin-regexp)
        ;; Try partial matching across path components before Orderless.
        completion-category-overrides
        '((file (styles partial-completion)))
        ;; Let each path component match as a substring on Emacs 31.
        completion-pcm-leading-wildcard t)
  (with-eval-after-load 'orderless
    (require 'orderless-kwd)
    ;; Add explicit regexp and mixed Pinyin matching.
    (add-to-list 'orderless-kwd-alist '(re orderless-regexp))
    (add-to-list 'orderless-kwd-alist
                 '(py yunge-pinyin-permissive-regexp))
    (add-to-list 'orderless-style-dispatchers
                 #'orderless-kwd-dispatch)))

(provide 'yunge-orderless)

;;; yunge-orderless.el ends here
