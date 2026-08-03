;;; yunge-orderless.el --- Completion matching -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-pinyin)

(defvar completion-pcm-leading-wildcard)
(defvar orderless-matching-styles)

(defun yunge-orderless-pinyin (component)
  "Return a Pinyin-aware regexp for alphabetic Orderless COMPONENT."
  (yunge-pinyin-regexp component))

(elpaca orderless
  (setq completion-styles '(orderless basic)
        orderless-matching-styles
        '(orderless-literal orderless-regexp yunge-orderless-pinyin)
        ;; Preserve native path and TRAMP completion semantics.
        completion-category-overrides
        '((file (styles partial-completion)))
        ;; Let each path component match as a substring on Emacs 31.
        completion-pcm-leading-wildcard t))

(provide 'yunge-orderless)

;;; yunge-orderless.el ends here
