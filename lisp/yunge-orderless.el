;;; yunge-orderless.el --- Completion matching -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defvar completion-pcm-leading-wildcard)

(elpaca orderless
  (setq completion-styles '(orderless basic)
        ;; Preserve native path and TRAMP completion semantics.
        completion-category-overrides
        '((file (styles partial-completion)))
        ;; Let each path component match as a substring on Emacs 31.
        completion-pcm-leading-wildcard t))

(provide 'yunge-orderless)

;;; yunge-orderless.el ends here
