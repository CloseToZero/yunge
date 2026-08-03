;;; yunge-orderless-test.el --- Orderless tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-orderless
  (orderless pyim pyim-cregexp))

(ert-deftest yunge-orderless-configures-completion ()
  (yunge-test-run-package-config
   'yunge-orderless 'orderless
   :setup
   '(setq yunge-test-category-defaults
          (copy-tree completion-category-defaults)
           yunge-test-completion-settings
           (list completion-styles
                 completion-category-overrides
                 completion-pcm-leading-wildcard
                 (if (boundp 'orderless-matching-styles)
                     orderless-matching-styles
                   :unbound)))
   :before-ready
   '(progn
      (when (featurep 'orderless)
        (error "Orderless was loaded before its Elpaca body ran"))
      (unless
          (equal (list completion-styles
                       completion-category-overrides
                       completion-pcm-leading-wildcard
                       (if (boundp 'orderless-matching-styles)
                           orderless-matching-styles
                         :unbound))
                 yunge-test-completion-settings)
        (error "Orderless configuration ran before package readiness")))
   :after-ready
   '(progn
      (unless
          (equal
           (list completion-styles
                 completion-category-overrides
                 completion-pcm-leading-wildcard
                 orderless-matching-styles)
           '((orderless basic)
             ((file (styles partial-completion)))
             t
             (orderless-literal orderless-regexp
                                yunge-orderless-pinyin)))
        (error "Unexpected Orderless configuration"))
      (unless (equal completion-category-defaults
                     yunge-test-category-defaults)
        (error "Completion category defaults were changed"))
      (when (featurep 'orderless)
        (error "Orderless was loaded before completion"))
      (let ((matches
             (completion-all-completions
              "buf sw"
              '("switch-to-buffer" "buffer-file-name" "find-file")
              nil 6)))
        (unless (and
                 (equal (substring-no-properties (car matches))
                        "switch-to-buffer")
                 (equal (cdr matches) 0))
          (error "Unexpected Orderless matches: %S" matches)))
      (unless (featurep 'orderless)
        (error "Orderless was not autoloaded by completion"))
      (let* ((chinese (string #x4fdd #x7559))
             (matches
              (completion-all-completions
               "bl" (list chinese "table" "other") nil 2))
             (tail matches)
             values)
        (while (consp tail)
          (push (substring-no-properties (car tail)) values)
          (setq tail (cdr tail)))
        (unless (and (member chinese values)
                     (member "table" values))
          (error "Unexpected Pinyin matches: %S" matches)))
      (when (completion-all-completions
             "=bl" (list (string #x4fdd #x7559)) nil 3)
        (error "Literal Orderless dispatch unexpectedly used Pinyin")))))

;;; yunge-orderless-test.el ends here
