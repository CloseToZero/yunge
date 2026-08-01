;;; yunge-orderless-test.el --- Orderless tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-orderless
  (orderless))

(ert-deftest yunge-orderless-configures-completion ()
  (yunge-test-run-package-config
   'yunge-orderless 'orderless
   :setup
   '(setq yunge-test-category-defaults
          (copy-tree completion-category-defaults)
          yunge-test-completion-settings
          (list completion-styles
                completion-category-overrides
                completion-pcm-leading-wildcard))
   :before-ready
   '(progn
      (when (featurep 'orderless)
        (error "Orderless was loaded before its Elpaca body ran"))
      (unless
          (equal (list completion-styles
                       completion-category-overrides
                       completion-pcm-leading-wildcard)
                 yunge-test-completion-settings)
        (error "Orderless configuration ran before package readiness")))
   :after-ready
   '(progn
      (unless
          (equal
           (list completion-styles
                 completion-category-overrides
                 completion-pcm-leading-wildcard)
           '((orderless basic)
             ((file (styles partial-completion)))
             t))
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
        (error "Orderless was not autoloaded by completion")))))

;;; yunge-orderless-test.el ends here
