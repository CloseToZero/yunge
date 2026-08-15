;;; yunge-orderless-test.el --- Orderless tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-orderless
  (orderless orderless-kwd yunge-pinyin-data))

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
             (yunge-orderless-pinyin)))
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
      (unless (and
               (eq (car orderless-style-dispatchers)
                   'orderless-kwd-dispatch)
               (eq (car (alist-get 're orderless-kwd-alist))
                   'orderless-regexp)
               (eq (car (alist-get 'py orderless-kwd-alist))
                   'yunge-pinyin-permissive-regexp))
        (error "Orderless keyword dispatch was not configured"))
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
        (error "Literal Orderless dispatch unexpectedly used Pinyin"))
      (let* ((query "zhongwen3")
             (candidate (concat (string #x4e2d #x6587) "3")))
        (unless (completion-all-completions
                 query (list candidate) nil (length query))
          (error "Joined Pinyin and digits did not match")))
      (when (completion-all-completions
             "a.b" '("axb") nil 3)
        (error "Default Orderless matching interpreted a regexp"))
      (unless (completion-all-completions
               ":re:a.b" '("axb") nil 7)
        (error "Explicit Orderless regexp did not match"))
      (let ((candidate (string #x80cc #x666f #x50cf #x7d20)))
        (when (completion-all-completions
               "beijx" (list candidate) nil 5)
          (error "Structured Pinyin unexpectedly allowed internal mixing"))
        (unless (completion-all-completions
                 ":py:beijx" (list candidate) nil 9)
          (error "Explicit permissive Pinyin did not match"))))))

;;; yunge-orderless-test.el ends here
