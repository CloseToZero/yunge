;;; yunge-orderless-test.el --- Orderless completion behavior -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-orderless
  (orderless orderless-kwd yunge-pinyin-data))

(ert-deftest yunge-orderless-completes-text-and-path-queries ()
  (yunge-test-run-package-config
   'yunge-orderless 'orderless
   :after-ready
   '(cl-labels
        ((candidates (query table)
           (let ((tail
                  (completion-all-completions
                   query table nil (length query)))
                 result)
             (while (consp tail)
               (push (substring-no-properties (car tail)) result)
               (setq tail (cdr tail)))
             (nreverse result))))
      (unless (equal (candidates "buf sw"
                                 '("switch-to-buffer"
                                   "buffer-file-name"
                                   "find-file"))
                     '("switch-to-buffer"))
        (error "Orderless did not combine completion components"))
      (let ((han (string #x4fdd #x7559))
            (han-with-digit (concat (string #x4e2d #x6587) "3"))
            (mixed (string #x80cc #x666f #x50cf #x7d20)))
        (let ((matches (candidates "bl" (list han "table" "other"))))
          (unless (equal (sort matches #'string<) (list "table" han))
            (error "Pinyin and literal completion did not both match")))
        (unless (equal (candidates "=bl" (list han "table")) '("table"))
          (error "Literal completion did not select only literal matches"))
        (unless (equal (candidates "zhongwen3" (list han-with-digit))
                       (list han-with-digit))
          (error "Pinyin and literal digits did not match together"))
        (unless (equal (candidates "a.b" '("axb" "a.b")) '("a.b"))
          (error "Ordinary completion did not treat punctuation literally"))
        (unless (equal (candidates ":re:a.b" '("axb")) '("axb"))
          (error "Explicit regexp completion did not match"))
        (when (candidates "beijx" (list mixed))
          (error "Structured Pinyin mixed syllables and initials"))
        (unless (equal (candidates ":py:beijx" (list mixed))
                       (list mixed))
          (error "Explicit mixed Pinyin completion did not match")))
      (let ((root (make-temp-file "yunge-orderless-" t)))
        (unwind-protect
            (let ((default-directory (file-name-as-directory root))
                  (han-file (concat (string #x4fdd #x7559) ".org")))
              (make-directory (expand-file-name "src/" root) t)
              (with-temp-file (expand-file-name "src/report.org" root))
              (with-temp-file (expand-file-name "src/other.org" root))
              (with-temp-file (expand-file-name han-file root))
              (unless (equal
                       (candidates "rc/por" #'completion-file-name-table)
                       '("src/report.org"))
                (error "File path components did not partially complete"))
              (unless (equal
                       (candidates "bl" #'completion-file-name-table)
                       (list han-file))
                (error "Chinese file name did not match Pinyin")))
          (delete-directory root t))))))

;;; yunge-orderless-test.el ends here
