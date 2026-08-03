;;; yunge-pinyin-test.el --- Shared Pinyin regexp tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-pinyin
  (pyim pyim-cregexp pyim-dhashcache))

(ert-deftest yunge-pinyin-expands-full-incomplete-and-initial-queries ()
  (yunge-test-load-package-config 'yunge-pinyin)
  (dolist (query '("baoliu" "baol" "bl"))
    (let ((regexp (yunge-pinyin-regexp query)))
      (should (string-match-p regexp "保留"))
      (should (string-match-p regexp query)))))

(ert-deftest yunge-pinyin-preserves-case-and-declines-regexps ()
  (yunge-test-load-package-config 'yunge-pinyin)
  (let ((regexp (yunge-pinyin-regexp "BL")))
    (should (string-match-p regexp "保留"))
    (should (string-match-p regexp "BL")))
  (should-not (yunge-pinyin-regexp "b.*l"))
  (should-not (yunge-pinyin-regexp "保留"))
  (should-not (yunge-pinyin-regexp "'")))

;;; yunge-pinyin-test.el ends here
