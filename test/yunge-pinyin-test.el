;;; yunge-pinyin-test.el --- Pinyin regexp tests -*- lexical-binding: t; -*-
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

(ert-deftest yunge-pinyin-expands-letter-runs-in-literal-text ()
  (yunge-test-load-package-config 'yunge-pinyin)
  (let ((regexp (yunge-pinyin-regexp "zhongwen3")))
    (should (string-match-p regexp "中文3"))
    (should (string-match-p regexp "zhongwen3"))
    (should-not (string-match-p regexp "中文2")))
  (let ((regexp (yunge-pinyin-regexp "b.*l")))
    (should (string-match-p regexp "b.*l"))
    (should-not (string-match-p regexp "bXXl"))))

(ert-deftest yunge-pinyin-preserves-case-and-quotes-non-pinyin-text ()
  (yunge-test-load-package-config 'yunge-pinyin)
  (let ((regexp (yunge-pinyin-regexp "BL")))
    (should (string-match-p regexp "保留"))
    (should (string-match-p regexp "BL")))
  (should (equal (yunge-pinyin-regexp "保留") "保留"))
  (should (equal (yunge-pinyin-regexp "'") "'"))
  (should-not (yunge-pinyin-regexp "")))

;;; yunge-pinyin-test.el ends here
