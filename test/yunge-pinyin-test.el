;;; yunge-pinyin-test.el --- Pinyin compiler tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-pinyin
  (yunge-pinyin-data))

(ert-deftest yunge-pinyin-normalizes-and-segments-queries ()
  (require 'yunge-pinyin)
  (should (equal (yunge-pinyin-normalize "LǛE") "lve"))
  (should (equal (car (yunge-pinyin-segmentations "baoliu"))
                 '("bao" "liu")))
  (should (equal (car (yunge-pinyin-segmentations "xi'an"))
                 '("xi" "an")))
  (let ((segmentations (yunge-pinyin-segmentations "xian")))
    (should (member '("xian") segmentations))
    (should (member '("xi" "an") segmentations))))

(ert-deftest yunge-pinyin-expands-full-incomplete-and-initial-queries ()
  (require 'yunge-pinyin)
  (dolist (query '("baoliu" "baol" "bl"))
    (let ((regexp (yunge-pinyin-regexp query)))
      (should (string-match-p regexp "保留"))
      (should (string-match-p regexp query)))))

(ert-deftest yunge-pinyin-supports-ambiguous-and-polyphonic-readings ()
  (require 'yunge-pinyin)
  (should (string-match-p (yunge-pinyin-regexp "xian") "西安"))
  (should (string-match-p (yunge-pinyin-regexp "xi'an") "西安"))
  (should-not (string-match-p (yunge-pinyin-regexp "xi'an") "先安"))
  (should (string-match-p (yunge-pinyin-regexp "chongqing") "重庆"))
  (should (string-match-p (yunge-pinyin-regexp "lvse") "绿色"))
  (should (string-match-p (yunge-pinyin-regexp "lǜse") "绿色")))

(ert-deftest yunge-pinyin-expands-letter-runs-in-literal-text ()
  (require 'yunge-pinyin)
  (let ((regexp (yunge-pinyin-regexp "zhongwen3")))
    (should (string-match-p regexp "中文3"))
    (should (string-match-p regexp "zhongwen3"))
    (should-not (string-match-p regexp "中文2")))
  (let ((regexp (yunge-pinyin-regexp "b.*l")))
    (should (string-match-p regexp "b.*l"))
    (should-not (string-match-p regexp "bXXl"))))

(ert-deftest yunge-pinyin-preserves-case-and-quotes-non-pinyin-text ()
  (require 'yunge-pinyin)
  (let ((regexp (yunge-pinyin-regexp "BL")))
    (should (string-match-p regexp "保留"))
    (should (string-match-p regexp "BL")))
  (should (equal (yunge-pinyin-regexp "保留") "保留"))
  (should (equal (yunge-pinyin-regexp "'") "'"))
  (should-not (yunge-pinyin-regexp "")))

(ert-deftest yunge-pinyin-keeps-useful-long-queries-expanded ()
  (require 'yunge-pinyin)
  (let* ((query "xianxingdaishuyushishixuanran")
         (regexp (yunge-pinyin-regexp query)))
    (should (> (length regexp) (length query)))
    (should (<= (length regexp) yunge-pinyin-regexp-budget))
    (should (string-match-p regexp "线性代数与实时渲染"))
    (should (string-match-p regexp query))))

(ert-deftest yunge-pinyin-keeps-bounded-initial-queries-useful ()
  (require 'yunge-pinyin)
  (let ((regexp (yunge-pinyin-regexp "xxdsyssxr")))
    (should (string-match-p regexp "线性代数与实时渲染"))
    (should (<= (length regexp) yunge-pinyin-regexp-budget))))

(ert-deftest yunge-pinyin-bounds-broad-initial-only-queries ()
  (require 'yunge-pinyin)
  (let* ((query "asdfkljadsflkasdjflksadlfk")
         (regexp (yunge-pinyin-regexp query)))
    (should (equal regexp (regexp-quote query)))
    (should (yunge-pinyin--valid-regexp-p regexp))
    (should (string-match-p regexp query))))

(ert-deftest yunge-pinyin-falls-back-when-final-regexp-is-invalid ()
  (require 'yunge-pinyin)
  (cl-letf (((symbol-function 'yunge-pinyin--valid-regexp-p)
             (lambda (_regexp) nil)))
    (should (equal (yunge-pinyin-regexp "baoliu") "baoliu"))))

;;; yunge-pinyin-test.el ends here
