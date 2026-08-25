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

(ert-deftest yunge-pinyin-structured-grammar-rejects-internal-mixing ()
  (require 'yunge-pinyin)
  (dolist (query '("beijx" "notion"))
    (should-not (yunge-pinyin-segmentations query 'structured))
    (should (equal (yunge-pinyin-regexp query 'structured)
                   (regexp-quote query)))))

(ert-deftest yunge-pinyin-permissive-grammar-allows-internal-mixing ()
  (require 'yunge-pinyin)
  (should (member '("bei" "j" "x")
                  (yunge-pinyin-segmentations "beijx" 'permissive)))
  (let ((regexp (yunge-pinyin-regexp "beijx" 'permissive)))
    (should (string-match-p regexp "背景像素"))
    (should (string-match-p regexp "beijx"))))

(ert-deftest yunge-pinyin-query-prefix-selects-permissive-grammar ()
  (require 'yunge-pinyin)
  (should (equal (yunge-pinyin-parse-query "beijx")
                 '("beijx" . structured)))
  (should (equal (yunge-pinyin-parse-query ":py:beijx")
                 '("beijx" . permissive)))
  (let ((regexp (yunge-pinyin-query-regexp ":py:beijx")))
    (should (string-match-p regexp "背景像素"))
    (should (string-match-p regexp "beijx"))
    (should-not (equal regexp (regexp-quote ":py:beijx")))))

(ert-deftest yunge-pinyin-permissive-grammar-can-be-the-default ()
  (require 'yunge-pinyin)
  (let ((yunge-pinyin-query-grammar 'permissive))
    (should (string-match-p (yunge-pinyin-regexp "beijx")
                            "背景像素"))))

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
    (should (<= (length regexp) yunge-pinyin-regexp-budget))
    (should (yunge-pinyin--valid-regexp-p regexp))
    (should (string-match-p regexp query))))

(ert-deftest yunge-pinyin-caches-complete-query-regexps ()
  (require 'yunge-pinyin)
  (let ((yunge-pinyin-regexp-cache-size 8)
        (calls 0)
        (original (symbol-function 'yunge-pinyin--segment-run)))
    (unwind-protect
        (progn
          (yunge-pinyin-clear-cache)
          (cl-letf (((symbol-function 'yunge-pinyin--segment-run)
                     (lambda (run grammar)
                       (cl-incf calls)
                       (funcall original run grammar))))
            (let ((first (yunge-pinyin-regexp "xianxing"))
                  (second (yunge-pinyin-regexp "xianxing")))
              (should (equal first second))
              (should (= calls 1)))))
      (yunge-pinyin-clear-cache))))

(ert-deftest yunge-pinyin-bounds-the-complete-query-cache ()
  (require 'yunge-pinyin)
  (let ((yunge-pinyin-regexp-cache-size 2)
        (calls 0)
        (original (symbol-function 'yunge-pinyin--segment-run)))
    (unwind-protect
        (progn
          (yunge-pinyin-clear-cache)
          (cl-letf (((symbol-function 'yunge-pinyin--segment-run)
                     (lambda (run grammar)
                       (cl-incf calls)
                       (funcall original run grammar))))
            (dolist (query '("baoliu" "xian" "shi"))
              (yunge-pinyin-regexp query))
            (should (= calls 3))
            ;; A just-compiled query remains hot.
            (yunge-pinyin-regexp "shi")
            (should (= calls 3))
            ;; A cache smaller than the working set must recompile at least
            ;; one query, without prescribing its storage or eviction order.
            (dolist (query '("baoliu" "xian" "shi"))
              (yunge-pinyin-regexp query))
            (should (> calls 3))
            (should (<= calls 6))))
      (yunge-pinyin-clear-cache))))

(ert-deftest yunge-pinyin-caches-grammar-results-separately ()
  (require 'yunge-pinyin)
  (let ((yunge-pinyin-regexp-cache-size 8)
        (calls 0)
        (original (symbol-function 'yunge-pinyin--segment-run)))
    (unwind-protect
        (progn
          (yunge-pinyin-clear-cache)
          (cl-letf (((symbol-function 'yunge-pinyin--segment-run)
                     (lambda (run grammar)
                       (cl-incf calls)
                       (funcall original run grammar))))
            (let ((structured
                   (yunge-pinyin-regexp "beijx" 'structured))
                  (permissive
                   (yunge-pinyin-regexp "beijx" 'permissive)))
              (should (equal structured "beijx"))
              (should (string-match-p permissive "背景像素"))
              (should
               (equal structured
                      (yunge-pinyin-regexp "beijx" 'structured)))
              (should
               (equal permissive
                      (yunge-pinyin-regexp "beijx" 'permissive)))
              (should (= calls 2)))))
      (yunge-pinyin-clear-cache))))

(ert-deftest yunge-pinyin-frequency-levels-preserve-bounded-results ()
  (require 'yunge-pinyin)
  (dolist (maximum-level '(4 5))
    (let ((yunge-pinyin-max-frequency-level maximum-level))
      (yunge-pinyin-clear-cache)
      (let ((regexp (yunge-pinyin-regexp "shi")))
        (should (> (length regexp) (length "shi")))
        (should (<= (length regexp) yunge-pinyin-regexp-budget))
        (should (yunge-pinyin--valid-regexp-p regexp))
        (should (string-match-p regexp "实时"))
        (should (string-match-p regexp "shi")))
      (let* ((query (concat "asdfkljadsflkasdjflksadlfk"
                            "asdfkljadsflkasdjflksadlfk"))
             (regexp (yunge-pinyin-regexp query)))
        (should (<= (length regexp) yunge-pinyin-regexp-budget))
        (should (yunge-pinyin--valid-regexp-p regexp))
        (should (string-match-p regexp query)))))
  (yunge-pinyin-clear-cache))

(ert-deftest yunge-pinyin-falls-back-when-final-regexp-is-invalid ()
  (require 'yunge-pinyin)
  (unwind-protect
      (progn
        (yunge-pinyin-clear-cache)
        (cl-letf (((symbol-function 'yunge-pinyin--valid-regexp-p)
                   (lambda (_regexp) nil)))
          (should (equal (yunge-pinyin-regexp "baoliu") "baoliu"))))
    (yunge-pinyin-clear-cache)))

;;; yunge-pinyin-test.el ends here
