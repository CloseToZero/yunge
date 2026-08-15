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
  (let ((yunge-pinyin-regexp-cache-size 2))
    (unwind-protect
        (progn
          (yunge-pinyin-clear-cache)
          (dolist (query '("baoliu" "xian" "shi"))
            (yunge-pinyin-regexp query))
          (should (= (hash-table-count yunge-pinyin--regexp-cache) 2))
          (should-not
           (gethash (yunge-pinyin--regexp-cache-key
                     "baoliu" 'structured)
                    yunge-pinyin--regexp-cache)))
      (yunge-pinyin-clear-cache))))

(ert-deftest yunge-pinyin-caches-grammar-results-separately ()
  (require 'yunge-pinyin)
  (let ((yunge-pinyin-regexp-cache-size 8))
    (unwind-protect
        (progn
          (yunge-pinyin-clear-cache)
          (should (equal (yunge-pinyin-regexp "beijx" 'structured)
                         "beijx"))
          (should (string-match-p
                   (yunge-pinyin-regexp "beijx" 'permissive)
                   "背景像素"))
          (should
           (gethash (yunge-pinyin--regexp-cache-key
                     "beijx" 'structured)
                    yunge-pinyin--regexp-cache))
          (should
           (gethash (yunge-pinyin--regexp-cache-key
                     "beijx" 'permissive)
                    yunge-pinyin--regexp-cache)))
      (yunge-pinyin-clear-cache))))

(ert-deftest yunge-pinyin-avoids-doomed-frequency-levels ()
  (require 'yunge-pinyin)
  (let ((original
         (symbol-function 'yunge-pinyin--compile-run-at-level)))
    (cl-labels
        ((levels-for
          (query maximum-level)
          (let ((yunge-pinyin-max-frequency-level maximum-level)
                levels)
            (yunge-pinyin-clear-cache)
            (cl-letf (((symbol-function
                        'yunge-pinyin--compile-run-at-level)
                       (lambda (literal paths level limit)
                         (push level levels)
                         (funcall original literal paths level limit))))
              (yunge-pinyin-regexp query))
            (nreverse levels))))
      (should (equal (levels-for "shi" 4) '(1 4)))
      (should (equal (levels-for "shi" 5) '(1 5)))
      (let ((fallback-levels
             (levels-for
              (concat "asdfkljadsflkasdjflksadlfk"
                      "asdfkljadsflkasdjflksadlfk")
              4)))
        (should (= (length fallback-levels) 1))
        (should (<= (car fallback-levels) 4))))))

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
