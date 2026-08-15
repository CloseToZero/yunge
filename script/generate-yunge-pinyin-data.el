;;; generate-yunge-pinyin-data.el --- Build Pinyin search data -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Generate `yunge-pinyin-data.el' from Unicode Unihan 17.0.0.
;;
;; Usage:
;;
;;   emacs --batch -Q --script script/generate-yunge-pinyin-data.el \
;;     Unihan_Readings.txt Unihan_DictionaryLikeData.txt \
;;     Unihan_OtherMappings.txt \
;;     lisp/yunge-pinyin-data.el
;;
;; The generated file contains data under the Unicode-3.0 license.  This
;; generator is original MIT-licensed code and does not copy implementation
;; code from another Pinyin library.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst yunge-pinyin-generator--unicode-version "17.0.0")

(defconst yunge-pinyin-generator--tone-replacements
  '(("ā" . "a") ("á" . "a") ("ǎ" . "a") ("à" . "a")
    ("ē" . "e") ("é" . "e") ("ě" . "e") ("è" . "e")
    ("ê" . "e")
    ("ī" . "i") ("í" . "i") ("ǐ" . "i") ("ì" . "i")
    ("ō" . "o") ("ó" . "o") ("ǒ" . "o") ("ò" . "o")
    ("ū" . "u") ("ú" . "u") ("ǔ" . "u") ("ù" . "u")
    ("ǖ" . "v") ("ǘ" . "v") ("ǚ" . "v") ("ǜ" . "v")
    ("ü" . "v")
    ("ḿ" . "m") ("ń" . "n") ("ň" . "n") ("ǹ" . "n")))

(defun yunge-pinyin-generator--normalize (reading)
  "Return a lowercase, toneless Pinyin key for READING."
  (let ((result (downcase reading)))
    (dolist (replacement yunge-pinyin-generator--tone-replacements)
      (setq result
            (string-replace (car replacement) (cdr replacement) result)))
    (and (string-match-p "\\`[a-zv]+\\'" result)
         result)))

(defun yunge-pinyin-generator--add-reading
    (readings codepoint reading &optional frequency)
  "Record READING for CODEPOINT in READINGS, optionally with FREQUENCY."
  (when-let* ((normalized (yunge-pinyin-generator--normalize reading)))
    (let ((character-readings
           (or (gethash codepoint readings)
               (puthash codepoint (make-hash-table :test #'equal)
                        readings))))
      (when (or frequency (not (gethash normalized character-readings)))
        (puthash normalized frequency character-readings)))))

(defun yunge-pinyin-generator--read-grades (file)
  "Return a CODEPOINT-to-grade table parsed from Unihan FILE."
  (let ((grades (make-hash-table :test #'eql)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              "^U[+]\\([0-9A-F]+\\)\tkGradeLevel\t\\([1-6]\\)$" nil t)
        (puthash (string-to-number (match-string 1) 16)
                 (string-to-number (match-string 2))
                 grades)))
    grades))

(defun yunge-pinyin-generator--read-readings (file)
  "Return a CODEPOINT-to-readings table parsed from Unihan FILE."
  (let ((readings (make-hash-table :test #'eql)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^U[+]\\([0-9A-F]+\\)\t"
                      "\\(kMandarin\\|kHanyuPinyin\\|kHanyuPinlu\\|kTGHZ2013\\)\t"
                      "\\(.+\\)$")
              nil t)
        (let ((codepoint (string-to-number (match-string 1) 16))
              (property (match-string 2))
              (value (match-string 3)))
          (pcase property
            ("kMandarin"
             (dolist (reading (split-string value "[ ,]+" t))
               (yunge-pinyin-generator--add-reading
                readings codepoint reading)))
            ("kHanyuPinyin"
             (dolist (entry (split-string value " " t))
               (when (string-match ":\\(.+\\)" entry)
                 (dolist (reading
                          (split-string (match-string 1 entry) "," t))
                   (yunge-pinyin-generator--add-reading
                    readings codepoint reading)))))
            ("kHanyuPinlu"
             (let ((position 0))
               (while (string-match
                       "\\([^ ()]+\\)(\\([0-9]+\\))" value position)
                 (yunge-pinyin-generator--add-reading
                  readings codepoint (match-string 1 value)
                  (string-to-number (match-string 2 value)))
                 (setq position (match-end 0)))))
            ("kTGHZ2013"
             (dolist (entry (split-string value " " t))
               (when (string-match ":\\(.+\\)" entry)
                 (dolist (reading
                          (split-string (match-string 1 entry) "," t))
                   (yunge-pinyin-generator--add-reading
                    readings codepoint reading)))))))))
    readings))

(defun yunge-pinyin-generator--read-tgh-ranks (file)
  "Return CODEPOINT-to-rank data from the 2013 kTGH property in FILE."
  (let ((ranks (make-hash-table :test #'eql)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward
              "^U[+]\\([0-9A-F]+\\)\tkTGH\t2013:\\([0-9]+\\)$" nil t)
        (puthash (string-to-number (match-string 1) 16)
                 (string-to-number (match-string 2))
                 ranks)))
    ranks))

(defun yunge-pinyin-generator--bucket (frequency grade tgh-rank)
  "Return the disjoint frequency bucket for FREQUENCY and GRADE.
TGH-RANK is the character's position in the 2013 standard character list."
  (cond
   ((and frequency (>= frequency 1000)) 0)
   ((and frequency (>= frequency 100)) 1)
   ((and frequency (>= frequency 10)) 2)
   (frequency 3)
   ((and grade (<= grade 1)) 0)
   ((and grade (<= grade 2)) 1)
   ((and grade (<= grade 4)) 2)
   ((and grade (<= grade 6)) 3)
   ((and tgh-rank (<= tgh-rank 3500)) 2)
   ((and tgh-rank (<= tgh-rank 6500)) 3)
   (t 4)))

(defun yunge-pinyin-generator--invert
    (readings grades tgh-ranks)
  "Invert READINGS using GRADES into Pinyin-keyed frequency buckets.
TGH-RANKS records positions in the 2013 standard character list."
  (let ((result (make-hash-table :test #'equal)))
    (maphash
     (lambda (codepoint character-readings)
       (maphash
        (lambda (reading frequency)
          (let* ((buckets
                  (or (gethash reading result)
                      (puthash reading
                               (vector nil nil nil nil nil) result)))
                 (bucket
                  (yunge-pinyin-generator--bucket
                   frequency (gethash codepoint grades)
                   (gethash codepoint tgh-ranks))))
            (push codepoint (aref buckets bucket))))
        character-readings))
     readings)
    (maphash
     (lambda (_reading buckets)
       (dotimes (index (length buckets))
         (setf (aref buckets index)
               (concat (sort (delete-dups (aref buckets index)) #'<)))))
     result)
    result))

(defun yunge-pinyin-generator--write (table output)
  "Write inverted Pinyin TABLE to OUTPUT."
  (let ((coding-system-for-write 'utf-8-unix)
        (print-escape-multibyte nil)
        (keys nil))
    (maphash (lambda (key _value) (push key keys)) table)
    (setq keys (sort keys #'string<))
    (with-temp-file output
      (insert ";;; yunge-pinyin-data.el --- Generated Pinyin data -*- lexical-binding: t; -*-\n")
      (insert ";; SPDX-FileCopyrightText: 1991-2025 Unicode, Inc.\n")
      (insert ";; SPDX-License-Identifier: Unicode-3.0\n\n")
      (insert ";;; Commentary:\n\n")
      (insert ";; Generated from Unicode Unihan "
              yunge-pinyin-generator--unicode-version
              " properties kMandarin, kHanyuPinyin, kHanyuPinlu,\n")
      (insert ";; kTGHZ2013, kGradeLevel, and kTGH.  Bucket strings are disjoint and\n")
      (insert ";; ordered from common to rare.  Regenerate this file with\n")
      (insert ";; script/generate-yunge-pinyin-data.el.\n\n")
      (insert ";;; Code:\n\n")
      (insert "(defconst yunge-pinyin-data-version ")
      (prin1 yunge-pinyin-generator--unicode-version (current-buffer))
      (insert ")\n\n")
      (insert "(defconst yunge-pinyin-data\n  '(\n")
      (dolist (key keys)
        (insert "    (")
        (prin1 key (current-buffer))
        (insert " . ")
        (prin1 (gethash key table) (current-buffer))
        (insert ")\n"))
      (insert "    ))\n\n")
      (insert "(provide 'yunge-pinyin-data)\n\n")
      (insert ";;; yunge-pinyin-data.el ends here\n"))))

(defun yunge-pinyin-generator-main
    (readings-file grades-file mappings-file output)
  "Generate OUTPUT from READINGS-FILE, GRADES-FILE, and MAPPINGS-FILE."
  (let* ((grades (yunge-pinyin-generator--read-grades grades-file))
         (readings (yunge-pinyin-generator--read-readings readings-file))
         (tgh-ranks (yunge-pinyin-generator--read-tgh-ranks mappings-file))
         (table (yunge-pinyin-generator--invert
                 readings grades tgh-ranks)))
    (yunge-pinyin-generator--write table output)
    (message "Generated %s with %d Pinyin syllables"
             output (hash-table-count table))))

(when noninteractive
  (unless (= (length command-line-args-left) 4)
    (error "Expected READINGS GRADES MAPPINGS OUTPUT, got %S"
           command-line-args-left))
  (apply #'yunge-pinyin-generator-main command-line-args-left))

;;; generate-yunge-pinyin-data.el ends here
