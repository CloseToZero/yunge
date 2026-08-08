;;; yunge-pinyin.el --- Shared Pinyin regexp support -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'subr-x)

(declare-function pyim-cregexp-build "pyim-cregexp"
                  (string &optional char-level-num chinese-only))

(defun yunge-pinyin--regexp-at-level (text char-level-num)
  "Expand Pinyin in TEXT using character level CHAR-LEVEL-NUM."
  (let ((position 0)
        regexp)
    (while (string-match "[A-Za-z][A-Za-z']*" text position)
      (let* ((beginning (match-beginning 0))
             (end (match-end 0))
             (run (substring text beginning end))
             (lower (downcase run))
             (expanded
              (pyim-cregexp-build lower char-level-num)))
        (setq regexp
              (concat
               regexp
               (regexp-quote (substring text position beginning))
               (if (equal run lower)
                   expanded
                 (format "\\(?:%s\\|%s\\)"
                         expanded (regexp-quote run)))))
        (setq position end)))
    (concat regexp (regexp-quote (substring text position)))))

(defun yunge-pinyin--valid-regexp-p (regexp)
  "Return non-nil when Emacs can compile REGEXP."
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun yunge-pinyin-regexp (text)
  "Return a regexp matching TEXT literally or through Pinyin.
ASCII letter runs are expanded as Pinyin.  All other text remains literal."
  (unless (string-empty-p text)
    (let ((literal (regexp-quote text)))
      (if (not (string-match-p "[A-Za-z]" text))
          literal
        ;; Pyim's regexp layer needs a dcache backend, but using it does not
        ;; register or enable Pyim as an Emacs input method.
        (require 'pyim-dhashcache)
        (require 'pyim-cregexp)
        (catch 'regexp
          ;; Pyim validates each letter run, but their concatenation can
          ;; still exceed Emacs's compiled-regexp limit.  Reduce the
          ;; character set until the complete regexp is accepted.
          (dolist (level '(4 3 2 1) literal)
            (let ((regexp
                   (yunge-pinyin--regexp-at-level text level)))
              (when (yunge-pinyin--valid-regexp-p regexp)
                (throw 'regexp regexp)))))))))

(elpaca pyim)

(provide 'yunge-pinyin)

;;; yunge-pinyin.el ends here
