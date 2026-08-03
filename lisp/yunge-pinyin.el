;;; yunge-pinyin.el --- Shared Pinyin regexp support -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function pyim-cregexp-build "pyim-cregexp"
                  (string &optional char-level-num chinese-only))

(defun yunge-pinyin-regexp (text)
  "Return a Pinyin-aware regexp for alphabetic TEXT, or nil.
The regexp keeps the original text as a possible match."
  (when (and (string-match-p "[A-Za-z]" text)
             (string-match-p "\\`[A-Za-z']+\\'" text))
    ;; Pyim's regexp layer needs a dcache backend, but using it does not
    ;; register or enable Pyim as an Emacs input method.
    (require 'pyim-dhashcache)
    (require 'pyim-cregexp)
    (let* ((lower (downcase text))
           (regexp (pyim-cregexp-build lower)))
      (if (equal text lower)
          regexp
        (format "\\(?:%s\\|%s\\)" regexp (regexp-quote text))))))

(elpaca pyim)

(provide 'yunge-pinyin)

;;; yunge-pinyin.el ends here
