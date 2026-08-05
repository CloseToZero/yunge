;;; yunge-comment.el --- Evil comment operator -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'evil)
(require 'newcomment)
(require 'subr-x)
(require 'yunge-key)

(declare-function evil-apply-on-block "evil-common"
                  (function beginning end pass-columns &rest arguments))

(defvar c-block-comment-ender)
(defvar c-block-comment-starter)
(defvar c-line-comment-starter)

(defun yunge-comment--c-family-syntax (type)
  "Return C-family comment delimiters for Evil range TYPE, or nil."
  (cond
   ((and (boundp 'c-block-comment-starter)
         c-block-comment-starter
         (boundp 'c-block-comment-ender)
         c-block-comment-ender)
    (if (and (eq type 'line)
             (boundp 'c-line-comment-starter)
             c-line-comment-starter)
        (cons (concat c-line-comment-starter " ") "")
      (cons (concat c-block-comment-starter " ")
            (concat " " c-block-comment-ender))))
   ((derived-mode-p 'c-ts-base-mode)
    (if (eq type 'line)
        '("// " . "")
      '("/* " . " */")))))

(defun yunge-comment--toggle-region (beginning end type)
  "Toggle comments between BEGINNING and END for Evil range TYPE."
  (if-let* ((syntax (yunge-comment--c-family-syntax type)))
      (let* ((comment-start (car syntax))
             (comment-end (cdr syntax))
             (comment-start-skip
              (concat (regexp-quote (string-trim-right comment-start))
                      "[ \t]*"))
             (comment-end-skip
              (if (string-empty-p comment-end)
                  "[ \t]*\\(?:\n\\|\\'\\)"
                (concat "[ \t]*"
                        (regexp-quote
                         (string-trim-left comment-end)))))
             (comment-continue
              (unless (string-empty-p comment-end)
                comment-start)))
        (comment-or-uncomment-region beginning end))
    (comment-or-uncomment-region beginning end)))

(evil-define-operator yunge-comment-toggle (beginning end type)
  "Toggle comments in the selected Evil range."
  :move-point nil
  (interactive "<R>")
  (if (eq type 'block)
      (evil-apply-on-block #'yunge-comment--toggle-region
                           beginning end nil type)
    (yunge-comment--toggle-region beginning end type)))

(yunge-key-evil-define
 '(normal visual) global-map
 '(("gc" yunge-comment-toggle "toggle comment")))

(provide 'yunge-comment)

;;; yunge-comment.el ends here
