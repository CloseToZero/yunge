;;; yunge-encoding.el --- Text and process encodings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

;; Keep text portable without overriding the Windows clipboard encoding.
(prefer-coding-system 'utf-8-unix)

;; Auto detection can settle on `no-conversion' when rg first emits ASCII,
;; leaving later UTF-8 matches displayed as octal byte escapes.
(add-to-list 'process-coding-system-alist
             '("\\`rg\\(?:\\.exe\\)?\\'" . (utf-8 . utf-8-unix)))

(provide 'yunge-encoding)

;;; yunge-encoding.el ends here
