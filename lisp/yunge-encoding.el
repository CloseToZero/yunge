;;; yunge-encoding.el --- Text and process encodings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

;; Keep text portable without overriding the Windows clipboard encoding.
(prefer-coding-system 'utf-8-unix)

(defvar w32-ansi-code-page)

(defconst yunge-encoding-process-input-coding-system
  (if (eq system-type 'windows-nt)
      (intern (format "cp%d" w32-ansi-code-page))
    'utf-8-unix)
  "Coding system used for arguments and input of UTF-8 subprocesses.")

;; Auto detection can settle on `no-conversion' when rg first emits ASCII,
;; leaving later UTF-8 matches displayed as octal byte escapes.  Native
;; Windows Emacs also encodes argv with the process input coding before using
;; CreateProcessA, so argv must use the active ANSI code page there.
(add-to-list 'process-coding-system-alist
             `("\\`rg\\(?:\\.exe\\)?\\'"
               . (utf-8 . ,yunge-encoding-process-input-coding-system)))

(provide 'yunge-encoding)

;;; yunge-encoding.el ends here
