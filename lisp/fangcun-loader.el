;;; fangcun-loader.el --- Lazy loading for Fangcun -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'seq)

(defgroup fangcun nil
  "Org-based personal knowledge management."
  :group 'org)

(defcustom fangcun-yiyus nil
  "Org note roots indexed by Fangcun.
Each entry has the form (ID :name NAME :root ROOT)."
  :type '(repeat
          (list :tag "Yiyu"
                (symbol :tag "ID")
                (const :name)
                (string :tag "Display name")
                (const :root)
                (directory :tag "Root")))
  :group 'fangcun)

(declare-function fangcun--setup-file-updates "fangcun" ())

(autoload 'fangcun--id-complete "fangcun")
(autoload 'fangcun--id-description "fangcun")
(autoload 'fangcun--id-find "fangcun")

;; Keep Fangcun available as an Org ID source even before a yiyu file is open.
(advice-add 'org-id-complete :around #'fangcun--id-complete)
(advice-add 'org-id-description :before-until #'fangcun--id-description)
(advice-add 'org-id-find :before-until #'fangcun--id-find)

(defun fangcun-loader--managed-file-p (file)
  "Return whether FILE belongs to a root in `fangcun-yiyus'."
  (and file
       (seq-some
        (lambda (yiyu)
          (let ((root (plist-get (cdr yiyu) :root)))
            (and (stringp root)
                 (file-in-directory-p file root))))
        fangcun-yiyus)))

(defun fangcun-loader--maybe-load ()
  "Load Fangcun when the current Org file belongs to a yiyu."
  (when (fangcun-loader--managed-file-p buffer-file-name)
    (require 'fangcun)
    (fangcun--setup-file-updates)))

(add-hook 'org-mode-hook #'fangcun-loader--maybe-load)

(provide 'fangcun-loader)

;;; fangcun-loader.el ends here
