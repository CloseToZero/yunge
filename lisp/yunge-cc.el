;;; yunge-cc.el --- C and C++ editing configuration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function dir-locals-to-string "files-x" (variables))
(declare-function project-current "project"
                  (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

(defvar auto-insert)
(defvar dir-locals-directory-cache)

(defconst yunge-cc-header-regexp "\\.h\\'"
  "Regexp matching C and C++ header file names.")

;; Most headers in this configuration are C++.  Project-local associations
;; take precedence over this global default in Emacs 31.
(add-to-list 'auto-mode-alist
             (cons yunge-cc-header-regexp 'c++-mode))

(defun yunge-cc--set-dir-local-c-header-mode (locals-file)
  "Update LOCALS-FILE so .h files use C mode, then save it.
LOCALS-FILE must name a directory-local variables file."
  (let ((auto-insert nil))
    (find-file locals-file))
  (widen)
  (goto-char (point-min))
  (forward-comment (point-max))
  (let* ((start (point))
         (variables (unless (eobp) (read (current-buffer))))
         (end (point)))
    (unless (listp variables)
      (user-error "Invalid directory-local variables in %s" locals-file))
    (let* ((mode-alist (alist-get 'auto-mode-alist variables))
           (new-mode-alist
            (cons (cons yunge-cc-header-regexp 'c-mode)
                  (assoc-delete-all yunge-cc-header-regexp mode-alist))))
      (setf (alist-get 'auto-mode-alist variables) new-mode-alist))
    (delete-region start end)
    (insert (dir-locals-to-string variables))
    (when (eobp)
      (insert "\n"))
    (setq dir-locals-directory-cache
          (assoc-delete-all (file-name-directory locals-file)
                            dir-locals-directory-cache))
    (save-buffer)))

(defun yunge-project-use-c-mode-for-headers ()
  "Make .h files in the current project open in C mode.
Store the override in the project root's `.dir-locals.el'.  Existing
header buffers keep their current major mode until reopened or reverted."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (locals-file (expand-file-name dir-locals-file root)))
    (require 'files-x)
    (save-window-excursion
      ;; In Emacs 31 `auto-mode-alist' is a special top-level dir-local key.
      (yunge-cc--set-dir-local-c-header-mode locals-file))
    (message "Project .h files now use C mode: %s" root)))

(provide 'yunge-cc)

;;; yunge-cc.el ends here
