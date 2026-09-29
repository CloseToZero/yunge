;;; fangcun-file.el --- Saved Fangcun file operations -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'fangcun)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(defun fangcun-file--required-string (value name)
  "Return non-empty string VALUE, or report invalid NAME."
  (unless (and (stringp value) (not (string-empty-p value)))
    (user-error "%s must be a non-empty string" name))
  value)

(defun fangcun-file--yiyu (id)
  "Return the active Fangcun yiyu named ID."
  (fangcun-file--required-string id "Yiyu ID")
  (or (seq-find
       (lambda (yiyu) (equal (fangcun-yiyu-id yiyu) id))
       (fangcun--ensure-session))
      (user-error "Unknown Fangcun yiyu: %s" id)))

(defun fangcun-file--relative-org-file (yiyu relative-file)
  "Return Org RELATIVE-FILE below YIYU, checking its root and extension."
  (fangcun-file--required-string relative-file "File")
  (when (file-name-absolute-p relative-file)
    (user-error "File must be relative to the yiyu root"))
  (let* ((root (fangcun-yiyu-root yiyu))
         (file (expand-file-name relative-file root)))
    (unless (file-in-directory-p file root)
      (user-error "File must stay below the yiyu root"))
    (unless (string-match-p "\\.org\\'" file)
      (user-error "File must name an Org file"))
    file))

(defun fangcun-file--indexed-node (id)
  "Return indexed node ID after ensuring the configured session."
  (fangcun-file--required-string id "Node ID")
  (fangcun--ensure-session)
  (or (fangcun-node-from-id id)
      (user-error "Fangcun node is not indexed: %s" id)))

(defun fangcun-file--node-region (node)
  "Return NODE's source region in the current Org buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((position (org-find-entry-with-id (fangcun-node-id node))))
        (unless position
          (user-error "Fangcun node ID no longer exists: %s"
                      (fangcun-node-id node)))
        (goto-char position))
      (org-back-to-heading-or-point-min t)
      (cons
       (if (= (org-outline-level) 0)
           (point-min)
         (line-beginning-position))
       (if (= (org-outline-level) 0)
           (point-max)
         (save-excursion (org-end-of-subtree t t)))))))

(defun fangcun-file-locate-node (id)
  "Locate indexed node ID in saved disk text.
Return a plist with :node, absolute :file, symbol :kind (file or heading),
1-based inclusive :start-line and :end-line, list :outline-path, and boolean
:modified-p.  The node metadata is indexed; the location reads current disk
content and does not return unsaved buffer text."
  (let* ((node (fangcun-file--indexed-node id))
         (file (fangcun--node-absolute-file node))
         (visiting-buffer (find-buffer-visiting file))
         (modified-p (and visiting-buffer
                          (buffer-modified-p visiting-buffer))))
    (unless (file-regular-p file)
      (user-error "Fangcun node file no longer exists: %s" file))
    (with-temp-buffer
      (setq default-directory (file-name-directory file))
      (insert-file-contents file)
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode)))
      (pcase-let* ((`(,beginning . ,end)
                    (fangcun-file--node-region node))
                   (heading-p
                    (save-excursion
                      (goto-char beginning)
                      (org-at-heading-p))))
        (list
         :node node
         :file file
         :kind (if heading-p 'heading 'file)
         :start-line (line-number-at-pos beginning t)
         :end-line (line-number-at-pos (max beginning (1- end)) t)
         :outline-path
         (when heading-p
           (save-excursion
             (goto-char beginning)
             (org-get-outline-path t)))
         :modified-p (and modified-p t))))))

(defun fangcun-file--index-saved-node (file yiyu id)
  "Index saved FILE in YIYU and return the node with ID.
The file is already saved; report a separate indexing failure clearly."
  (condition-case error-data
      (progn
        (fangcun--db-update-file-in-yiyu file yiyu t)
        (or (fangcun-node-from-id id)
            (user-error "Created Fangcun node was not indexed: %s" id)))
    (error
     (user-error
      "Saved %s, but indexing failed: %s; fix the error and run fangcun-db-sync"
      file (error-message-string error-data)))))

(defun fangcun-file--edit-saved-org-file (file yiyu function)
  "Call FUNCTION in saved Org FILE, then save and reindex it for YIYU.
Refuse an unsaved visiting buffer.  Refresh a clean visiting buffer when disk
has changed, preserving its modes before editing."
  (let* ((visiting-buffer (find-buffer-visiting file))
         (buffer (or visiting-buffer (find-file-noselect file)))
         (temporary-buffer-p (null visiting-buffer))
         result)
    (unwind-protect
        (with-current-buffer buffer
          (unless (derived-mode-p 'org-mode)
            (user-error "Fangcun edits require an Org buffer"))
          (when (buffer-modified-p)
            (user-error
             "Fangcun file has unsaved changes; save it before editing: %s"
             file))
          (unless (verify-visited-file-modtime buffer)
            (revert-buffer t t t))
          (save-excursion
            (save-restriction
              (widen)
              (atomic-change-group
                (setq result (funcall function))
                (let ((fangcun-db-update-on-save nil))
                  (save-buffer)))))
          (fangcun-file--index-saved-node file yiyu result))
      (when (and temporary-buffer-p (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (buffer-modified-p)
            (set-buffer-modified-p nil)))
        (kill-buffer buffer)))))

(defun fangcun-file--heading-at-path (heading-path)
  "Return the unique heading position matching HEADING-PATH."
  (let (matches)
    (org-map-entries
     (lambda ()
       (when (equal (org-get-outline-path t) heading-path)
         (push (point) matches)))
     nil 'file)
    (pcase matches
      ('nil
       (user-error "Org heading path does not exist: %S" heading-path))
      (`(,position) position)
      (_
       (user-error "Org heading path is ambiguous: %S" heading-path)))))

(defun fangcun-file-create-heading-node (yiyu-id relative-file heading-path)
  "Save an ID on HEADING-PATH in YIYU-ID's Org RELATIVE-FILE.
Return the indexed node.  HEADING-PATH is a non-empty list of non-empty heading
titles.  Refuse unsaved visiting buffers; refresh clean buffers changed on disk.
If indexing fails after the save, the saved file remains for a later sync."
  (unless (and (listp heading-path)
               heading-path
               (seq-every-p
                (lambda (title)
                  (and (stringp title) (not (string-empty-p title))))
                heading-path))
    (user-error "Heading path must be a non-empty list of heading titles"))
  (let* ((yiyu (fangcun-file--yiyu yiyu-id))
         (file (fangcun-file--relative-org-file yiyu relative-file)))
    (unless (file-regular-p file)
      (user-error "Fangcun file does not exist: %s" relative-file))
    (fangcun-file--edit-saved-org-file
     file yiyu
     (lambda ()
       (goto-char (fangcun-file--heading-at-path heading-path))
       (fangcun--node-id-get-create)))))

(defun fangcun-file-create-node (yiyu-id relative-file &optional title)
  "Create an ID-bearing Org RELATIVE-FILE in YIYU-ID and return its node.
TITLE is an optional string.  Never overwrite an existing file, including one
created concurrently.  If indexing fails after creation, the saved file
remains for a later sync."
  (unless (or (null title) (stringp title))
    (user-error "Title must be a string"))
  (let* ((yiyu (fangcun-file--yiyu yiyu-id))
         (root (fangcun-yiyu-root yiyu))
         (file (fangcun-file--relative-org-file yiyu relative-file))
         (directory (file-name-directory file)))
    (when-let* ((reason (fangcun--new-file-name-error file root)))
      (user-error "%s" reason))
    (make-directory directory t)
    (let ((id (org-id-new)))
      (with-temp-buffer
        (setq default-directory directory
              buffer-file-coding-system 'utf-8-unix)
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (unless (string-empty-p (or title ""))
          (insert "#+title: " title "\n"))
        (insert "\n")
        (goto-char (point-min))
        (org-entry-put (point) "ID" id)
        (write-region (point-min) (point-max) file nil 'silent nil 'excl))
      (fangcun-file--index-saved-node file yiyu id))))

(provide 'fangcun-file)

;;; fangcun-file.el ends here
