;;; fangcun.el --- ID-based Org note navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'sqlite)
(require 'subr-x)
(require 'yunge-state)

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

(defcustom fangcun-database-file
  (expand-file-name "fangcun.sqlite" yunge-var-directory)
  "SQLite database used by Fangcun."
  :type 'file
  :group 'fangcun)

(cl-defstruct fangcun-yiyu
  id
  name
  root)

(cl-defstruct fangcun-node
  id
  yiyu-id
  yiyu-name
  yiyu-root
  file
  title)

(defun fangcun--configured-yiyus ()
  "Return normalized entries from `fangcun-yiyus'."
  (mapcar
   (lambda (entry)
     (let ((id (car entry))
           (name (plist-get (cdr entry) :name))
           (root (plist-get (cdr entry) :root)))
       (unless (and (symbolp id)
                    (stringp name)
                    (not (string-empty-p name))
                    (stringp root)
                    (not (string-empty-p root)))
         (user-error "Invalid Fangcun yiyu: %S" entry))
       (make-fangcun-yiyu
        :id (symbol-name id)
        :name name
        :root (file-name-as-directory (expand-file-name root)))))
   fangcun-yiyus))

(defun fangcun--call-with-database (function)
  "Call FUNCTION with an open Fangcun database."
  (unless (sqlite-available-p)
    (user-error "This Emacs was built without SQLite support"))
  (let* ((file (expand-file-name fangcun-database-file))
         (directory (file-name-directory file))
         database)
    (make-directory directory t)
    (setq database (sqlite-open file nil))
    (unwind-protect
        (progn
          (sqlite-execute database "PRAGMA foreign_keys = ON")
          (funcall function database))
      (sqlite-close database))))

(defun fangcun--initialize-database (database)
  "Create the current Fangcun schema in DATABASE when needed."
  (sqlite-execute
   database
   (concat
    "CREATE TABLE IF NOT EXISTS yiyus ("
    "id TEXT PRIMARY KEY, "
    "name TEXT NOT NULL, "
    "root TEXT NOT NULL)"))
  (sqlite-execute
   database
   (concat
    "CREATE TABLE IF NOT EXISTS nodes ("
    "id TEXT PRIMARY KEY, "
    "yiyu_id TEXT NOT NULL, "
    "file TEXT NOT NULL, "
    "title TEXT NOT NULL, "
    "FOREIGN KEY (yiyu_id) REFERENCES yiyus (id))")))

(defun fangcun--display-title (title fallback)
  "Return a plain display TITLE, or FALLBACK when it is empty."
  (let ((display
         (and title
              (string-trim
               (substring-no-properties
                (org-link-display-format title))))))
    (if (and display (not (string-empty-p display)))
        display
      fallback)))

(defun fangcun--file-title (relative-file)
  "Return the current Org file title, falling back to RELATIVE-FILE."
  (let* ((keywords (org-collect-keywords '("title")))
         (titles (cdr (assoc "TITLE" keywords))))
    (fangcun--display-title
     (and titles (string-join titles " "))
     (file-name-sans-extension relative-file))))

(defun fangcun--collect-nodes-from-buffer
  (buffer yiyu relative-file)
  "Return Fangcun nodes parsed from Org BUFFER.
RELATIVE-FILE names BUFFER's file relative to YIYU's root."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let ((file-title (fangcun--file-title relative-file))
              nodes)
          (goto-char (point-min))
          (when-let* ((id (org-id-get)))
            (push
             (make-fangcun-node
              :id id
              :yiyu-id (fangcun-yiyu-id yiyu)
              :yiyu-name (fangcun-yiyu-name yiyu)
              :yiyu-root (fangcun-yiyu-root yiyu)
              :file relative-file
              :title file-title)
             nodes))
          (org-map-entries
           (lambda ()
             (when-let* ((id (org-id-get)))
               (push
                (make-fangcun-node
                 :id id
                 :yiyu-id (fangcun-yiyu-id yiyu)
                 :yiyu-name (fangcun-yiyu-name yiyu)
                 :yiyu-root (fangcun-yiyu-root yiyu)
                 :file relative-file
                 :title
                 (fangcun--display-title
                  (org-get-heading t t t) id))
                nodes)))
           nil nil)
          (nreverse nodes))))))

(defun fangcun--parse-file (yiyu file)
  "Return Fangcun nodes from FILE on disk, owned by YIYU."
  (let* ((relative-file
          (file-relative-name file (fangcun-yiyu-root yiyu)))
         (buffer (find-buffer-visiting file))
         (reusable
          (and buffer
               (with-current-buffer buffer
                 (and (derived-mode-p 'org-mode)
                      (not (buffer-modified-p))
                      (verify-visited-file-modtime buffer))))))
    (if reusable
        (fangcun--collect-nodes-from-buffer
         buffer yiyu relative-file)
      (with-temp-buffer
        (setq default-directory (file-name-directory file))
        (insert-file-contents file)
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (fangcun--collect-nodes-from-buffer
         (current-buffer) yiyu relative-file)))))

(defun fangcun--insert-yiyu (database yiyu)
  "Insert YIYU into DATABASE."
  (sqlite-execute
   database
   "INSERT INTO yiyus (id, name, root) VALUES (?, ?, ?)"
   (vector
    (fangcun-yiyu-id yiyu)
    (fangcun-yiyu-name yiyu)
    (fangcun-yiyu-root yiyu))))

(defun fangcun--insert-node (database node)
  "Insert NODE into DATABASE."
  (sqlite-execute
   database
   (concat
    "INSERT INTO nodes "
    "(id, yiyu_id, file, title) "
    "VALUES (?, ?, ?, ?)")
   (vector
    (fangcun-node-id node)
    (fangcun-node-yiyu-id node)
    (fangcun-node-file node)
    (fangcun-node-title node))))

;;;###autoload
(defun fangcun-db-sync ()
  "Rebuild the Fangcun database from configured yiyu roots."
  (interactive)
  (let ((yiyus (fangcun--configured-yiyus)))
    (unless yiyus
      (user-error "Configure `fangcun-yiyus' before syncing"))
    (let ((database-file (expand-file-name fangcun-database-file)))
      (when (file-exists-p database-file)
        (delete-file database-file)))
    (let ((result
           (fangcun--call-with-database
            (lambda (database)
              (fangcun--initialize-database database)
              (with-sqlite-transaction database
                (let ((file-count 0)
                      (node-count 0))
                  (dolist (yiyu yiyus)
                    (unless (file-directory-p
                             (fangcun-yiyu-root yiyu))
                      (user-error
                       "Fangcun yiyu does not exist: %s"
                       (fangcun-yiyu-root yiyu)))
                    (fangcun--insert-yiyu database yiyu)
                    (dolist
                        (file
                         (directory-files-recursively
                          (fangcun-yiyu-root yiyu) "\\.org\\'"))
                      (cl-incf file-count)
                      (dolist (node (fangcun--parse-file yiyu file))
                        (fangcun--insert-node database node)
                        (cl-incf node-count))))
                  (list :yiyus (length yiyus)
                        :files file-count
                        :nodes node-count)))))))
      (message "Fangcun indexed %d nodes from %d files in %d yiyu roots"
               (plist-get result :nodes)
               (plist-get result :files)
               (plist-get result :yiyus))
      result)))

(defun fangcun--node-from-row (row)
  "Return a Fangcun node represented by SQLite ROW."
  (make-fangcun-node
   :id (elt row 0)
   :yiyu-id (elt row 1)
   :yiyu-name (elt row 2)
   :yiyu-root (elt row 3)
   :file (elt row 4)
   :title (elt row 5)))

(defun fangcun-node-list ()
  "Return all nodes currently stored in the Fangcun database."
  (fangcun--call-with-database
   (lambda (database)
     (fangcun--initialize-database database)
     (mapcar
      #'fangcun--node-from-row
      (sqlite-select
       database
       (concat
        "SELECT n.id, n.yiyu_id, y.name, y.root, n.file, n.title "
        "FROM nodes AS n "
        "JOIN yiyus AS y ON y.id = n.yiyu_id "
        "ORDER BY n.title COLLATE NOCASE, y.name COLLATE NOCASE, "
        "n.file, n.id"))))))

(defun fangcun--node-candidate (node)
  "Return a unique completion candidate for NODE."
  (let ((title (fangcun-node-title node)))
    (propertize
     (concat
      title
      (propertize
       (concat "\u2063" (fangcun-node-id node))
       'invisible t))
     'fangcun-node node)))

(defun fangcun--node-annotation (candidate)
  "Return the location annotation for CANDIDATE."
  (when-let* ((node (get-text-property 0 'fangcun-node candidate)))
    (propertize
     (format "  %s › %s"
             (fangcun-node-yiyu-name node)
             (fangcun-node-file node))
     'face 'completions-annotations)))

(defun fangcun--read-node ()
  "Read and return a Fangcun node."
  (let* ((nodes (fangcun-node-list))
         (candidates
          (mapcar
           (lambda (node)
             (cons (fangcun--node-candidate node) node))
           nodes))
         (completion-extra-properties
          '(:category fangcun-node
            :annotation-function fangcun--node-annotation)))
    (unless candidates
      (user-error "No Fangcun nodes; run `fangcun-db-sync' first"))
    (let ((choice
           (completing-read "Fangcun node: " candidates nil t)))
      (cdr (assoc choice candidates)))))

(defun fangcun-node-visit (node)
  "Visit the Org NODE and return it."
  (let ((file
         (expand-file-name
          (fangcun-node-file node)
          (fangcun-node-yiyu-root node))))
    (unless (file-exists-p file)
      (user-error "Fangcun node file no longer exists: %s" file))
    (find-file file)
    (widen)
    (if-let* ((position
               (org-find-entry-with-id (fangcun-node-id node))))
        (goto-char position)
      (user-error "Fangcun node ID no longer exists: %s"
                  (fangcun-node-id node)))
    (org-fold-show-context 'link-search)
    node))

;;;###autoload
(defun fangcun-node-find ()
  "Choose a Fangcun node and visit it."
  (interactive)
  (fangcun-node-visit (fangcun--read-node)))

;;;###autoload
(defun fangcun-node-insert ()
  "Choose a Fangcun node and insert an Org ID link to it."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun node links can only be inserted in Org buffers"))
  (let ((node (fangcun--read-node)))
    (insert
     (org-link-make-string
      (concat "id:" (fangcun-node-id node))
      (fangcun-node-title node)))
    node))

(provide 'fangcun)

;;; fangcun.el ends here
