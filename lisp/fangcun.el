;;; fangcun.el --- ID-based Org note navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'button)
(require 'yunge-state)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'seq)
(require 'sqlite)
(require 'subr-x)
(require 'yunge-jump-history)

(defgroup fangcun nil
  "Org-based personal knowledge management."
  :group 'org)

(defun fangcun--set-database-file (symbol value)
  "Set SYMBOL to the normalized absolute file name VALUE."
  (unless (and (stringp value) (file-name-absolute-p value))
    (error "%s must be an absolute file name: %S" symbol value))
  (set-default symbol (expand-file-name value)))

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
  (yunge-var-file "fangcun" "fangcun.sqlite")
  "Absolute file name of the SQLite database used by Fangcun."
  :type 'file
  :set #'fangcun--set-database-file
  :group 'fangcun)

(defcustom fangcun-db-update-on-save t
  "Whether saving a Fangcun Org file updates its database entries."
  :type 'boolean
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
  title
  aliases)

(cl-defstruct fangcun-link
  source-id
  target-id
  position)

(cl-defstruct fangcun-backlink
  node
  position)

(defconst fangcun-backlinks-buffer-name "*Fangcun Backlinks*")

(defvar-local fangcun-backlinks-target-id nil
  "ID of the node shown in the current Fangcun backlinks buffer.")

(defvar-keymap fangcun-backlinks-mode-map
  :parent special-mode-map
  "RET" #'fangcun-backlink-visit)

(define-derived-mode fangcun-backlinks-mode special-mode "Fangcun Backlinks"
  "Major mode for displaying backlinks to one Fangcun node."
  (setq-local revert-buffer-function #'fangcun-backlinks-refresh))

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

(defun fangcun--yiyu-containing-file (file yiyus)
  "Return the member of YIYUS containing FILE, or nil."
  (seq-find
   (lambda (yiyu)
     (file-in-directory-p file (fangcun-yiyu-root yiyu)))
   yiyus))

(defun fangcun--portable-file-name-error (name)
  "Return why file NAME is not portable, or nil."
  (let ((invalid
         (delete-dups
          (seq-filter
           (lambda (character)
             (or (< character 32)
                 (memq character
                       '(?< ?> ?: ?\" ?/ ?\\ ?| ?? ?*))))
           (string-to-list name)))))
    (cond
     ((member name '("" "." ".."))
      (format "%S is not a file name" name))
     (invalid
      (format
       "File name %S contains non-portable characters: %s"
       name
       (mapconcat
        (lambda (character)
          (if (< character 32)
              (format "U+%04X" character)
            (char-to-string character)))
        invalid ", ")))
     ((or (string-prefix-p " " name)
          (string-suffix-p " " name)
          (string-suffix-p "." name))
      (format
       "File name %S starts or ends with a non-portable character"
       name))
     ((string-match-p
       (concat
        "\\`\\(?:con\\|prn\\|aux\\|nul\\|"
        "com[1-9]\\|lpt[1-9]\\)"
        "\\(?:\\..*\\)?\\'")
       (downcase name))
      (format "File name %S is reserved on Windows" name)))))

(defun fangcun--new-file-name-error (name directory)
  "Return why NAME cannot name a new file in DIRECTORY, or nil."
  (let ((file (expand-file-name name directory)))
    (cond
     ((fangcun--portable-file-name-error name))
     ((not (string-suffix-p ".org" name))
      "Fangcun file names must end with .org")
     ((not (file-directory-p directory))
      (format "Fangcun directory does not exist: %s" directory))
     ((file-exists-p file)
      (format "File already exists: %s" file))
     ((find-buffer-visiting file)
      (format "A buffer is already visiting: %s" file))
     ((let* ((name (file-name-nondirectory file))
             (conflict
              (seq-find
               (lambda (entry)
                 (and (not (equal entry name))
                      (string-equal (downcase entry)
                                    (downcase name))))
               (directory-files directory nil nil t))))
        (when conflict
          (format
           "File name differs only by case from existing %S"
           conflict)))))))

(defun fangcun--call-with-database (function)
  "Call FUNCTION with an open, initialized Fangcun database."
  (unless (sqlite-available-p)
    (user-error "This Emacs was built without SQLite support"))
  (let* ((file fangcun-database-file)
         (directory (file-name-directory file))
         (new-database-p (not (file-exists-p file)))
         database)
    (when new-database-p
      (make-directory directory t))
    (setq database (sqlite-open file nil))
    (unwind-protect
        (progn
          (sqlite-execute database "PRAGMA foreign_keys = ON")
          (when new-database-p
            (fangcun--create-schema database))
          (funcall function database))
      (sqlite-close database))))

(defun fangcun--create-schema (database)
  "Create the current Fangcun schema in a new DATABASE."
  (sqlite-execute
   database
   (concat
    "CREATE TABLE yiyus ("
    "id TEXT PRIMARY KEY, "
    "name TEXT NOT NULL, "
    "root TEXT NOT NULL)"))
  ;; The database resolves an ID to its file.  Org searches that file for the
  ;; entry, avoiding byte positions that become stale when a buffer changes.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE nodes ("
    "id TEXT PRIMARY KEY, "
    "yiyu_id TEXT NOT NULL, "
    "file TEXT NOT NULL, "
    "title TEXT NOT NULL, "
    "FOREIGN KEY (yiyu_id) REFERENCES yiyus (id))"))
  ;; Store aliases as rows instead of serializing them into NODES because each
  ;; alias is an independent completion name.  The composite primary key also
  ;; prevents duplicate names for one node and supports cascading node deletes.
  ;; The same alias may belong to different nodes; completion disambiguates
  ;; those nodes by their yiyu and file.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE aliases ("
    "node_id TEXT NOT NULL, "
    "alias TEXT NOT NULL, "
    "PRIMARY KEY (node_id, alias), "
    "FOREIGN KEY (node_id) REFERENCES nodes (id) "
    "ON DELETE CASCADE)"))
  ;; Keep one row for every ID-link occurrence.  Backlink views may group
  ;; rows by SOURCE_ID, but POSITION is needed to visit the chosen link.
  ;;
  ;; SOURCE_ID references the nearest enclosing Fangcun node because links
  ;; without an owning node are not part of the Fangcun graph.
  ;;
  ;; TARGET_ID deliberately has no foreign key.  An Org ID link may be
  ;; unresolved or point outside the configured yiyu roots.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE links ("
    "source_id TEXT NOT NULL, "
    "target_id TEXT NOT NULL, "
    "position INTEGER NOT NULL, "
    "FOREIGN KEY (source_id) REFERENCES nodes (id) "
    "ON DELETE CASCADE)"))
  ;; Backlink lookup starts from TARGET_ID.  SOURCE_ID does not need another
  ;; index until Fangcun has a query that searches links in that direction.
  (sqlite-execute
   database
   "CREATE INDEX links_target_id ON links (target_id)"))

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

(defun fangcun--aliases-at-point ()
  "Return the aliases assigned to the current Org entry."
  (when-let* ((value (org-entry-get (point) "ALIASES")))
    (delete-dups (split-string-and-unquote value))))

(defun fangcun--node-id-at-point ()
  "Return the nearest enclosing Fangcun node ID, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (org-back-to-heading-or-point-min t)
      (let ((id (org-id-get)))
        (while (and (not id) (not (bobp)))
          (if (org-up-heading-safe)
              (setq id (org-id-get))
            (goto-char (point-min))
            (setq id (org-id-get))))
        id))))

(defun fangcun--element-owner-id (element)
  "Return the nearest Fangcun node ID containing Org ELEMENT, or nil."
  (seq-some
   (lambda (ancestor)
     (when (memq (org-element-type ancestor) '(headline org-data))
       (org-element-property :ID ancestor)))
   (org-element-lineage element)))

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
          ;; Point may already be on the first heading.  Without this check,
          ;; its ID would be collected here and again by `org-map-entries'.
          (when (= (org-outline-level) 0)
            (when-let* ((id (org-id-get)))
              (push
               (make-fangcun-node
                :id id
                :yiyu-id (fangcun-yiyu-id yiyu)
                :yiyu-name (fangcun-yiyu-name yiyu)
                :yiyu-root (fangcun-yiyu-root yiyu)
                :file relative-file
                :title file-title
                :aliases (fangcun--aliases-at-point))
               nodes)))
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
                  (org-get-heading t t t) id)
                 :aliases (fangcun--aliases-at-point))
                nodes)))
           nil nil)
          (nreverse nodes))))))

(defun fangcun--collect-links-from-buffer (buffer)
  "Return ID links owned by Fangcun nodes in Org BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let (links)
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (element)
              (when (equal (org-element-property :type element) "id")
                (when-let*
                    ((source-id
                      (fangcun--element-owner-id element)))
                  (let* ((path
                          (org-element-property :path element))
                         (target-id
                          ;; A search suffix selects a location inside the
                          ;; target node; the backlink belongs to the node.
                          (if (string-match "::.*\\'" path)
                              (substring path 0 (match-beginning 0))
                            path))
                         (position
                          (org-element-property :begin element)))
                    (push
                     (make-fangcun-link
                      :source-id source-id
                      :target-id target-id
                      :position position)
                     links))))))
          (nreverse links))))))

(defun fangcun--collect-file-data-from-buffer
    (buffer yiyu relative-file)
  "Return nodes and links parsed from Org BUFFER.
RELATIVE-FILE names BUFFER's file relative to YIYU's root."
  (list :nodes
        (fangcun--collect-nodes-from-buffer
         buffer yiyu relative-file)
        :links
        (fangcun--collect-links-from-buffer buffer)))

(defun fangcun--parse-file (yiyu file)
  "Return Fangcun nodes and links from FILE on disk, owned by YIYU."
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
        (fangcun--collect-file-data-from-buffer
         buffer yiyu relative-file)
      (with-temp-buffer
        (setq default-directory (file-name-directory file))
        (insert-file-contents file)
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (fangcun--collect-file-data-from-buffer
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
    (fangcun-node-title node)))
  (dolist (alias (fangcun-node-aliases node))
    (sqlite-execute
     database
     "INSERT INTO aliases (node_id, alias) VALUES (?, ?)"
     (vector (fangcun-node-id node) alias))))

(defun fangcun--insert-link (database link)
  "Insert LINK into DATABASE."
  (sqlite-execute
   database
   (concat
    "INSERT INTO links "
    "(source_id, target_id, position) "
    "VALUES (?, ?, ?)")
   (vector
    (fangcun-link-source-id link)
    (fangcun-link-target-id link)
    (fangcun-link-position link))))

(defun fangcun--insert-file-data (database data)
  "Insert parsed Fangcun file DATA into DATABASE and return its counts."
  (let ((nodes (plist-get data :nodes))
        (links (plist-get data :links)))
    (dolist (node nodes)
      (fangcun--insert-node database node))
    (dolist (link links)
      (fangcun--insert-link database link))
    (list :nodes (length nodes)
          :aliases (apply #'+ (mapcar
                               (lambda (node)
                                 (length (fangcun-node-aliases node)))
                               nodes))
          :links (length links))))

;;;###autoload
(defun fangcun-db-sync ()
  "Rebuild the Fangcun database from configured yiyu roots."
  (interactive)
  (let ((yiyus (fangcun--configured-yiyus)))
    (unless yiyus
      (user-error "Configure `fangcun-yiyus' before syncing"))
    (let ((database-file fangcun-database-file))
      (when (file-exists-p database-file)
        (delete-file database-file)))
    (let ((result
           (fangcun--call-with-database
            (lambda (database)
              (with-sqlite-transaction database
                (let ((file-count 0)
                      (node-count 0)
                      (alias-count 0)
                      (link-count 0))
                  (dolist (yiyu yiyus)
                    (unless (file-directory-p
                             (fangcun-yiyu-root yiyu))
                      (user-error
                       "Fangcun yiyu does not exist: %s"
                       (fangcun-yiyu-root yiyu)))
                    (fangcun--insert-yiyu database yiyu)
                    (dolist (file
                             (directory-files-recursively
                              (fangcun-yiyu-root yiyu) "\\.org\\'"))
                      (cl-incf file-count)
                      (let ((counts
                             (fangcun--insert-file-data
                              database
                              (fangcun--parse-file yiyu file))))
                        (cl-incf node-count
                                 (plist-get counts :nodes))
                        (cl-incf alias-count
                                 (plist-get counts :aliases))
                        (cl-incf link-count
                                 (plist-get counts :links)))))
                  (list :yiyus (length yiyus)
                        :files file-count
                        :nodes node-count
                        :aliases alias-count
                        :links link-count)))))))
      (message
       (concat
        "Fangcun indexed %d nodes, %d aliases, and %d links "
        "from %d files in %d yiyu roots")
       (plist-get result :nodes)
       (plist-get result :aliases)
       (plist-get result :links)
       (plist-get result :files)
       (plist-get result :yiyus))
      result)))

(defun fangcun--db-update-file-in-yiyu (file yiyu &optional no-message)
  "Replace database entries for saved Org FILE owned by YIYU.
When NO-MESSAGE is non-nil, do not report the indexed counts."
  (unless (file-regular-p file)
    (user-error "Fangcun file does not exist: %s" file))
  (unless (string-match-p "\\.org\\'" file)
    (user-error "Fangcun only indexes Org files: %s" file))
  (when-let* ((buffer (find-buffer-visiting file)))
    (when (buffer-modified-p buffer)
      (user-error "Save the Fangcun file before updating it")))
  (unless (file-exists-p fangcun-database-file)
    (user-error "Run fangcun-db-sync before updating individual files"))
  (let* ((relative-file
          (file-relative-name file (fangcun-yiyu-root yiyu)))
         (data (fangcun--parse-file yiyu file))
         (result
          (fangcun--call-with-database
           (lambda (database)
             (let ((row
                    (car
                     (sqlite-select
                      database
                      (concat
                       "SELECT name, root FROM yiyus "
                       "WHERE id = ?")
                      (vector (fangcun-yiyu-id yiyu))))))
               (unless
                   (and row
                        (equal (elt row 0)
                               (fangcun-yiyu-name yiyu))
                        (file-equal-p
                         (elt row 1) (fangcun-yiyu-root yiyu)))
                 (user-error
                  (concat
                   "Fangcun yiyu configuration changed; "
                   "run fangcun-db-sync"))))
             (with-sqlite-transaction database
               (sqlite-execute
                database
                (concat
                 "DELETE FROM nodes "
                 "WHERE yiyu_id = ? AND file = ?")
                (vector (fangcun-yiyu-id yiyu) relative-file))
               (fangcun--insert-file-data database data))))))
    (unless no-message
      (message "Fangcun indexed %d nodes, %d aliases, and %d links from %s"
               (plist-get result :nodes)
               (plist-get result :aliases)
               (plist-get result :links)
               (abbreviate-file-name file)))
    result))

;;;###autoload
(defun fangcun-db-update-file (file &optional no-message)
  "Replace database entries for saved Org FILE.
When NO-MESSAGE is non-nil, do not report the indexed counts."
  (interactive
   (list
    (or buffer-file-name
        (user-error "The current buffer is not visiting a file"))))
  (setq file (expand-file-name file))
  (let* ((yiyus (fangcun--configured-yiyus))
         (yiyu
          (or (fangcun--yiyu-containing-file file yiyus)
              (user-error
               "File is outside the configured Fangcun yiyus: %s"
               file))))
    (fangcun--db-update-file-in-yiyu file yiyu no-message)))

(defun fangcun--update-after-save ()
  "Update the current saved file when Fangcun manages it."
  (when (and fangcun-db-update-on-save
             buffer-file-name
             (file-exists-p fangcun-database-file)
             (string-match-p "\\.org\\'" buffer-file-name))
    (let* ((yiyus (fangcun--configured-yiyus))
           (yiyu
            (fangcun--yiyu-containing-file buffer-file-name yiyus)))
      (when yiyu
        (fangcun--db-update-file-in-yiyu buffer-file-name yiyu t)))))

;;;###autoload
(defun fangcun--setup-update-on-save ()
  "Arrange for the current Org buffer to update Fangcun after saving."
  (add-hook 'after-save-hook #'fangcun--update-after-save nil t))

;;;###autoload
(add-hook 'org-mode-hook #'fangcun--setup-update-on-save)

(defun fangcun--node-from-row (row)
  "Return a Fangcun node represented by SQLite ROW."
  (make-fangcun-node
   :id (elt row 0)
   :yiyu-id (elt row 1)
   :yiyu-name (elt row 2)
   :yiyu-root (elt row 3)
   :file (elt row 4)
   :title (elt row 5)))

(defun fangcun--attach-aliases (nodes rows)
  "Attach aliases from SQLite ROWS to NODES and return NODES.
Each row contains a node ID followed by one alias."
  (let ((nodes-by-id (make-hash-table :test #'equal)))
    (dolist (node nodes)
      (push node (gethash (fangcun-node-id node) nodes-by-id)))
    (dolist (row rows)
      (dolist (node (gethash (elt row 0) nodes-by-id))
        (push (elt row 1) (fangcun-node-aliases node))))
    (dolist (node nodes)
      (setf (fangcun-node-aliases node)
            (nreverse (fangcun-node-aliases node))))
    nodes))

(defun fangcun-node-from-id (id)
  "Return the Fangcun node named ID, or nil when it is not indexed."
  (fangcun--call-with-database
   (lambda (database)
     (when-let* ((row
                  (car
                   (sqlite-select
                    database
                    (concat
                     "SELECT n.id, n.yiyu_id, y.name, y.root, "
                     "n.file, n.title "
                     "FROM nodes AS n "
                     "JOIN yiyus AS y ON y.id = n.yiyu_id "
                     "WHERE n.id = ? LIMIT 1")
                    (vector id))))
                 (node (fangcun--node-from-row row)))
       (fangcun--attach-aliases
        (list node)
        (sqlite-select
         database
         (concat
          "SELECT node_id, alias FROM aliases "
          "WHERE node_id = ? ORDER BY alias COLLATE NOCASE")
         (vector id)))
       node))))

(defun fangcun-node-list ()
  "Return all nodes currently stored in the Fangcun database."
  (fangcun--call-with-database
   (lambda (database)
     (let ((nodes
            (mapcar
             #'fangcun--node-from-row
             (sqlite-select
              database
              (concat
               "SELECT n.id, n.yiyu_id, y.name, y.root, "
               "n.file, n.title "
               "FROM nodes AS n "
               "JOIN yiyus AS y ON y.id = n.yiyu_id "
               "ORDER BY n.title COLLATE NOCASE, "
               "y.name COLLATE NOCASE, n.file, n.id")))))
       (fangcun--attach-aliases
        nodes
        (sqlite-select
         database
         (concat
          "SELECT node_id, alias FROM aliases "
          "ORDER BY node_id, alias COLLATE NOCASE")))))))

(defun fangcun--id-find (id &optional markerp)
  "Return the Fangcun location of ID, or nil to let Org continue.
When MARKERP is non-nil, return the location as a marker."
  (setq id
        (cond
         ((symbolp id) (symbol-name id))
         ((numberp id) (number-to-string id))
         (t id)))
  (when (file-exists-p fangcun-database-file)
    (when-let* ((row
                 (car
                  (fangcun--call-with-database
                   (lambda (database)
                     (sqlite-select
                      database
                      (concat
                       "SELECT y.root, n.file "
                       "FROM nodes AS n "
                       "JOIN yiyus AS y ON y.id = n.yiyu_id "
                       "WHERE n.id = ?")
                      (vector id)))))))
      (org-id-find-id-in-file
       id (expand-file-name (elt row 1) (elt row 0)) markerp))))

(advice-add 'org-id-find :before-until #'fangcun--id-find)

(defun fangcun--backlink-from-row (row)
  "Return a Fangcun backlink represented by SQLite ROW."
  (make-fangcun-backlink
   :node (fangcun--node-from-row (cl-subseq row 0 6))
   :position (elt row 6)))

(defun fangcun--attach-backlink-aliases
    (database backlinks target-id)
  "Attach aliases to BACKLINKS targeting TARGET-ID in DATABASE."
  (fangcun--attach-aliases
   (mapcar #'fangcun-backlink-node backlinks)
   (sqlite-select
    database
    (concat
     "SELECT DISTINCT a.node_id, a.alias "
     "FROM aliases AS a "
     "JOIN links AS l ON l.source_id = a.node_id "
     "WHERE l.target_id = ? "
     "ORDER BY a.node_id, a.alias COLLATE NOCASE")
    (vector target-id)))
  backlinks)

(defun fangcun-backlink-list (target-id)
  "Return unique source nodes linking to TARGET-ID.
When one source contains several links, retain its first occurrence."
  (fangcun--call-with-database
   (lambda (database)
     (fangcun--attach-backlink-aliases
      database
      (mapcar
       #'fangcun--backlink-from-row
       (sqlite-select
        database
        (concat
         "SELECT n.id, n.yiyu_id, y.name, y.root, "
         "n.file, n.title, first_link.position "
         "FROM ("
         "SELECT source_id, MIN(position) AS position "
         "FROM links WHERE target_id = ? GROUP BY source_id"
         ") AS first_link "
         "JOIN nodes AS n ON n.id = first_link.source_id "
         "JOIN yiyus AS y ON y.id = n.yiyu_id "
         "ORDER BY n.title COLLATE NOCASE, "
         "y.name COLLATE NOCASE, n.file, n.id")
        (vector target-id)))
      target-id))))

(defun fangcun-backlink-occurrence-list (target-id)
  "Return every indexed backlink occurrence to TARGET-ID."
  (fangcun--call-with-database
   (lambda (database)
     (fangcun--attach-backlink-aliases
      database
      (mapcar
       #'fangcun--backlink-from-row
       (sqlite-select
        database
        (concat
         "SELECT n.id, n.yiyu_id, y.name, y.root, "
         "n.file, n.title, l.position "
         "FROM links AS l "
         "JOIN nodes AS n ON n.id = l.source_id "
         "JOIN yiyus AS y ON y.id = n.yiyu_id "
         "WHERE l.target_id = ? "
         "ORDER BY n.title COLLATE NOCASE, "
         "y.name COLLATE NOCASE, n.file, n.id, l.position")
        (vector target-id)))
      target-id))))

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

(defun fangcun--node-candidates (node)
  "Return completion pairs for NODE's title and aliases."
  (mapcar
   (lambda (title)
     (let ((candidate-node (copy-fangcun-node node)))
       (setf (fangcun-node-title candidate-node) title)
       (cons (fangcun--node-candidate candidate-node)
             candidate-node)))
   (delete-dups
    (cons (fangcun-node-title node)
          (copy-sequence (fangcun-node-aliases node))))))

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
           (mapcan #'fangcun--node-candidates nodes))
         (completion-extra-properties
          '(:category fangcun-node
            :annotation-function fangcun--node-annotation)))
    (unless candidates
      (user-error "No Fangcun nodes; run `fangcun-db-sync' first"))
    (let ((choice
           (completing-read "Fangcun node: " candidates nil t)))
      (cdr (assoc choice candidates)))))

(defun fangcun--read-backlink (target-id)
  "Read and return a backlink to TARGET-ID."
  (let* ((backlinks (fangcun-backlink-list target-id))
         (candidates
           (mapcan
            (lambda (backlink)
              (mapcar
               (lambda (candidate)
                 (let ((candidate-backlink
                        (copy-fangcun-backlink backlink)))
                   (setf
                    (fangcun-backlink-node candidate-backlink)
                    (cdr candidate))
                   (cons (car candidate) candidate-backlink)))
               (fangcun--node-candidates
                (fangcun-backlink-node backlink))))
            backlinks))
         (completion-extra-properties
          '(:category fangcun-node
            :annotation-function fangcun--node-annotation)))
    (unless candidates
      (user-error "No backlinks to the current Fangcun node"))
    (let ((choice
           (completing-read "Fangcun backlink: " candidates nil t)))
      (cdr (assoc choice candidates)))))

(defun fangcun--node-absolute-file (node)
  "Return the absolute file name containing NODE."
  (expand-file-name
   (fangcun-node-file node)
   (fangcun-node-yiyu-root node)))

(defun fangcun-node-visit (node)
  "Visit the Org NODE and return it."
  (let ((file (fangcun--node-absolute-file node)))
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

(defun fangcun--read-new-file (title directory)
  "Read a new file name suggested by TITLE in DIRECTORY."
  (let ((initial
         (unless (string-empty-p title)
           (concat title ".org")))
        (prompt "New Fangcun file name: ")
        name error)
    (while
        (progn
          (setq name (read-string prompt initial)
                error
                (fangcun--new-file-name-error name directory))
          (when error
            (setq prompt
                  (format "New Fangcun file name [%s]: " error)
                  initial name))
          error))
    (expand-file-name name directory)))

;;;###autoload
(defun fangcun-file-node-create ()
  "Visit a new unsaved Org file with a Fangcun file node."
  (interactive)
  (let* ((yiyus (fangcun--configured-yiyus))
         (current-yiyu
          (and buffer-file-name
               (fangcun--yiyu-containing-file buffer-file-name yiyus))))
    (unless yiyus
      (user-error "Configure `fangcun-yiyus' before creating a node"))
    (let* ((yiyu
            (or current-yiyu
                (if (null (cdr yiyus))
                    (car yiyus)
                  (let* ((candidates
                          (mapcar
                           (lambda (entry)
                             (cons (fangcun-yiyu-name entry) entry))
                           yiyus))
                         (choice
                          (completing-read
                           "Fangcun yiyu: " candidates nil t)))
                    (cdr (assoc choice candidates))))))
           (root (fangcun-yiyu-root yiyu))
           (directory
            (if current-yiyu
                (file-name-directory buffer-file-name)
              root)))
      (unless (file-directory-p root)
        (user-error "Fangcun yiyu does not exist: %s" root))
      (let* ((title (read-string "Node title (empty to omit): "))
             (file
              (fangcun--read-new-file title directory)))
        (find-file file)
        (goto-char (point-min))
        (unless (string-empty-p title)
          (insert "#+title: " title "\n"))
        (insert "\n")
        (goto-char (point-min))
        (let ((id (org-id-new)))
          (org-entry-put (point) "ID" id)
          (org-cycle-set-startup-visibility)
          (goto-char (point-max))
          id)))))

;;;###autoload
(defun fangcun-node-find ()
  "Choose a Fangcun node by title or alias and visit it."
  (interactive)
  (fangcun-node-visit (fangcun--read-node)))

;;;###autoload
(defun fangcun-node-insert ()
  "Choose a Fangcun node and insert an Org ID link to it.
The chosen title or alias becomes the link description."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun node links can only be inserted in Org buffers"))
  (let ((node (fangcun--read-node)))
    (insert
     (org-link-make-string
      (concat "id:" (fangcun-node-id node))
      (fangcun-node-title node)))
    node))

(defun fangcun--backlink-at-point ()
  "Return the Fangcun backlink represented by the button at point."
  (when-let* ((button (button-at (point))))
    (button-get button 'fangcun-backlink)))

(defun fangcun-backlink-visit (backlink)
  "Visit the indexed link represented by BACKLINK and return it."
  (interactive
   (list
    (or (fangcun--backlink-at-point)
        (user-error "No Fangcun backlink at point"))))
  (fangcun-node-visit (fangcun-backlink-node backlink))
  (goto-char (fangcun-backlink-position backlink))
  (org-fold-show-context 'link-search)
  backlink)

;;;###autoload
(defun fangcun-backlink-find ()
  "Choose a backlink to the current Fangcun node and visit its link."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun backlinks are only available in Org buffers"))
  (let* ((target-id
          (or (fangcun--node-id-at-point)
              (user-error "Point is not inside a Fangcun node")))
         (backlink (fangcun--read-backlink target-id)))
    (fangcun-backlink-visit backlink)))

(defun fangcun--backlink-preview (backlink)
  "Return a one-line preview of BACKLINK."
  (let ((file
         (fangcun--node-absolute-file
          (fangcun-backlink-node backlink)))
        (position (fangcun-backlink-position backlink)))
    (cond
     ((not (file-readable-p file))
      "[Source file is unavailable]")
     (t
      (with-temp-buffer
        (insert-file-contents file)
        (if (> position (point-max))
            "[Link position is stale; synchronize Fangcun]"
          (goto-char position)
          (string-trim
           (substring-no-properties
            (org-link-display-format
             (buffer-substring
              (line-beginning-position)
              (line-end-position)))))))))))

(defun fangcun--backlink-button-action (button)
  "Visit the Fangcun backlink represented by BUTTON."
  (fangcun-backlink-visit
   (button-get button 'fangcun-backlink)))

(define-button-type 'fangcun-backlink-button
  'action #'fangcun--backlink-button-action
  'face 'link
  'follow-link t
  'help-echo "Visit this backlink")

(defun fangcun-backlinks-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the current Fangcun backlinks buffer."
  (interactive)
  (unless (derived-mode-p 'fangcun-backlinks-mode)
    (user-error "This is not a Fangcun backlinks buffer"))
  (let* ((target-id fangcun-backlinks-target-id)
         (target
          (or (fangcun-node-from-id target-id)
              (user-error "Fangcun node is no longer indexed: %s"
                          target-id)))
         (backlinks (fangcun-backlink-occurrence-list target-id))
         (inhibit-read-only t)
         previous-source-id)
    (erase-buffer)
    (insert (propertize
             (format "Backlinks to %s" (fangcun-node-title target))
             'face 'bold)
            "\n\n")
    (if (null backlinks)
        (insert "No backlinks.\n")
      (dolist (backlink backlinks)
        (let* ((source (fangcun-backlink-node backlink))
               (source-id (fangcun-node-id source)))
          (unless (equal source-id previous-source-id)
            (when previous-source-id
              (insert "\n"))
            (insert (propertize (fangcun-node-title source) 'face 'bold)
                    (propertize
                     (format "  %s — %s"
                             (fangcun-node-yiyu-name source)
                             (fangcun-node-file source))
                     'face 'shadow)
                    "\n")
            (setq previous-source-id source-id))
          (insert "  ")
          (insert-text-button
           (fangcun--backlink-preview backlink)
           :type 'fangcun-backlink-button
           'fangcun-backlink backlink)
          (insert "\n"))))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (when-let* ((button (next-button (point))))
      (goto-char (button-start button)))))

;;;###autoload
(defun fangcun-backlinks ()
  "Display every backlink occurrence to the Fangcun node at point."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun backlinks are only available in Org buffers"))
  (let ((target-id
         (or (fangcun--node-id-at-point)
             (user-error "Point is not inside a Fangcun node")))
        (buffer (get-buffer-create fangcun-backlinks-buffer-name)))
    (with-current-buffer buffer
      (fangcun-backlinks-mode)
      (setq fangcun-backlinks-target-id target-id)
      (fangcun-backlinks-refresh))
    (pop-to-buffer buffer)
    buffer))

(dolist (command '(fangcun-node-find fangcun-backlink-visit))
  (yunge-jump-history-track-command command))

(provide 'fangcun)

;;; fangcun.el ends here
