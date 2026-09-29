;;; fangcun-store.el --- SQLite storage for Fangcun -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'fangcun-model)
(require 'json)
(require 'sqlite)
(require 'subr-x)

(defun fangcun-store-call-with-database (file function)
  "Call FUNCTION with an initialized SQLite database at absolute path FILE.
The connection is valid only during FUNCTION and is closed on every exit."
  (unless (sqlite-available-p)
    (user-error "This Emacs was built without SQLite support"))
  (let* ((directory (file-name-directory file))
         (new-database-p (not (file-exists-p file)))
         database)
    (when new-database-p
      (make-directory directory t))
    (setq database (sqlite-open file nil))
    (unwind-protect
        (progn
          (sqlite-execute database "PRAGMA foreign_keys = ON")
          (when new-database-p
            (fangcun-store--create-schema database))
          (funcall function database))
      (sqlite-close database))))

(defun fangcun-store--create-schema (database)
  "Create the current Fangcun schema in a new DATABASE."
  (sqlite-execute
   database
   (concat
    "CREATE TABLE yiyus ("
    "id TEXT PRIMARY KEY, "
    "name TEXT NOT NULL, "
    "root TEXT NOT NULL)"))
  ;; Track files without nodes too, so synchronization can detect their removal.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE files ("
    "yiyu_id TEXT NOT NULL, "
    "file TEXT NOT NULL, "
    "mtime REAL NOT NULL, "
    "size INTEGER NOT NULL, "
    "PRIMARY KEY (yiyu_id, file), "
    "FOREIGN KEY (yiyu_id) REFERENCES yiyus (id) "
    "ON DELETE CASCADE)"))
  ;; Store the owning file rather than a buffer position; Org resolves the ID.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE nodes ("
    "id TEXT PRIMARY KEY, "
    "yiyu_id TEXT NOT NULL, "
    "file TEXT NOT NULL, "
    "title TEXT NOT NULL, "
    "outline_path TEXT NOT NULL, "
    "FOREIGN KEY (yiyu_id, file) "
    "REFERENCES files (yiyu_id, file) ON DELETE CASCADE)"))
  ;; Different nodes may share an alias; completion disambiguates them.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE aliases ("
    "node_id TEXT NOT NULL, "
    "alias TEXT NOT NULL, "
    "PRIMARY KEY (node_id, alias), "
    "FOREIGN KEY (node_id) REFERENCES nodes (id) "
    "ON DELETE CASCADE)"))
  (sqlite-execute
   database
   (concat
    "CREATE TABLE tags ("
    "node_id TEXT NOT NULL, "
    "tag TEXT NOT NULL, "
    "PRIMARY KEY (node_id, tag), "
    "FOREIGN KEY (node_id) REFERENCES nodes (id) "
    "ON DELETE CASCADE)"))
  ;; Keep occurrence positions for backlink navigation.  Targets may be
  ;; unresolved or outside Fangcun, so only sources have a foreign key.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE links ("
    "source_id TEXT NOT NULL, "
    "target_id TEXT NOT NULL, "
    "position INTEGER NOT NULL, "
    "FOREIGN KEY (source_id) REFERENCES nodes (id) "
    "ON DELETE CASCADE)"))
  (sqlite-execute
   database
   "CREATE INDEX links_target_id ON links (target_id)"))

(defun fangcun-store--insert-yiyu (database yiyu)
  "Insert YIYU into DATABASE."
  (sqlite-execute
   database
   "INSERT INTO yiyus (id, name, root) VALUES (?, ?, ?)"
   (vector
    (fangcun-yiyu-id yiyu)
    (fangcun-yiyu-name yiyu)
    (fangcun-yiyu-root yiyu))))

(defun fangcun-store--insert-file (database state)
  "Insert Fangcun file STATE into DATABASE."
  (sqlite-execute
   database
   (concat
    "INSERT INTO files "
    "(yiyu_id, file, mtime, size) "
    "VALUES (?, ?, ?, ?)")
   (vector
    (fangcun-yiyu-id (fangcun-file-state-yiyu state))
    (fangcun-file-state-relative-file state)
    (fangcun-file-state-mtime state)
    (fangcun-file-state-size state))))

(defun fangcun-store--insert-node (database node)
  "Insert NODE into DATABASE."
  (sqlite-execute
   database
   (concat
    "INSERT INTO nodes "
    "(id, yiyu_id, file, title, outline_path) "
    "VALUES (?, ?, ?, ?, ?)")
   (vector
    (fangcun-node-id node)
    (fangcun-node-yiyu-id node)
    (fangcun-node-file node)
    (fangcun-node-title node)
    (json-serialize (vconcat (fangcun-node-outline-path node)))))
  (dolist (alias (fangcun-node-aliases node))
    (sqlite-execute
     database
     "INSERT INTO aliases (node_id, alias) VALUES (?, ?)"
     (vector (fangcun-node-id node) alias)))
  (dolist (tag (fangcun-node-tags node))
    (sqlite-execute
     database
     "INSERT INTO tags (node_id, tag) VALUES (?, ?)"
     (vector (fangcun-node-id node) tag))))

(defun fangcun-store--insert-link (database link)
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

(defun fangcun-store--insert-file-data (database data)
  "Insert parsed Fangcun file DATA into DATABASE and return its counts."
  (let ((nodes (plist-get data :nodes))
        (links (plist-get data :links)))
    (dolist (node nodes)
      (fangcun-store--insert-node database node))
    (dolist (link links)
      (fangcun-store--insert-link database link))
    (list :nodes (length nodes)
          :aliases (apply #'+ (mapcar
                               (lambda (node)
                                 (length (fangcun-node-aliases node)))
                               nodes))
          :tags (apply #'+ (mapcar
                            (lambda (node)
                              (length (fangcun-node-tags node)))
                            nodes))
          :links (length links))))

(defun fangcun-store-file-state-key (state)
  "Return (yiyu-id . relative-file) for file STATE."
  (cons
   (fangcun-yiyu-id (fangcun-file-state-yiyu state))
   (fangcun-file-state-relative-file state)))

(defun fangcun-store-roots-match-p (database yiyus)
  "Return whether DATABASE contains exactly YIYUS."
  (let ((lessp
         (lambda (left right)
           (string-lessp (car left) (car right)))))
    (equal
     (sort
      (sqlite-select database "SELECT id, name, root FROM yiyus")
      lessp)
     (sort
      (mapcar
       (lambda (yiyu)
         (list (fangcun-yiyu-id yiyu)
               (fangcun-yiyu-name yiyu)
               (fangcun-yiyu-root yiyu)))
       yiyus)
      lessp))))

(defun fangcun-store-file-states (database)
  "Return a hash of indexed file states in DATABASE.
Keys are (yiyu-id . relative-file); values are (mtime . size)."
  (let ((states (make-hash-table :test #'equal)))
    (dolist (row
             (sqlite-select
              database
              "SELECT yiyu_id, file, mtime, size FROM files"))
      (puthash (cons (elt row 0) (elt row 1))
               (cons (elt row 2) (elt row 3))
               states))
    states))

(defun fangcun-store-counts (database)
  "Return the current Fangcun row counts in DATABASE."
  (let ((row
         (car
          (sqlite-select
           database
           (concat
            "SELECT "
            "(SELECT COUNT(*) FROM yiyus), "
            "(SELECT COUNT(*) FROM files), "
            "(SELECT COUNT(*) FROM nodes), "
            "(SELECT COUNT(*) FROM aliases), "
            "(SELECT COUNT(*) FROM tags), "
            "(SELECT COUNT(*) FROM links)")))))
    (list :yiyus (elt row 0)
          :files (elt row 1)
          :nodes (elt row 2)
          :aliases (elt row 3)
          :tags (elt row 4)
          :links (elt row 5))))

(defun fangcun-store-file-state (database yiyu relative-file)
  "Return indexed mtime and size for RELATIVE-FILE in YIYU, or nil."
  (when-let* ((row
              (car
               (sqlite-select
                database
                (concat "SELECT mtime, size FROM files "
                        "WHERE yiyu_id = ? AND file = ?")
                (vector (fangcun-yiyu-id yiyu) relative-file)))))
    (cons (elt row 0) (elt row 1))))

(defun fangcun-store-yiyu-match-p (database yiyu)
  "Return whether DATABASE records YIYU with its current name and root."
  (when-let* ((row
              (car (sqlite-select
                    database "SELECT name, root FROM yiyus WHERE id = ?"
                    (vector (fangcun-yiyu-id yiyu))))))
    (and (equal (elt row 0) (fangcun-yiyu-name yiyu))
         (file-equal-p (elt row 1) (fangcun-yiyu-root yiyu)))))

(defun fangcun-store-replace-files (database parsed deleted)
  "Atomically replace PARSED files and remove DELETED keys in DATABASE.
PARSED contains (file-state . parsed-data) pairs.  DELETED contains
(yiyu-id . relative-file) keys.  Return inserted row counts."
  (let ((counts (list :nodes 0 :aliases 0 :tags 0 :links 0)))
    (with-sqlite-transaction database
      (dolist (key
               (append deleted
                       (mapcar (lambda (entry)
                                 (fangcun-store-file-state-key (car entry)))
                               parsed)))
        (sqlite-execute
         database "DELETE FROM files WHERE yiyu_id = ? AND file = ?"
         (vector (car key) (cdr key))))
      (dolist (entry parsed)
        (fangcun-store--insert-file database (car entry))
        (let ((inserted (fangcun-store--insert-file-data database (cdr entry))))
          (dolist (key '(:nodes :aliases :tags :links))
            (plist-put counts key (+ (plist-get counts key)
                                     (plist-get inserted key)))))))
    counts))

(defun fangcun-store-rebuild (file yiyus parsed)
  "Replace database FILE with YIYUS and PARSED file data atomically.
PARSED contains (file-state . parsed-data) pairs.  Return whole-index counts."
  (let* ((directory (file-name-directory file))
         (replacement-file nil))
    (make-directory directory t)
    (setq replacement-file
          (make-temp-file (expand-file-name ".fangcun-rebuild-" directory)
                          nil ".sqlite"))
    ;; Open an unclaimed path so the connection creates its schema.
    (delete-file replacement-file)
    (unwind-protect
        (let ((counts
               (fangcun-store-call-with-database
                replacement-file
                (lambda (database)
                  (with-sqlite-transaction database
                    (dolist (yiyu yiyus)
                      (fangcun-store--insert-yiyu database yiyu))
                    (dolist (entry parsed)
                      (fangcun-store--insert-file database (car entry))
                      (fangcun-store--insert-file-data database (cdr entry)))
                    (fangcun-store-counts database))))))
          (rename-file replacement-file file t)
          counts)
      (when (file-exists-p replacement-file)
        (delete-file replacement-file)))))

(defun fangcun-store--node-from-row (row)
  "Return a Fangcun node represented by SQLite ROW."
  (make-fangcun-node
   :id (elt row 0)
   :yiyu-id (elt row 1)
   :yiyu-name (elt row 2)
   :yiyu-root (elt row 3)
   :file (elt row 4)
   :title (elt row 5)
   :outline-path
   (json-parse-string (elt row 6) :array-type 'list)))

(defun fangcun-store--attach-node-values (nodes rows slot)
  "Attach values from SQLite ROWS to SLOT of NODES and return NODES.
Each row contains a node ID followed by one value."
  (let ((nodes-by-id (make-hash-table :test #'equal)))
    (dolist (node nodes)
      (push node (gethash (fangcun-node-id node) nodes-by-id)))
    (dolist (row rows)
      (dolist (node (gethash (elt row 0) nodes-by-id))
        (push (elt row 1)
              (cl-struct-slot-value 'fangcun-node slot node))))
    (dolist (node nodes)
      (setf (cl-struct-slot-value 'fangcun-node slot node)
            (nreverse
             (cl-struct-slot-value 'fangcun-node slot node))))
    nodes))

(defun fangcun-store--attach-aliases (nodes rows)
  "Attach aliases from SQLite ROWS to NODES and return NODES."
  (fangcun-store--attach-node-values nodes rows 'aliases))

(defun fangcun-store--attach-tags (nodes rows)
  "Attach tags from SQLite ROWS to NODES and return NODES."
  (fangcun-store--attach-node-values nodes rows 'tags))

(defun fangcun-store-node-from-id (file id)
  "Return node ID from database FILE, or nil when it is not indexed."
  (fangcun-store-call-with-database file
   (lambda (database)
     (when-let* ((row
                  (car
                   (sqlite-select
                    database
                    (concat
                     "SELECT n.id, n.yiyu_id, y.name, y.root, "
                     "n.file, n.title, n.outline_path "
                     "FROM nodes AS n "
                     "JOIN yiyus AS y ON y.id = n.yiyu_id "
                     "WHERE n.id = ? LIMIT 1")
                    (vector id))))
                 (node (fangcun-store--node-from-row row)))
       (fangcun-store--attach-tags
        (fangcun-store--attach-aliases
         (list node)
         (sqlite-select
          database
          (concat
           "SELECT node_id, alias FROM aliases "
           "WHERE node_id = ? ORDER BY alias COLLATE NOCASE")
          (vector id)))
        (sqlite-select
         database
         (concat
          "SELECT node_id, tag FROM tags "
          "WHERE node_id = ? ORDER BY tag COLLATE NOCASE")
         (vector id)))
       node))))

(defun fangcun-store-node-list (file)
  "Return all nodes currently stored in database FILE."
  (fangcun-store-call-with-database file
   (lambda (database)
     (let ((nodes
            (mapcar
             #'fangcun-store--node-from-row
             (sqlite-select
              database
              (concat
               "SELECT n.id, n.yiyu_id, y.name, y.root, "
               "n.file, n.title, n.outline_path "
               "FROM nodes AS n "
               "JOIN yiyus AS y ON y.id = n.yiyu_id "
               "ORDER BY n.title COLLATE NOCASE, "
               "y.name COLLATE NOCASE, n.file, n.id")))))
       (fangcun-store--attach-tags
        (fangcun-store--attach-aliases
         nodes
         (sqlite-select
          database
          (concat
           "SELECT node_id, alias FROM aliases "
           "ORDER BY node_id, alias COLLATE NOCASE")))
        (sqlite-select
         database
         (concat
          "SELECT node_id, tag FROM tags "
          "ORDER BY node_id, tag COLLATE NOCASE")))))))

(defun fangcun-store-id-location (file id)
  "Return the indexed (root . relative-file) for ID in FILE, or nil."
  (fangcun-store-call-with-database
   file
   (lambda (database)
     (when-let* ((row
                  (car (sqlite-select
                        database
                        (concat "SELECT y.root, n.file FROM nodes AS n "
                                "JOIN yiyus AS y ON y.id = n.yiyu_id "
                                "WHERE n.id = ?")
                        (vector id)))))
       (cons (elt row 0) (elt row 1))))))

(defun fangcun-store-tags (file)
  "Return distinct indexed tag names from database FILE."
  (fangcun-store-call-with-database
   file
   (lambda (database)
     (mapcar (lambda (row) (elt row 0))
             (sqlite-select
              database
              "SELECT DISTINCT tag FROM tags ORDER BY tag COLLATE NOCASE")))))

(defun fangcun-store--backlink-from-row (row)
  "Return a Fangcun backlink represented by SQLite ROW."
  (make-fangcun-backlink
   :node (fangcun-store--node-from-row (cl-subseq row 0 7))
   :position (elt row 7)
   :count (and (> (length row) 8) (elt row 8))))

(defun fangcun-store--attach-backlink-node-data
    (database backlinks target-id)
  "Attach aliases and tags to BACKLINKS targeting TARGET-ID in DATABASE."
  (let ((nodes (mapcar #'fangcun-backlink-node backlinks)))
    (fangcun-store--attach-aliases
     nodes
     (sqlite-select
      database
      (concat
       "SELECT DISTINCT a.node_id, a.alias "
       "FROM aliases AS a "
       "JOIN links AS l ON l.source_id = a.node_id "
       "WHERE l.target_id = ? "
       "ORDER BY a.node_id, a.alias COLLATE NOCASE")
      (vector target-id)))
    (fangcun-store--attach-tags
     nodes
     (sqlite-select
      database
      (concat
       "SELECT DISTINCT t.node_id, t.tag "
       "FROM tags AS t "
       "JOIN links AS l ON l.source_id = t.node_id "
       "WHERE l.target_id = ? "
       "ORDER BY t.node_id, t.tag COLLATE NOCASE")
      (vector target-id))))
  backlinks)

(defun fangcun-store-backlink-list (file target-id)
  "Return unique source nodes linking to TARGET-ID in database FILE.
When one source contains several links, retain its first occurrence."
  (fangcun-store-call-with-database file
   (lambda (database)
     (fangcun-store--attach-backlink-node-data
      database
      (mapcar
       #'fangcun-store--backlink-from-row
       (sqlite-select
        database
        (concat
         "SELECT n.id, n.yiyu_id, y.name, y.root, "
         "n.file, n.title, n.outline_path, first_link.position, "
         "first_link.occurrence_count "
         "FROM ("
         "SELECT source_id, MIN(position) AS position, "
         "COUNT(*) AS occurrence_count "
         "FROM links WHERE target_id = ? GROUP BY source_id"
         ") AS first_link "
         "JOIN nodes AS n ON n.id = first_link.source_id "
         "JOIN yiyus AS y ON y.id = n.yiyu_id "
         "ORDER BY n.title COLLATE NOCASE, "
         "y.name COLLATE NOCASE, n.file, n.id")
        (vector target-id)))
      target-id))))

(defun fangcun-store-backlink-occurrence-list (file target-id)
  "Return every indexed backlink occurrence to TARGET-ID in database FILE."
  (fangcun-store-call-with-database file
   (lambda (database)
     (fangcun-store--attach-backlink-node-data
      database
      (mapcar
       #'fangcun-store--backlink-from-row
       (sqlite-select
        database
        (concat
         "SELECT n.id, n.yiyu_id, y.name, y.root, "
         "n.file, n.title, n.outline_path, l.position "
         "FROM links AS l "
         "JOIN nodes AS n ON n.id = l.source_id "
         "JOIN yiyus AS y ON y.id = n.yiyu_id "
         "WHERE l.target_id = ? "
         "ORDER BY n.title COLLATE NOCASE, "
         "y.name COLLATE NOCASE, n.file, n.id, l.position")
        (vector target-id)))
      target-id))))

(provide 'fangcun-store)

;;; fangcun-store.el ends here
