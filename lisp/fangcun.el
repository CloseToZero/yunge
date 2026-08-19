;;; fangcun.el --- ID-based Org note navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'button)
(require 'crm)
(require 'fangcun-loader)
(require 'json)
(require 'yunge-state)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'seq)
(require 'sqlite)
(require 'subr-x)
(require 'yunge-jump-history)

(defun fangcun--set-database-file (symbol value)
  "Set SYMBOL to the normalized absolute file name VALUE."
  (unless (and (stringp value) (file-name-absolute-p value))
    (error "%s must be an absolute file name: %S" symbol value))
  (set-default symbol (expand-file-name value)))

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

(defcustom fangcun-native-helper-enabled t
  "Whether Fangcun may use its native scanner and directory monitor.
When the helper is unavailable, synchronization falls back to Emacs."
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
  aliases
  tags)

(cl-defstruct fangcun-link
  source-id
  target-id
  position)

(cl-defstruct fangcun-file-state
  yiyu
  relative-file
  absolute-file
  mtime
  size)

(cl-defstruct fangcun-backlink
  node
  position
  count)

(defconst fangcun-backlinks-buffer-name "*Fangcun Backlinks*")

(defvar-local fangcun-backlinks-target-id nil
  "ID of the node shown in the current Fangcun backlinks buffer.")

(defconst fangcun--native-event-idle-delay 1
  "Idle seconds before Fangcun processes native file events.")

(defconst fangcun--source-directory
  (file-name-directory
   (or load-file-name
       (locate-library "fangcun")
       (error "Cannot locate the Fangcun library")))
  "Directory containing the loaded Fangcun library.")

(defconst fangcun--native-helper-manifest
  (expand-file-name
   "../native/fangcun-watch/Cargo.toml"
   fangcun--source-directory)
  "Cargo manifest of the Fangcun native helper.")

(defconst fangcun--native-helper-source-hash-file
  (expand-file-name
   "source.sha256"
   (file-name-directory fangcun--native-helper-manifest))
  "Tracked source hash embedded in the Fangcun native helper.")

(defconst fangcun--native-build-buffer-name "*Fangcun Helper Build*"
  "Name of the Fangcun native helper build buffer.")

(define-error 'fangcun-native-helper-outdated
  "Fangcun native helper is outdated")

(defvar fangcun--session-active-p nil
  "Whether this Emacs session has synchronized Fangcun once.")

(defvar fangcun--session-yiyus nil
  "Normalized yiyus used by the active Fangcun session.")

(defvar fangcun--native-build-process nil
  "Process building the Fangcun native helper, or nil.")

(defvar fangcun--native-watch-process nil
  "Process monitoring Fangcun yiyus, or nil.")

(defvar fangcun--native-watch-output ""
  "Incomplete output from the Fangcun native monitor.")

(defvar fangcun--native-watch-yiyus nil
  "Signature of yiyus watched by the native monitor.")

(defvar fangcun--native-restart-count 0
  "Number of native monitor restarts attempted this session.")

(defvar fangcun--native-event-timer nil
  "Timer for pending native file events.")

(defvar fangcun--native-pending-files
  (make-hash-table :test #'equal)
  "Files awaiting reconciliation after native events.")

(defvar fangcun--native-pending-full-sync-p nil
  "Whether native events require a complete incremental sync.")

(defvar fangcun--native-warning-shown-p nil
  "Whether helper degradation was already reported this session.")

(defvar-keymap fangcun-backlinks-mode-map
  :parent special-mode-map
  "RET" #'fangcun-backlink-visit)

(define-derived-mode fangcun-backlinks-mode special-mode "Fangcun Backlinks"
  "Major mode for displaying backlinks to one Fangcun node."
  (setq-local revert-buffer-function #'fangcun-backlinks-refresh))

(defun fangcun--configured-yiyus ()
  "Return normalized entries from `fangcun-yiyus'."
  (let ((yiyus
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
              (setq root
                    (file-name-as-directory (expand-file-name root)))
              (when (file-remote-p root)
                (user-error
                 "Remote Fangcun yiyus are not supported: %s" root))
              (make-fangcun-yiyu
               :id (symbol-name id)
               :name name
               :root root)))
          fangcun-yiyus))
        (ids (make-hash-table :test #'equal)))
    (dolist (yiyu yiyus)
      (let ((id (fangcun-yiyu-id yiyu)))
        (when (gethash id ids)
          (user-error "Duplicate Fangcun yiyu ID: %s" id))
        (puthash id t ids)))
    (cl-loop
     for (yiyu . rest) on yiyus
     do (dolist (other rest)
          (let ((root (fangcun-yiyu-root yiyu))
                (other-root (fangcun-yiyu-root other)))
            (when (or (equal root other-root)
                      (file-equal-p root other-root)
                      (file-in-directory-p root other-root)
                      (file-in-directory-p other-root root))
              (user-error
               "Fangcun yiyu roots overlap: %s and %s"
               root other-root)))))
    yiyus))

(defun fangcun--normalize-yiyu-id (id)
  "Return ID as a valid Fangcun yiyu symbol."
  (let ((name
         (string-trim
          (cond
           ((symbolp id) (symbol-name id))
           ((stringp id) id)
           (t "")))))
    (unless (string-match-p
             "\\`[[:alnum:]][[:alnum:]_-]*\\'" name)
      (user-error
       (concat
        "Fangcun yiyu ID must start with a letter or number and contain "
        "only letters, numbers, underscores, or hyphens: %S")
       id))
    (intern name)))

(defun fangcun--apply-yiyu-configuration ()
  "Synchronize Fangcun after `fangcun-yiyus' changes."
  (let ((yiyus (fangcun--configured-yiyus)))
    (fangcun--shutdown-native-helper)
    (if yiyus
        (prog1 (fangcun--sync-yiyus yiyus t)
          (fangcun--activate-session yiyus))
      (setq fangcun--session-yiyus nil)
      (fangcun--rebuild-database nil nil t))))

(defun fangcun--read-new-yiyu ()
  "Read arguments for `fangcun-yiyu-add'."
  (let* ((root
          (file-name-as-directory
           (expand-file-name
            (read-directory-name "Yiyu root: " nil nil t))))
         (basename
          (file-name-nondirectory (directory-file-name root)))
         (id (read-string "Yiyu ID: " nil nil basename))
         (name (read-string "Yiyu display name: " nil nil basename)))
    (list id name root)))

;;;###autoload
(defun fangcun-yiyu-add (id name root)
  "Add and persist a Fangcun yiyu named ID, NAME, and ROOT.
ROOT must be an existing local directory which does not overlap another
configured yiyu.  Synchronize the Fangcun index after saving the setting."
  (interactive (fangcun--read-new-yiyu))
  (setq id (fangcun--normalize-yiyu-id id)
        name (and (stringp name) (string-trim name))
        root (and (stringp root)
                  (file-name-as-directory (expand-file-name root))))
  (unless (and name (not (string-empty-p name)))
    (user-error "Fangcun yiyu display name cannot be empty"))
  (when (file-remote-p root)
    (user-error "Remote Fangcun yiyus are not supported: %s" root))
  (unless (and root (file-directory-p root))
    (user-error "Fangcun yiyu root does not exist: %s" root))
  (let ((value
         (append fangcun-yiyus
                 (list (list id :name name :root root)))))
    (let ((fangcun-yiyus value))
      (fangcun--configured-yiyus))
    (customize-save-variable 'fangcun-yiyus value)
    (fangcun--apply-yiyu-configuration)
    (message "Added Fangcun yiyu %s (%s)" name id)))

(defun fangcun--read-yiyu-to-remove ()
  "Read arguments for `fangcun-yiyu-remove'."
  (let* ((yiyus (fangcun--configured-yiyus))
         (_ (unless yiyus
              (user-error "No Fangcun yiyus are configured")))
         (candidates
          (mapcar
           (lambda (yiyu)
             (cons
              (format "%s (%s) — %s"
                      (fangcun-yiyu-name yiyu)
                      (fangcun-yiyu-id yiyu)
                      (abbreviate-file-name (fangcun-yiyu-root yiyu)))
              yiyu))
           yiyus))
         (choice
          (completing-read "Remove yiyu: " candidates nil t))
         (yiyu (cdr (assoc choice candidates))))
    (unless
        (yes-or-no-p
         (format
          "Remove %s from Fangcun?  Notes in %s will not be deleted. "
          (fangcun-yiyu-name yiyu)
          (abbreviate-file-name (fangcun-yiyu-root yiyu))))
      (user-error "Removing Fangcun yiyu cancelled"))
    (list (fangcun-yiyu-id yiyu))))

;;;###autoload
(defun fangcun-yiyu-remove (id)
  "Stop indexing and forget the configured Fangcun yiyu ID.
When called interactively, ask for confirmation.  Never delete the root
directory or any notes below it."
  (interactive (fangcun--read-yiyu-to-remove))
  (let* ((id (symbol-name (fangcun--normalize-yiyu-id id)))
         (entry
          (seq-find
           (lambda (candidate)
             (equal (symbol-name (car candidate)) id))
           fangcun-yiyus)))
    (unless entry
      (user-error "Unknown Fangcun yiyu ID: %s" id))
    (let ((value (delq entry (copy-sequence fangcun-yiyus))))
      (customize-save-variable 'fangcun-yiyus value)
      (fangcun--apply-yiyu-configuration)
      (message "Removed Fangcun yiyu %s; notes were not deleted" id))))

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

(defun fangcun--new-file-name-error (file root)
  "Return why FILE cannot name a new file below ROOT, or nil."
  (setq file (expand-file-name file))
  (let ((directory (file-name-directory file))
        (name (file-name-nondirectory file))
        relative-components)
    (cond
     ((file-remote-p file)
      "Fangcun files must be local")
     ((not (file-in-directory-p file root))
      (format "Fangcun files must stay below %s" root))
     ((progn
        (setq relative-components
              (split-string
               (subst-char-in-string
                ?\\ ?/ (file-relative-name file root))
               "/" t))
        (seq-some
         #'fangcun--portable-file-name-error
         relative-components)))
     ((not (string-suffix-p ".org" name))
      "Fangcun file names must end with .org")
     ((file-exists-p file)
      (format "File already exists: %s" file))
     ((find-buffer-visiting file)
      (format "A buffer is already visiting: %s" file))
     ((when (file-directory-p directory)
        (when-let* ((conflict
                     (seq-find
                      (lambda (entry)
                        (and (not (equal entry name))
                             (string-equal (downcase entry)
                                           (downcase name))))
                      (directory-files directory nil nil t))))
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
  ;; FILES is the synchronization boundary.  It records every indexed Org
  ;; file, including files without nodes, and owns the graph data parsed from
  ;; that file.  MTIME and SIZE are cheap change detectors rather than a
  ;; content identity; a full rebuild remains the fallback for rare misses.
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
  ;; The database resolves an ID to its file.  Org searches that file for the
  ;; entry, avoiding byte positions that become stale when a buffer changes.
  ;; File ownership also lets one file-row deletion remove its nodes and,
  ;; through their foreign keys, their aliases, tags, and outgoing links.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE nodes ("
    "id TEXT PRIMARY KEY, "
    "yiyu_id TEXT NOT NULL, "
    "file TEXT NOT NULL, "
    "title TEXT NOT NULL, "
    "FOREIGN KEY (yiyu_id, file) "
    "REFERENCES files (yiyu_id, file) ON DELETE CASCADE)"))
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
  ;; Store the effective Org tags of each node as individual rows.  This keeps
  ;; inherited and file tags searchable without duplicating node records.  The
  ;; composite primary key removes duplicates, and deleting a node removes its
  ;; tags.  No tag index is needed until a query filters by tag directly.
  (sqlite-execute
   database
   (concat
    "CREATE TABLE tags ("
    "node_id TEXT NOT NULL, "
    "tag TEXT NOT NULL, "
    "PRIMARY KEY (node_id, tag), "
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

(defun fangcun--goto-node-at-point ()
  "Move to the nearest enclosing Fangcun node and return its ID, or nil."
  (org-back-to-heading-or-point-min t)
  (let ((id (org-id-get)))
    (while (and (not id) (not (bobp)))
      (if (org-up-heading-safe)
          (setq id (org-id-get))
        (goto-char (point-min))
        (setq id (org-id-get))))
    id))

(defun fangcun--node-id-at-point ()
  "Return the nearest enclosing Fangcun node ID, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (fangcun--goto-node-at-point))))

(defun fangcun--effective-tags-at-point ()
  "Return the effective Org tags of the node at point."
  (delete-dups
   (mapcar
    #'substring-no-properties
    (if (= (org-outline-level) 0)
        org-file-tags
      (org-get-tags)))))

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
        ;; A reused Org buffer may have stale file-tag options after an
        ;; external edit.  Refresh them before collecting effective tags.
        (org-set-regexps-and-options 'tags-only)
        (let ((file-title (fangcun--file-title relative-file))
              (case-fold-search t)
              (id-property-re (org-re-property "ID"))
              nodes)
          (goto-char (point-min))
          ;; Point may already be on the first heading.  Without this check,
          ;; its ID would be collected here and again below.
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
                :aliases (fangcun--aliases-at-point)
                :tags (fangcun--effective-tags-at-point))
               nodes)))
          ;; Fangcun nodes are sparse among Org headings.  Search possible ID
          ;; properties directly, then let Org reject lookalikes outside a
          ;; property drawer.
          (goto-char (point-min))
          (while (re-search-forward id-property-re nil t)
            (let ((id (match-string-no-properties 3)))
              (when (org-at-property-p)
                (save-excursion
                  (org-back-to-heading-or-point-min t)
                  (unless (= (org-outline-level) 0)
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
                      :aliases (fangcun--aliases-at-point)
                      :tags (fangcun--effective-tags-at-point))
                     nodes))))))
          (nreverse nodes))))))

(defun fangcun--collect-links-from-buffer (buffer)
  "Return ID links owned by Fangcun nodes in Org BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (links)
          (while (re-search-forward org-link-any-re nil t)
            ;; The search leaves point after the link.  Move onto it so Org
            ;; can reject matches in source blocks, comments, properties, and
            ;; keywords.
            (backward-char)
            (let ((element (org-element-context)))
              (when (and (eq (org-element-type element) 'link)
                         (equal
                          (org-element-property :type element)
                          "id"))
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

(defun fangcun--reusable-file-buffer (file)
  "Return FILE's visited Org buffer when it still matches the file on disk."
  (when-let* ((buffer (find-buffer-visiting file)))
    (when (with-current-buffer buffer
            (and (derived-mode-p 'org-mode)
                 (not (buffer-modified-p))
                 (verify-visited-file-modtime buffer)))
      buffer)))

(defun fangcun--parse-file (yiyu file)
  "Return Fangcun nodes and links from FILE on disk, owned by YIYU."
  (let* ((relative-file
          (file-relative-name file (fangcun-yiyu-root yiyu)))
         (buffer (fangcun--reusable-file-buffer file)))
    (if buffer
        (fangcun--collect-file-data-from-buffer
         buffer yiyu relative-file)
      (with-temp-buffer
        (setq default-directory (file-name-directory file))
        (insert-file-contents file)
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (fangcun--collect-file-data-from-buffer
         (current-buffer) yiyu relative-file)))))

(defun fangcun--read-file-state (yiyu file)
  "Return the synchronization state of FILE owned by YIYU."
  (let ((attributes (file-attributes file 'string)))
    (make-fangcun-file-state
     :yiyu yiyu
     :relative-file
     (file-relative-name file (fangcun-yiyu-root yiyu))
     :absolute-file file
     :mtime
     (float-time (file-attribute-modification-time attributes))
     :size (file-attribute-size attributes))))

(defun fangcun--cargo-target-directory ()
  "Return the Cargo target directory for Fangcun native packages."
  (yunge-var-subdirectory "fangcun/cargo-target"))

(defun fangcun--native-helper-program ()
  "Return the expected Fangcun helper executable."
  (expand-file-name
   (concat "release/fangcun-watch"
           (when (eq system-type 'windows-nt) ".exe"))
   (fangcun--cargo-target-directory)))

(defun fangcun--native-helper-build-id ()
  "Return the expected Fangcun native helper build ID, or nil."
  (when (file-readable-p fangcun--native-helper-source-hash-file)
    (with-temp-buffer
      (insert-file-contents fangcun--native-helper-source-hash-file)
      (let ((build-id (string-trim (buffer-string))))
        (unless (string-empty-p build-id)
          build-id)))))

(defun fangcun--native-helper-available-p ()
  "Return whether the Fangcun native helper can be started."
  (and fangcun-native-helper-enabled
       (file-executable-p (fangcun--native-helper-program))))

(defun fangcun--validate-native-ready-message (message)
  "Validate native helper ready MESSAGE against the tracked build ID."
  (let ((expected (fangcun--native-helper-build-id))
        (actual (alist-get 'build-id message)))
    (unless expected
      (error "Fangcun native helper source hash is unavailable"))
    (unless (and (equal (alist-get 'kind message) "ready")
                 (equal actual expected))
      (signal
       'fangcun-native-helper-outdated
       (list
        (format "Expected build %s, got %s"
                expected (or actual "an unversioned helper")))))))

(defun fangcun--elisp-scan-file-states (yiyus)
  "Return Org file states below YIYUS using Emacs file operations."
  (let (states)
    (dolist (yiyu yiyus)
      (dolist (file
               (directory-files-recursively
                (fangcun-yiyu-root yiyu) "\\.org\\'"))
        (push (fangcun--read-file-state yiyu file) states)))
    (nreverse states)))

(defun fangcun--native-command-arguments (command yiyus)
  "Return helper arguments for COMMAND and YIYUS."
  ;; The helper receives no stdin.  Its argv is COMMAND followed by repeated
  ;; YIYU-ID ROOT pairs, and its stdout is the NDJSON protocol documented in
  ;; native/fangcun-watch/README.org.
  (cons
   command
   (mapcan
    (lambda (yiyu)
      (list (fangcun-yiyu-id yiyu)
            (fangcun-yiyu-root yiyu)))
    yiyus)))

(defun fangcun--native-scan-file-states (yiyus)
  "Return Org file states below YIYUS using the native helper."
  (let ((program (fangcun--native-helper-program))
        (yiyus-by-id (make-hash-table :test #'equal))
        ready
        states)
    (dolist (yiyu yiyus)
      (puthash (fangcun-yiyu-id yiyu) yiyu yiyus-by-id))
    (with-temp-buffer
      (let ((status
             (apply
              #'process-file program nil t nil
              (fangcun--native-command-arguments "scan" yiyus))))
        (unless (zerop status)
          (error "Native Fangcun scan failed: %s"
                 (string-trim (buffer-string)))))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((message
                (json-parse-string
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))
                 :object-type 'alist))
               (kind (alist-get 'kind message)))
          (if ready
              (progn
                (unless (equal kind "state")
                  (error "Unexpected native Fangcun scan message: %S"
                         message))
                (let* ((id (alist-get 'yiyu message))
                       (yiyu (gethash id yiyus-by-id))
                       (file
                        (expand-file-name (alist-get 'file message))))
                  (unless yiyu
                    (error
                     "Native Fangcun scan returned unknown yiyu: %s"
                     id))
                  (push
                   (make-fangcun-file-state
                    :yiyu yiyu
                    :relative-file
                    (file-relative-name file
                                        (fangcun-yiyu-root yiyu))
                    :absolute-file file
                    :mtime (alist-get 'mtime message)
                    :size (alist-get 'size message))
                   states)))
            (fangcun--validate-native-ready-message message)
            (setq ready t)))
        (forward-line 1)))
    (unless ready
      (error "Native Fangcun scan returned no ready message"))
    (nreverse states)))

(defun fangcun--scan-file-states (yiyus)
  "Return the current Org file states below YIYUS."
  ;; Validate every root before a missing root could be mistaken for an empty
  ;; root and cause its indexed files to be removed.
  (dolist (yiyu yiyus)
    (unless (file-directory-p (fangcun-yiyu-root yiyu))
      (user-error "Fangcun yiyu does not exist: %s"
                  (fangcun-yiyu-root yiyu))))
  (if (and yiyus (fangcun--native-helper-available-p))
      (condition-case error-data
          (fangcun--native-scan-file-states yiyus)
        (fangcun-native-helper-outdated
         (fangcun--build-native-helper)
         (fangcun--elisp-scan-file-states yiyus))
        (error
         (display-warning
          'fangcun
          (concat
           (error-message-string error-data)
           "; falling back to Emacs")
          :warning)
         (fangcun--elisp-scan-file-states yiyus)))
    (fangcun--elisp-scan-file-states yiyus)))

(defun fangcun--insert-yiyu (database yiyu)
  "Insert YIYU into DATABASE."
  (sqlite-execute
   database
   "INSERT INTO yiyus (id, name, root) VALUES (?, ?, ?)"
   (vector
    (fangcun-yiyu-id yiyu)
    (fangcun-yiyu-name yiyu)
    (fangcun-yiyu-root yiyu))))

(defun fangcun--insert-file (database state)
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
     (vector (fangcun-node-id node) alias)))
  (dolist (tag (fangcun-node-tags node))
    (sqlite-execute
     database
     "INSERT INTO tags (node_id, tag) VALUES (?, ?)"
     (vector (fangcun-node-id node) tag))))

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
          :tags (apply #'+ (mapcar
                            (lambda (node)
                              (length (fangcun-node-tags node)))
                            nodes))
          :links (length links))))

(defun fangcun--file-state-key (state)
  "Return the database key for Fangcun file STATE."
  (cons
   (fangcun-yiyu-id (fangcun-file-state-yiyu state))
   (fangcun-file-state-relative-file state)))

(defun fangcun--database-yiyus-match-p (database yiyus)
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

(defun fangcun--database-file-states (database)
  "Return the indexed file states in DATABASE, keyed by yiyu and file."
  (let ((states (make-hash-table :test #'equal)))
    (dolist (row
             (sqlite-select
              database
              "SELECT yiyu_id, file, mtime, size FROM files"))
      (puthash (cons (elt row 0) (elt row 1))
               (cons (elt row 2) (elt row 3))
               states))
    states))

(defun fangcun--database-counts (database)
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

(defun fangcun--rebuild-database (yiyus states &optional no-message)
  "Replace the Fangcun database with YIYUS and file STATES.
When NO-MESSAGE is non-nil, do not report the indexed counts."
  (let ((parsed
         (mapcar
          (lambda (state)
            (cons
             state
             (fangcun--parse-file
              (fangcun-file-state-yiyu state)
              (fangcun-file-state-absolute-file state))))
          states))
        (database-file fangcun-database-file)
        (directory (file-name-directory fangcun-database-file))
        replacement-file)
    (make-directory directory t)
    (setq replacement-file
          (make-temp-file
           (expand-file-name ".fangcun-rebuild-" directory)
           nil ".sqlite"))
    ;; `fangcun--call-with-database' initializes only a nonexistent file.
    ;; MAKE-TEMP-FILE reserves a unique same-directory name first.
    (delete-file replacement-file)
    (unwind-protect
        (let ((result
               (let ((fangcun-database-file replacement-file))
                 (fangcun--call-with-database
                  (lambda (database)
                    (with-sqlite-transaction database
                      (dolist (yiyu yiyus)
                        (fangcun--insert-yiyu database yiyu))
                      (dolist (entry parsed)
                        (fangcun--insert-file database (car entry))
                        (fangcun--insert-file-data database (cdr entry)))
                      (fangcun--database-counts database)))))))
          (rename-file replacement-file database-file t)
          (unless no-message
            (message
             (concat
              "Fangcun indexed %d nodes, %d aliases, %d tags, and %d links "
              "from %d files in %d yiyu roots")
             (plist-get result :nodes)
             (plist-get result :aliases)
             (plist-get result :tags)
             (plist-get result :links)
             (plist-get result :files)
             (plist-get result :yiyus)))
          result)
      (when (file-exists-p replacement-file)
        (delete-file replacement-file)))))

(defun fangcun--sync-database (states &optional no-message)
  "Synchronize an existing Fangcun database with file STATES.
When NO-MESSAGE is non-nil, do not report the changed file counts."
  (fangcun--call-with-database
   (lambda (database)
     (let ((database-states
            (fangcun--database-file-states database))
           (current-keys (make-hash-table :test #'equal))
           (missing (make-symbol "missing"))
           changed deleted
           (added-count 0)
           (updated-count 0))
       (dolist (state states)
         (let* ((key (fangcun--file-state-key state))
                (current
                 (cons (fangcun-file-state-mtime state)
                       (fangcun-file-state-size state)))
                (stored (gethash key database-states missing)))
           (puthash key t current-keys)
           (unless (equal stored current)
             (if (eq stored missing)
                 (cl-incf added-count)
               (cl-incf updated-count))
             (push state changed))))
       (maphash
        (lambda (key _state)
          (unless (gethash key current-keys)
            (push key deleted)))
        database-states)
       (setq changed (nreverse changed)
             deleted (nreverse deleted))
       ;; Parse before changing the database.  A parse error therefore leaves
       ;; the last successful index untouched.
       (let ((parsed
              (mapcar
               (lambda (state)
                 (cons
                  state
                  (fangcun--parse-file
                   (fangcun-file-state-yiyu state)
                   (fangcun-file-state-absolute-file state))))
               changed)))
         (with-sqlite-transaction database
           (dolist (key
                    (append deleted
                            (mapcar #'fangcun--file-state-key changed)))
             (sqlite-execute
              database
              (concat
               "DELETE FROM files "
               "WHERE yiyu_id = ? AND file = ?")
              (vector (car key) (cdr key))))
           (dolist (entry parsed)
             (fangcun--insert-file database (car entry))
             (fangcun--insert-file-data database (cdr entry))))
         (unless no-message
           (message
            "Fangcun synchronized files: %d added, %d updated, %d removed"
            added-count updated-count (length deleted)))
         (fangcun--database-counts database))))))

;;;###autoload
(defun fangcun-db-rebuild ()
  "Rebuild the Fangcun database from every configured yiyu file."
  (interactive)
  (let ((yiyus (fangcun--configured-yiyus)))
    (unless yiyus
      (user-error "Configure `fangcun-yiyus' before syncing"))
    (prog1
        (fangcun--rebuild-database
         yiyus (fangcun--scan-file-states yiyus))
      (fangcun--activate-session yiyus))))

(defun fangcun--sync-yiyus (yiyus no-message)
  "Synchronize YIYUS, suppressing results when NO-MESSAGE is non-nil."
  (let ((states (fangcun--scan-file-states yiyus)))
    (if (and
         (file-exists-p fangcun-database-file)
         (fangcun--call-with-database
          (lambda (database)
            (fangcun--database-yiyus-match-p database yiyus))))
        (fangcun--sync-database states no-message)
      (fangcun--rebuild-database yiyus states no-message))))

;;;###autoload
(defun fangcun-db-sync (&optional no-message)
  "Synchronize the Fangcun database with configured yiyu files.
When NO-MESSAGE is non-nil, do not report synchronization results."
  (interactive)
  (let ((yiyus (fangcun--configured-yiyus)))
    (unless yiyus
      (user-error "Configure `fangcun-yiyus' before syncing"))
    (prog1 (fangcun--sync-yiyus yiyus no-message)
      (fangcun--activate-session yiyus))))

(defun fangcun--native-yiyu-signature (yiyus)
  "Return the monitor-relevant signature of YIYUS."
  (mapcar
   (lambda (yiyu)
     (cons (fangcun-yiyu-id yiyu)
           (fangcun-yiyu-root yiyu)))
   yiyus))

(defun fangcun--native-warning (format-string &rest arguments)
  "Report one native helper warning using FORMAT-STRING and ARGUMENTS."
  (unless fangcun--native-warning-shown-p
    (setq fangcun--native-warning-shown-p t)
    (display-warning
     'fangcun
     (apply #'format format-string arguments)
     :warning)))

(defun fangcun--stop-native-watch ()
  "Stop the Fangcun native monitor intentionally."
  (when (process-live-p fangcun--native-watch-process)
    (process-put fangcun--native-watch-process
                 'fangcun-intentional-stop t)
    (delete-process fangcun--native-watch-process))
  (setq fangcun--native-watch-process nil
        fangcun--native-watch-output ""
        fangcun--native-watch-yiyus nil))

(defun fangcun--schedule-native-events ()
  "Schedule processing of queued native monitor events."
  (when (timerp fangcun--native-event-timer)
    (cancel-timer fangcun--native-event-timer))
  (setq fangcun--native-event-timer
        (run-with-idle-timer
         fangcun--native-event-idle-delay nil
         #'fangcun--process-native-events)))

(defun fangcun--queue-native-full-sync ()
  "Request a complete incremental sync after Emacs becomes idle."
  (setq fangcun--native-pending-full-sync-p t)
  (clrhash fangcun--native-pending-files)
  (fangcun--schedule-native-events))

(defun fangcun--queue-native-files (files)
  "Queue absolute FILES for reconciliation after Emacs becomes idle."
  (unless fangcun--native-pending-full-sync-p
    (dolist (file files)
      (puthash (expand-file-name file) t
               fangcun--native-pending-files)))
  (fangcun--schedule-native-events))

(defun fangcun--handle-native-message (process line)
  "Handle one NDJSON LINE from native monitor PROCESS."
  (let* ((message
          (json-parse-string line
                             :object-type 'alist
                             :array-type 'list))
          (kind (alist-get 'kind message)))
    (if (process-get process 'fangcun-ready)
        (pcase kind
          ("event"
           (fangcun--queue-native-files (alist-get 'paths message)))
          ((or "rescan" "error")
           (when (equal kind "error")
             (display-warning
              'fangcun
              (format "Native monitor reported: %s"
                      (alist-get 'message message))
              :warning))
           (fangcun--queue-native-full-sync))
          (_
           (error "Unexpected native Fangcun monitor message: %S"
                  message)))
      (fangcun--validate-native-ready-message message)
      (process-put process 'fangcun-ready t)
      ;; A full scan closes the gap before the monitor became ready.
      (fangcun--queue-native-full-sync))))

(defun fangcun--native-watch-filter (process output)
  "Collect and handle complete NDJSON lines in native monitor OUTPUT."
  (setq fangcun--native-watch-output
        (concat fangcun--native-watch-output output))
  (let (newline)
    (while (setq newline
                 (string-match "\n" fangcun--native-watch-output))
      (let ((line
             (string-trim-right
              (substring fangcun--native-watch-output 0 newline)
              "\r")))
        (setq fangcun--native-watch-output
              (substring fangcun--native-watch-output (1+ newline)))
        (unless (string-empty-p line)
          (condition-case error-data
              (fangcun--handle-native-message process line)
            (fangcun-native-helper-outdated
             (when (eq process fangcun--native-watch-process)
               (fangcun--stop-native-watch)
               (fangcun--build-native-helper)))
            (error
             (display-warning
              'fangcun
              (format "Invalid native monitor output: %s"
                      (error-message-string error-data))
              :warning)
             (fangcun--queue-native-full-sync))))))))

(defun fangcun--native-watch-sentinel (process _event)
  "Recover when native monitor PROCESS exits unexpectedly."
  (when (and (memq (process-status process) '(exit signal failed))
             (eq process fangcun--native-watch-process))
    (setq fangcun--native-watch-process nil
          fangcun--native-watch-output "")
    (unless (process-get process 'fangcun-intentional-stop)
      (if (and fangcun--session-active-p
               (< fangcun--native-restart-count 1))
          (progn
            (cl-incf fangcun--native-restart-count)
            (fangcun--start-native-watch fangcun--session-yiyus))
        (fangcun--native-warning
         (concat
          "Fangcun native monitoring stopped; external changes require "
          "`fangcun-db-sync'"))))))

(defun fangcun--start-native-watch (yiyus)
  "Start recursively monitoring YIYUS."
  (let ((signature (fangcun--native-yiyu-signature yiyus)))
    (when (and yiyus (fangcun--native-helper-available-p))
      (unless (and (process-live-p fangcun--native-watch-process)
                   (equal signature fangcun--native-watch-yiyus))
        (fangcun--stop-native-watch)
        (setq fangcun--native-watch-output ""
              fangcun--native-watch-yiyus signature)
        (condition-case error-data
            (setq fangcun--native-watch-process
                  (make-process
                   :name "fangcun-watch"
                   :command
                   (cons
                     (fangcun--native-helper-program)
                     (fangcun--native-command-arguments
                      "watch" yiyus))
                   :coding 'utf-8-unix
                   :connection-type 'pipe
                   :noquery t
                   :filter #'fangcun--native-watch-filter
                   :sentinel #'fangcun--native-watch-sentinel))
          (error
           (setq fangcun--native-watch-process nil
                 fangcun--native-watch-yiyus nil)
           (fangcun--native-warning
            "Cannot start Fangcun native monitoring: %s"
            (error-message-string error-data))))))))

(defun fangcun--native-build-sentinel (process _event)
  "Start native monitoring when helper build PROCESS succeeds."
  (when (memq (process-status process) '(exit signal failed))
    (when (eq process fangcun--native-build-process)
      (setq fangcun--native-build-process nil))
    (if (and (zerop (process-exit-status process))
              (fangcun--native-helper-available-p))
        (progn
          (message "Built Fangcun native helper")
          (when fangcun--session-active-p
            (fangcun--start-native-watch fangcun--session-yiyus)))
      (fangcun--native-warning
       (concat
        "Fangcun native helper build failed; external changes require "
        "`fangcun-db-sync'.  See %s")
       (buffer-name (process-buffer process))))))

(defun fangcun--build-native-helper ()
  "Build the Fangcun native helper asynchronously when possible."
  (unless (process-live-p fangcun--native-build-process)
    (if-let* ((cargo (executable-find "cargo")))
        (let ((buffer (get-buffer-create fangcun--native-build-buffer-name))
              (target (fangcun--cargo-target-directory)))
          (fangcun--stop-native-watch)
          (make-directory target t)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Fangcun native helper build\n\n"))
            (setq default-directory fangcun--source-directory)
            (compilation-mode))
          (setq fangcun--native-build-process
                (make-process
                 :name "fangcun-helper-build"
                 :buffer buffer
                 :command
                 (list cargo "build" "--release" "--locked"
                       "--manifest-path"
                       fangcun--native-helper-manifest
                       "--target-dir" target)
                 :noquery t
                 :sentinel #'fangcun--native-build-sentinel))
          (display-buffer buffer)
          (message "Building Fangcun native helper..."))
      (fangcun--native-warning
       (concat
        "Cargo is unavailable; Fangcun external changes require "
        "`fangcun-db-sync'")))))

(defun fangcun--ensure-native-helper (yiyus)
  "Start or build the native helper for YIYUS when enabled."
  (when (and fangcun-native-helper-enabled
             yiyus)
    (cond
     ((process-live-p fangcun--native-build-process))
     ((fangcun--native-helper-available-p)
      (fangcun--start-native-watch yiyus))
     (t
      (fangcun--build-native-helper)))))

;;;###autoload
(defun fangcun-native-build ()
  "Build the Fangcun native helper asynchronously."
  (interactive)
  (fangcun--build-native-helper))

(defun fangcun--install-operation-advice ()
  "Install exact file operation synchronization once."
  ;; These operations are infrequent and give immediate database state.  Keep
  ;; them active with the monitor; its duplicate event will compare unchanged.
  (unless (advice-member-p #'fangcun--around-rename-file
                           'rename-file)
    (advice-add 'rename-file :around #'fangcun--around-rename-file))
  (unless (advice-member-p #'fangcun--around-delete-file
                           'delete-file)
    (advice-add 'delete-file :around #'fangcun--around-delete-file))
  (unless (advice-member-p #'fangcun--around-vc-delete-file
                           'vc-delete-file)
    (advice-add 'vc-delete-file :around
                #'fangcun--around-vc-delete-file)))

(defun fangcun--activate-session (yiyus)
  "Activate synchronization for the current Fangcun session and YIYUS."
  (setq fangcun--session-active-p t
        fangcun--session-yiyus yiyus)
  (fangcun--install-operation-advice)
  (fangcun--ensure-native-helper yiyus))

(defun fangcun--shutdown-native-helper ()
  "Stop Fangcun helper processes and pending work before Emacs exits."
  (setq fangcun--session-active-p nil)
  (fangcun--stop-native-watch)
  (when (timerp fangcun--native-event-timer)
    (cancel-timer fangcun--native-event-timer))
  (setq fangcun--native-event-timer nil)
  (when (process-live-p fangcun--native-build-process)
    (set-process-sentinel fangcun--native-build-process #'ignore)
    (delete-process fangcun--native-build-process))
  (setq fangcun--native-build-process nil))

(add-hook 'kill-emacs-hook #'fangcun--shutdown-native-helper)

(defun fangcun--ensure-session (&optional yiyus)
  "Synchronize Fangcun on its first use in this Emacs session.
Return the normalized configured YIYUS, obtaining them when omitted."
  (setq yiyus (or yiyus (fangcun--configured-yiyus)))
  (unless yiyus
    (user-error "Configure `fangcun-yiyus' before using Fangcun"))
  (unless (and fangcun--session-active-p
               (file-exists-p fangcun-database-file))
    (fangcun--sync-yiyus yiyus t)
    (fangcun--activate-session yiyus))
  yiyus)

(defun fangcun--db-update-file-in-yiyu
    (file yiyu &optional no-message read-disk)
  "Replace database entries for saved Org FILE owned by YIYU.
When NO-MESSAGE is non-nil, do not report the indexed counts.
When READ-DISK is non-nil, ignore an unsaved visiting buffer."
  (unless (file-regular-p file)
    (user-error "Fangcun file does not exist: %s" file))
  (unless (string-match-p "\\.org\\'" file)
    (user-error "Fangcun only indexes Org files: %s" file))
  (when-let* ((buffer (find-buffer-visiting file)))
    (when (and (not read-disk)
               (buffer-modified-p buffer))
      (user-error "Save the Fangcun file before updating it")))
  (unless (file-exists-p fangcun-database-file)
    (user-error "Run fangcun-db-sync before updating individual files"))
  (let* ((state (fangcun--read-file-state yiyu file))
         (relative-file (fangcun-file-state-relative-file state))
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
                 "DELETE FROM files "
                 "WHERE yiyu_id = ? AND file = ?")
                (vector (fangcun-yiyu-id yiyu) relative-file))
               (fangcun--insert-file database state)
               (fangcun--insert-file-data database data))))))
    (unless no-message
      (message
       (concat
        "Fangcun indexed %d nodes, %d aliases, %d tags, and %d links "
        "from %s")
       (plist-get result :nodes)
       (plist-get result :aliases)
       (plist-get result :tags)
       (plist-get result :links)
       (abbreviate-file-name file)))
    result))

(defun fangcun--database-file-state (database yiyu relative-file)
  "Return DATABASE state for RELATIVE-FILE in YIYU, or nil."
  (when-let* ((row
               (car
                (sqlite-select
                 database
                 (concat
                  "SELECT mtime, size FROM files "
                  "WHERE yiyu_id = ? AND file = ?")
                 (vector (fangcun-yiyu-id yiyu)
                         relative-file)))))
    (cons (elt row 0) (elt row 1))))

(defun fangcun--forget-file-in-yiyu (file yiyu)
  "Remove indexed data for FILE owned by YIYU."
  (let ((relative-file
         (file-relative-name file (fangcun-yiyu-root yiyu))))
    (fangcun--call-with-database
     (lambda (database)
       (sqlite-execute
        database
        (concat
         "DELETE FROM files "
         "WHERE yiyu_id = ? AND file = ?")
        (vector (fangcun-yiyu-id yiyu) relative-file))))))

(defun fangcun--reconcile-file (file)
  "Reconcile one absolute FILE with the active Fangcun database."
  (when (and fangcun--session-active-p
             (file-exists-p fangcun-database-file)
             (string-match-p "\\.org\\'" file))
    (when-let* ((yiyu
                 (fangcun--yiyu-containing-file
                  file fangcun--session-yiyus)))
      (if (file-regular-p file)
          (let* ((state (fangcun--read-file-state yiyu file))
                 (relative-file
                  (fangcun-file-state-relative-file state))
                 (current
                  (cons (fangcun-file-state-mtime state)
                        (fangcun-file-state-size state)))
                 (stored
                  (fangcun--call-with-database
                   (lambda (database)
                     (fangcun--database-file-state
                      database yiyu relative-file)))))
            (unless (equal stored current)
              (fangcun--db-update-file-in-yiyu
               file yiyu t t)))
        (fangcun--forget-file-in-yiyu file yiyu)))))

(defun fangcun--process-native-events ()
  "Reconcile file events queued by the native monitor."
  (setq fangcun--native-event-timer nil)
  (let ((full-sync fangcun--native-pending-full-sync-p)
        files)
    (setq fangcun--native-pending-full-sync-p nil)
    (maphash
     (lambda (file _value)
       (push file files))
     fangcun--native-pending-files)
    (clrhash fangcun--native-pending-files)
    (when (and fangcun--session-active-p
               (file-exists-p fangcun-database-file))
      (condition-case error-data
          (if full-sync
              (fangcun--sync-yiyus fangcun--session-yiyus t)
            (dolist (file files)
              (fangcun--reconcile-file file)))
        (error
         (display-warning
          'fangcun
          (format "Automatic database reconciliation failed: %s"
                  (error-message-string error-data))
          :warning))))))

(defun fangcun--reconcile-file-operation (old-files &optional new-file)
  "Reconcile OLD-FILES and optional NEW-FILE after an operation."
  (when (and fangcun--session-active-p
             (file-exists-p fangcun-database-file))
    (condition-case error-data
        (progn
          (dolist (file old-files)
            (when (string-match-p "\\.org\\'" file)
              (when-let* ((yiyu
                           (fangcun--yiyu-containing-file
                            file fangcun--session-yiyus)))
                (fangcun--forget-file-in-yiyu file yiyu))))
          (when new-file
            (fangcun--reconcile-file new-file)))
      (error
       (display-warning
        'fangcun
        (format "File operation database update failed: %s"
                (error-message-string error-data))
        :warning)))))

(defun fangcun--rename-destination (file new-name)
  "Return the final destination when FILE is renamed to NEW-NAME."
  (let ((file (expand-file-name file))
        (new-name (expand-file-name new-name)))
    (if (file-directory-p new-name)
        (expand-file-name (file-name-nondirectory file) new-name)
      new-name)))

(defun fangcun--around-rename-file
    (function file new-name &rest arguments)
  "Call FUNCTION to rename FILE to NEW-NAME, then reconcile Fangcun."
  (let ((old-file (expand-file-name file))
        (new-file (fangcun--rename-destination file new-name)))
    (prog1 (apply function file new-name arguments)
      (fangcun--reconcile-file-operation
       (list old-file) new-file))))

(defun fangcun--around-delete-file (function file &rest arguments)
  "Call FUNCTION to delete FILE, then reconcile Fangcun."
  (let ((absolute-file (expand-file-name file)))
    (prog1 (apply function file arguments)
      (fangcun--reconcile-file-operation
       (list absolute-file)))))

(defun fangcun--around-vc-delete-file
    (function file-or-files &rest arguments)
  "Call FUNCTION to delete FILE-OR-FILES, then reconcile Fangcun."
  (let ((files
         (mapcar #'expand-file-name
                 (ensure-list file-or-files))))
    (prog1 (apply function file-or-files arguments)
      (fangcun--reconcile-file-operation files))))

;;;###autoload
(defun fangcun-db-update-file (file &optional no-message)
  "Replace database entries for saved Org FILE.
When NO-MESSAGE is non-nil, do not report the indexed counts."
  (interactive
   (list
    (or buffer-file-name
        (user-error "The current buffer is not visiting a file"))))
  (setq file (expand-file-name file))
  (let* ((yiyus (fangcun--ensure-session))
         (yiyu
          (or (fangcun--yiyu-containing-file file yiyus)
              (user-error
               "File is outside the configured Fangcun yiyus: %s"
               file))))
    (fangcun--db-update-file-in-yiyu file yiyu no-message)))

(defun fangcun--update-current-file ()
  "Update the current unmodified file when Fangcun manages it."
  (when (and buffer-file-name
             (file-exists-p fangcun-database-file)
             (string-match-p "\\.org\\'" buffer-file-name))
    (let* ((yiyus (fangcun--configured-yiyus))
           (yiyu
            (fangcun--yiyu-containing-file buffer-file-name yiyus)))
      (when yiyu
        (fangcun--db-update-file-in-yiyu buffer-file-name yiyu t)))))

(defun fangcun--update-after-save ()
  "Update the current Fangcun file after saving it."
  (when fangcun-db-update-on-save
    (fangcun--update-current-file)))

(defun fangcun--update-after-revert ()
  "Update the current Fangcun file after reverting it from disk."
  (fangcun--update-current-file))

(defun fangcun--setup-file-updates ()
  "Arrange for the current Org buffer to update Fangcun from disk."
  (add-hook 'after-save-hook #'fangcun--update-after-save nil t)
  (add-hook 'after-revert-hook #'fangcun--update-after-revert nil t))

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

(defun fangcun--attach-tags (nodes rows)
  "Attach tags from SQLite ROWS to NODES and return NODES.
Each row contains a node ID followed by one tag."
  (let ((nodes-by-id (make-hash-table :test #'equal)))
    (dolist (node nodes)
      (push node (gethash (fangcun-node-id node) nodes-by-id)))
    (dolist (row rows)
      (dolist (node (gethash (elt row 0) nodes-by-id))
        (push (elt row 1) (fangcun-node-tags node))))
    (dolist (node nodes)
      (setf (fangcun-node-tags node)
            (nreverse (fangcun-node-tags node))))
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
       (fangcun--attach-tags
        (fangcun--attach-aliases
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
       (fangcun--attach-tags
        (fangcun--attach-aliases
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

;;;###autoload
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

(defun fangcun--backlink-from-row (row)
  "Return a Fangcun backlink represented by SQLite ROW."
  (make-fangcun-backlink
   :node (fangcun--node-from-row (cl-subseq row 0 6))
   :position (elt row 6)
   :count (and (> (length row) 7) (elt row 7))))

(defun fangcun--attach-backlink-node-data
    (database backlinks target-id)
  "Attach aliases and tags to BACKLINKS targeting TARGET-ID in DATABASE."
  (let ((nodes (mapcar #'fangcun-backlink-node backlinks)))
    (fangcun--attach-aliases
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
    (fangcun--attach-tags
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

(defun fangcun-backlink-list (target-id)
  "Return unique source nodes linking to TARGET-ID.
When one source contains several links, retain its first occurrence."
  (fangcun--call-with-database
   (lambda (database)
     (fangcun--attach-backlink-node-data
      database
      (mapcar
       #'fangcun--backlink-from-row
       (sqlite-select
        database
        (concat
         "SELECT n.id, n.yiyu_id, y.name, y.root, "
         "n.file, n.title, first_link.position, "
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

(defun fangcun-backlink-occurrence-list (target-id)
  "Return every indexed backlink occurrence to TARGET-ID."
  (fangcun--call-with-database
   (lambda (database)
     (fangcun--attach-backlink-node-data
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
  (let ((title (fangcun-node-title node))
        (tags (fangcun-node-tags node)))
    (propertize
     (concat
      title
      (when tags
        (propertize
         (concat "  #" (string-join tags " #"))
         'face 'org-tag))
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

(defun fangcun--valid-tag-p (tag)
  "Return non-nil when TAG is a valid Org tag name."
  (and (stringp tag)
       (string-match-p (concat "\\`\\(?:" org-tag-re "\\)\\'") tag)))

(defun fangcun--validate-tags (tags)
  "Return validated TAGS without text properties or duplicates."
  (setq tags
        (delete-dups
         (mapcar #'substring-no-properties tags)))
  (dolist (tag tags)
    (unless (fangcun--valid-tag-p tag)
      (user-error
       (concat
        "Invalid Org tag %S; use letters, numbers, _, @, #, or %%")
       tag)))
  tags)

(defun fangcun--tag-completions ()
  "Return known Org and Fangcun tags for completion."
  (let (tags)
    (dolist (entry
             (append org-current-tag-alist
                     org-tag-persistent-alist
                     org-tag-alist
                     (org-get-buffer-tags)))
      (when-let* ((tag
                   (cond
                    ((stringp entry) entry)
                    ((and (consp entry) (stringp (car entry)))
                     (car entry)))))
        (when (fangcun--valid-tag-p tag)
          (push (substring-no-properties tag) tags))))
    (dolist (row
             (fangcun--call-with-database
              (lambda (database)
                (sqlite-select
                 database
                 "SELECT DISTINCT tag FROM tags ORDER BY tag COLLATE NOCASE"))))
      (push (elt row 0) tags))
    (sort (delete-dups tags) #'string-lessp)))

(defun fangcun--read-tags (current-tags)
  "Read node tags, initially offering CURRENT-TAGS."
  (let ((crm-separator "[ \t]*:[ \t]*")
        (completion-extra-properties '(:category org-tag))
        (prompt "Node tags: ")
        (initial (org-make-tag-string current-tags))
        tags invalid)
    (while
        (progn
          (setq tags
                (delete
                 ""
                 (mapcar
                  #'string-trim
                  (completing-read-multiple
                   prompt (fangcun--tag-completions)
                   nil nil initial 'org-tags-history)))
                invalid
                (seq-find
                 (lambda (tag)
                   (not (fangcun--valid-tag-p tag)))
                 tags))
          (when invalid
            (setq prompt (format "Node tags [invalid %S]: " invalid)
                  initial (org-make-tag-string tags)))
          invalid))
    (fangcun--validate-tags tags)))

(defun fangcun--local-tags-at-point ()
  "Return the local Org tags of the node at point."
  (mapcar
   #'substring-no-properties
   (if (= (org-outline-level) 0)
       org-file-tags
     (org-get-tags nil t))))

(defun fangcun--set-file-tags (tags)
  "Replace the current file node's FILETAGS keywords with TAGS."
  (let ((case-fold-search t)
        (value (org-make-tag-string tags))
        (limit
         (save-excursion
           (goto-char (point-min))
           (if (re-search-forward org-outline-regexp-bol nil t)
               (line-beginning-position)
             (point-max))))
        ranges)
    (goto-char (point-min))
    (while (re-search-forward "^#\\+filetags:[ \t]*.*$" limit t)
      (push (cons (line-beginning-position)
                  (line-end-position))
            ranges))
    (setq ranges (nreverse ranges))
    (cond
     ((and tags ranges)
      (dolist (range (reverse (cdr ranges)))
        (delete-region
         (car range)
         (min (point-max) (1+ (cdr range)))))
      (goto-char (caar ranges))
      (delete-region (caar ranges) (cdar ranges))
      (insert "#+filetags: " value))
     (tags
      (goto-char
       (or (cdr (org-get-property-block (point-min)))
           (user-error "The Fangcun file node has no property drawer")))
      (forward-line)
      (insert "#+filetags: " value "\n"))
     (t
      (dolist (range (reverse ranges))
        (delete-region
         (car range)
         (min (point-max) (1+ (cdr range)))))))
    (org-set-regexps-and-options 'tags-only)))

(defun fangcun--set-local-tags-at-point (tags)
  "Replace the local Org tags of the node at point with TAGS."
  ;; `org-set-tags' only supports headlines, so file nodes need to update
  ;; FILETAGS separately.
  (if (= (org-outline-level) 0)
      (fangcun--set-file-tags tags)
    (org-set-tags tags)))

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

(defun fangcun--read-new-file (title directory root)
  "Read a new file below ROOT, starting in DIRECTORY.
Suggest a file name from TITLE when it is non-empty."
  (let ((initial
         (unless (string-empty-p title)
           (concat title ".org")))
        (prompt "New Fangcun file: ")
        file error)
    (while
        (progn
          (setq file
                (expand-file-name
                 (read-file-name prompt directory nil nil initial)
                 directory)
                error
                (fangcun--new-file-name-error file root))
          (when error
            (setq prompt
                  (format "New Fangcun file [%s]: " error)
                  initial
                  (if (file-remote-p file)
                      file
                    (file-relative-name file directory))))
          error))
    file))

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
    (fangcun--ensure-session yiyus)
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
              (fangcun--read-new-file title directory root)))
        (make-directory (file-name-directory file) t)
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
  (fangcun--ensure-session)
  (fangcun-node-visit (fangcun--read-node)))

;;;###autoload
(defun fangcun-node-insert ()
  "Choose a Fangcun node and insert an Org ID link to it.
The chosen title or alias becomes the link description."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun node links can only be inserted in Org buffers"))
  (fangcun--ensure-session)
  (let ((node (fangcun--read-node)))
    (insert
     (org-link-make-string
      (concat "id:" (fangcun-node-id node))
      (fangcun-node-title node)))
    node))

;;;###autoload
(defun fangcun-node-set-tags (&optional tags)
  "Set local TAGS on the nearest enclosing Fangcun node.
Interactively, edit the current local tags with completion."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun node tags are only available in Org buffers"))
  (fangcun--ensure-session)
  (save-excursion
    (save-restriction
      (widen)
      (org-set-regexps-and-options 'tags-only)
      (unless (fangcun--goto-node-at-point)
        (user-error "Point is not inside a Fangcun node"))
      (when (called-interactively-p 'interactive)
        (setq tags
              (fangcun--read-tags
               (fangcun--local-tags-at-point))))
      (setq tags (fangcun--validate-tags tags))
      (fangcun--set-local-tags-at-point tags)
      tags)))

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
  (let ((target-id
         (or (fangcun--node-id-at-point)
             (user-error "Point is not inside a Fangcun node"))))
    (fangcun--ensure-session)
    (let ((backlink (fangcun--read-backlink target-id)))
      (fangcun-backlink-visit backlink))))

(defun fangcun--backlink-preview (backlink buffer)
  "Return a one-line preview of BACKLINK from BUFFER."
  (let ((position (fangcun-backlink-position backlink)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (if (> position (point-max))
              "[Link position is stale; synchronize Fangcun]"
            (goto-char position)
            (string-trim
             (substring-no-properties
              (org-link-display-format
               (buffer-substring
                (line-beginning-position)
                (line-end-position)))))))))))

(defun fangcun--backlink-previews (backlinks)
  "Return an eq table mapping BACKLINKS to one-line previews.
Each source file is read from disk at most once."
  (let ((backlinks-by-file (make-hash-table :test #'equal))
        (previews (make-hash-table :test #'eq)))
    (dolist (backlink backlinks)
      (let ((file
             (fangcun--node-absolute-file
              (fangcun-backlink-node backlink))))
        (puthash file
                 (cons backlink (gethash file backlinks-by-file))
                 backlinks-by-file)))
    (cl-labels
        ((record-previews
          (file-backlinks buffer)
          (dolist (backlink file-backlinks)
            (puthash backlink
                     (fangcun--backlink-preview backlink buffer)
                     previews))))
      (maphash
       (lambda (file file-backlinks)
         (if (not (file-readable-p file))
             (dolist (backlink file-backlinks)
               (puthash backlink
                        "[Source file is unavailable]"
                        previews))
           (if-let* ((buffer (fangcun--reusable-file-buffer file)))
               (record-previews file-backlinks buffer)
             (with-temp-buffer
               (insert-file-contents file)
               (record-previews
                file-backlinks (current-buffer))))))
       backlinks-by-file))
    previews))

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
         (previews (fangcun--backlink-previews backlinks))
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
           (gethash backlink previews)
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
    (fangcun--ensure-session)
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
