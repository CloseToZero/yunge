;;; fangcun-mcp.el --- MCP tools for Fangcun -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'fangcun)
(require 'fangcun-file)
(require 'json)
(require 'seq)
(require 'subr-x)

(defconst fangcun-mcp--default-page-size 20
  "Default number of results returned by a paginated tool call.")

(defconst fangcun-mcp--maximum-page-size 100
  "Maximum number of results returned by a paginated tool call.")

(defconst fangcun-mcp--read-only-annotations
  '(:readOnlyHint t
    :destructiveHint :false
    :openWorldHint :false)
  "MCP annotations shared by read-only Fangcun tools.")

(defun fangcun-mcp--required-string (arguments property)
  "Return the non-empty string PROPERTY from ARGUMENTS."
  (let ((value (plist-get arguments property)))
    (unless (and (stringp value) (not (string-empty-p value)))
      (user-error "%s must be a non-empty string" property))
    value))

(defun fangcun-mcp--required-string-list (arguments property)
  "Return the non-empty list of non-empty strings PROPERTY from ARGUMENTS."
  (let ((value (plist-get arguments property)))
    (unless (and (listp value)
                 value
                 (seq-every-p
                  (lambda (item)
                    (and (stringp item)
                         (not (string-empty-p item))))
                  value))
      (user-error "%s must be a non-empty array of non-empty strings"
                  property))
    value))

(defun fangcun-mcp--node-object (node)
  "Return the JSON object describing Fangcun NODE."
  (list
   :id (fangcun-node-id node)
   :title (fangcun-node-title node)
   :aliases (vconcat (fangcun-node-aliases node))
   :tags (vconcat (fangcun-node-tags node))
   :yiyu (fangcun-node-yiyu-id node)
   :yiyuName (fangcun-node-yiyu-name node)
   :file (fangcun-node-file node)))

(defun fangcun-mcp--page-size (arguments)
  "Return and validate the page size in ARGUMENTS."
  (let ((page-size
         (or (plist-get arguments :pageSize)
             fangcun-mcp--default-page-size)))
    (unless
        (and (integerp page-size)
             (<= 1 page-size fangcun-mcp--maximum-page-size))
      (user-error
       ":pageSize must be an integer between 1 and %d"
       fangcun-mcp--maximum-page-size))
    page-size))

(defun fangcun-mcp--encode-cursor (kind scope key)
  "Return an opaque cursor for KIND, SCOPE, and sort KEY."
  (base64-encode-string
   (encode-coding-string
    (json-serialize
     (list
      :version 1
      :kind kind
      :scope scope
      :key (vconcat key)))
    'utf-8)
   t))

(defun fangcun-mcp--decode-cursor
    (cursor expected-kind expected-scope key-predicate)
  "Decode CURSOR and validate its expected context and key.
EXPECTED-KIND and EXPECTED-SCOPE bind the cursor to one result set.
KEY-PREDICATE returns non-nil for a valid decoded sort key."
  (when cursor
    (unless (and (stringp cursor) (not (string-empty-p cursor)))
      (user-error ":cursor must be a non-empty string"))
    (condition-case error-data
        (let* ((object
                (json-parse-string
                 (decode-coding-string
                  (base64-decode-string cursor)
                  'utf-8)
                 :object-type 'plist
                 :array-type 'list
                 :null-object nil
                 :false-object nil))
               (version (plist-get object :version))
               (kind (plist-get object :kind))
               (scope (plist-get object :scope))
               (key (plist-get object :key)))
          (unless (and (= version 1)
                       (equal kind expected-kind)
                       (equal scope expected-scope)
                       (funcall key-predicate key))
            (user-error
             "Cursor does not belong to this request"))
          key)
      (user-error
       (signal (car error-data) (cdr error-data)))
      (error (user-error "Invalid cursor")))))

(defun fangcun-mcp--normalize-search-string (value)
  "Return VALUE without properties and normalized for search."
  (downcase (substring-no-properties (or value ""))))

(defun fangcun-mcp--contains-p (needle haystack)
  "Return non-nil when HAYSTACK contains NEEDLE."
  (string-match-p (regexp-quote needle) haystack))

(defun fangcun-mcp--word-contains-p (needle haystack)
  "Return non-nil when HAYSTACK contains NEEDLE as a word."
  (string-match-p
   (concat "\\_<" (regexp-quote needle) "\\_>")
   haystack))

(defun fangcun-mcp--some-string-p (predicate strings)
  "Return non-nil when PREDICATE accepts an element of STRINGS."
  (seq-some predicate strings))

(defun fangcun-mcp--term-match (term fields)
  "Return the best score and field for TERM in normalized FIELDS."
  (let ((title (plist-get fields :title))
        (aliases (plist-get fields :aliases))
        (tags (plist-get fields :tags))
        (file (plist-get fields :file))
        (yiyu (plist-get fields :yiyu)))
    (cond
     ((equal term title) (cons 100 "title"))
     ((member term aliases) (cons 100 "alias"))
     ((string-prefix-p term title) (cons 90 "title"))
     ((fangcun-mcp--some-string-p
       (lambda (value) (string-prefix-p term value))
       aliases)
      (cons 90 "alias"))
     ((fangcun-mcp--word-contains-p term title)
      (cons 80 "title"))
     ((fangcun-mcp--some-string-p
       (lambda (value)
         (fangcun-mcp--word-contains-p term value))
       aliases)
      (cons 80 "alias"))
     ((fangcun-mcp--contains-p term title)
      (cons 70 "title"))
     ((fangcun-mcp--some-string-p
       (lambda (value) (fangcun-mcp--contains-p term value))
       aliases)
      (cons 70 "alias"))
     ((member term tags) (cons 60 "tag"))
     ((fangcun-mcp--some-string-p
       (lambda (value) (fangcun-mcp--contains-p term value))
       tags)
      (cons 50 "tag"))
     ((fangcun-mcp--contains-p term file) (cons 30 "file"))
     ((fangcun-mcp--some-string-p
       (lambda (value) (fangcun-mcp--contains-p term value))
       yiyu)
      (cons 20 "yiyu")))))

(defun fangcun-mcp--whole-query-rank (query fields)
  "Return the whole-title or alias match rank for QUERY in FIELDS."
  (let ((title (plist-get fields :title))
        (aliases (plist-get fields :aliases)))
    (cond
     ((or (equal query title) (member query aliases)) 3)
     ((or (string-prefix-p query title)
          (fangcun-mcp--some-string-p
           (lambda (value) (string-prefix-p query value))
           aliases))
      2)
     ((or (fangcun-mcp--contains-p query title)
          (fangcun-mcp--some-string-p
           (lambda (value) (fangcun-mcp--contains-p query value))
           aliases))
      1)
     (t 0))))

(defun fangcun-mcp--search-result (node terms query)
  "Return NODE's ranked search result for TERMS and QUERY, or nil."
  (let* ((fields
          (list
           :title
           (fangcun-mcp--normalize-search-string
            (fangcun-node-title node))
           :aliases
           (mapcar
            #'fangcun-mcp--normalize-search-string
            (fangcun-node-aliases node))
           :tags
           (mapcar
            #'fangcun-mcp--normalize-search-string
            (fangcun-node-tags node))
           :file
           (fangcun-mcp--normalize-search-string
            (fangcun-node-file node))
           :yiyu
           (mapcar
            #'fangcun-mcp--normalize-search-string
            (list
             (fangcun-node-yiyu-id node)
             (fangcun-node-yiyu-name node)))))
         (matches
          (mapcar
           (lambda (term)
             (fangcun-mcp--term-match term fields))
           terms)))
    (when (seq-every-p #'identity matches)
      (let* ((scores (mapcar #'car matches))
              (matched-fields
               (delete-dups (mapcar #'cdr matches)))
             (key
              (list
               (fangcun-mcp--whole-query-rank query fields)
               (apply #'min scores)
               (apply #'+ scores)
               (plist-get fields :title)
               (fangcun-mcp--normalize-search-string
                (fangcun-node-yiyu-id node))
               (plist-get fields :file)
               (fangcun-node-id node))))
        (list
         :node node
         :matched-fields matched-fields
         :key key)))))

(defun fangcun-mcp--string-key-before-p (left right)
  "Return non-nil when string key LEFT sorts before RIGHT."
  (cond
   ((null left) nil)
   ((string-lessp (car left) (car right)) t)
   ((equal (car left) (car right))
    (fangcun-mcp--string-key-before-p (cdr left) (cdr right)))
   (t nil)))

(defun fangcun-mcp--search-key-before-p (left right)
  "Return non-nil when search key LEFT sorts before RIGHT."
  (cond
   ((> (nth 0 left) (nth 0 right)) t)
   ((< (nth 0 left) (nth 0 right)) nil)
   ((> (nth 1 left) (nth 1 right)) t)
   ((< (nth 1 left) (nth 1 right)) nil)
   ((> (nth 2 left) (nth 2 right)) t)
   ((< (nth 2 left) (nth 2 right)) nil)
   (t
    (fangcun-mcp--string-key-before-p
     (nthcdr 3 left) (nthcdr 3 right)))))

(defun fangcun-mcp--valid-search-key-p (key)
  "Return non-nil when KEY is a valid search cursor key."
  (and (listp key)
       (= (length key) 7)
       (seq-every-p #'numberp (seq-take key 3))
       (seq-every-p #'stringp (nthcdr 3 key))))

(defun fangcun-mcp--items-after-key
    (items cursor-key key-function before-p)
  "Return ITEMS strictly after CURSOR-KEY in their current order."
  (if (null cursor-key)
      items
    (seq-drop-while
     (lambda (item)
       (not
        (funcall
         before-p cursor-key (funcall key-function item))))
     items)))

(defun fangcun-mcp--page (items page-size)
  "Return a page and optional continuation key from sorted ITEMS."
  (let* ((window (seq-take items (1+ page-size)))
         (has-more (> (length window) page-size))
         (page (if has-more (butlast window) window)))
    (list
     :items page
     :next-key
     (and has-more
          (plist-get (car (last page)) :key)))))

(defun fangcun-mcp--list-yiyus (_arguments)
  "Return the configured Fangcun yiyus."
  (list :yiyus
        (vconcat
         (mapcar
          (lambda (yiyu)
            (list
             :id (fangcun-yiyu-id yiyu)
             :name (fangcun-yiyu-name yiyu)
             :root (fangcun-yiyu-root yiyu)))
          (fangcun--configured-yiyus)))))

(defun fangcun-mcp--search-nodes (arguments)
  "Search Fangcun nodes described by MCP ARGUMENTS."
  (fangcun--ensure-session)
  (let* ((raw-query
          (fangcun-mcp--required-string arguments :query))
          (terms
           (mapcar
            #'fangcun-mcp--normalize-search-string
            (split-string raw-query nil t)))
          (query
           (if terms
               (string-join terms " ")
             (user-error ":query must contain a search term")))
         (page-size (fangcun-mcp--page-size arguments))
         (cursor-key
          (fangcun-mcp--decode-cursor
           (plist-get arguments :cursor)
           "search-nodes" query
           #'fangcun-mcp--valid-search-key-p))
          (results
           (sort
            (delq
             nil
             (mapcar
              (lambda (node)
                (fangcun-mcp--search-result node terms query))
              (fangcun-node-list)))
            (lambda (left right)
              (fangcun-mcp--search-key-before-p
               (plist-get left :key)
               (plist-get right :key)))))
         (remaining
          (fangcun-mcp--items-after-key
           results cursor-key
           (lambda (result) (plist-get result :key))
           #'fangcun-mcp--search-key-before-p))
         (page (fangcun-mcp--page remaining page-size))
         (items (plist-get page :items))
         (next-key (plist-get page :next-key)))
    (append
     (list
      :nodes
      (vconcat
       (mapcar
        (lambda (result)
          (list
           :node
           (fangcun-mcp--node-object
            (plist-get result :node))
           :matchedFields
           (vconcat (plist-get result :matched-fields))))
        items)))
     (when next-key
       (list
        :nextCursor
        (fangcun-mcp--encode-cursor
         "search-nodes" query next-key))))))

(defun fangcun-mcp--node-by-id (id)
  "Return the indexed Fangcun node named ID."
  (fangcun--ensure-session)
  (or (fangcun-node-from-id id)
      (user-error "Fangcun node is not indexed: %s" id)))

(defun fangcun-mcp--locate-node (arguments)
  "Locate a Fangcun node described by MCP ARGUMENTS."
  (let* ((location
          (fangcun-file-locate-node
           (fangcun-mcp--required-string arguments :id)))
         (node (plist-get location :node)))
    (list
     :node (fangcun-mcp--node-object node)
     :location
     (list
      :absoluteFile (plist-get location :file)
      :kind (symbol-name (plist-get location :kind))
      :startLine (plist-get location :start-line)
      :endLine (plist-get location :end-line)
      :outlinePath (vconcat (plist-get location :outline-path))
      :modifiedInEmacs (if (plist-get location :modified-p) t :false)))))

(defun fangcun-mcp--backlink-key (backlink)
  "Return the stable display-order key for BACKLINK."
  (let ((node (fangcun-backlink-node backlink)))
    (list
     (fangcun-mcp--normalize-search-string
      (fangcun-node-title node))
     (fangcun-mcp--normalize-search-string
      (fangcun-node-yiyu-name node))
     (fangcun-mcp--normalize-search-string
      (fangcun-node-file node))
     (fangcun-node-id node))))

(defun fangcun-mcp--valid-backlink-key-p (key)
  "Return non-nil when KEY is a valid backlink cursor key."
  (and (listp key)
       (= (length key) 4)
       (seq-every-p #'stringp key)))

(defun fangcun-mcp--list-backlinks (arguments)
  "List backlinks described by MCP ARGUMENTS."
  (let* ((id (fangcun-mcp--required-string arguments :id))
         (target (fangcun-mcp--node-by-id id))
         (page-size (fangcun-mcp--page-size arguments))
         (cursor-key
          (fangcun-mcp--decode-cursor
           (plist-get arguments :cursor)
           "list-backlinks" id
           #'fangcun-mcp--valid-backlink-key-p))
         (results
          (sort
           (mapcar
            (lambda (backlink)
              (list
               :backlink backlink
               :key (fangcun-mcp--backlink-key backlink)))
            (fangcun-backlink-list id))
           (lambda (left right)
             (fangcun-mcp--string-key-before-p
              (plist-get left :key)
              (plist-get right :key)))))
         (remaining
          (fangcun-mcp--items-after-key
           results cursor-key
           (lambda (result) (plist-get result :key))
           #'fangcun-mcp--string-key-before-p))
         (page (fangcun-mcp--page remaining page-size))
         (items (plist-get page :items))
         (next-key (plist-get page :next-key))
         (backlinks
          (mapcar
           (lambda (result) (plist-get result :backlink))
           items)))
    (append
     (list
      :target (fangcun-mcp--node-object target)
      :backlinks
      (vconcat
       (mapcar
        (lambda (backlink)
          (list
           :source
           (fangcun-mcp--node-object
            (fangcun-backlink-node backlink))
           :occurrenceCount
           (or (fangcun-backlink-count backlink) 1)
           :firstPosition
           (fangcun-backlink-position backlink)))
        backlinks)))
     (when next-key
       (list
        :nextCursor
        (fangcun-mcp--encode-cursor
         "list-backlinks" id next-key))))))

(defun fangcun-mcp--create-heading-node (arguments)
  "Create and index a Fangcun heading node from MCP ARGUMENTS."
  (fangcun-mcp--node-object
   (fangcun-file-create-heading-node
    (fangcun-mcp--required-string arguments :yiyu)
    (fangcun-mcp--required-string arguments :file)
    (fangcun-mcp--required-string-list arguments :headingPath))))

(defun fangcun-mcp--create-file-node (arguments)
  "Create and index a Fangcun file node from MCP ARGUMENTS."
  (let ((title (plist-get arguments :title)))
    (unless (or (null title) (stringp title))
      (user-error ":title must be a string"))
    (fangcun-mcp--node-object
     (fangcun-file-create-node
      (fangcun-mcp--required-string arguments :yiyu)
      (fangcun-mcp--required-string arguments :file)
      title))))

(defun fangcun-mcp-register-tools (register-tool)
  "Pass Fangcun MCP tools to REGISTER-TOOL.
REGISTER-TOOL receives each tool's name, description, schema, handler, and
optional annotations, in that order."
  (funcall register-tool
           "fangcun_list_yiyus"
           (concat
            "List configured 一隅（yiyu） note roots in 方寸（Fangcun）, "
            "including their identifiers, display names, and absolute root paths. "
            "These are ordinary Org files: when a root is accessible, use the "
            "client's filesystem tools for normal reads, literal full-text search, "
            "link editing, and edits to existing files. Use "
            "fangcun_create_file_node to create and index a new ID-bearing note. "
            "When native monitoring is active, Fangcun indexes saved external "
            "changes after an idle delay; otherwise run fangcun-db-sync in Emacs.")
           '(:type "object" :additionalProperties :false)
           #'fangcun-mcp--list-yiyus
           fangcun-mcp--read-only-annotations)

  (funcall register-tool
           "fangcun_search_nodes"
           (concat
            "Search indexed 方寸（Fangcun） Org note nodes by title, alias, tag, "
            "一隅（yiyu）, or relative file. Results are relevance-ranked and "
            "returned one cursor page at a time. This is metadata discovery, not "
            "literal content search; use the client's filesystem search for that.")
           '(:type "object"
                   :properties
                   (:query (:type "string" :description "Whitespace-separated search terms")
                           :pageSize
                           (:type "integer" :minimum 1 :maximum 100 :default 20
                                  :description "Maximum nodes to return in this page")
                           :cursor
                           (:type "string"
                                  :description "Opaque nextCursor from the preceding search page"))
                   :required ["query"]
                   :additionalProperties :false)
           #'fangcun-mcp--search-nodes
           fangcun-mcp--read-only-annotations)

  (funcall register-tool
           "fangcun_locate_node"
           (concat
            "Locate an indexed 方寸（Fangcun） node on disk without returning its "
            "content. The result gives its absolute file, line range, outline path, "
            "and whether Emacs has unsaved changes. Prefer this tool before using "
            "the client's filesystem tools for bounded reads, search, or edits.")
           '(:type "object"
                   :properties
                   (:id (:type "string" :description "方寸（Fangcun） node ID"))
                   :required ["id"]
                   :additionalProperties :false)
           #'fangcun-mcp--locate-node
           fangcun-mcp--read-only-annotations)

  (funcall register-tool
           "fangcun_list_backlinks"
           (concat
            "List unique 方寸（Fangcun） nodes containing indexed Org ID links "
            "to a target node. This uses Fangcun's cross-一隅（yiyu） link graph "
            "and reports each owning source node, occurrence count, and first "
            "position without reading note content. Results use cursor pagination.")
           '(:type "object"
                   :properties
                   (:id (:type "string" :description "Target 方寸（Fangcun） node ID")
                        :pageSize
                        (:type "integer" :minimum 1 :maximum 100 :default 20
                               :description "Maximum source nodes to return in this page")
                        :cursor
                        (:type "string"
                               :description "Opaque nextCursor from the preceding backlink page"))
                   :required ["id"]
                   :additionalProperties :false)
           #'fangcun-mcp--list-backlinks
           fangcun-mcp--read-only-annotations)

  (funcall register-tool
           "fangcun_create_file_node"
           (concat
            "Create a new 方寸（Fangcun） Org note file in a configured "
            "一隅（yiyu）. This tool assigns its file-level Org ID, saves the file, "
            "and indexes the node before returning successfully. It never "
            "overwrites an existing file. Add note content afterward with the "
            "client's filesystem tools.")
           '(:type "object"
                   :properties
                   (:yiyu
                    (:type "string" :description "Configured 一隅（yiyu） ID")
                    :file
                    (:type "string"
                           :description "Portable .org path relative to the 一隅（yiyu） root")
                    :title (:type "string" :description "Optional Org title"))
                   :required ["yiyu" "file"]
                   :additionalProperties :false)
           #'fangcun-mcp--create-file-node
           '(:readOnlyHint :false
                           :destructiveHint :false
                           :idempotentHint :false
                           :openWorldHint :false))

  (funcall register-tool
           "fangcun_create_heading_node"
           (concat
            "Give one existing Org heading its own 方寸（Fangcun） node ID, then "
            "save and index the file. Emacs generates the ID; an existing local ID "
            "is retained. The exact heading path excludes TODO keywords, priorities, "
            "and tags. The tool refuses files with unsaved Emacs edits and "
            "refreshes an unmodified buffer from disk when the file has changed.")
           '(:type "object"
                   :properties
                   (:yiyu
                    (:type "string" :description "Configured 一隅（yiyu） ID")
                    :file
                    (:type "string"
                           :description "Existing .org path relative to the 一隅（yiyu） root")
                    :headingPath
                    (:type "array"
                           :items (:type "string" :minLength 1)
                           :minItems 1
                           :description
                           "Exact outermost-to-innermost Org heading titles"))
                   :required ["yiyu" "file" "headingPath"]
                   :additionalProperties :false)
           #'fangcun-mcp--create-heading-node
           '(:readOnlyHint :false
                           :destructiveHint :false
                           :idempotentHint t
                           :openWorldHint :false)))

(provide 'fangcun-mcp)

;;; fangcun-mcp.el ends here
