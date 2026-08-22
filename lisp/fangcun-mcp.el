;;; fangcun-mcp.el --- Fangcun tools for Yunge MCP -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'fangcun)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'yunge-mcp)

(defconst fangcun-mcp--default-page-size 20
  "Default number of results returned by a paginated tool call.")

(defconst fangcun-mcp--maximum-page-size 100
  "Maximum number of results returned by a paginated tool call.")

(defconst fangcun-mcp--default-read-lines 60
  "Default maximum lines returned by one node read.")

(defconst fangcun-mcp--maximum-read-lines 500
  "Maximum lines returned by one node read.")

(defconst fangcun-mcp--default-read-characters 6000
  "Default maximum characters returned by one node read.")

(defconst fangcun-mcp--maximum-read-characters 50000
  "Maximum characters returned by one node read.")

(defconst fangcun-mcp--maximum-line-preview-characters 500
  "Maximum characters returned in a changed-line preview.")

(defconst fangcun-mcp--read-only-annotations
  '(:readOnlyHint t
    :destructiveHint :false
    :openWorldHint :false)
  "MCP annotations shared by read-only Fangcun tools.")

(defconst fangcun-mcp--additive-write-annotations
  '(:readOnlyHint :false
    :destructiveHint :false
    :idempotentHint :false
    :openWorldHint :false)
  "MCP annotations shared by additive Fangcun write tools.")

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
  (vconcat
   (mapcar
    (lambda (yiyu)
      (list
       :id (fangcun-yiyu-id yiyu)
       :name (fangcun-yiyu-name yiyu)
       :root (fangcun-yiyu-root yiyu)))
    (fangcun--configured-yiyus))))

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

(defun fangcun-mcp--node-region-in-buffer (node)
  "Return the source region of NODE in the current Org buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((position
             (org-find-entry-with-id (fangcun-node-id node))))
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
         (save-excursion
           (org-end-of-subtree t t)))))))

(defun fangcun-mcp--node-source-in-buffer (node)
  "Return NODE's Org source from the current buffer."
  (pcase-let ((`(,beginning . ,end)
               (fangcun-mcp--node-region-in-buffer node)))
    (buffer-substring-no-properties beginning end)))

(defun fangcun-mcp--node-absolute-file (node)
  "Return the absolute file name containing NODE."
  (expand-file-name
   (fangcun-node-file node)
   (fangcun-node-yiyu-root node)))

(defun fangcun-mcp--call-with-node-disk-buffer (node function)
  "Call FUNCTION with NODE and its absolute file in a temporary Org buffer."
  (let ((file (fangcun-mcp--node-absolute-file node)))
    (unless (file-regular-p file)
      (user-error "Fangcun node file no longer exists: %s" file))
    (with-temp-buffer
      (setq default-directory (file-name-directory file))
      (insert-file-contents file)
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode)))
      (funcall function node file))))

(defun fangcun-mcp--node-location-in-buffer (node file)
  "Return NODE's on-disk location in the current Org buffer for FILE."
  (pcase-let* ((`(,beginning . ,end)
                (fangcun-mcp--node-region-in-buffer node))
               (heading-p
                (save-excursion
                  (goto-char beginning)
                  (org-at-heading-p)))
               (visiting-buffer (find-buffer-visiting file))
               (modified-p
                (and visiting-buffer
                     (buffer-modified-p visiting-buffer))))
    (list
     :absoluteFile file
     :kind (if heading-p "heading" "file")
     :startLine (line-number-at-pos beginning t)
     :endLine
     (line-number-at-pos
      (max beginning (1- end)) t)
     :outlinePath
     (if heading-p
         (save-excursion
           (goto-char beginning)
           (vconcat (org-get-outline-path t)))
       [])
     :modifiedInEmacs (if modified-p t :false))))

(defun fangcun-mcp--locate-node (arguments)
  "Locate a Fangcun node described by MCP ARGUMENTS."
  (let* ((id (fangcun-mcp--required-string arguments :id))
         (node (fangcun-mcp--node-by-id id)))
    (fangcun-mcp--call-with-node-disk-buffer
     node
     (lambda (disk-node file)
       (list
        :node (fangcun-mcp--node-object disk-node)
        :location
        (fangcun-mcp--node-location-in-buffer disk-node file))))))

(defun fangcun-mcp--read-node-source (node)
  "Return NODE's Org source, reusing a visiting buffer when possible."
  (let ((file (fangcun-mcp--node-absolute-file node)))
    (unless (file-regular-p file)
      (user-error "Fangcun node file no longer exists: %s" file))
    (if-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (fangcun-mcp--node-source-in-buffer node))
      (with-temp-buffer
        (setq default-directory (file-name-directory file))
        (insert-file-contents file)
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (fangcun-mcp--node-source-in-buffer node)))))

(defun fangcun-mcp--bounded-integer
    (arguments property default minimum maximum)
  "Return integer PROPERTY in ARGUMENTS bounded by MINIMUM and MAXIMUM.
Use DEFAULT when PROPERTY is absent."
  (let ((value
         (if (plist-member arguments property)
             (plist-get arguments property)
           default)))
    (unless (and (integerp value)
                 (<= minimum value maximum))
      (user-error "%s must be an integer between %d and %d"
                  property minimum maximum))
    value))

(defun fangcun-mcp--integer-at-least (arguments property minimum)
  "Return integer PROPERTY from ARGUMENTS after checking MINIMUM."
  (let ((value (plist-get arguments property)))
    (unless (and (integerp value) (>= value minimum))
      (user-error "%s must be an integer of at least %d"
                  property minimum))
    value))

(defun fangcun-mcp--source-position (position)
  "Return the absolute one-based line and zero-based column at POSITION."
  (save-excursion
    (goto-char position)
    (list
     :line (line-number-at-pos position t)
     :column (- position (line-beginning-position)))))

(defun fangcun-mcp--read-node-chunk-in-buffer
    (node file arguments)
  "Read a bounded chunk of NODE from FILE using MCP ARGUMENTS."
  (pcase-let* ((`(,beginning . ,end)
                (fangcun-mcp--node-region-in-buffer node))
               (location
                (fangcun-mcp--node-location-in-buffer node file))
               (node-start-line (plist-get location :startLine))
               (node-end-line (plist-get location :endLine))
               (start-line
                (if (plist-member arguments :startLine)
                    (fangcun-mcp--integer-at-least
                     arguments :startLine 1)
                  node-start-line))
               (start-column
                (if (plist-member arguments :startColumn)
                    (fangcun-mcp--integer-at-least
                     arguments :startColumn 0)
                  0))
               (maximum-lines
                (fangcun-mcp--bounded-integer
                 arguments :maxLines
                 fangcun-mcp--default-read-lines 1
                 fangcun-mcp--maximum-read-lines))
               (maximum-characters
                (fangcun-mcp--bounded-integer
                 arguments :maxCharacters
                 fangcun-mcp--default-read-characters 1
                 fangcun-mcp--maximum-read-characters)))
    (unless (<= node-start-line start-line node-end-line)
      (user-error
       ":startLine must be within node lines %d through %d"
       node-start-line node-end-line))
    (goto-char (point-min))
    (unless (zerop (forward-line (1- start-line)))
      (user-error ":startLine is outside the source file: %d"
                  start-line))
    (let ((line-beginning (point))
          (line-end (line-end-position)))
      (when (> start-column (- line-end line-beginning))
        (user-error ":startColumn is outside source line %d: %d"
                    start-line start-column))
      (goto-char (+ line-beginning start-column)))
    (let ((chunk-beginning (point)))
      (unless (<= beginning chunk-beginning end)
        (user-error "Requested position is outside the source node"))
      (let* ((line-limit
              (save-excursion
                (forward-line maximum-lines)
                (point)))
             (character-limit
              (min end (+ chunk-beginning maximum-characters)))
             (chunk-end (min end line-limit character-limit))
             (has-more (< chunk-end end))
             (start-position
              (fangcun-mcp--source-position chunk-beginning))
             (end-position
              (fangcun-mcp--source-position chunk-end)))
        (append
         (list
          :node (fangcun-mcp--node-object node)
          :location location
          :content
          (buffer-substring-no-properties chunk-beginning chunk-end)
          :range
          (list
           :start start-position
           :endExclusive end-position)
          :hasMore (if has-more t :false))
         (when has-more
           (list :nextPosition end-position)))))))

(defun fangcun-mcp--read-node (arguments)
  "Read a bounded Fangcun node chunk described by MCP ARGUMENTS."
  (let* ((id (fangcun-mcp--required-string arguments :id))
         (node (fangcun-mcp--node-by-id id)))
    (fangcun-mcp--call-with-node-disk-buffer
     node
     (lambda (disk-node file)
       (fangcun-mcp--read-node-chunk-in-buffer
        disk-node file arguments)))))

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
         (include-preview (plist-get arguments :includePreview))
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
           items))
         (previews
          (when include-preview
            (fangcun--backlink-previews backlinks))))
    (append
     (list
      :target (fangcun-mcp--node-object target)
      :backlinks
      (vconcat
       (mapcar
        (lambda (backlink)
          (append
           (list
            :source
            (fangcun-mcp--node-object
             (fangcun-backlink-node backlink))
            :occurrenceCount
            (or (fangcun-backlink-count backlink) 1)
            :firstPosition
            (fangcun-backlink-position backlink))
           (when include-preview
             (list :preview (gethash backlink previews)))))
        backlinks)))
     (when next-key
       (list
        :nextCursor
        (fangcun-mcp--encode-cursor
         "list-backlinks" id next-key))))))

(defun fangcun-mcp--yiyu-by-id (id yiyus)
  "Return the member of YIYUS named ID."
  (or (seq-find
       (lambda (yiyu)
         (equal (fangcun-yiyu-id yiyu) id))
       yiyus)
      (user-error "Unknown Fangcun yiyu: %s" id)))

(defun fangcun-mcp--existing-org-file (yiyu relative-file)
  "Return an existing Org file below YIYU named RELATIVE-FILE."
  (when (file-name-absolute-p relative-file)
    (user-error ":file must be relative to the yiyu root"))
  (let* ((root (fangcun-yiyu-root yiyu))
         (file (expand-file-name relative-file root)))
    (unless (file-in-directory-p file root)
      (user-error ":file must stay below the yiyu root"))
    (unless (string-match-p "\\.org\\'" file)
      (user-error ":file must name an Org file"))
    (unless (file-regular-p file)
      (user-error "Fangcun file does not exist: %s" relative-file))
    file))

(defun fangcun-mcp--call-with-org-file-edit (file yiyu function)
  "Call FUNCTION in saved Org FILE, then save and reindex it for YIYU.
Refuse to save an already modified visiting buffer."
  (let* ((visiting-buffer (find-buffer-visiting file))
         (buffer (or visiting-buffer (find-file-noselect file)))
         (temporary-buffer-p (null visiting-buffer))
         result)
    (unwind-protect
        (with-current-buffer buffer
          (unless (derived-mode-p 'org-mode)
            (user-error "Fangcun MCP edits require an Org buffer"))
          (when (buffer-modified-p)
            (user-error
             "Fangcun file has unsaved changes; save it before MCP edits: %s"
             file))
          (save-excursion
            (save-restriction
              (widen)
              (atomic-change-group
                (setq result (funcall function))
                (let ((fangcun-db-update-on-save nil))
                  (save-buffer)))))
          (fangcun--db-update-file-in-yiyu file yiyu t)
          result)
      (when (and temporary-buffer-p (buffer-live-p buffer))
        (with-current-buffer buffer
          (when (buffer-modified-p)
            (set-buffer-modified-p nil)))
        (kill-buffer buffer)))))

(defun fangcun-mcp--heading-at-path (heading-path)
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

(defun fangcun-mcp--create-heading-node (arguments)
  "Create and index a Fangcun heading node from MCP ARGUMENTS."
  (let* ((yiyu-id
          (fangcun-mcp--required-string arguments :yiyu))
         (relative-file
          (fangcun-mcp--required-string arguments :file))
         (heading-path
          (fangcun-mcp--required-string-list arguments :headingPath))
         (yiyus (fangcun--ensure-session))
         (yiyu (fangcun-mcp--yiyu-by-id yiyu-id yiyus))
         (file (fangcun-mcp--existing-org-file yiyu relative-file))
         (id
          (fangcun-mcp--call-with-org-file-edit
           file yiyu
           (lambda ()
             (goto-char
              (fangcun-mcp--heading-at-path heading-path))
             (fangcun--node-id-get-create)))))
    (fangcun-mcp--node-object
     (or (fangcun-node-from-id id)
         (user-error "Created Fangcun node was not indexed: %s" id)))))

(defun fangcun-mcp--link-anchor (arguments)
  "Return the validated link anchor described by ARGUMENTS."
  (let ((before-p (plist-member arguments :beforeText))
        (after-p (plist-member arguments :afterText)))
    (when (eq (not before-p) (not after-p))
      (user-error "Specify exactly one of :beforeText and :afterText"))
    (let* ((relation (if before-p "before" "after"))
           (property (if before-p :beforeText :afterText))
           (text (fangcun-mcp--required-string arguments property))
           (occurrence
            (when (plist-member arguments :occurrence)
              (fangcun-mcp--integer-at-least
               arguments :occurrence 1))))
      (list :relation relation
            :text text
            :occurrence occurrence))))

(defun fangcun-mcp--goto-link-anchor (node anchor)
  "Move to the insertion point for ANCHOR in NODE and describe it."
  (pcase-let* ((`(,beginning . ,end)
                (fangcun-mcp--node-region-in-buffer node))
               (relation (plist-get anchor :relation))
               (text (plist-get anchor :text))
               (requested-occurrence
                (plist-get anchor :occurrence))
               (matches nil))
    (save-restriction
      (narrow-to-region beginning end)
      (goto-char (point-min))
      (while (search-forward text nil t)
        (push (cons (match-beginning 0) (match-end 0)) matches))
      (setq matches (nreverse matches))
      (unless matches
        (user-error "Link anchor does not exist in the source node: %S"
                    text))
      (when (and (null requested-occurrence) (cdr matches))
        (user-error
         (concat
          "Link anchor occurs %d times in the source node; "
          "specify :occurrence")
         (length matches)))
      (let* ((occurrence (or requested-occurrence 1))
             (match (nth (1- occurrence) matches)))
        (unless match
          (user-error
           "Link anchor has only %d occurrences, not %d"
           (length matches) occurrence))
        (goto-char
         (if (equal relation "before")
             (car match)
           (cdr match)))
        (list
         :line (line-number-at-pos (point) t)
         :column (- (point) (line-beginning-position))
         :relation relation
         :occurrence occurrence)))))

(defun fangcun-mcp--line-preview (position)
  "Return a bounded changed-line preview around POSITION."
  (save-excursion
    (goto-char position)
    (let* ((beginning (line-beginning-position))
           (end (line-end-position))
           (maximum fangcun-mcp--maximum-line-preview-characters)
           (available (- end beginning))
           (half (/ maximum 2))
           (window-beginning
            (max beginning
                 (min (- position half)
                      (- end maximum))))
           (window-end (min end (+ window-beginning maximum))))
      (list
       :text
       (buffer-substring-no-properties window-beginning window-end)
       :startColumn (- window-beginning beginning)
       :truncatedBefore (if (> window-beginning beginning) t :false)
       :truncatedAfter (if (< window-end end) t :false)
       :originalCharacters available))))

(defun fangcun-mcp--insert-node-link (arguments)
  "Insert a Fangcun ID link described by MCP ARGUMENTS."
  (let* ((source-id
          (fangcun-mcp--required-string arguments :sourceId))
         (target-id
          (fangcun-mcp--required-string arguments :targetId))
         (anchor (fangcun-mcp--link-anchor arguments))
         (include-content (plist-get arguments :includeContent))
         (description
          (if (plist-member arguments :description)
              (plist-get arguments :description)
            nil))
         (source (fangcun-mcp--node-by-id source-id))
         (target (fangcun-mcp--node-by-id target-id))
         (yiyus (fangcun--ensure-session))
         (yiyu
          (fangcun-mcp--yiyu-by-id
           (fangcun-node-yiyu-id source) yiyus))
         (file
          (expand-file-name
           (fangcun-node-file source)
           (fangcun-yiyu-root yiyu))))
    (unless (memq include-content '(nil t))
      (user-error ":includeContent must be a boolean"))
    (unless (or (not (plist-member arguments :description))
                (stringp description))
      (user-error ":description must be a string"))
    (let* ((resolved-description
            (if (plist-member arguments :description)
                description
              (fangcun-node-title target)))
           (link
            (org-link-make-string
             (concat "id:" target-id)
             (unless (string-empty-p resolved-description)
               resolved-description)))
           (write-result
            (fangcun-mcp--call-with-org-file-edit
             file yiyu
             (lambda ()
               (let* ((position
                       (fangcun-mcp--goto-link-anchor
                        source anchor))
                      (insertion-point (point)))
                 (insert link)
                 (list
                  :location
                  (list
                   :absoluteFile file
                   :line (plist-get position :line)
                   :column (plist-get position :column))
                  :anchor
                  (list
                   :relation (plist-get position :relation)
                   :occurrence (plist-get position :occurrence))
                  :linePreview
                  (fangcun-mcp--line-preview insertion-point))))))
           (updated-source (fangcun-mcp--node-by-id source-id))
           (updated-target (fangcun-mcp--node-by-id target-id)))
      (append
       (list
        :source (fangcun-mcp--node-object updated-source)
        :target (fangcun-mcp--node-object updated-target)
        :link link
        :location (plist-get write-result :location)
        :anchor (plist-get write-result :anchor)
        :linePreview (plist-get write-result :linePreview)
        :saved t
        :indexed t)
       (when include-content
         (list
          :content
          (fangcun-mcp--read-node-source updated-source)))))))

(defun fangcun-mcp--create-file-node (arguments)
  "Create and index a Fangcun file node from MCP ARGUMENTS."
  (let* ((yiyu-id
          (fangcun-mcp--required-string arguments :yiyu))
         (relative-file
          (fangcun-mcp--required-string arguments :file))
         (title (or (plist-get arguments :title) ""))
         (content (or (plist-get arguments :content) "")))
    (unless (and (stringp title) (stringp content))
      (user-error ":title and :content must be strings"))
    (when (file-name-absolute-p relative-file)
      (user-error ":file must be relative to the yiyu root"))
    (let* ((yiyus (fangcun--ensure-session))
           (yiyu (fangcun-mcp--yiyu-by-id yiyu-id yiyus))
           (root (fangcun-yiyu-root yiyu))
           (file (expand-file-name relative-file root))
           (directory (file-name-directory file)))
      (when-let* ((reason
                   (fangcun--new-file-name-error file root)))
        (user-error "%s" reason))
      (make-directory directory t)
      (let ((id (org-id-new)))
        (with-temp-buffer
          (setq default-directory directory
                buffer-file-coding-system 'utf-8-unix)
          (let ((org-inhibit-startup t))
            (delay-mode-hooks (org-mode)))
          (unless (string-empty-p title)
            (insert "#+title: " title "\n"))
          (insert "\n")
          (unless (string-empty-p content)
            (insert content)
            (unless (bolp)
              (insert "\n")))
          (goto-char (point-min))
          (org-entry-put (point) "ID" id)
          (write-region (point-min) (point-max) file nil 'silent))
        (fangcun--db-update-file-in-yiyu file yiyu t)
        (fangcun-mcp--node-object
         (or (fangcun-node-from-id id)
             (user-error
              "Created Fangcun node was not indexed: %s" id)))))))

(yunge-mcp-register-tool
 "fangcun_list_yiyus"
 (concat
  "List configured 一隅（yiyu） note roots in 方寸（Fangcun）, "
  "including their identifiers, display names, and absolute root paths. "
  "These are ordinary Org files: when a root is accessible, use the "
  "client's filesystem tools for normal reads, literal full-text search, "
  "link deletion, and general edits. Fangcun watches saved external "
  "changes and updates its index automatically.")
 '(:type "object" :additionalProperties :false)
 #'fangcun-mcp--list-yiyus
 fangcun-mcp--read-only-annotations)

(yunge-mcp-register-tool
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

(yunge-mcp-register-tool
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

(yunge-mcp-register-tool
 "fangcun_read_node"
 (concat
  "Read a bounded on-disk chunk of an indexed 方寸（Fangcun） note node. "
  "Use this fallback only when the client cannot access the located file "
  "with its own filesystem tools. Continue with nextPosition while "
  "hasMore is true; every call has line and character limits.")
 '(:type "object"
   :properties
   (:id (:type "string" :description "方寸（Fangcun） node ID")
    :startLine
    (:type "integer" :minimum 1
     :description
     "Optional absolute one-based line, normally from nextPosition")
    :startColumn
    (:type "integer" :minimum 0
     :description
     "Optional zero-based column, normally from nextPosition")
    :maxLines
    (:type "integer" :minimum 1 :maximum 500 :default 60
     :description "Maximum lines returned by this call")
    :maxCharacters
    (:type "integer" :minimum 1 :maximum 50000 :default 6000
     :description "Maximum characters returned by this call"))
   :required ["id"]
   :additionalProperties :false)
 #'fangcun-mcp--read-node
 fangcun-mcp--read-only-annotations)

(yunge-mcp-register-tool
 "fangcun_list_backlinks"
 (concat
  "List unique 方寸（Fangcun） note nodes containing indexed Org links "
  "to a target node. Each result includes its occurrence count and first "
  "position. Results are returned one cursor page at a time; source-line "
  "previews can be included on request.")
 '(:type "object"
   :properties
   (:id (:type "string" :description "Target 方寸（Fangcun） node ID")
    :pageSize
    (:type "integer" :minimum 1 :maximum 100 :default 20
     :description "Maximum source nodes to return in this page")
    :cursor
    (:type "string"
     :description "Opaque nextCursor from the preceding backlink page")
    :includePreview
    (:type "boolean"
     :description
     "Include the first occurrence preview for each source node"
     :default :false))
   :required ["id"]
   :additionalProperties :false)
 #'fangcun-mcp--list-backlinks
 fangcun-mcp--read-only-annotations)

(yunge-mcp-register-tool
 "fangcun_create_file_node"
 (concat
  "Create and save a new 方寸（Fangcun） Org file with a file-level "
  "note node in a configured 一隅（yiyu）.")
 '(:type "object"
   :properties
    (:yiyu
     (:type "string" :description "Configured 一隅（yiyu） ID")
     :file
     (:type "string"
      :description "Portable .org path relative to the 一隅（yiyu） root")
     :title (:type "string" :description "Optional Org title")
    :content (:type "string" :description "Optional initial Org content"))
   :required ["yiyu" "file"]
   :additionalProperties :false)
 #'fangcun-mcp--create-file-node
 fangcun-mcp--additive-write-annotations)

(yunge-mcp-register-tool
 "fangcun_create_heading_node"
 (concat
  "Give one existing Org heading its own 方寸（Fangcun） node ID, then "
  "save and index the file. An existing local ID is retained. The exact "
  "heading path excludes TODO keywords, priorities, and tags.")
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
   :openWorldHint :false))

(yunge-mcp-register-tool
 "fangcun_insert_node_link"
 (concat
  "Insert an Org id: link from one indexed 方寸（Fangcun） node to "
  "another, then save and reindex the source file. Copy a unique exact "
  "beforeText or afterText anchor from a bounded file read; when it "
  "repeats, select its one-based occurrence. The target title is the "
  "default description; pass an empty description for a bare link. The "
  "result includes the absolute write location and a bounded line preview. "
  "Full updated content is omitted unless includeContent is true. For link "
  "deletion or general edits, use the client's filesystem tools; Fangcun "
  "watches saved changes and updates its index automatically.")
 '(:type "object"
   :properties
   (:sourceId
    (:type "string" :description "Source 方寸（Fangcun） node ID")
    :targetId
    (:type "string" :description "Target 方寸（Fangcun） node ID")
    :beforeText
    (:type "string" :minLength 1
     :description "Insert immediately before this exact source text")
    :afterText
    (:type "string" :minLength 1
     :description "Insert immediately after this exact source text")
    :occurrence
    (:type "integer" :minimum 1
     :description
     "One-based anchor occurrence; required only when the anchor repeats")
    :description
    (:type "string"
     :description
     "Optional link description; empty creates [[id:ID]]")
    :includeContent
    (:type "boolean" :default :false
     :description "Include the complete updated source-node content"))
   :required ["sourceId" "targetId"]
   :oneOf
   [(:required ["beforeText"])
    (:required ["afterText"])]
   :additionalProperties :false)
 #'fangcun-mcp--insert-node-link
 fangcun-mcp--additive-write-annotations)

(provide 'fangcun-mcp)

;;; fangcun-mcp.el ends here
