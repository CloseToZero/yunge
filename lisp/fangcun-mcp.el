;;; fangcun-mcp.el --- Fangcun tools for Yunge MCP -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'fangcun)
(require 'seq)
(require 'subr-x)
(require 'yunge-mcp)

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

(defun fangcun-mcp--node-search-text (node)
  "Return the searchable text belonging to NODE."
  (string-join
   (append
    (list
     (fangcun-node-title node)
     (fangcun-node-yiyu-id node)
     (fangcun-node-yiyu-name node)
     (fangcun-node-file node))
    (fangcun-node-aliases node)
    (fangcun-node-tags node))
   "\n"))

(defun fangcun-mcp--list-yiyus (_arguments)
  "Return the configured Fangcun yiyus."
  (vconcat
   (mapcar
    (lambda (yiyu)
      (list
       :id (fangcun-yiyu-id yiyu)
       :name (fangcun-yiyu-name yiyu)))
    (fangcun--configured-yiyus))))

(defun fangcun-mcp--search-nodes (arguments)
  "Search Fangcun nodes described by MCP ARGUMENTS."
  (fangcun--ensure-session)
  (let* ((query (fangcun-mcp--required-string arguments :query))
         (terms (split-string query nil t))
         (limit (or (plist-get arguments :limit) 20)))
    (unless (and (integerp limit) (<= 1 limit 100))
      (user-error ":limit must be an integer between 1 and 100"))
    (vconcat
     (mapcar
      #'fangcun-mcp--node-object
      (seq-take
       (seq-filter
        (lambda (node)
          (let ((case-fold-search t)
                (text (fangcun-mcp--node-search-text node)))
            (seq-every-p
             (lambda (term)
               (string-match-p (regexp-quote term) text))
             terms)))
        (fangcun-node-list))
       limit)))))

(defun fangcun-mcp--node-by-id (id)
  "Return the indexed Fangcun node named ID."
  (fangcun--ensure-session)
  (or (fangcun-node-from-id id)
      (user-error "Fangcun node is not indexed: %s" id)))

(defun fangcun-mcp--node-source-in-buffer (node)
  "Return NODE's Org source from the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (org-find-entry-with-id (fangcun-node-id node))
        (user-error "Fangcun node ID no longer exists: %s"
                    (fangcun-node-id node)))
      (org-back-to-heading-or-point-min t)
      (let ((beginning
             (if (= (org-outline-level) 0)
                 (point-min)
               (line-beginning-position)))
            (end
             (if (= (org-outline-level) 0)
                 (point-max)
               (save-excursion
                 (org-end-of-subtree t t)))))
        (buffer-substring-no-properties beginning end)))))

(defun fangcun-mcp--read-node-source (node)
  "Return NODE's Org source, reusing a visiting buffer when possible."
  (let ((file
         (expand-file-name
          (fangcun-node-file node)
          (fangcun-node-yiyu-root node))))
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

(defun fangcun-mcp--read-node (arguments)
  "Read a Fangcun node described by MCP ARGUMENTS."
  (let* ((id (fangcun-mcp--required-string arguments :id))
         (node (fangcun-mcp--node-by-id id)))
    (list
     :node (fangcun-mcp--node-object node)
     :content (fangcun-mcp--read-node-source node))))

(defun fangcun-mcp--list-backlinks (arguments)
  "List backlinks described by MCP ARGUMENTS."
  (let* ((id (fangcun-mcp--required-string arguments :id))
         (target (fangcun-mcp--node-by-id id)))
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
          :position (fangcun-backlink-position backlink)))
       (fangcun-backlink-occurrence-list id))))))

(defun fangcun-mcp--yiyu-by-id (id yiyus)
  "Return the member of YIYUS named ID."
  (or (seq-find
       (lambda (yiyu)
         (equal (fangcun-yiyu-id yiyu) id))
       yiyus)
      (user-error "Unknown Fangcun yiyu: %s" id)))

(defun fangcun-mcp--create-file-node (arguments)
  "Create and index a Fangcun file node from MCP ARGUMENTS."
  (let* ((yiyu-id
          (fangcun-mcp--required-string arguments :yiyu))
         (file-name
          (fangcun-mcp--required-string arguments :file))
         (title (or (plist-get arguments :title) ""))
         (content (or (plist-get arguments :content) ""))
         (relative-directory
          (or (plist-get arguments :directory) ".")))
    (unless (and (stringp title) (stringp content)
                 (stringp relative-directory))
      (user-error ":title, :content, and :directory must be strings"))
    (when (file-name-absolute-p relative-directory)
      (user-error ":directory must be relative to the yiyu root"))
    (let* ((yiyus (fangcun--ensure-session))
           (yiyu (fangcun-mcp--yiyu-by-id yiyu-id yiyus))
           (root (fangcun-yiyu-root yiyu))
           (directory
            (file-name-as-directory
             (expand-file-name relative-directory root)))
           (file (expand-file-name file-name directory)))
      (unless (file-in-directory-p directory root)
        (user-error "Fangcun directory is outside yiyu %s: %s"
                    yiyu-id relative-directory))
      (unless (equal file-name (file-name-nondirectory file-name))
        (user-error ":file must be a file name without a directory"))
      (when-let* ((reason
                   (fangcun--new-file-name-error file-name directory)))
        (user-error "%s" reason))
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
  "including their identifiers and display names.")
 '(:type "object" :additionalProperties :false)
 #'fangcun-mcp--list-yiyus
 fangcun-mcp--read-only-annotations)

(yunge-mcp-register-tool
 "fangcun_search_nodes"
 (concat
  "Search indexed 方寸（Fangcun） Org note nodes by title, alias, tag, "
  "一隅（yiyu）, or relative file.")
 '(:type "object"
   :properties
   (:query (:type "string" :description "Whitespace-separated search terms")
    :limit (:type "integer" :minimum 1 :maximum 100 :default 20))
   :required ["query"]
   :additionalProperties :false)
 #'fangcun-mcp--search-nodes
 fangcun-mcp--read-only-annotations)

(yunge-mcp-register-tool
 "fangcun_read_node"
 "Read the Org source and metadata of an indexed 方寸（Fangcun） note node."
 '(:type "object"
   :properties
   (:id (:type "string" :description "方寸（Fangcun） node ID"))
   :required ["id"]
   :additionalProperties :false)
 #'fangcun-mcp--read-node
 fangcun-mcp--read-only-annotations)

(yunge-mcp-register-tool
 "fangcun_list_backlinks"
 "List every indexed Org link that points to a 方寸（Fangcun） note node."
 '(:type "object"
   :properties
   (:id (:type "string" :description "Target 方寸（Fangcun） node ID"))
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
    :directory
    (:type "string"
     :description "Existing directory relative to the 一隅（yiyu） root"
     :default ".")
    :file (:type "string" :description "Portable .org file name")
    :title (:type "string" :description "Optional Org title")
    :content (:type "string" :description "Optional initial Org content"))
   :required ["yiyu" "file"]
   :additionalProperties :false)
 #'fangcun-mcp--create-file-node
 '(:readOnlyHint :false
   :destructiveHint :false
   :idempotentHint :false
   :openWorldHint :false))

(provide 'fangcun-mcp)

;;; fangcun-mcp.el ends here
