;;; fangcun-mcp-test.el --- Fangcun MCP behavior -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'fangcun-test-helper)
(require 'fangcun-mcp)
(require 'yunge-mcp)

(defun fangcun-mcp-test--response (name arguments)
  "Return the decoded Yunge MCP response for Fangcun tool NAME and ARGUMENTS."
  (json-parse-string
   (decode-coding-string
    (base64-decode-string
     (yunge-mcp-dispatch
      (json-serialize
       (list :operation "call-tool" :name name :arguments arguments))))
    'utf-8)
   :object-type 'plist
   :array-type 'array
   :null-object nil
   :false-object :false))

(defun fangcun-mcp-test--value (name arguments)
  "Return successful tool NAME's value for ARGUMENTS."
  (let ((response (fangcun-mcp-test--response name arguments)))
    (should (eq (plist-get response :ok) t))
    (plist-get response :value)))

(defun fangcun-mcp-test--error (name arguments)
  "Return the user error reported by tool NAME for ARGUMENTS."
  (let ((response (fangcun-mcp-test--response name arguments)))
    (should (eq (plist-get response :ok) :false))
    (let ((error-data (plist-get response :error)))
      (should (equal (plist-get error-data :type) "user-error"))
      (should (not (string-empty-p (plist-get error-data :message))))
      error-data)))

(ert-deftest fangcun-mcp-locates-nodes-without-reading-content ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let* ((located (fangcun-mcp-test--value
                     "fangcun_locate_node" '(:id "theorem")))
           (location (plist-get located :location)))
      (should (file-equal-p
               (plist-get location :absoluteFile)
               personal-file))
      (should (equal (plist-get location :kind) "heading"))
      (should (= (plist-get location :startLine) 6))
      (should (= (plist-get location :endLine) 10))
      (should (equal (append (plist-get location :outlinePath) nil)
                     '("A theorem")))
      (should (eq (plist-get location :modifiedInEmacs) :false))
      (should-not (plist-member located :content)))))

(ert-deftest fangcun-mcp-ranks-and-pages-node-searches ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     (expand-file-name "graph.org" personal-root)
     (concat
      ":PROPERTIES:\n"
      ":ID: graph-exact\n"
      ":END:\n"
      "#+title: Graph theory\n"))
    (fangcun-test--write-file
     (expand-file-name "graph-extended.org" work-root)
     (concat
      ":PROPERTIES:\n"
      ":ID: graph-prefix\n"
      ":END:\n"
      "#+title: Graph theory extended\n"))
    (fangcun-test--write-file
     (expand-file-name "alias.org" personal-root)
     (concat
      ":PROPERTIES:\n"
      ":ID: graph-alias\n"
      ":ALIASES: \"Graph theory\"\n"
      ":END:\n"
      "#+title: Topology\n"))
    (fangcun-test--write-file
     (expand-file-name "tagged.org" work-root)
     (concat
      ":PROPERTIES:\n"
      ":ID: graph-tags\n"
      ":END:\n"
      "#+title: Networks\n"
      "#+filetags: :graph:theory:\n"))
    (fangcun-db-sync)
    (let* ((first
           (fangcun-mcp-test--value
            "fangcun_search_nodes"
            '(:query "GRAPH theory" :pageSize 1)))
           (first-items (plist-get first :nodes))
           (cursor (plist-get first :nextCursor))
           (second
            (fangcun-mcp-test--value
             "fangcun_search_nodes"
             (list
              :query "graph theory"
              :pageSize 3
              :cursor cursor)))
           (second-items (plist-get second :nodes))
           (second-ids
            (mapcar
             (lambda (item)
               (plist-get (plist-get item :node) :id))
             (append second-items nil))))
      (should (= (length first-items) 1))
      (should
       (equal
        (plist-get
         (plist-get (aref first-items 0) :node)
         :id)
        "graph-exact"))
      (should
       (equal
        (append
         (plist-get (aref first-items 0) :matchedFields)
         nil)
        '("title")))
      (should (stringp cursor))
      (should (= (length second-items) 3))
      (should
       (equal
        second-ids
        '("graph-alias" "graph-prefix" "graph-tags")))
      (should
       (equal
        (append
         (plist-get (aref second-items 0) :matchedFields)
         nil)
        '("alias")))
      (should
       (equal
        (append
         (plist-get (aref second-items 2) :matchedFields)
         nil)
        '("tag")))
      (should-not (plist-member second :nextCursor))
      (fangcun-mcp-test--error
       "fangcun_search_nodes" (list :query "graph" :cursor cursor))
      (fangcun-mcp-test--error
       "fangcun_search_nodes" '(:query "graph" :pageSize 0))
      (fangcun-mcp-test--error
       "fangcun_search_nodes" '(:query "graph" :cursor "not-a-cursor")))))

(ert-deftest fangcun-mcp-lists-configured-yiyus ()
  (fangcun-test-with-notes
    (should
     (equal
      (fangcun-mcp-test--value "fangcun_list_yiyus" nil)
      `[(:id "personal" :name "Personal" :root ,personal-root)
        (:id "work" :name "Work" :root ,work-root)]))))

(ert-deftest fangcun-mcp-groups-and-pages-backlink-sources ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: target\n"
      ":END:\n"
      "#+title: Target\n\n"
      "* Other source\n"
      ":PROPERTIES:\n"
      ":ID: other-source\n"
      ":END:\n"
      "[[id:target]].\n\n"
      "* Source\n"
      ":PROPERTIES:\n"
      ":ID: source\n"
      ":END:\n"
      "[[id:target]] and [[id:target]].\n"))
    (fangcun-db-sync)
    (let* ((result (fangcun-mcp-test--value
                    "fangcun_list_backlinks" '(:id "target" :pageSize 100)))
           (backlinks (plist-get result :backlinks))
           (source
            (seq-find
             (lambda (backlink)
               (equal
                (plist-get (plist-get backlink :source) :id)
                "source"))
             backlinks))
           (first-page
            (fangcun-mcp-test--value
             "fangcun_list_backlinks" '(:id "target" :pageSize 1)))
           (cursor (plist-get first-page :nextCursor))
           (second-page
            (fangcun-mcp-test--value
             "fangcun_list_backlinks"
             (list
              :id "target"
              :pageSize 1
              :cursor cursor)))
           (paged-backlinks
            (append
             (plist-get first-page :backlinks)
             (plist-get second-page :backlinks)
             nil)))
      (should (= (length backlinks) 2))
      (should
       (seq-every-p
        (lambda (backlink)
          (and (integerp (plist-get backlink :firstPosition))
               (integerp (plist-get backlink :occurrenceCount))))
        backlinks))
      (should (= (plist-get source :occurrenceCount) 2))
      (should-not
       (seq-some
        (lambda (backlink)
          (plist-member backlink :content))
        backlinks))
      (should (= (length paged-backlinks) 2))
      (should (stringp cursor))
      (should-not (plist-member second-page :nextCursor))
      (should
       (seq-every-p
        (lambda (backlink)
          (and (plist-member backlink :source)
               (plist-member backlink :firstPosition)))
        paged-backlinks))
      (fangcun-mcp-test--error
       "fangcun_list_backlinks" (list :id "source" :cursor cursor)))))

(ert-deftest fangcun-mcp-creates-and-indexes-file-nodes ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (cl-letf (((symbol-function 'org-id-new)
               (lambda (&optional _prefix) "mcp-created")))
      (let* ((result
              (fangcun-mcp-test--value
               "fangcun_create_file_node"
               '(:yiyu "personal"
                  :file "note/created.org"
                  :title "Created note")))
             (file (expand-file-name "note/created.org" personal-root)))
        (should (equal (plist-get result :id) "mcp-created"))
        (should (file-exists-p file))
        (should (fangcun-node-from-id "mcp-created"))
        (with-temp-buffer
          (insert-file-contents file)
          (should (search-forward "#+title: Created note" nil t)))))))

(ert-deftest fangcun-mcp-creates-and-indexes-heading-nodes ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":END:\n"
      "#+title: Notes\n\n"
      "* Parent :tag:\n"
      "** TODO Child\n"
      "Body.\n"))
    (fangcun-db-sync)
    (cl-letf (((symbol-function 'org-id-new)
               (lambda (&optional _prefix) "mcp-heading")))
      (let ((result
             (fangcun-mcp-test--value
              "fangcun_create_heading_node"
              '(:yiyu "personal"
                :file "theorems.org"
                :headingPath ["Parent" "Child"]))))
        (should (equal (plist-get result :id) "mcp-heading"))
        (should (equal (plist-get result :title) "Child"))
        (should (fangcun-node-from-id "mcp-heading"))
        (should
         (equal
          (plist-get
           (fangcun-mcp-test--value
            "fangcun_create_heading_node"
            '(:yiyu "personal"
              :file "theorems.org"
              :headingPath ["Parent" "Child"]))
           :id)
          "mcp-heading"))))
    (with-temp-buffer
      (insert-file-contents personal-file)
      (org-mode)
      (goto-char (point-min))
      (re-search-forward "^\\*\\* TODO Child$")
      (should (equal (org-entry-get (point) "ID") "mcp-heading")))))

(ert-deftest fangcun-mcp-creates-heading-after-external-file-change ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((visiting-buffer (find-file-noselect personal-file)))
      (with-temp-buffer
        (insert-file-contents personal-file)
        (goto-char (point-max))
        (insert "\n* Added outside Emacs\n")
        (write-region (point-min) (point-max) personal-file nil 'silent))
      (set-file-times personal-file
                      (time-add (current-time) (seconds-to-time 5)))
      (let* ((result
              (fangcun-mcp-test--value
               "fangcun_create_heading_node"
               '(:yiyu "personal"
                 :file "theorems.org"
                 :headingPath ["Added outside Emacs"])))
             (id (plist-get result :id)))
        (should (and (stringp id) (not (string-empty-p id))))
        (should (equal (fangcun-node-id (fangcun-node-from-id id)) id))
        (with-current-buffer visiting-buffer
          (goto-char (point-min))
          (re-search-forward "^\\* Added outside Emacs$")
          (should (equal (org-entry-get (point) "ID") id))
          (should-not (buffer-modified-p)))
        (with-temp-buffer
          (insert-file-contents personal-file)
          (org-mode)
          (goto-char (point-min))
          (re-search-forward "^\\* Added outside Emacs$")
          (should (equal (org-entry-get (point) "ID") id)))))))

(ert-deftest fangcun-mcp-rejects-ambiguous-heading-paths ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n:ID: personal-file\n:END:\n"
      "* Repeated\n* Repeated\n"))
    (fangcun-db-sync)
    (fangcun-mcp-test--error
     "fangcun_create_heading_node"
     '(:yiyu "personal"
       :file "theorems.org"
       :headingPath ["Repeated"]))))

(ert-deftest fangcun-mcp-refuses-to-save-unrelated-buffer-changes ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n:ID: personal-file\n:END:\n"
      "* Heading\n"))
    (fangcun-db-sync)
    (with-current-buffer (find-file-noselect personal-file)
      (goto-char (point-max))
      (insert "Unsaved.\n")
      (with-temp-buffer
        (insert-file-contents personal-file)
        (goto-char (point-max))
        (insert "* Added outside Emacs\n")
        (write-region (point-min) (point-max) personal-file nil 'silent))
      (set-file-times personal-file
                      (time-add (current-time) (seconds-to-time 5)))
      (fangcun-mcp-test--error
       "fangcun_create_heading_node"
       '(:yiyu "personal"
         :file "theorems.org"
         :headingPath ["Heading"]))
      (should (buffer-modified-p))
      (should (string-suffix-p "Unsaved.\n" (buffer-string))))
    (with-temp-buffer
      (insert-file-contents personal-file)
      (should-not (search-forward "Unsaved." nil t))
      (goto-char (point-min))
      (should (search-forward "Added outside Emacs" nil t)))))

(provide 'fangcun-mcp-test)

;;; fangcun-mcp-test.el ends here
