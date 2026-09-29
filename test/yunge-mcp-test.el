;;; yunge-mcp-test.el --- Yunge MCP tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-mcp)

(defvar server-eval-args-left)

(defun yunge-mcp-test--decode (encoded)
  "Decode an ENCODED Yunge MCP bridge response."
  (json-parse-string
   (decode-coding-string (base64-decode-string encoded) 'utf-8)
   :object-type 'plist
   :array-type 'list
   :null-object nil
   :false-object nil))

(ert-deftest yunge-mcp-lists-registered-tools-in-name-order ()
  (let ((yunge-mcp--tools (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'yunge-mcp--load-tools) #'ignore))
      (yunge-mcp-register-tool
       "zeta" "Last" '(:type "object") #'ignore)
      (yunge-mcp-register-tool
       "alpha" "First" '(:type "object") #'ignore
       '(:readOnlyHint t))
      (let* ((response
              (yunge-mcp-test--decode
               (yunge-mcp-dispatch
                "{\"operation\":\"list-tools\"}")))
             (tools (plist-get response :value)))
        (should (plist-get response :ok))
        (should
         (equal (mapcar (lambda (tool) (plist-get tool :name)) tools)
                '("alpha" "zeta")))
        (should
         (eq (plist-get (plist-get (car tools) :annotations)
                        :readOnlyHint)
             t))))))

(ert-deftest yunge-mcp-lists-fangcun-tools-with-schemas-and-hints ()
  (let* ((response
          (yunge-mcp-test--decode
           (yunge-mcp-dispatch "{\"operation\":\"list-tools\"}")))
         (tools (plist-get response :value))
         (search
          (seq-find (lambda (tool)
                      (equal (plist-get tool :name) "fangcun_search_nodes"))
                    tools))
         (backlinks
          (seq-find (lambda (tool)
                      (equal (plist-get tool :name) "fangcun_list_backlinks"))
                    tools))
         (file-create
          (seq-find (lambda (tool)
                      (equal (plist-get tool :name) "fangcun_create_file_node"))
                    tools))
         (heading-create
          (seq-find (lambda (tool)
                      (equal (plist-get tool :name) "fangcun_create_heading_node"))
                    tools)))
    (should (eq (plist-get response :ok) t))
    (should
     (equal (mapcar (lambda (tool) (plist-get tool :name)) tools)
            '("fangcun_create_file_node"
              "fangcun_create_heading_node"
              "fangcun_list_backlinks"
              "fangcun_list_yiyus"
              "fangcun_locate_node"
              "fangcun_search_nodes")))
    (dolist (tool tools)
      (should (not (string-empty-p (plist-get tool :description)))))
    (dolist (tool (list search backlinks))
      (let ((properties
             (plist-get (plist-get tool :inputSchema) :properties)))
        (should (plist-member properties :pageSize))
        (should (plist-member properties :cursor))))
    (let ((file-properties
           (plist-get (plist-get file-create :inputSchema) :properties)))
      (should (plist-member file-properties :title))
      (should-not (plist-member file-properties :content)))
    (dolist (tool (list file-create heading-create))
      (let ((hints (plist-get tool :annotations)))
        (should (plist-member hints :readOnlyHint))
        (should (plist-member hints :destructiveHint))
        (should-not (plist-get hints :readOnlyHint))
        (should-not (plist-get hints :destructiveHint))))
    (should (plist-member (plist-get file-create :annotations) :idempotentHint))
    (should (plist-member (plist-get heading-create :annotations) :idempotentHint))
    (should-not (plist-get (plist-get file-create :annotations) :idempotentHint))
    (should (plist-get (plist-get heading-create :annotations) :idempotentHint))))

(ert-deftest yunge-mcp-dispatches-tool-arguments ()
  (let ((yunge-mcp--tools (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'yunge-mcp--load-tools) #'ignore))
      (yunge-mcp-register-tool
       "echo" "Echo" '(:type "object")
       (lambda (arguments)
         (list :value (plist-get arguments :value))))
      (let ((response
             (yunge-mcp-test--decode
              (yunge-mcp-dispatch
               (concat
                "{\"operation\":\"call-tool\","
                "\"name\":\"echo\","
                "\"arguments\":{\"value\":\"hello\"}}")))))
        (should (plist-get response :ok))
        (should
         (equal (plist-get (plist-get response :value) :value)
                "hello"))))))

(ert-deftest yunge-mcp-returns-tool-errors-as-data ()
  (let ((yunge-mcp--tools (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'yunge-mcp--load-tools) #'ignore))
      (let ((response
             (yunge-mcp-test--decode
              (yunge-mcp-dispatch
               (concat
                "{\"operation\":\"call-tool\","
                "\"name\":\"missing\",\"arguments\":{}}")))))
        (should-not (plist-get response :ok))
        (should
         (string-match-p
          "Unknown Yunge MCP tool"
          (plist-get (plist-get response :error) :message)))))))

(ert-deftest yunge-mcp-server-dispatch-consumes-one-client-argument ()
  (let* ((request
          (encode-coding-string
           (concat
            "{\"operation\":\"call-tool\","
            "\"name\":\"echo\","
            "\"arguments\":{\"value\":\"中文检索\"}}")
           'utf-8))
         (server-eval-args-left
          (list "test-build"
                (base64-encode-string request t)
                "untouched"))
        (yunge-mcp--tools (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'yunge-mcp--load-tools) #'ignore)
              ((symbol-function 'yunge-mcp--helper-build-id)
               (lambda () "test-build")))
      (yunge-mcp-register-tool
       "echo" "Echo" '(:type "object")
       (lambda (arguments)
         (list :value (plist-get arguments :value))))
      (let ((response
             (yunge-mcp-test--decode
              (yunge-mcp-server-dispatch))))
        (should
         (equal (plist-get (plist-get response :value) :value)
                "中文检索")))
      (should (equal server-eval-args-left '("untouched"))))))

(ert-deftest yunge-mcp-server-dispatch-rejects-an-outdated-helper ()
  (let ((server-eval-args-left
         '("eyJvcGVyYXRpb24iOiJsaXN0LXRvb2xzIn0=")))
    (cl-letf (((symbol-function 'yunge-mcp--helper-build-id)
               (lambda () "test-build")))
      (let ((error-data
             (should-error (yunge-mcp-server-dispatch)
                           :type 'user-error)))
        (should
         (string-match-p
          (regexp-quote "M-x yunge-mcp-install")
          (error-message-string error-data)))))))

(ert-deftest yunge-mcp-server-dispatch-rejects-a-mismatched-build ()
  (let ((server-eval-args-left
         '("old-build" "eyJvcGVyYXRpb24iOiJsaXN0LXRvb2xzIn0=")))
    (cl-letf (((symbol-function 'yunge-mcp--helper-build-id)
               (lambda () "test-build")))
      (should-error (yunge-mcp-server-dispatch) :type 'user-error))))

(provide 'yunge-mcp-test)

;;; yunge-mcp-test.el ends here
