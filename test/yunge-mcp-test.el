;;; yunge-mcp-test.el --- Yunge MCP tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-mcp)
(require 'yunge-mcp-setup)

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

(ert-deftest yunge-mcp-fangcun-tools-describe-domain-names ()
  (require 'fangcun-mcp)
  (dolist (tool (append (yunge-mcp--tool-list) nil))
    (should
     (string-match-p
      "方寸（Fangcun）"
      (plist-get tool :description)))))

(ert-deftest yunge-mcp-fangcun-list-tools-describe-cursor-pages ()
  (require 'fangcun-mcp)
  (dolist (name '("fangcun_search_nodes" "fangcun_list_backlinks"))
    (let* ((tool (gethash name yunge-mcp--tools))
           (schema (yunge-mcp-tool-input-schema tool))
           (properties (plist-get schema :properties)))
      (should tool)
      (should (plist-member properties :pageSize))
      (should (plist-member properties :cursor))
      (should-not (plist-member properties :limit)))))

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

(ert-deftest yunge-mcp-registers-json-without-losing-other-settings ()
  (let ((file (make-temp-file "yunge-mcp-json-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             "{\"theme\":\"dark\",\"mcpServers\":{"
             "\"other\":{\"command\":\"other\"}}}"))
          (let ((yunge-var-directory "C:/state/"))
            (yunge-mcp--register-json file :mcpServers))
          (let* ((configuration (yunge-mcp--json-read-object file))
                 (servers (plist-get configuration :mcpServers)))
            (should (equal (plist-get configuration :theme) "dark"))
            (should
             (equal (plist-get (plist-get servers :other) :command)
                    "other"))
            (should
             (equal (plist-get (plist-get servers :yunge) :command)
                    "c:/state/yunge-mcp/bin/yunge-mcp.exe"))))
      (delete-file file))))

(ert-deftest yunge-mcp-agent-display-names-ignore-input-case ()
  (should (eq (yunge-mcp--agent-from-display-name "codex") 'codex))
  (should
   (eq (yunge-mcp--agent-from-display-name "claude code")
       'claude-code)))

(ert-deftest yunge-mcp-replaces-only-its-codex-section ()
  (let ((file (make-temp-file "yunge-mcp-codex-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             "model = \"gpt\"\n\n"
             "[mcp_servers.yunge]\ncommand = \"old\"\n\n"
             "[mcp_servers.other]\ncommand = \"other\"\n"))
          (let ((yunge-var-directory "C:/state/"))
            (cl-letf (((symbol-function 'yunge-mcp--codex-config-file)
                       (lambda () file)))
              (yunge-mcp--register-codex)))
          (with-temp-buffer
            (insert-file-contents file)
            (let ((contents (buffer-string)))
              (should (string-match-p "model = \"gpt\"" contents))
              (should (string-match-p
                       "command = \"other\"" contents))
              (should (string-match-p
                       (regexp-quote
                        "command = \"c:/state/yunge-mcp/bin/")
                       contents))
              (should-not (string-match-p "command = \"old\"" contents)))))
      (delete-file file))))

(ert-deftest yunge-mcp-registers-selected-agents-when-not-installed ()
  (let ((directory (make-temp-file "yunge-mcp-agents-" t))
        (yunge-var-directory "C:/state/"))
    (unwind-protect
        (let ((files
               (mapcar
                (lambda (name)
                  (cons name (expand-file-name
                              (concat (symbol-name name) ".json")
                              directory)))
                '(codex claude-code gemini cursor vscode))))
          (cl-letf (((symbol-function 'yunge-mcp--register-codex)
                     (lambda ()
                       (yunge-mcp--register-json
                        (alist-get 'codex files) :mcpServers)))
                    ((symbol-function 'yunge-mcp--claude-config-file)
                     (lambda () (alist-get 'claude-code files)))
                    ((symbol-function 'yunge-mcp--gemini-config-file)
                     (lambda () (alist-get 'gemini files)))
                    ((symbol-function 'yunge-mcp--cursor-config-file)
                     (lambda () (alist-get 'cursor files)))
                    ((symbol-function 'yunge-mcp--vscode-config-file)
                     (lambda () (alist-get 'vscode files))))
            ;; Agent executables deliberately do not participate in setup.
            (cl-letf (((symbol-function 'executable-find)
                       (lambda (_program) nil)))
              (yunge-mcp-register-agents
               '(codex claude-code gemini cursor vscode))))
          (dolist (entry files)
            (should (file-exists-p (cdr entry)))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-runtime-records-the-running-emacs-connection ()
  (require 'yunge-server)
  (let ((directory (make-temp-file "yunge-mcp-runtime-" t)))
    (unwind-protect
        (let ((yunge-var-directory
               (file-name-as-directory directory)))
          (cl-letf (((symbol-function 'yunge-server-start) #'ignore)
                    ((symbol-function 'yunge-mcp--emacsclient-program)
                     (lambda () "C:/Emacs/emacsclient.exe"))
                    ((symbol-function 'yunge-mcp--connection-arguments)
                     (lambda ()
                       '("--server-file" "C:/state/server"))))
            (yunge-mcp--write-runtime))
          (let ((runtime
                 (yunge-mcp--json-read-object
                  (yunge-mcp--runtime-file))))
            (should (= (plist-get runtime :version) 1))
            (should
             (equal (plist-get runtime :emacsclient)
                    "C:/Emacs/emacsclient.exe"))
            (should
             (equal (append
                     (plist-get runtime :connectionArguments) nil)
                    '("--server-file" "C:/state/server")))))
      (delete-directory directory t))))

(provide 'yunge-mcp-test)

;;; yunge-mcp-test.el ends here
