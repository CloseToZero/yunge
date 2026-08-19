;;; yunge-mcp.el --- MCP tool dispatch for Yunge -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'yunge-state)

(cl-defstruct yunge-mcp-tool
  name
  description
  input-schema
  function
  annotations)

(defvar yunge-mcp--tools (make-hash-table :test #'equal)
  "Tools available through the Yunge MCP server.")

(defvar server-eval-args-left)

(defconst yunge-mcp--native-source-hash-file
  (expand-file-name
   "native/yunge-mcp/source.sha256" yunge-config-directory)
  "File containing the expected Yunge MCP helper build ID.")

(defun yunge-mcp--helper-build-id ()
  "Return the expected Yunge MCP helper build ID, or nil."
  (when (file-readable-p yunge-mcp--native-source-hash-file)
    (with-temp-buffer
      (insert-file-contents yunge-mcp--native-source-hash-file)
      (let ((build-id (string-trim (buffer-string))))
        (unless (string-empty-p build-id)
          build-id)))))

(defun yunge-mcp--server-request-argument ()
  "Consume and validate one helper request from client arguments."
  (let ((actual-build-id (pop server-eval-args-left))
        (request-argument (pop server-eval-args-left))
        (expected-build-id (yunge-mcp--helper-build-id)))
    (unless expected-build-id
      (user-error
       "Yunge MCP source build ID is unavailable; run M-x yunge-mcp-install"))
    (unless (and request-argument
                 (equal actual-build-id expected-build-id))
      (user-error
       "Yunge MCP helper is outdated; run M-x yunge-mcp-install"))
    request-argument))

(defun yunge-mcp-register-tool
    (name description input-schema function &optional annotations)
  "Register an MCP tool named NAME.
DESCRIPTION and INPUT-SCHEMA describe the tool to MCP clients.  FUNCTION
receives its arguments as a plist.  ANNOTATIONS contains MCP tool hints."
  (puthash
   name
   (make-yunge-mcp-tool
    :name name
    :description description
    :input-schema input-schema
    :function function
    :annotations annotations)
   yunge-mcp--tools))

(defun yunge-mcp--load-tools ()
  "Load the tools currently exposed by Yunge."
  (require 'fangcun-mcp))

(defun yunge-mcp--tool-description (tool)
  "Return the MCP description object for TOOL."
  (let ((description
         (list
          :name (yunge-mcp-tool-name tool)
          :description (yunge-mcp-tool-description tool)
          :inputSchema (yunge-mcp-tool-input-schema tool))))
    (when-let* ((annotations (yunge-mcp-tool-annotations tool)))
      (setq description
            (append description (list :annotations annotations))))
    description))

(defun yunge-mcp--tool-list ()
  "Return registered MCP tool descriptions in name order."
  (yunge-mcp--load-tools)
  (let (tools)
    (maphash
     (lambda (_name tool)
       (push (yunge-mcp--tool-description tool) tools))
     yunge-mcp--tools)
    (vconcat
     (sort tools
           (lambda (left right)
             (string-lessp
              (plist-get left :name)
              (plist-get right :name)))))))

(defun yunge-mcp--call-tool (name arguments)
  "Call the registered tool NAME with ARGUMENTS."
  (yunge-mcp--load-tools)
  (if-let* ((tool (gethash name yunge-mcp--tools)))
      (funcall (yunge-mcp-tool-function tool) arguments)
    (user-error "Unknown Yunge MCP tool: %s" name)))

(defun yunge-mcp--handle-request (request)
  "Return the response object for decoded MCP bridge REQUEST."
  (condition-case error-data
      (let ((operation (plist-get request :operation)))
        (list
         :ok t
         :value
         (pcase operation
           ("list-tools" (yunge-mcp--tool-list))
           ("call-tool"
            (yunge-mcp--call-tool
             (plist-get request :name)
             (or (plist-get request :arguments) nil)))
           (_ (user-error "Unknown Yunge MCP operation: %S"
                          operation)))))
    (error
     (list
      :ok :false
      :error
      (list
       :type (symbol-name (car error-data))
       :message (error-message-string error-data))))))

(defun yunge-mcp-dispatch (request-json)
  "Dispatch REQUEST-JSON and return a base64-encoded JSON response."
  (let* ((request
          (json-parse-string
           request-json
           :object-type 'plist
           :array-type 'list
           :null-object nil
           :false-object nil))
         (response (yunge-mcp--handle-request request)))
    (base64-encode-string
     (encode-coding-string (json-serialize response) 'utf-8)
     t)))

(defun yunge-mcp-server-dispatch ()
  "Validate the native helper and dispatch its Base64 request."
  (yunge-mcp-dispatch
   (decode-coding-string
    (base64-decode-string (yunge-mcp--server-request-argument))
    'utf-8)))

(provide 'yunge-mcp)

;;; yunge-mcp.el ends here
