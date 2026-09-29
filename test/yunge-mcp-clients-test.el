;;; yunge-mcp-clients-test.el --- Yunge MCP client configuration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-mcp-clients)

(defun yunge-mcp-clients-test--read-json (file)
  "Read JSON FILE while preserving object, array, null, and false types."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer
     :object-type 'hash-table :array-type 'array
     :null-object :null :false-object :false)))

(ert-deftest yunge-mcp-clients-preserves-unrelated-json-values-and-file-mode ()
  (let* ((directory (make-temp-file "yunge-mcp-clients-" t))
         (file (expand-file-name "claude.json" directory))
         (program "C:/Yunge/yunge-mcp.exe"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             "{\"theme\":{},\"other\":{\"empty\":{},"
             "\"array\":[],\"none\":null,\"disabled\":false},"
             "\"mcpServers\":{\"other\":{\"command\":\"other\"}}}"))
          (let ((mode (file-modes file)))
            (cl-letf (((symbol-function 'yunge-mcp-clients--claude-config-file)
                       (lambda () file)))
              (yunge-mcp-clients-register '(claude-code) program))
            (should (= (file-modes file) mode)))
          (let* ((configuration (yunge-mcp-clients-test--read-json file))
                 (other (gethash "other" configuration))
                 (servers (gethash "mcpServers" configuration))
                 (yunge (gethash "yunge" servers)))
            (should (hash-table-p (gethash "theme" configuration)))
            (should (hash-table-p (gethash "empty" other)))
            (should (equal (gethash "array" other) []))
            (should (eq (gethash "none" other) :null))
            (should (eq (gethash "disabled" other) :false))
            (should (equal (gethash "command" (gethash "other" servers))
                           "other"))
            (should (equal (gethash "command" yunge) program))
            (should (equal (gethash "args" yunge) []))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-clients-rejects-invalid-json-without-changing-the-file ()
  (let* ((directory (make-temp-file "yunge-mcp-invalid-" t))
         (file (expand-file-name "claude.json" directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'yunge-mcp-clients--claude-config-file)
                   (lambda () file)))
          (dolist (contents '("" "null" "[]" "{\"mcpServers\":null}" "{"))
            (with-temp-file file (insert contents))
            (should-error
             (yunge-mcp-clients-register '(claude-code) "C:/Yunge/mcp.exe")
             :type 'user-error)
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) contents)))
            (should
             (equal
              (directory-files directory nil directory-files-no-dot-files-regexp)
              '("claude.json")))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-clients-keeps-the-original-when-replacement-fails ()
  (let* ((directory (make-temp-file "yunge-mcp-rename-" t))
         (file (expand-file-name "claude.json" directory))
         (contents "{\"theme\":\"dark\"}\n"))
    (unwind-protect
        (progn
          (with-temp-file file (insert contents))
          (cl-letf (((symbol-function 'yunge-mcp-clients--claude-config-file)
                     (lambda () file))
                    ((symbol-function 'rename-file)
                     (lambda (&rest _arguments) (error "Rename failed"))))
            (should-error
             (yunge-mcp-clients-register '(claude-code) "C:/Yunge/mcp.exe")))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) contents)))
          (should
           (equal
            (directory-files directory nil directory-files-no-dot-files-regexp)
            '("claude.json"))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-clients-preserves-a-symlinked-config ()
  (let* ((directory (make-temp-file "yunge-mcp-link-" t))
         (target (expand-file-name "target.json" directory))
         (link (expand-file-name "claude.json" directory)))
    (unwind-protect
        (progn
          (with-temp-file target (insert "{\"theme\":\"dark\"}"))
          (condition-case nil
              (make-symbolic-link target link)
            (file-error (ert-skip "Symbolic links are unavailable")))
          (cl-letf (((symbol-function 'yunge-mcp-clients--claude-config-file)
                     (lambda () link)))
            (yunge-mcp-clients-register '(claude-code) "C:/Yunge/mcp.exe"))
          (should (file-symlink-p link))
          (let ((configuration (yunge-mcp-clients-test--read-json target)))
            (should (equal (gethash "theme" configuration) "dark"))
            (should
             (equal
              (gethash "command"
                       (gethash "yunge" (gethash "mcpServers" configuration)))
              "C:/Yunge/mcp.exe"))))
      (delete-directory directory t))))

(defun yunge-mcp-clients-test--debug-helper ()
  "Return the locally built MCP helper, or skip with its build command."
  (let ((program
         (expand-file-name
          (concat "native/yunge-mcp/target/debug/yunge-mcp"
                  (when (eq system-type 'windows-nt) ".exe"))
          yunge-test-root)))
    (unless (file-executable-p program)
      (ert-skip
       "Build the debug helper with cargo test --manifest-path native/yunge-mcp/Cargo.toml"))
    program))

(ert-deftest yunge-mcp-clients-edits-codex-through-the-built-helper ()
  (let* ((program (yunge-mcp-clients-test--debug-helper))
         (directory (make-temp-file "yunge-mcp-codex-" t))
         (file (expand-file-name "config.toml" directory)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             "model = \"gpt\"\n\n"
             "[mcp_servers.yunge]\ncommand = \"old\"\n\n"
             "[mcp_servers.other] # keep\ncommand = \"other\"\n"))
          (cl-letf (((symbol-function 'yunge-mcp-clients--codex-config-file)
                     (lambda () file)))
            (yunge-mcp-clients-register '(codex) program))
          (with-temp-buffer
            (insert-file-contents file)
            (let ((contents (buffer-string)))
              (should (string-match-p "model = \"gpt\"" contents))
              (should (string-match-p "command = \"other\"" contents))
              (should (string-match-p (regexp-quote program) contents))
              (should-not (string-match-p "command = \"old\"" contents)))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-clients-preserves-codex-config-on-errors ()
  (let* ((program (yunge-mcp-clients-test--debug-helper))
         (directory (make-temp-file "yunge-mcp-codex-invalid-" t))
         (file (expand-file-name "config.toml" directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'yunge-mcp-clients--codex-config-file)
                   (lambda () file)))
          (dolist (contents '("mcp_servers = 1\n"
                              "secret = \"do-not-print-this\"\n[broken\n"))
            (with-temp-file file (insert contents))
            (let ((error-data
                   (should-error
                    (yunge-mcp-clients-register '(codex) program)
                    :type 'user-error)))
              (should (string-match-p
                       (regexp-quote file) (error-message-string error-data)))
              (should-not (string-match-p
                           "do-not-print-this" (error-message-string error-data))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) contents))))
          (let ((contents "model = \"keep\"\n"))
            (with-temp-file file (insert contents))
            (cl-letf (((symbol-function 'call-process-region)
                       (lambda (&rest _arguments)
                         (insert "{\"error\":\"unsupported\"}")
                         0)))
              (let ((error-data
                     (should-error
                      (yunge-mcp-clients-register '(codex) program)
                      :type 'user-error)))
                (should (string-match-p
                         "yunge-mcp-install" (error-message-string error-data)))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string) contents)))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-clients-reads-targets-without-case-sensitivity ()
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest _arguments) '("codex" "CLAUDE CODE"))))
    (should (equal (yunge-mcp-clients-read) '(codex claude-code))))
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest _arguments) '("none"))))
    (should-not (yunge-mcp-clients-read)))
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest _arguments) '("Codex" "none"))))
    (should-error (yunge-mcp-clients-read) :type 'user-error)))

(provide 'yunge-mcp-clients-test)

;;; yunge-mcp-clients-test.el ends here
