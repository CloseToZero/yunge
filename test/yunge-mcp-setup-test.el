;;; yunge-mcp-setup-test.el --- Yunge MCP setup behavior -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-mcp-setup)

(ert-deftest yunge-mcp-registers-selected-clients-without-installed-programs ()
  (let* ((directory (make-temp-file "yunge-mcp-clients-" t))
         (yunge-var-directory (file-name-as-directory directory))
         (files
          (mapcar
           (lambda (client)
             (cons client
                   (expand-file-name
                    (concat (symbol-name client)
                            (if (eq client 'codex) ".toml" ".json"))
                    directory)))
           '(codex claude-code gemini cursor vscode)))
         (program (yunge-mcp-program)))
    (unwind-protect
        (cl-letf (((symbol-function 'yunge-mcp-clients--codex-config-file)
                   (lambda () (alist-get 'codex files)))
                  ((symbol-function 'yunge-mcp-clients--claude-config-file)
                   (lambda () (alist-get 'claude-code files)))
                  ((symbol-function 'yunge-mcp-clients--gemini-config-file)
                   (lambda () (alist-get 'gemini files)))
                  ((symbol-function 'yunge-mcp-clients--cursor-config-file)
                   (lambda () (alist-get 'cursor files)))
                  ((symbol-function 'yunge-mcp-clients--vscode-config-file)
                   (lambda () (alist-get 'vscode files)))
                  ((symbol-function 'executable-find)
                   (lambda (_program) nil)))
          (yunge-mcp-register-clients
           '(codex claude-code gemini cursor vscode))
          (dolist (entry files)
            (with-temp-buffer
              (insert-file-contents (cdr entry))
              (if (eq (car entry) 'codex)
                  (should
                   (search-forward (concat "command = \"" program "\"") nil t))
                (let* ((configuration
                        (json-parse-string
                         (buffer-string) :object-type 'hash-table))
                       (key (if (eq (car entry) 'vscode) "servers" "mcpServers"))
                       (server (gethash "yunge" (gethash key configuration))))
                  (should (equal (gethash "command" server) program))
                  (should (equal (gethash "args" server) [])))))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-runtime-records-the-running-emacs-connection ()
  (require 'yunge-server)
  (let ((directory (make-temp-file "yunge-mcp-runtime-" t)))
    (unwind-protect
        (let ((yunge-var-directory (file-name-as-directory directory)))
          (cl-letf (((symbol-function 'yunge-server-start) #'ignore)
                    ((symbol-function 'yunge-mcp--emacsclient-program)
                     (lambda () "C:/Emacs/emacsclient.exe"))
                    ((symbol-function 'yunge-mcp--connection-arguments)
                     (lambda () '("--server-file" "C:/state/server"))))
            (yunge-mcp--write-runtime))
          (let* ((runtime-file (yunge-mcp--runtime-file))
                 (runtime
                  (with-temp-buffer
                    (insert-file-contents runtime-file)
                    (json-parse-buffer
                     :object-type 'hash-table :array-type 'array))))
            (should (= (gethash "version" runtime) 1))
            (should (equal (gethash "emacsclient" runtime)
                           "C:/Emacs/emacsclient.exe"))
            (should (equal (gethash "connectionArguments" runtime)
                           ["--server-file" "C:/state/server"]))))
      (delete-directory directory t))))

(provide 'yunge-mcp-setup-test)

;;; yunge-mcp-setup-test.el ends here
