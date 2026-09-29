;;; yunge-mcp-setup-test.el --- Yunge MCP setup behavior -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-mcp-setup)

(ert-deftest yunge-mcp-registers-json-clients-without-installed-programs ()
  (let* ((directory (make-temp-file "yunge-mcp-clients-" t))
         (yunge-var-directory (file-name-as-directory directory))
         (files
          (mapcar
           (lambda (client)
             (cons client
                   (expand-file-name
                    (concat (symbol-name client)
                            ".json")
                    directory)))
           '(claude-code gemini cursor vscode)))
         (program (yunge-mcp-program)))
    (unwind-protect
        (cl-letf (((symbol-function 'yunge-mcp-clients--claude-config-file)
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
           '(claude-code gemini cursor vscode))
          (dolist (entry files)
            (with-temp-buffer
              (insert-file-contents (cdr entry))
              (let* ((configuration
                      (json-parse-string
                       (buffer-string) :object-type 'hash-table))
                     (key (if (eq (car entry) 'vscode) "servers" "mcpServers"))
                     (server (gethash "yunge" (gethash key configuration))))
                (should (equal (gethash "command" server) program))
                (should (equal (gethash "args" server) [])))))
          (should (equal (yunge-mcp-clients-choice)
                         '(claude-code gemini cursor vscode))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-setup-records-none-only-after-success ()
  (let* ((directory (make-temp-file "yunge-mcp-choice-" t))
         (yunge-var-directory (file-name-as-directory directory))
         pending)
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _arguments) '("None")))
                  ((symbol-function 'yunge-mcp--start-build)
                   (lambda (complete) (setq pending complete))))
          (yunge-mcp-setup)
          (should (eq (yunge-mcp-clients-choice) :unselected))
          (funcall pending '(error "build failed"))
          (should (eq (yunge-mcp-clients-choice) :unselected))
          (yunge-mcp-setup)
          (funcall pending nil)
          (should-not (yunge-mcp-clients-choice))
          (cl-letf (((symbol-function 'completing-read-multiple)
                     (lambda (&rest _arguments)
                       (ert-fail "Saved None must not prompt again"))))
            (yunge-mcp-setup)
            (funcall pending nil)
            (should-not (yunge-mcp-clients-choice))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-setup-keeps-saved-clients-and-retries-registration-failures ()
  (let* ((directory (make-temp-file "yunge-mcp-choice-files-" t))
         (yunge-var-directory (file-name-as-directory directory))
         (file (expand-file-name "client.json" directory))
         pending)
    (unwind-protect
        (cl-letf (((symbol-function 'yunge-mcp-clients--claude-config-file)
                   (lambda () file))
                  ((symbol-function 'yunge-mcp--start-build)
                   (lambda (complete) (setq pending complete))))
          (with-temp-file file (insert "{\"mcpServers\": null}"))
          (should-error (yunge-mcp-register-clients '(claude-code))
                        :type 'user-error)
          (should (eq (yunge-mcp-clients-choice) :unselected))
          (with-temp-file file (insert "{}"))
          (yunge-mcp-register-clients '(claude-code))
          (with-temp-file file (insert "{\"custom\": true}"))
          (cl-letf (((symbol-function 'completing-read-multiple)
                     (lambda (&rest _arguments)
                       (ert-fail "A saved choice must not prompt again"))))
            (yunge-mcp-setup)
            (funcall pending nil))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "{\"custom\": true}")))
          (should (equal (yunge-mcp-clients-choice) '(claude-code))))
      (delete-directory directory t))))

(ert-deftest yunge-mcp-install-completes-after-a-real-build-process ()
  (require 'yunge-server)
  (let* ((directory (make-temp-file "yunge-mcp-build-" t))
         (yunge-var-directory (file-name-as-directory directory))
         (source (expand-file-name "built.exe" directory))
         (emacs-program (expand-file-name invocation-name invocation-directory))
         (real-make-process (symbol-function 'make-process))
         (real-file-executable-p (symbol-function 'file-executable-p))
         failed completions)
    (unwind-protect
        (progn
          (with-temp-file source (insert "built helper"))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (name) (when (equal name "cargo") emacs-program)))
                    ((symbol-function 'yunge-mcp--built-program)
                     (lambda () source))
                    ((symbol-function 'file-executable-p)
                     (lambda (file)
                       (or (equal file source)
                           (funcall real-file-executable-p file))))
                    ((symbol-function 'yunge-server-start) #'ignore)
                    ((symbol-function 'yunge-mcp--emacsclient-program)
                     (lambda () emacs-program))
                    ((symbol-function 'yunge-mcp--connection-arguments)
                     (lambda () '("--socket-name" "test")))
                    ((symbol-function 'make-process)
                     (lambda (&rest options)
                       (apply real-make-process
                              (plist-put options :command
                                         (list emacs-program "--batch" "-Q"
                                               "--eval"
                                               (if failed "(kill-emacs 1)"
                                                 "(kill-emacs 0)")))))))
            (dotimes (_attempt 2)
              (yunge-mcp-install
               (lambda (failure) (push failure completions)))
              (let ((deadline (+ (float-time) 10)))
                (while (and (null completions) (< (float-time) deadline))
                  (accept-process-output nil 0.1)))
              (should completions)
              (if failed
                  (should (eq (caar completions) 'error))
                (should (equal completions '(nil)))
                (with-temp-buffer
                  (insert-file-contents (yunge-mcp-program))
                  (should (equal (buffer-string) "built helper")))
                (should (file-exists-p (yunge-mcp--runtime-file))))
              (setq failed t
                    completions nil))))
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
