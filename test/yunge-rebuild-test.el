;;; yunge-rebuild-test.el --- Local rebuild behavior -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-rebuild)
(require 'fangcun)
(require 'yunge-mcp-setup)
(require 'yunge-reader-setup)

(ert-deftest yunge-rebuild-continues-after-a-failed-step-and-reports-it ()
  (let* ((directory (make-temp-file "yunge-rebuild-" t))
         (yunge-var-directory (file-name-as-directory directory))
         (yunge-rebuild--running-p nil)
         (yunge-rebuild--step 0)
         (yunge-rebuild--results nil)
         (yunge-rebuild--initial-choice nil)
         pending)
    (unwind-protect
        (progn
          (yunge-mcp-clients-save-choice nil)
          (cl-letf (((symbol-function 'yunge-autoload-generate)
                     #'ignore)
                    ((symbol-function 'fangcun-native-build)
                     (lambda (complete) (setq pending complete)))
                    ((symbol-function 'yunge-mcp-install)
                     (lambda (complete) (funcall complete nil)))
                    ((symbol-function 'yunge-reader-setup)
                     (lambda (complete) (funcall complete nil)))
                    ((symbol-function 'run-at-time)
                     (lambda (_time _repeat function &rest arguments)
                       (apply function arguments)))
                    ((symbol-function 'display-buffer) #'ignore))
            (yunge-rebuild)
            (should-error (yunge-rebuild) :type 'user-error)
            (funcall pending '(error "compile failed")))
          (with-current-buffer yunge-rebuild--buffer-name
            (goto-char (point-min))
            (should (search-forward "Autoloads: success" nil t))
            (should (search-forward "Fangcun helper: compile failed" nil t))
            (should (search-forward "MCP helper: success" nil t))
            (should (search-forward "Reader and PDFium: success" nil t))
            (should (search-forward "3 succeeded, 1 failed" nil t))))
      (when-let* ((buffer (get-buffer yunge-rebuild--buffer-name)))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest yunge-rebuild-cancelled-client-choice-starts-nothing ()
  (let* ((directory (make-temp-file "yunge-rebuild-choice-" t))
         (yunge-var-directory (file-name-as-directory directory))
         (yunge-rebuild--running-p nil)
         started)
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (&rest _arguments) (signal 'quit nil)))
                  ((symbol-function 'yunge-autoload-generate)
                   (lambda () (setq started t))))
          (should (condition-case nil (yunge-rebuild) (quit t)))
          (should-not started)
          (should (eq (yunge-mcp-clients-choice) :unselected)))
      (delete-directory directory t))))

(provide 'yunge-rebuild-test)

;;; yunge-rebuild-test.el ends here
