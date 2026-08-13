;;; yunge-test-test.el --- Test command tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-test)

(ert-deftest yunge-test-separates-source-and-isolated-state-roots ()
  (let* ((root (make-temp-file "yunge-test-command-" t))
         (yunge-config-directory
          (file-name-as-directory (expand-file-name "source" root)))
         (yunge-var-directory
          (file-name-as-directory (expand-file-name "state" root)))
         (buffer (get-buffer-create "*yunge-test*"))
         command
         xdg-config-home)
    (unwind-protect
        (progn
          (make-directory yunge-config-directory t)
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest arguments)
                       (setq command (plist-get arguments :command)
                             xdg-config-home (getenv "XDG_CONFIG_HOME"))
                       'yunge-test-process))
                    ((symbol-function 'display-buffer) #'ignore))
            (yunge-test))
          (should
           (equal
            command
            (list
             (expand-file-name invocation-name invocation-directory)
             "--batch" "-Q"
             "-L" (expand-file-name "test/" yunge-config-directory)
             "-l" "yunge-test-runner")))
          (should
           (equal
            xdg-config-home
            (yunge-var-subdirectory "test")))
          (with-current-buffer buffer
            (should (equal default-directory yunge-config-directory))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(provide 'yunge-test-test)

;;; yunge-test-test.el ends here
