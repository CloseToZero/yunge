;;; yunge-project-test.el --- Project tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-project)

(declare-function project-root "project" (project))

(yunge-test-deftest-lazy-load yunge-project
  (project))

(ert-deftest yunge-project-keeps-submodules-separate ()
  (skip-unless (executable-find "git"))
  (require 'project)
  (let* ((workspace (make-temp-file "yunge-project-" t))
         (parent (expand-file-name "parent/" workspace))
         (origin (expand-file-name "child-origin/" workspace))
         (submodule (expand-file-name "deps/child/" parent)))
    (cl-labels
        ((git (directory &rest args)
           (with-temp-buffer
             (let ((status
                    (apply #'process-file "git" nil (current-buffer) nil
                           "-C" directory args)))
               (unless (equal status 0)
                 (ert-fail (format "git %S failed: %s" args (buffer-string))))))))
      (unwind-protect
          (progn
            (make-directory parent)
            (make-directory (expand-file-name "src/" origin) t)
            (with-temp-file (expand-file-name "src/entry.txt" origin)
              (insert "module contents\n"))
            (git origin "init" "--quiet")
            (git origin "add" "src/entry.txt")
            (git origin "-c" "user.name=Yunge Test"
                 "-c" "user.email=yunge-test@example.invalid"
                 "-c" "commit.gpgSign=false"
                 "commit" "--quiet" "-m" "Initial module")
            (git parent "init" "--quiet")
            (git parent "-c" "protocol.file.allow=always"
                 "submodule" "add" "--quiet" origin "deps/child")
            (let ((parent-project (project-current nil parent))
                  (child-project
                   (project-current nil (expand-file-name "src/" submodule))))
              (should parent-project)
              (should child-project)
              (should (file-equal-p (project-root parent-project) parent))
              (should (file-equal-p (project-root child-project) submodule))))
        (delete-directory workspace t)))))

(provide 'yunge-project-test)

;;; yunge-project-test.el ends here
