;;; yunge-path-test.el --- Buffer path tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-path)

(yunge-test-deftest-lazy-load yunge-path
  (evil dired magit project yunge-reader compile))

(defmacro yunge-path-test--with-kill-ring (&rest body)
  "Run BODY without reading or writing the system clipboard."
  (declare (indent 0) (debug t))
  `(let ((kill-ring nil)
         (kill-ring-yank-pointer nil)
         (interprogram-cut-function nil)
         (interprogram-paste-function nil))
     ,@body))

(ert-deftest yunge-path-copies-current-file-absolute-path ()
  (with-temp-buffer
    (let* ((buffer-file-name "relative/file name.el")
           (expected (expand-file-name buffer-file-name)))
      (yunge-path-test--with-kill-ring
        (call-interactively #'yunge-copy-buffer-absolute-path)
        (should (equal (current-kill 0) expected))))))

(ert-deftest yunge-path-copies-current-file-project-path ()
  (with-temp-buffer
    (let* ((root (expand-file-name "project/" temporary-file-directory))
           (buffer-file-name (expand-file-name "lisp/file.el" root))
           (project 'project))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (_maybe-prompt directory)
                   (should (equal directory
                                  (file-name-directory buffer-file-name)))
                   project))
                ((symbol-function 'project-root)
                 (lambda (candidate)
                   (should (eq candidate project))
                   root)))
        (yunge-path-test--with-kill-ring
          (call-interactively #'yunge-copy-buffer-project-path)
          (should (equal (current-kill 0) "lisp/file.el")))))))

(ert-deftest yunge-path-indirect-buffers-copy-the-original-file ()
  (with-temp-buffer
    (let* ((root (expand-file-name "project/" temporary-file-directory))
           (file (expand-file-name "notes/note.org" root))
           (base (current-buffer))
           indirect)
      (setq buffer-file-name file)
      (unwind-protect
          (progn
            (setq indirect (make-indirect-buffer base " *yunge-path-indirect*" t))
            (with-current-buffer indirect
              (setq default-directory temporary-file-directory)
              (should-not buffer-file-name)
              (cl-letf (((symbol-function 'project-current)
                         (lambda (_maybe-prompt directory)
                           (should (equal directory (file-name-directory file)))
                           'project))
                        ((symbol-function 'project-root) (lambda (_project) root)))
                (yunge-path-test--with-kill-ring
                  (call-interactively #'yunge-copy-buffer-absolute-path)
                  (should (equal (current-kill 0) file))
                  (call-interactively #'yunge-copy-buffer-project-path)
                  (should (equal (current-kill 0) "notes/note.org"))))))
        (when (buffer-live-p indirect)
          (kill-buffer indirect))))))

(ert-deftest yunge-path-process-buffers-copy-the-recorded-directory ()
  (require 'shell)
  (require 'compile)
  (let* ((root (file-name-as-directory yunge-test-state-root))
         (directory (expand-file-name "build/" root)))
    (make-directory directory t)
    (dolist (mode '(shell-mode compilation-mode))
      (with-temp-buffer
        (setq default-directory directory)
        (funcall mode)
        (cl-letf (((symbol-function 'project-current)
                   (lambda (_maybe-prompt context-directory)
                     (should (equal context-directory directory))
                     'project))
                  ((symbol-function 'project-root) (lambda (_project) root)))
          (yunge-path-test--with-kill-ring
            (call-interactively #'yunge-copy-buffer-absolute-path)
            (should (equal (current-kill 0) directory))
            (call-interactively #'yunge-copy-buffer-project-path)
            (should (equal (current-kill 0) "build/"))
            (setq default-directory root)
            (cl-letf (((symbol-function 'project-current)
                       (lambda (_maybe-prompt _directory) 'project)))
              (call-interactively #'yunge-copy-buffer-project-path))
            (should (equal (current-kill 0) "."))))))))

(ert-deftest yunge-path-rejects-buffers-without-an-associated-path ()
  (with-temp-buffer
    (yunge-path-test--with-kill-ring
      (kill-new "previous value")
      (dolist (command '(yunge-copy-buffer-absolute-path
                         yunge-copy-buffer-project-path))
        (should-error (call-interactively command) :type 'user-error)
        (should (equal (current-kill 0) "previous value"))))))

(ert-deftest yunge-path-project-copy-requires-a-project ()
  (with-temp-buffer
    (let ((buffer-file-name (expand-file-name "file.el" temporary-file-directory)))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (_maybe-prompt _directory) nil)))
        (yunge-path-test--with-kill-ring
          (kill-new "previous value")
          (should-error (call-interactively #'yunge-copy-buffer-project-path)
                        :type 'user-error)
          (should (equal (current-kill 0) "previous value")))))))

(ert-deftest yunge-path-preserves-remote-file-names ()
  (with-temp-buffer
    (let ((buffer-file-name "/ssh:example:/srv/project/file name.el"))
      (yunge-path-test--with-kill-ring
        (call-interactively #'yunge-copy-buffer-absolute-path)
        (should (equal (current-kill 0) buffer-file-name))))))

;;; yunge-path-test.el ends here
