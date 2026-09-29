;;; yunge-elpaca-bootstrap-test.el --- Elpaca installation tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defmacro yunge-elpaca-bootstrap-test--with-repository (&rest body)
  "Run BODY with a local Git package and isolated Emacs state."
  (declare (indent 0))
  `(progn
     (skip-unless (executable-find "git"))
     (yunge-test-run-emacs
      "--eval"
      (prin1-to-string
       '(progn
          (require 'ert)
          (require 'yunge-state)
          (defvar elpaca-directory)
          (defvar elpaca-sources-directory)
          (defvar elpaca-builds-directory)
          (defvar elpaca-order)
          (let* ((root (make-temp-file "yunge-elpaca-" t))
                 (upstream (expand-file-name "upstream/" root))
                 (yunge-var-directory (expand-file-name "var/" root))
                 (elpaca-directory (expand-file-name "elpaca/" yunge-var-directory))
                 (elpaca-sources-directory (expand-file-name "source/" elpaca-directory))
                 (elpaca-builds-directory (expand-file-name "build/" elpaca-directory))
                 (repo (expand-file-name "elpaca/" elpaca-sources-directory))
                 (build (expand-file-name "elpaca/" elpaca-builds-directory))
                 (elpaca-order `(elpaca :repo ,upstream :ref "HEAD")))
            (unwind-protect
                (progn
                  (make-directory upstream t)
                  (with-temp-file (expand-file-name "elpaca.el" upstream)
                    (insert
                     ";;; elpaca.el --- Test package -*- lexical-binding: t; -*-\n"
                     "(require 'loaddefs-gen)\n"
                     "(defun elpaca-generate-autoloads (_package directory)\n"
                     "  (loaddefs-generate directory\n"
                     "    (expand-file-name \"elpaca-autoloads.el\" directory) nil nil nil t))\n"
                     ";;;###autoload\n"
                     "(defmacro elpaca (&rest _arguments) nil)\n"
                     ";;;###autoload\n"
                     "(defun elpaca-no-symlink-mode (&optional _argument) nil)\n"
                     "(defun elpaca-process-queues () nil)\n"
                     ";;;###autoload\n"
                     "(defun yunge-elpaca-bootstrap-test-command () 'ready)\n"
                     "(provide 'elpaca)\n"))
                  (let ((default-directory upstream))
                    (dolist (args '(("init" "--quiet") ("add" "elpaca.el")
                                    ("-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                                     "-c" "commit.gpgsign=false"
                                     "commit" "--quiet" "-m" "Fixture")))
                      (should (zerop (apply #'call-process "git" nil nil nil args)))))
                  ,@body)
              (delete-directory root t))))))))

(ert-deftest yunge-elpaca-bootstrap-retries-a-failed-installation ()
  (yunge-elpaca-bootstrap-test--with-repository
    (setf (plist-get (cdr elpaca-order) :ref) "missing-ref")
    (let ((failure (should-error (require 'yunge-elpaca-bootstrap))))
      (should (string-match-p "Elpaca installation failed" (error-message-string failure)))
      (should (string-match-p "elpaca-bootstrap" (error-message-string failure))))
    (should-not (file-exists-p repo))
    (with-current-buffer "*elpaca-bootstrap*"
      (should (string-match-p "missing-ref" (buffer-string))))
    (setf (plist-get (cdr elpaca-order) :ref) "HEAD")
    (require 'yunge-elpaca-bootstrap)
    (should (file-readable-p (expand-file-name "elpaca-autoloads.el" repo)))
    (should (eq (yunge-elpaca-bootstrap-test-command) 'ready))))

(ert-deftest yunge-elpaca-bootstrap-recovers-with-an-incomplete-build ()
  (yunge-elpaca-bootstrap-test--with-repository
    (make-directory repo t)
    (copy-file (expand-file-name "elpaca.el" upstream) (expand-file-name "elpaca.el" repo))
    (make-directory build t)
    (with-temp-file (expand-file-name "elpaca.el" build)
      (insert "(error \"The incomplete build should not be loaded\")\n"))
    (require 'yunge-elpaca-bootstrap)
    (should (file-readable-p (expand-file-name "elpaca-autoloads.el" repo)))
    (should (eq (yunge-elpaca-bootstrap-test-command) 'ready))))

(ert-deftest yunge-elpaca-bootstrap-preserves-incomplete-existing-source ()
  (yunge-elpaca-bootstrap-test--with-repository
    (make-directory repo t)
    (with-temp-file (expand-file-name "local-work.txt" repo) (insert "keep me"))
    (let ((failure (should-error (require 'yunge-elpaca-bootstrap))))
      (should (string-match-p "move it aside" (error-message-string failure)))
      (should (string-match-p (regexp-quote repo) (error-message-string failure))))
    (should (equal (with-temp-buffer
                     (insert-file-contents (expand-file-name "local-work.txt" repo))
                     (buffer-string))
                   "keep me"))))

;;; yunge-elpaca-bootstrap-test.el ends here
