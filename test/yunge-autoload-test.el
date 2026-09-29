;;; yunge-autoload-test.el --- Autoload tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-autoload)

(defmacro yunge-autoload-test--with-source (&rest body)
  "Run BODY in a clean Emacs with a temporary source tree and cache."
  (declare (indent 0))
  `(yunge-test-run-emacs
    "--eval"
    (prin1-to-string
     '(progn
        (require 'ert)
        (require 'yunge-autoload)
        (let* ((root (make-temp-file "yunge-autoload-" t))
               (yunge-autoload-source-directory (expand-file-name "source/" root))
               (source-file (expand-file-name "fixture.el" yunge-autoload-source-directory))
               (yunge-autoload-cache-directory (expand-file-name "autoload/" root))
               (yunge-autoload-loaddefs-file
                (expand-file-name "yunge-loaddefs.el" yunge-autoload-cache-directory))
               (yunge-autoload-cache-hash-file
                (expand-file-name "yunge-loaddefs.sha256" yunge-autoload-cache-directory))
               (yunge-autoload-repository-hash-file (expand-file-name "autoloads.sha256" root))
               warnings)
          (unwind-protect
              (progn
                (make-directory yunge-autoload-source-directory t)
                (with-temp-file source-file
                  (insert
                   ";;; fixture.el --- Autoload fixture -*- lexical-binding: t; -*-\n"
                   ";;;###autoload\n"
                   "(defun yunge-autoload-test-command ()\n"
                   "  \"Run the fixture.\"\n"
                   "  (interactive)\n"
                   "  'loaded)\n"
                   ";;;###autoload\n"
                   "(defcustom yunge-autoload-test-option nil\n"
                   "  \"An autoloaded option.\" :type 'boolean :group 'emacs)\n"
                   "(provide 'yunge-autoload-test-library)\n"))
                (cl-letf (((symbol-function 'display-warning)
                           (lambda (type message &rest _arguments)
                             (push (cons type message) warnings))))
                  ,@body))
            (delete-directory root t)))))))

(ert-deftest yunge-autoload-generates-a-lazy-command ()
  (yunge-autoload-test--with-source
    (yunge-autoload-generate)
    (should-not warnings)
    (should (file-exists-p yunge-autoload-loaddefs-file))
    (should (equal (yunge-autoload--read-hash yunge-autoload-repository-hash-file)
                   (yunge-autoload--read-hash yunge-autoload-cache-hash-file)))
    (should (autoloadp (symbol-function 'yunge-autoload-test-command)))
    (should-not (featurep 'yunge-autoload-test-library))
    (should (eq (yunge-autoload-test-command) 'loaded))
    (should (featurep 'yunge-autoload-test-library))))

(ert-deftest yunge-autoload-bootstraps-a-cache-outside-the-source-tree ()
  (yunge-autoload-test--with-source
    (yunge-autoload-generate)
    (let ((expected (yunge-autoload--read-hash yunge-autoload-repository-hash-file))
          (relocated (make-temp-file "yunge-autoload-cache-" t)))
      (unwind-protect
          (let* ((yunge-autoload-cache-directory (expand-file-name "nested/cache/" relocated))
                 (yunge-autoload-loaddefs-file
                  (expand-file-name "other-loaddefs.el" yunge-autoload-cache-directory))
                 (yunge-autoload-cache-hash-file
                  (expand-file-name "other-loaddefs.sha256" yunge-autoload-cache-directory)))
            (fmakunbound 'yunge-autoload-test-command)
            (yunge-autoload-load)
            (should-not warnings)
            (should (equal expected
                           (yunge-autoload--read-hash yunge-autoload-repository-hash-file)))
            (should (equal expected
                           (yunge-autoload--read-hash yunge-autoload-cache-hash-file)))
            (should (autoloadp (symbol-function 'yunge-autoload-test-command)))
            (should-not (featurep 'yunge-autoload-test-library))
            (should (eq (yunge-autoload-test-command) 'loaded)))
        (delete-directory relocated t)))))

(ert-deftest yunge-autoload-warns-about-changed-declarations-until-regenerated ()
  (yunge-autoload-test--with-source
    (yunge-autoload-generate)
    (with-temp-buffer
      (insert-file-contents source-file)
      (search-forward "Run the fixture.")
      (replace-match "Run the updated fixture." t t)
      (write-region (point-min) (point-max) source-file nil 'silent))
    ;; Generate the new repository version without replacing this session's cache.
    (let ((yunge-autoload-loaddefs-file (expand-file-name "new-loaddefs.el" root))
          (yunge-autoload-cache-hash-file (expand-file-name "new-loaddefs.sha256" root)))
      (yunge-autoload-generate))
    (fmakunbound 'yunge-autoload-test-command)
    (yunge-autoload-load)
    (should (autoloadp (symbol-function 'yunge-autoload-test-command)))
    (should (eq (caar warnings) 'yunge-autoload))
    (should (string-match-p "yunge-autoload-generate" (cdar warnings)))
    (setq warnings nil)
    (yunge-autoload-generate)
    (should-not warnings)
    (should (eq (yunge-autoload-test-command) 'loaded))))

(ert-deftest yunge-autoload-repository-hash-is-current ()
  (let ((file (make-temp-file "yunge-loaddefs-test-" nil ".el")))
    (unwind-protect
        (progn
          (yunge-autoload--generate-file file)
          (should (equal (yunge-autoload--loaddefs-hash file)
                         (yunge-autoload--read-hash yunge-autoload-repository-hash-file))))
      (delete-file file))))

;;; yunge-autoload-test.el ends here
