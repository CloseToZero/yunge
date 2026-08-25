;;; yunge-autoload-test.el --- Autoload tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-autoload)

(ert-deftest yunge-autoload-generates-cache-and-hashes ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'yunge-autoload)
       (let* ((root (make-temp-file "yunge-autoload-" t))
              (yunge-autoload-source-directory
               (expand-file-name "source/" root))
              (source-file
               (expand-file-name "fixture.el"
                                 yunge-autoload-source-directory))
              (yunge-autoload-cache-directory
               (expand-file-name "autoload/" root))
              (yunge-autoload-loaddefs-file
               (expand-file-name "yunge-loaddefs.el"
                                 yunge-autoload-cache-directory))
              (yunge-autoload-repository-hash-file
               (expand-file-name "autoloads.sha256" root))
              (yunge-autoload-cache-hash-file
               (expand-file-name "yunge-loaddefs.sha256"
                                 yunge-autoload-cache-directory)))
         (unwind-protect
             (progn
               (make-directory yunge-autoload-source-directory t)
               (with-temp-file source-file
                 (insert
                  ";;; fixture.el --- Autoload fixture\n\n"
                  ";;;###autoload\n"
                  "(defun yunge-autoload-test-command ()\n"
                  "  (interactive)\n"
                  "  'loaded)\n\n"
                  "(provide 'yunge-autoload-test-library)\n"))
               (yunge-autoload-generate)
               (dolist (file
                        (list yunge-autoload-loaddefs-file
                              yunge-autoload-repository-hash-file
                              yunge-autoload-cache-hash-file))
                 (unless (file-exists-p file)
                   (error "Autoload generation did not create %s" file)))
               (let ((repository-hash
                      (yunge-autoload--read-hash
                       yunge-autoload-repository-hash-file)))
                 (unless
                     (and
                      (equal
                       repository-hash
                       (yunge-autoload--read-hash
                        yunge-autoload-cache-hash-file))
                      (equal
                       repository-hash
                       (yunge-autoload--loaddefs-hash
                        yunge-autoload-loaddefs-file)))
                   (error "Generated autoload hashes differ")))
               (unless
                   (autoloadp
                    (symbol-function
                     'yunge-autoload-test-command))
                 (error "The fixture command was not autoloaded"))
               (when (featurep 'yunge-autoload-test-library)
                 (error "Generating autoloads loaded the fixture"))
               (unless
                   (eq (yunge-autoload-test-command) 'loaded)
                 (error "The fixture command returned the wrong value"))
               (unless (featurep 'yunge-autoload-test-library)
                 (error "The fixture library was not loaded")))
           (delete-directory root t)))))))

(ert-deftest yunge-autoload-bootstraps-missing-cache ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'cl-lib)
       (require 'yunge-autoload)
       (let* ((root (make-temp-file "yunge-autoload-" t))
              (yunge-autoload-source-directory
               (expand-file-name "source/" root))
              (source-file
               (expand-file-name "fixture.el"
                                 yunge-autoload-source-directory))
              (yunge-autoload-cache-directory
               (expand-file-name "autoload/" root))
              (yunge-autoload-loaddefs-file
               (expand-file-name "yunge-loaddefs.el"
                                 yunge-autoload-cache-directory))
              (yunge-autoload-repository-hash-file
               (expand-file-name "autoloads.sha256" root))
              (yunge-autoload-cache-hash-file
               (expand-file-name "yunge-loaddefs.sha256"
                                 yunge-autoload-cache-directory))
              warning)
         (unwind-protect
             (progn
               (make-directory yunge-autoload-source-directory t)
               (with-temp-file source-file
                 (insert
                  ";;; fixture.el --- Autoload fixture\n\n"
                  ";;;###autoload\n"
                  "(defun yunge-autoload-test-command ()\n"
                  "  (interactive)\n"
                  "  'loaded)\n\n"
                  "(provide 'yunge-autoload-test-library)\n"))
               (yunge-autoload--generate-file
                yunge-autoload-loaddefs-file)
               (let ((expected-hash
                      (yunge-autoload--loaddefs-hash
                       yunge-autoload-loaddefs-file)))
                 (with-temp-file
                     yunge-autoload-repository-hash-file
                   (insert expected-hash "\n"))
                 (delete-directory yunge-autoload-cache-directory t)
                 (cl-letf
                     (((symbol-function 'display-warning)
                       (lambda (type message &rest _arguments)
                         (setq warning (cons type message)))))
                   (yunge-autoload-load))
                 (unless
                     (equal expected-hash
                            (yunge-autoload--read-hash
                             yunge-autoload-repository-hash-file))
                   (error "Bootstrap changed the repository hash"))
                 (unless
                     (equal expected-hash
                            (yunge-autoload--read-hash
                             yunge-autoload-cache-hash-file))
                   (error "Bootstrap recorded the wrong cache hash")))
               (when warning
                 (error "Bootstrap displayed a warning: %S" warning))
               (unless
                   (autoloadp
                    (symbol-function
                     'yunge-autoload-test-command))
                 (error "The bootstrapped command was not autoloaded"))
               (when (featurep 'yunge-autoload-test-library)
                 (error "Bootstrap loaded the fixture library")))
           (delete-directory root t)))))))

(ert-deftest yunge-autoload-loads-stale-cache-and-warns ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'cl-lib)
       (require 'yunge-autoload)
       (let* ((root (make-temp-file "yunge-autoload-" t))
              (yunge-autoload-cache-directory
               (expand-file-name "autoload/" root))
              (yunge-autoload-loaddefs-file
               (expand-file-name "yunge-loaddefs.el"
                                 yunge-autoload-cache-directory))
              (yunge-autoload-repository-hash-file
               (expand-file-name "autoloads.sha256" root))
              (yunge-autoload-cache-hash-file
               (expand-file-name "yunge-loaddefs.sha256"
                                 yunge-autoload-cache-directory))
              warning)
         (unwind-protect
             (progn
               (make-directory yunge-autoload-cache-directory t)
               (with-temp-file yunge-autoload-loaddefs-file
                 (prin1
                  '(autoload 'yunge-autoload-test-command
                       "yunge-autoload-test-library" nil t)
                  (current-buffer)))
               (with-temp-file yunge-autoload-repository-hash-file
                 (insert "new\n"))
               (with-temp-file yunge-autoload-cache-hash-file
                 (insert "old\n"))
               (cl-letf (((symbol-function 'display-warning)
                          (lambda (type message &rest _arguments)
                            (setq warning (cons type message)))))
                 (yunge-autoload-load))
               (unless (autoloadp
                        (symbol-function
                         'yunge-autoload-test-command))
                 (error "The stale cache was not loaded"))
               (unless
                   (and (eq (car warning) 'yunge-autoload)
                        (string-match-p
                         "yunge-autoload-generate" (cdr warning)))
                 (error "The stale cache warning was not displayed")))
           (delete-directory root t)))))))

(ert-deftest yunge-autoload-repository-hash-is-current ()
  (make-directory yunge-autoload-cache-directory t)
  (let ((file
         (make-temp-file
          (expand-file-name "yunge-loaddefs-test-"
                            yunge-autoload-cache-directory)
          nil ".el")))
    (unwind-protect
        (progn
          (yunge-autoload--generate-file file)
          (should
           (equal
            (yunge-autoload--loaddefs-hash file)
            (yunge-autoload--read-hash
             yunge-autoload-repository-hash-file))))
      (delete-file file))))

;;; yunge-autoload-test.el ends here
