;;; yunge-embark-test.el --- Embark tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function embark--targets "embark")

(defvar embark-buffer-map)
(defvar embark-command-map)
(defvar embark-function-map)
(defvar embark-general-map)
(defvar embark-tab-map)
(defvar embark-url-map)

(defun yunge-embark-test--assert-file-target (path)
  "Check that PATH is the first complete file target at point."
  (let ((target
         (abbreviate-file-name
          (expand-file-name path yunge-test-root))))
    (with-temp-buffer
      (setq default-directory yunge-test-root)
      (insert path ":12:3")
      (search-backward (file-name-base path))
      (let ((file-target (car (embark--targets))))
        (should (eq (plist-get file-target :orig-type) 'file))
        (should (equal (plist-get file-target :orig-target) target))
        (should
         (equal (plist-get file-target :bounds)
                (cons (point-min)
                      (+ (point-min) (length path)))))))))

(yunge-test-deftest-lazy-load yunge-embark
  (embark embark-consult))

(ert-deftest yunge-embark-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-embark 'embark
   :before-ready
   '(progn
      (when (featurep 'embark)
        (error "Embark was loaded before its Elpaca body ran"))
      (when (eq (key-binding (kbd "M-a")) 'embark-act)
        (error "Embark was bound before its Elpaca body ran")))
   :after-ready
   '(progn
      (unless (eq (key-binding (kbd "M-a")) 'embark-act)
        (error "Embark was not bound after package readiness"))
      (when (featurep 'embark)
        (error "Embark was loaded by its configuration")))))

(ert-deftest yunge-embark-loads-consult-integration-on-demand ()
  (yunge-test-load-package-config 'yunge-embark)
  (require 'consult)
  (require 'embark)
  (should (featurep 'embark-consult)))

(ert-deftest yunge-embark-preserves-upstream-registrations ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'embark)
       (let ((targets
              (mapcar
               (lambda (finder)
                 (if (eq finder 'embark-target-file-at-point)
                     'yunge-embark-target-file-at-point
                   finder))
               embark-target-finders))
             (actions (copy-tree embark-keymap-alist))
             (indicators (copy-tree embark-indicators))
             (help-key embark-help-key))
         (defmacro elpaca (_order &rest body)
           (cons 'progn body))
         (require 'yunge-embark)
         (unless (and (equal embark-target-finders targets)
                      (equal embark-keymap-alist actions)
                      (equal embark-indicators indicators)
                      (equal embark-help-key help-key))
           (error "Unexpected Embark configuration changes")))))))

(ert-deftest yunge-embark-binds-action-keys ()
  (yunge-test-load-package-config 'yunge-embark)
  (require 'embark)

  (yunge-test-keymap-keys
   embark-general-map
   '(("C-q" . embark-toggle-quit)
     ("q")
     ("w")
     ("y" . embark-copy-as-kill)))

  (yunge-test-keymap-keys
   embark-buffer-map
   '(("b" . embark-bury-buffer)
     ("k")
     ("K")
     ("q" . kill-buffer)
     ("Q" . embark-kill-buffer-and-window)
     ("z")))

  (yunge-test-keymap-keys
   embark-tab-map
   '(("RET" . tab-bar-select-tab-by-name)
     ("k")
     ("q" . tab-bar-close-tab-by-name)
     ("s")))

  (yunge-test-keymap-keys
   embark-function-map
   '(("D b" . debug-on-entry)
     ("D c" . cancel-debug-on-entry)
     ("D m" . elp-instrument-function)
     ("D r" . elp-restore-function)
     ("D t" . trace-function)
     ("D u" . untrace-function)
     ("k")
     ("K")
     ("m")
     ("M")
     ("t")
     ("T")))

  ;; Commands inherit the function debugging actions.
  (yunge-test-keymap-keys
   embark-command-map
   '(("b" . where-is)
     ("D b" . debug-on-entry)))

  (yunge-test-keymap-keys
   embark-url-map
   '(("d" . embark-download-url))))

(ert-deftest yunge-embark-targets-windows-paths ()
  (skip-unless (eq system-type 'windows-nt))
  (yunge-test-load-package-config 'yunge-embark)
  (require 'embark)
  (let* ((missing-directory
          (make-temp-name
           (expand-file-name "var/test/embark-missing-"
                             yunge-test-root))))
    (dolist (separator '("/" "\\"))
      (let* ((path
              (string-replace
               "/" separator
               (expand-file-name "cde.txt" missing-directory))))
        (should-not (file-exists-p path))
        (yunge-embark-test--assert-file-target path)))))

(ert-deftest yunge-embark-targets-explicit-relative-path ()
  (yunge-test-load-package-config 'yunge-embark)
  (require 'embark)
  (let* ((missing-directory
          (make-temp-name
           (expand-file-name "var/test/embark-relative-"
                             yunge-test-root)))
         (absolute (expand-file-name "file.el" missing-directory))
         (path (file-relative-name absolute yunge-test-root)))
    (should-not (file-exists-p absolute))
    (yunge-embark-test--assert-file-target path)))

;;; yunge-embark-test.el ends here
