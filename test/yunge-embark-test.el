;;; yunge-embark-test.el --- Embark tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function embark-target-file-at-point "embark")

(defvar embark-buffer-map)
(defvar embark-command-map)
(defvar embark-function-map)
(defvar embark-general-map)
(defvar embark-tab-map)
(defvar embark-url-map)

(defun yunge-embark-test--file-target (path)
  "Return Embark's file target for PATH."
  (with-temp-buffer
    (insert path)
    (goto-char (point-min))
    (embark-target-file-at-point)))

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
       (let ((targets (copy-sequence embark-target-finders))
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
  (let* ((directory (make-temp-file "yunge-embark-" t))
         (existing (expand-file-name "existing.el" directory))
         (missing (expand-file-name "missing.el" directory)))
    (unwind-protect
        (progn
          (write-region "" nil existing nil 'silent)
          (dolist (separator '("/" "\\"))
            (let* ((existing-text
                    (string-replace "/" separator existing))
                   (existing-target
                    (yunge-embark-test--file-target existing-text))
                   (missing-text
                    (string-replace "/" separator missing))
                   (missing-target
                    (yunge-embark-test--file-target missing-text)))
              (should (equal (cadr existing-target)
                             (abbreviate-file-name existing)))
              (should (equal (cddr existing-target)
                             (cons 1 (1+ (length existing-text)))))
              ;; Keep FFAP's native existence-based fallback.
              (should (file-exists-p (cadr missing-target)))
              (should-not (equal (cadr missing-target)
                                 (abbreviate-file-name missing))))))
      (delete-directory directory t))))

;;; yunge-embark-test.el ends here
