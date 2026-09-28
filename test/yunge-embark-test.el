;;; yunge-embark-test.el --- Embark tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function embark-target-file-at-point "embark")

(defvar embark-buffer-map)
(defvar embark-command-map)
(defvar embark-function-map)
(defvar embark-general-map)
(defvar embark-indicators)
(defvar embark-tab-map)
(defvar embark-url-map)
(defvar unread-command-events)

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

(ert-deftest yunge-embark-copies-git-ssh-addresses-as-https ()
  (yunge-test-enable-evil)
  (yunge-test-load-package-config 'yunge-embark)
  (require 'embark)
  (dolist (address '("git@git.meitu.com:conan/conan-meitu-index.git"
                     "git\\@git.meitu.com:conan/conan-meitu-index.git"))
    (with-temp-buffer
      (insert "Clone `" address "` here")
      (search-backward "conan-meitu")
      (let ((target (yunge-embark-target-git-ssh-at-point)))
        (should (eq (car target) 'git-ssh))
        (should (equal (cadr target) address))
        (should (equal (buffer-substring-no-properties
                        (caddr target) (cdddr target)) address)))
      (let ((kill-ring nil)
            (unread-command-events (list ?h))
            (embark-indicators nil)
            (non-essential t))
        ;; Batch Emacs reads action input from stdin. Run Embark's real
        ;; target-injection hook at that input boundary instead.
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _)
                     (yunge-test-with-evil-minibuffer
                       (let ((original-input (minibuffer-contents-no-properties)))
                         (unwind-protect
                             (progn
                               (delete-minibuffer-contents)
                               (run-hooks 'minibuffer-setup-hook)
                               (minibuffer-contents-no-properties))
                           (delete-minibuffer-contents)
                           (insert original-input)
                           (remove-hook 'post-command-hook #'exit-minibuffer t)))))))
          (call-interactively (key-binding (kbd "M-a"))))
        (should (equal (car kill-ring)
                       "https://git.meitu.com/conan/conan-meitu-index.git")))
      (goto-char (point-min))
      (should-not (yunge-embark-target-git-ssh-at-point))))
  (should-error (yunge-embark-copy-git-ssh-as-https
                 "https://example.com/repo.git")
                :type 'user-error))

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
