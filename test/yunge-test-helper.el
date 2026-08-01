;;; yunge-test-helper.el --- Test support -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'bytecomp)
(require 'seq)

(declare-function evil-visual-state "evil-states")
(declare-function evil-mode "evil")
(declare-function which-key--get-bindings "which-key")

(defvar evil-emacs-state-modes)
(defvar evil-insert-state-modes)
(defvar evil-motion-state-modes)
(defvar evil-state)
(defvar evil-want-integration)
(defvar evil-want-keybinding)
(defvar which-key-replacement-alist)

(defconst yunge-test-root
  (expand-file-name ".." (file-name-directory load-file-name)))

(startup-redirect-eln-cache
 (expand-file-name "var/eln-cache/" yunge-test-root))

(add-to-list 'load-path (expand-file-name "lisp" yunge-test-root))

(defvar elpaca-directory
  (expand-file-name "var/elpaca/" yunge-test-root))
(defvar elpaca-cache-directory
  (expand-file-name "cache/" elpaca-directory))
(defvar elpaca-builds-directory
  (expand-file-name "build/" elpaca-directory))
(defvar elpaca-sources-directory
  (expand-file-name "source/" elpaca-directory))

(defun yunge-test-add-package-path (&rest packages)
  "Add Elpaca build directories for PACKAGES to `load-path'."
  (dolist (name packages)
    (add-to-list
     'load-path
     (expand-file-name (format "var/elpaca/build/%s" name)
                       yunge-test-root))))

(defun yunge-test-run-emacs (&rest arguments)
  "Run clean Emacs with ARGUMENTS and fail if it exits unsuccessfully."
  (with-temp-buffer
    (let ((status
           (apply #'call-process
                  (expand-file-name invocation-name invocation-directory)
                  nil t nil "--batch" "-Q" arguments)))
      (unless (equal status 0)
        (ert-fail
         (format "Emacs exited with %S:\n%s" status (buffer-string))))
      (buffer-string))))

(defun yunge-test-byte-compile-diagnostics (file)
  "Return byte compiler diagnostics for FILE without writing an elc file."
  (let ((log-buffer (generate-new-buffer " *yunge-byte-compile*"))
        diagnostics)
    (unwind-protect
        (let ((byte-compile-dest-file-function #'ignore)
              (byte-compile-error-on-warn nil)
              (byte-compile-log-buffer log-buffer)
              (byte-compile-verbose nil)
              (byte-compile-warnings t)
              (byte-compile-log-warning-function
               (lambda (message position _fill level)
                 (push (list position level message) diagnostics))))
          (condition-case error-data
              (byte-compile-file file)
            (error
             (push (list nil :error (error-message-string error-data))
                   diagnostics))))
      (when (buffer-live-p log-buffer)
        (kill-buffer log-buffer)))
    (nreverse diagnostics)))

(defun yunge-test-enable-evil ()
  "Load Evil and enable the configuration's leader support."
  (require 'elpaca-autoloads)
  (require 'yunge-evil)
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-want-integration t
        evil-want-keybinding nil)
  (require 'evil)
  (evil-mode 1))

(defun yunge-test-key (key expected)
  "Check that KEY resolves to EXPECTED in the current buffer."
  (should (eq (key-binding (kbd key)) expected)))

(defun yunge-test-evil-normal-keys (mode bindings)
  "Activate major MODE and check its normal-state BINDINGS."
  (with-temp-buffer
    (funcall mode)
    (should (eq evil-state 'normal))
    (dolist (binding bindings)
      (yunge-test-key (car binding) (cdr binding)))))

(defun yunge-test-evil-visual-keys (mode bindings)
  "Activate major MODE and check its visual-state BINDINGS."
  (with-temp-buffer
    (funcall mode)
    (should (eq evil-state 'normal))
    (evil-visual-state)
    (should (eq evil-state 'visual))
    (dolist (binding bindings)
      (yunge-test-key (car binding) (cdr binding)))))

(defun yunge-test-which-key-bindings (mode bindings &optional prefix)
  "Check Which-Key descriptions for MODE BINDINGS below optional PREFIX."
  (dolist (binding bindings)
    (let ((description (nth 2 binding)))
      (when description
        (let ((key (if prefix
                       (concat prefix " " (car binding))
                     (car binding)))
              (case-fold-search nil)
              found)
          (dolist (replacement
                   (cdr (assq mode which-key-replacement-alist)))
            (when (string-match-p
                   (caar replacement) (key-description (kbd key)))
              (setq found (cddr replacement))))
          (should (equal found description)))))))

(defun yunge-test-which-key-prefix (prefix bindings)
  "Check Which-Key BINDINGS shown below PREFIX in the current buffer."
  (let ((visible (which-key--get-bindings (kbd prefix))))
    (dolist (binding bindings)
      (when-let* ((description (nth 2 binding)))
        (let ((entry
               (seq-find
                (lambda (candidate)
                  (equal (substring-no-properties (car candidate))
                         (car binding)))
                visible)))
          (should entry)
          (should
           (equal (substring-no-properties (nth 2 entry))
                  description)))))))

(defun yunge-test-which-key-prefix-bindings (mode prefix bindings)
  "Check Which-Key BINDINGS shown below PREFIX after activating MODE."
  (with-temp-buffer
    (funcall mode)
    (should (eq evil-state 'normal))
    (yunge-test-which-key-prefix prefix bindings)))

(provide 'yunge-test-helper)

;;; yunge-test-helper.el ends here
