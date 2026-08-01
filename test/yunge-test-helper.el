;;; yunge-test-helper.el --- Test support -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defconst yunge-test-root
  (expand-file-name ".." (file-name-directory load-file-name)))

(startup-redirect-eln-cache
 (expand-file-name "var/eln-cache/" yunge-test-root))

(require 'ert)
(require 'bytecomp)
(require 'cl-lib)
(require 'seq)

(declare-function evil-change-state "evil-core")
(declare-function evil-visual-state "evil-states")
(declare-function evil-local-mode "evil-core")
(declare-function evil-mode "evil")
(declare-function which-key--get-bindings "which-key")

(defvar evil-emacs-state-modes)
(defvar evil-insert-state-modes)
(defvar evil-local-mode)
(defvar evil-motion-state-modes)
(defvar evil-state)
(defvar evil-want-integration)
(defvar evil-want-keybinding)
(defvar which-key-replacement-alist)

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

(defun yunge-test-package-arguments (packages)
  "Return command-line load path arguments for PACKAGES."
  (apply #'append
         (mapcar
          (lambda (name)
            (list "-L"
                  (expand-file-name
                   (format "var/elpaca/build/%s" name)
                   yunge-test-root)))
          packages)))

(defun yunge-test-run-emacs (&rest arguments)
  "Run clean Emacs with ARGUMENTS and fail if it exits unsuccessfully."
  (with-temp-buffer
    (let ((process-environment (copy-sequence process-environment))
          status)
      (setenv "XDG_CONFIG_HOME"
              (expand-file-name "var/test/" yunge-test-root))
      (setq status
            (apply #'call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil "--batch" "-Q" arguments))
      (unless (equal status 0)
        (ert-fail
         (format "Emacs exited with %S:\n%s" status (buffer-string))))
      (buffer-string))))

(defun yunge-test-assert-lazy-load (library features)
  "Load configuration LIBRARY without eagerly loading FEATURES."
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string '(defmacro elpaca (&rest _body) nil))
   "-L" (expand-file-name "lisp" yunge-test-root)
   "-l" (symbol-name library)
   "--eval"
   (prin1-to-string
    `(dolist (feature ',features)
       (when (featurep feature)
         (error "%S eagerly loaded %S" ',library feature))))))

(defmacro yunge-test-deftest-lazy-load (library features)
  "Define a lazy-load test for configuration LIBRARY and FEATURES."
  (declare (indent 1))
  `(ert-deftest ,(intern (format "%s-loads-lazily" library)) ()
     (yunge-test-assert-lazy-load ',library ',features)))

(cl-defun yunge-test-run-package-config
    (library package &key dependencies setup before-ready after-ready)
  "Test the Elpaca lifecycle of LIBRARY's PACKAGE configuration.
DEPENDENCIES are additional package build directories.  Evaluate SETUP
before loading LIBRARY, BEFORE-READY after loading it, and AFTER-READY
after activating PACKAGE and executing its deferred configuration."
  (let ((autoloads (intern (format "%s-autoloads" package)))
        (arguments
         (yunge-test-package-arguments (cons package dependencies))))
    (apply
     #'yunge-test-run-emacs
     (append
      arguments
      (list
       "--eval"
       (prin1-to-string
        `(progn
           ,setup
           (defvar yunge-test-elpaca-declarations nil)
           (defmacro elpaca (order &rest body)
             (list 'push
                   (list 'list
                         (list 'quote order)
                         (list 'quote body))
                   'yunge-test-elpaca-declarations))))
       "-L" (expand-file-name "lisp" yunge-test-root)
       "-l" (symbol-name library)
       "--eval"
       (prin1-to-string
        `(progn
           (unless
               (equal (mapcar #'car
                              (reverse yunge-test-elpaca-declarations))
                      '(,package))
             (error "Unexpected Elpaca declarations: %S"
                    yunge-test-elpaca-declarations))
           ,before-ready
           (require ',autoloads)
           (dolist (declaration
                    (reverse yunge-test-elpaca-declarations))
             (eval (cons 'progn (cadr declaration)) t))
           ,after-ready)))))))

(defun yunge-test-load-package-config (library)
  "Load LIBRARY and synchronously execute its Elpaca configuration."
  (cl-letf (((symbol-function 'elpaca)
             (cons 'macro
                   (lambda (_order &rest body)
                     (cons 'progn body)))))
    (require library)))

(defun yunge-test--restore-evil-minibuffer (map local-mode state)
  "Restore minibuffer MAP and Evil LOCAL-MODE and STATE."
  (use-local-map map)
  (if local-mode
      (progn
        (unless evil-local-mode
          (evil-local-mode 1))
        (evil-change-state state))
    (when evil-local-mode
      (evil-local-mode -1))))

(defmacro yunge-test-with-evil-minibuffer (&rest body)
  "Run BODY in the minibuffer buffer and restore its local state."
  (declare (indent 0) (debug t))
  `(with-current-buffer (window-buffer (minibuffer-window))
     (let ((original-map (current-local-map))
           (original-evil-local-mode
            (bound-and-true-p evil-local-mode))
           (original-evil-state
            (and (bound-and-true-p evil-local-mode) evil-state)))
       (unwind-protect
           (progn ,@body)
         (yunge-test--restore-evil-minibuffer
          original-map original-evil-local-mode original-evil-state)))))

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
  (yunge-test-add-package-path 'evil 'goto-chg 'which-key)
  ;; Preserve the production load order required by `evil-want-minibuffer'.
  (require 'yunge-minibuffer)
  (require 'evil-autoloads)
  (yunge-test-load-package-config 'yunge-evil)
  (unless (bound-and-true-p evil-mode)
    (ert-fail "The Evil package configuration did not enable Evil")))

(defun yunge-test-key (key expected)
  "Check that KEY resolves to EXPECTED in the current buffer."
  (should (eq (key-binding (kbd key)) expected)))

(defun yunge-test-keys (bindings)
  "Check that each key in BINDINGS resolves in the current buffer."
  (dolist (binding bindings)
    (yunge-test-key (car binding) (cdr binding))))

(defun yunge-test-evil-keys (state bindings)
  "Check Evil STATE and BINDINGS in the current buffer."
  (should (eq evil-state state))
  (yunge-test-keys bindings))

(defun yunge-test-assert-calls-interactively
    (function expected &rest arguments)
  "Check that FUNCTION calls EXPECTED interactively with ARGUMENTS."
  (let (called)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (command &rest _arguments)
                 (setq called command))))
      (apply function arguments))
    (should (eq called expected))))

(defun yunge-test-evil-normal-keys (mode bindings)
  "Activate major MODE and check its normal-state BINDINGS."
  (with-temp-buffer
    (funcall mode)
    (yunge-test-evil-keys 'normal bindings)))

(defun yunge-test-evil-visual-keys (mode bindings)
  "Activate major MODE and check its visual-state BINDINGS."
  (with-temp-buffer
    (funcall mode)
    (should (eq evil-state 'normal))
    (evil-visual-state)
    (yunge-test-evil-keys 'visual bindings)))

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
