;;; yunge-test-helper.el --- Test support -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defconst yunge-test-root
  (expand-file-name ".." (file-name-directory load-file-name)))

(defvar yunge-config-directory
  (file-name-as-directory yunge-test-root))

(defvar yunge-var-directory
  (expand-file-name "var/" yunge-test-root))

(startup-redirect-eln-cache
 (expand-file-name "eln-cache/" yunge-var-directory))

(require 'ert)
(require 'bytecomp)
(require 'cl-lib)
(require 'seq)

(declare-function evil-change-state "evil-core")
(declare-function evil-visual-state "evil-states")
(declare-function evil-local-mode "evil-core")
(declare-function evil-mode "evil")
(defvar evil-emacs-state-modes)
(defvar evil-insert-state-modes)
(defvar evil-local-mode)
(defvar evil-motion-state-modes)
(defvar evil-state)
(defvar evil-want-integration)
(defvar evil-want-keybinding)

(add-to-list 'load-path (expand-file-name "lisp" yunge-test-root))

(defvar elpaca-directory
  (expand-file-name "elpaca/" yunge-var-directory))
(defvar elpaca-cache-directory
  (expand-file-name "cache/" elpaca-directory))
(defvar elpaca-builds-directory
  (expand-file-name "build/" elpaca-directory))
(defvar elpaca-sources-directory
  (expand-file-name "source/" elpaca-directory))

(defun yunge-test-package-list ()
  "Return the packages available to tests.
Elpaca manages itself separately, so it is not recorded in its lock file."
  (cons
   'elpaca
   (mapcar
    #'car
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "elpaca-lock.el" yunge-test-root))
      (read (current-buffer))))))

(defun yunge-test-package-directory (package)
  "Return PACKAGE's Elpaca build directory."
  (expand-file-name (format "elpaca/build/%s" package)
                    yunge-var-directory))

(defun yunge-test-package-arguments ()
  "Return command-line load path arguments for locked packages."
  (apply #'append
         (mapcar
          (lambda (name)
            (list "-L" (yunge-test-package-directory name)))
          (yunge-test-package-list))))

(dolist (package (reverse (yunge-test-package-list)))
  ;; `add-to-list' prepends, so reverse the list to match `-L' order.
  (add-to-list 'load-path (yunge-test-package-directory package)))

(defun yunge-test-run-emacs (&rest arguments)
  "Run clean Emacs with ARGUMENTS and fail if it exits unsuccessfully."
  (with-temp-buffer
    (let ((process-environment (copy-sequence process-environment))
          (test-config-home
           (expand-file-name "test/" yunge-var-directory))
          status)
      (make-directory (expand-file-name "emacs/" test-config-home) t)
      (setenv "XDG_CONFIG_HOME" test-config-home)
      (setq status
            (apply #'call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil "--batch" "-Q"
                   (append
                    (yunge-test-package-arguments)
                    (list "-L" (expand-file-name "lisp" yunge-test-root))
                    (list
                     "--eval"
                     (prin1-to-string
                      `(defvar yunge-config-directory
                         ,(file-name-as-directory yunge-test-root))))
                    arguments)))
      (unless (equal status 0)
        (ert-fail
         (format "Emacs exited with %S:\n%s" status (buffer-string))))
      (buffer-string))))

(defun yunge-test-assert-lazy-load (library features)
  "Load configuration LIBRARY without eagerly loading FEATURES."
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string '(defmacro elpaca (&rest _body) nil))
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
    (library package &key setup before-ready after-ready)
  "Test the Elpaca lifecycle of LIBRARY's PACKAGE configuration.
Evaluate SETUP before loading LIBRARY, BEFORE-READY after loading it, and
AFTER-READY after activating PACKAGE and executing its deferred configuration.
Other Elpaca declarations remain deferred."
  (let ((autoloads (intern (format "%s-autoloads" package))))
    (yunge-test-run-emacs
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
     "-l" (symbol-name library)
     "--eval"
     (prin1-to-string
      `(let* ((declarations
               (reverse yunge-test-elpaca-declarations))
              (declaration
               (seq-find
                (lambda (candidate)
                  (let ((order (car candidate)))
                    (eq (if (consp order) (car order) order)
                        ',package)))
                declarations)))
         (unless declaration
           (error "No declaration for %S: %S" ',package declarations))
         ,before-ready
         (require ',autoloads)
         (eval (cons 'progn (cadr declaration)) t)
         ,after-ready)))))

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

(defun yunge-test-keymap-keys (map bindings)
  "Check that each key in BINDINGS resolves in keymap MAP."
  (dolist (binding bindings)
    (should (eq (lookup-key map (kbd (car binding)))
                (cdr binding)))))

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

(provide 'yunge-test-helper)

;;; yunge-test-helper.el ends here
