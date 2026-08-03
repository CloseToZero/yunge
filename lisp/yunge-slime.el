;;; yunge-slime.el --- Common Lisp development -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

(declare-function evil-add-command-properties "evil-common")
(declare-function evil-set-initial-state "evil-core")
(declare-function slime "slime")
(declare-function slime-c-p-c-completion-at-point "slime-c-p-c")
(declare-function slime-connected-p "slime")
(declare-function slime-lisp-mode-hook "slime")
(declare-function slime-repl "slime-repl")
(declare-function slime-setup "slime")

(defvar lisp-mode-map)
(defvar slime-completion-at-point-functions)
(defvar slime-default-lisp)
(defvar slime-lisp-implementations)
(defvar slime-repl-history-file)
(defvar slime-repl-history-size)

(defconst yunge-slime-eval-bindings
  '(("b" slime-eval-buffer "buffer")
    ("d" slime-eval-defun "defun")
    ("e" slime-eval-last-expression "last expression")
    ("r" slime-eval-region "region")))

(defvar-keymap yunge-slime-eval-map
  :doc "Keymap for evaluating Common Lisp code.")

(yunge-key-define yunge-slime-eval-map
                  yunge-slime-eval-bindings)

(defconst yunge-slime-compile-bindings
  '(("d" slime-compile-defun "defun")
    ("f" slime-compile-and-load-file "file")
    ("r" slime-compile-region "region")))

(defvar-keymap yunge-slime-compile-map
  :doc "Keymap for compiling Common Lisp code.")

(yunge-key-define yunge-slime-compile-map
                  yunge-slime-compile-bindings)

(defconst yunge-slime-quit-bindings
  '(("q" slime-quit-lisp "quit Lisp")
    ("r" slime-restart-inferior-lisp "restart Lisp")))

(defvar-keymap yunge-slime-quit-map
  :doc "Keymap for quitting or restarting the Common Lisp process.")

(yunge-key-define yunge-slime-quit-map
                  yunge-slime-quit-bindings)

(defconst yunge-slime-help-bindings
  '(("a" slime-apropos "apropos")
    ("f" slime-describe-function "describe function")
    ("h" slime-documentation-lookup "HyperSpec")
    ("s" slime-describe-symbol "describe symbol")))

(defvar-keymap yunge-slime-help-map
  :doc "Keymap for Common Lisp documentation commands.")

(yunge-key-define yunge-slime-help-map
                  yunge-slime-help-bindings)

(defconst yunge-slime-macro-bindings
  '(("a" slime-macroexpand-all "expand all")
    ("o" slime-macroexpand-1 "expand once")))

(defvar-keymap yunge-slime-macro-map
  :doc "Keymap for expanding Common Lisp macros.")

(yunge-key-define yunge-slime-macro-map
                  yunge-slime-macro-bindings)

(defconst yunge-slime-repl-clear-bindings
  '(("b" slime-repl-clear-buffer "buffer")
    ("o" slime-repl-clear-output "latest output")))

(defvar-keymap yunge-slime-repl-clear-map
  :doc "Keymap for clearing SLIME REPL output.")

(yunge-key-define yunge-slime-repl-clear-map
                  yunge-slime-repl-clear-bindings)

(defconst yunge-slime-repl-command-bindings
  `(("c" ,yunge-slime-repl-clear-map "clear")
    ("h" ,yunge-slime-help-map "help")
    ("i" slime-repl-inspect "inspect")
    ("m" ,yunge-slime-macro-map "macro")
    ("p" slime-repl-set-package "set package")
    ("q" ,yunge-slime-quit-map "quit")))

(defvar-keymap yunge-slime-repl-command-map
  :doc "Keymap for SLIME REPL commands.")

(yunge-key-define yunge-slime-repl-command-map
                  yunge-slime-repl-command-bindings)

(defun yunge-slime-completion-at-point ()
  "Complete a Common Lisp symbol when SLIME is connected."
  (when (slime-connected-p)
    (slime-c-p-c-completion-at-point)))

(defun yunge-slime--default-implementation ()
  "Return the preferred available Common Lisp implementation."
  (cond
   ((executable-find "ros") 'roswell)
   ((executable-find "sbcl") 'sbcl)))

(defun yunge-slime-repl ()
  "Open the SLIME REPL, starting the configured Lisp when necessary."
  (interactive)
  (if (and (featurep 'slime) (slime-connected-p))
      (progn
        (slime-setup)
        (slime-repl))
    (call-interactively #'slime)))

(defconst yunge-slime-command-bindings
  `(("c" ,yunge-slime-compile-map "compile")
    ("e" ,yunge-slime-eval-map "evaluate")
    ("h" ,yunge-slime-help-map "help")
    ("m" ,yunge-slime-macro-map "macro")
    ("q" ,yunge-slime-quit-map "quit")
    ("r" yunge-slime-repl "REPL")
    ("s" slime "start")))

(defvar-keymap yunge-slime-command-map
  :doc "Keymap for Common Lisp development commands.")

(yunge-key-define yunge-slime-command-map
                  yunge-slime-command-bindings)

(defconst yunge-slime-navigation-bindings
  '(("gd" slime-edit-definition "definition")
    ("K" slime-describe-symbol "describe symbol")))

(defconst yunge-slime-source-bindings
  `(,@yunge-slime-navigation-bindings
    ([localleader] ,yunge-slime-command-map nil)))

(defconst yunge-slime-repl-normal-bindings
  `(,@yunge-slime-navigation-bindings
    ([localleader] ,yunge-slime-repl-command-map nil)))

(defconst yunge-slime-repl-prompt-bindings
  '(("C-j" slime-repl-next-prompt "next prompt")
    ("C-k" slime-repl-previous-prompt "previous prompt")))

(defconst yunge-slime-repl-return-bindings
  '(("RET" slime-repl-return "submit input")
    ("<return>" slime-repl-return nil)))

(defun yunge-slime--setup-source-keys ()
  "Set up Evil bindings in Common Lisp source buffers."
  (yunge-key-evil-define '(normal visual) lisp-mode-map
                         yunge-slime-source-bindings))

(defun yunge-slime--setup-repl-keys ()
  "Set up Evil bindings in SLIME REPL buffers."
  (evil-set-initial-state 'slime-repl-mode 'insert)
  (yunge-key-evil-define-minor-mode
   'normal 'slime-repl-map-mode yunge-slime-repl-normal-bindings)
  (yunge-key-evil-define-minor-mode
   '(normal insert) 'slime-repl-map-mode
   yunge-slime-repl-prompt-bindings)
  (yunge-key-evil-define-minor-mode
   '(normal insert) 'slime-repl-map-mode
   yunge-slime-repl-return-bindings))

(with-eval-after-load 'evil
  ;; SLIME resolves definitions asynchronously, so Evil must record the
  ;; origin before the command returns rather than compare locations later.
  (evil-add-command-properties 'slime-edit-definition :jump t)
  (with-eval-after-load 'lisp-mode
    (yunge-slime--setup-source-keys))
  (with-eval-after-load 'slime-repl
    (yunge-slime--setup-repl-keys)))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-slime-command-map yunge-slime-command-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-compile-map yunge-slime-compile-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-eval-map yunge-slime-eval-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-help-map yunge-slime-help-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-macro-map yunge-slime-macro-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-repl-clear-map yunge-slime-repl-clear-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-repl-command-map yunge-slime-repl-command-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-quit-map yunge-slime-quit-bindings))

(elpaca slime
  (let ((directory
         (expand-file-name "var/slime/" user-emacs-directory)))
    (make-directory directory t)
    ;; Corfu queries CAPFs automatically, including after a Lisp exits.  Keep
    ;; local filename completion then, but do not send an RPC without a Lisp.
    (setq slime-completion-at-point-functions
          '(yunge-slime-completion-at-point
            slime-filename-completion)
          slime-default-lisp (yunge-slime--default-implementation)
          slime-lisp-implementations
          '((roswell ("ros" "run"))
            (sbcl ("sbcl")))
          slime-repl-history-file
          (expand-file-name "repl-history.eld" directory)
          slime-repl-history-size 1000)
    ;; SLIME's hand-written autoload file adds this hook, but Elpaca regenerates
    ;; that file from cookies and omits the uncookied hook registration.
    (autoload 'slime-lisp-mode-hook "slime")
    (add-hook 'lisp-mode-hook #'slime-lisp-mode-hook)))

(provide 'yunge-slime)

;;; yunge-slime.el ends here
