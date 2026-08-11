;;; yunge-slime.el --- Common Lisp development -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-state)

(require 'yunge-evil)
(require 'yunge-key)

(declare-function evil-add-command-properties "evil-common")
(declare-function evil-set-initial-state "evil-core")
(declare-function slime "slime")
(declare-function slime-c-p-c-completion-at-point "slime-c-p-c")
(declare-function slime-connected-p "slime")
(declare-function slime-eval "slime")
(declare-function slime-get-region-properties "slime")
(declare-function slime-lisp-mode-hook "slime")
(declare-function slime-repl "slime-repl")
(declare-function slime-scratch "slime-scratch")
(declare-function slime-setup "slime")
(declare-function slime-trace-dialog-fetch-traces "slime-trace-dialog")
(declare-function slime-update-threads-buffer "slime")

(defvar lisp-mode-map)
(defvar slime-completion-at-point-functions)
(defvar slime-connection-list-mode-map)
(defvar slime-default-lisp)
(defvar slime-apropos-mode-map)
(defvar slime-inspector-mode-map)
(defvar slime-lisp-implementations)
(defvar slime-repl-history-file)
(defvar slime-repl-history-size)
(defvar slime-thread-control-mode-map)
(defvar slime-trace-dialog--detail-mode-map)
(defvar slime-trace-dialog-mode-map)
(defvar slime-xref-mode-map)
(defvar sldb-mode-map)

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

(defconst yunge-slime-process-bindings
  '(("c" slime-list-connections "connections")
    ("q" slime-quit-lisp "quit Lisp")
    ("r" slime-restart-inferior-lisp "restart Lisp")
    ("t" slime-list-threads "threads")))

(defvar-keymap yunge-slime-process-map
  :doc "Keymap for managing Common Lisp processes and threads.")

(yunge-key-define yunge-slime-process-map
                  yunge-slime-process-bindings)

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

(defconst yunge-slime-trace-bindings
  '(("d" slime-trace-dialog "dialog")
    ("t" slime-trace-dialog-toggle-trace "toggle trace")
    ("T" slime-trace-dialog-toggle-complex-trace
     "toggle complex trace")))

(defvar-keymap yunge-slime-trace-map
  :doc "Keymap for tracing Common Lisp calls.")

(yunge-key-define yunge-slime-trace-map
                  yunge-slime-trace-bindings)

(defconst yunge-slime-repl-clear-bindings
  '(("b" slime-repl-clear-buffer "buffer")
    ("o" slime-repl-clear-output "latest output")))

(defvar-keymap yunge-slime-repl-clear-map
  :doc "Keymap for clearing SLIME REPL output.")

(yunge-key-define yunge-slime-repl-clear-map
                  yunge-slime-repl-clear-bindings)

(defconst yunge-slime-repl-command-bindings
  `(("b" yunge-slime-scratch "scratch")
    ("c" ,yunge-slime-repl-clear-map "clear")
    ("h" ,yunge-slime-help-map "help")
    ("i" slime-repl-inspect "inspect")
    ("m" ,yunge-slime-macro-map "macro")
    ("p" slime-repl-set-package "set package")
    ("q" ,yunge-slime-process-map "process")
    ("t" ,yunge-slime-trace-map "trace")))

(defvar-keymap yunge-slime-repl-command-map
  :doc "Keymap for SLIME REPL commands.")

(yunge-key-define yunge-slime-repl-command-map
                  yunge-slime-repl-command-bindings)

(defconst yunge-slime-inspector-command-bindings
  '(("e" slime-inspector-eval "evaluate")
    ("f" slime-inspector-fetch-all "fetch all")
    ("h" slime-inspector-history "history")
    ("p" slime-inspector-pprint "pretty print")
    ("v" slime-inspector-toggle-verbose "toggle verbose")))

(defvar-keymap yunge-slime-inspector-command-map
  :doc "Keymap for SLIME Inspector commands.")

(yunge-key-define yunge-slime-inspector-command-map
                  yunge-slime-inspector-command-bindings)

(defconst yunge-slime-xref-command-bindings
  '(("c" slime-recompile-xref "recompile reference")
    ("C" slime-recompile-all-xrefs "recompile all")))

(defvar-keymap yunge-slime-xref-command-map
  :doc "Keymap for SLIME cross-reference commands.")

(yunge-key-define yunge-slime-xref-command-map
                  yunge-slime-xref-command-bindings)

(defun yunge-slime-completion-at-point ()
  "Complete a Common Lisp symbol when SLIME is connected."
  (when (slime-connected-p)
    (slime-c-p-c-completion-at-point)))

(defun yunge-slime--default-implementation ()
  "Return the preferred available Common Lisp implementation."
  (cond
   ((executable-find "ros") 'roswell)
   ((executable-find "sbcl") 'sbcl)))

(defun yunge-slime--enable-existing-lisp-buffers ()
  "Enable SLIME in existing Common Lisp buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'lisp-mode)
        (slime-lisp-mode-hook)))))

(defun yunge-slime-repl ()
  "Open the SLIME REPL, starting the configured Lisp when necessary."
  (interactive)
  (if (and (featurep 'slime) (slime-connected-p))
      (progn
        (slime-setup)
        (slime-repl))
    (call-interactively #'slime)))

(defun yunge-slime-scratch ()
  "Open the SLIME scratch buffer."
  (interactive)
  (require 'slime-scratch)
  (slime-scratch))

(defconst yunge-slime-command-bindings
  `(("b" yunge-slime-scratch "scratch")
    ("c" ,yunge-slime-compile-map "compile")
    ("e" ,yunge-slime-eval-map "evaluate")
    ("h" ,yunge-slime-help-map "help")
    ("m" ,yunge-slime-macro-map "macro")
    ("q" ,yunge-slime-process-map "process")
    ("r" yunge-slime-repl "REPL")
    ("s" slime "start")
    ("t" ,yunge-slime-trace-map "trace")))

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

(defconst yunge-slime-apropos-normal-bindings
  `(("RET" push-button "activate button")
    ("q" quit-window "quit")
    ("C-j" slime-apropos-next-symbol "next symbol")
    ("C-k" slime-apropos-previous-symbol "previous symbol")
    ,@yunge-key-button-navigation-bindings))

(defconst yunge-slime-inspector-normal-bindings
  `(("RET" slime-inspector-operate-on-point "inspect or act")
    ("q" slime-inspector-quit "quit")
    ("C-j" slime-inspector-next-inspectable-object "next object")
    ("C-k" slime-inspector-previous-inspectable-object "previous object")
    ("gh" slime-inspector-pop "history back")
    ("gl" slime-inspector-next "history forward")
    ("gf" slime-inspector-show-source "visit source")
    ("gr" slime-inspector-reinspect "refresh")
    ("K" slime-inspector-describe "describe object")
    ;; Inspector buffers also enable the generic popup minor mode.  Remap its
    ;; commands so quitting still releases the remote inspector state and
    ;; describing still acts on the inspected object.
    ([remap quit-window] slime-inspector-quit nil)
    ([remap slime-describe-symbol] slime-inspector-describe nil)
    ([localleader] ,yunge-slime-inspector-command-map nil)))

(defconst yunge-slime-xref-normal-bindings
  `(("RET" slime-goto-xref "visit")
    ("q" quit-window "quit")
    ("C-j" slime-xref-next-line "next reference")
    ("C-k" slime-xref-prev-line "previous reference")
    ("gf" slime-show-xref "show source")
    ([localleader] ,yunge-slime-xref-command-map nil)))

(defconst yunge-slime-debugger-normal-bindings
  '(("RET" sldb-default-action "invoke at point")
    ("q" sldb-quit "quit to top level")
    ("a" sldb-abort "abort")
    ("c" sldb-continue "continue")
    ("s" sldb-step "step into")
    ("n" sldb-next "step over")
    ("o" sldb-out "step out")
    ("b" sldb-break-on-return "break on return")
    ("C-j" sldb-down "next frame")
    ("C-k" sldb-up "previous frame")
    ("M-j" sldb-details-down "next frame with details")
    ("M-k" sldb-details-up "previous frame with details")
    ("gg" sldb-beginning-of-backtrace "first frame")
    ("G" sldb-end-of-backtrace "last frame")
    ("]]" sldb-cycle "cycle restarts/backtrace")
    ("gf" sldb-show-source "show source")
    ("za" sldb-toggle-details "toggle frame details")
    ("<tab>" sldb-toggle-details nil)
    ("e" sldb-eval-in-frame "evaluate in frame")
    ("E" sldb-pprint-eval-in-frame "pretty-print evaluation")
    ("i" sldb-inspect-in-frame "inspect in frame")
    ("D" sldb-disassemble "disassemble")
    ("r" sldb-restart-frame "restart frame")
    ("R" sldb-return-from-frame "return from frame")
    ("I" sldb-invoke-restart-by-name "invoke named restart")
    ("0" sldb-invoke-restart-0 "invoke restart 0")
    ("1" sldb-invoke-restart-1 "invoke restart 1")
    ("2" sldb-invoke-restart-2 "invoke restart 2")
    ("3" sldb-invoke-restart-3 "invoke restart 3")
    ("4" sldb-invoke-restart-4 "invoke restart 4")
    ("5" sldb-invoke-restart-5 "invoke restart 5")
    ("6" sldb-invoke-restart-6 "invoke restart 6")
    ("7" sldb-invoke-restart-7 "invoke restart 7")
    ("8" sldb-invoke-restart-8 "invoke restart 8")
    ("9" sldb-invoke-restart-9 "invoke restart 9")))

(defconst yunge-slime-popup-normal-bindings
  `(("q" quit-window "quit")
    ,@yunge-slime-navigation-bindings))

(defconst yunge-slime-macroexpansion-normal-bindings
  '(("gr" slime-macroexpand-again "repeat expansion")
    ("u" slime-macroexpand-undo "undo expansion")
    ;; SLIME's ordinary minor-mode remaps cannot override Evil's auxiliary
    ;; localleader map, so repeat them at the same precedence.
    ([remap slime-macroexpand-1] slime-macroexpand-1-inplace nil)
    ([remap slime-macroexpand-all] slime-macroexpand-all-inplace nil)))

(defconst yunge-slime-trace-copy-bindings
  '(("r" slime-trace-dialog-copy-down-to-repl "value to REPL")))

(defvar-keymap yunge-slime-trace-copy-map
  :doc "Keymap for copying SLIME trace values.")

(yunge-key-define yunge-slime-trace-copy-map
                  yunge-slime-trace-copy-bindings)

(defconst yunge-slime-trace-dialog-command-bindings
  '(("a" slime-trace-dialog-autofollow-mode "toggle autofollow")
    ("d" slime-trace-dialog-hide-details-mode "toggle details")))

(defvar-keymap yunge-slime-trace-dialog-command-map
  :doc "Keymap for SLIME Trace Dialog commands.")

(yunge-key-define yunge-slime-trace-dialog-command-map
                  yunge-slime-trace-dialog-command-bindings)

(defun yunge-slime-trace-dialog-fetch-all ()
  "Fetch every outstanding SLIME Trace Dialog entry."
  (interactive)
  (slime-trace-dialog-fetch-traces t))

(defconst yunge-slime-trace-dialog-normal-bindings
  `(("RET" push-button "activate button")
    ("q" quit-window "quit")
    ("C-j" slime-trace-dialog-next-button "next item")
    ("C-k" slime-trace-dialog-prev-button "previous item")
    ("gr" slime-trace-dialog-fetch-status "refresh status")
    ("f" slime-trace-dialog-fetch-traces "fetch next batch")
    ("F" yunge-slime-trace-dialog-fetch-all "fetch all")
    ("x" slime-trace-dialog-clear-fetched-traces "clear all traces")
    ("y" ,yunge-slime-trace-copy-map "copy")
    ,@yunge-key-button-navigation-bindings
    ([localleader] ,yunge-slime-trace-dialog-command-map nil)))

(defconst yunge-slime-trace-detail-normal-bindings
  `(("RET" push-button "activate button")
    ("q" quit-window "quit")
    ("C-j" slime-trace-dialog-next-button "next item")
    ("C-k" slime-trace-dialog-prev-button "previous item")
    ("y" ,yunge-slime-trace-copy-map "copy")
    ,@yunge-key-button-navigation-bindings))

(defun yunge-slime-thread-kill ()
  "Kill the thread at point or threads covered by the active region."
  (interactive)
  (let* ((active (use-region-p))
         (start (and active (region-beginning)))
         (end (and active (region-end)))
         (threads
          (if active
              (delete-dups
               (delq nil
                     (slime-get-region-properties
                      'thread-index start
                      ;; Emacs excludes the region end, whereas SLIME's
                      ;; helper treats its END argument as inclusive.
                      (if (> end start) (1- end) start))))
            (when-let* ((thread
                         (get-text-property (point) 'thread-index)))
              (list thread)))))
    (unless threads
      (user-error "No thread selected"))
    (slime-eval `(cl:mapc 'swank:kill-nth-thread ',threads))
    (slime-update-threads-buffer)))

(defconst yunge-slime-thread-normal-bindings
  '(("RET" slime-thread-debug "debug thread")
    ("C-j" next-line "next thread")
    ("C-k" previous-line "previous thread")
    ("gr" slime-update-threads-buffer "refresh")
    ("q" slime-quit-threads-buffer "quit")
    ("a" slime-thread-attach "attach")
    ("x" yunge-slime-thread-kill "kill thread")
    ([remap quit-window] slime-quit-threads-buffer nil)))

(defconst yunge-slime-thread-visual-bindings
  '(("x" yunge-slime-thread-kill "kill threads")))

(defconst yunge-slime-connection-normal-bindings
  '(("RET" slime-connection-list-make-default "make default")
    ("C-j" next-line "next connection")
    ("C-k" previous-line "previous connection")
    ("gr" slime-update-connection-list "refresh")
    ("q" quit-window "quit")
    ("r" slime-restart-connection-at-point "restart connection")
    ("x" slime-quit-connection-at-point "quit connection")))

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

(defun yunge-slime--setup-view-keys ()
  "Set up Evil bindings in SLIME read-only views."
  (dolist (mode '(slime-apropos-mode
                  slime-connection-list-mode
                  slime-inspector-mode
                  slime-thread-control-mode
                  slime-xref-mode
                  sldb-mode))
    (evil-set-initial-state mode 'normal))
  (yunge-key-evil-define 'normal slime-apropos-mode-map
                         yunge-slime-apropos-normal-bindings)
  (yunge-key-evil-define 'normal slime-inspector-mode-map
                         yunge-slime-inspector-normal-bindings)
  (yunge-key-evil-define 'normal slime-xref-mode-map
                         yunge-slime-xref-normal-bindings)
  (yunge-key-evil-define 'normal sldb-mode-map
                         yunge-slime-debugger-normal-bindings)
  (yunge-key-evil-define-minor-mode
   'normal 'slime-popup-buffer-mode
   yunge-slime-popup-normal-bindings)
  (yunge-key-evil-define-minor-mode
   'normal 'slime-macroexpansion-minor-mode
   yunge-slime-macroexpansion-normal-bindings)
  (yunge-key-evil-define 'normal slime-thread-control-mode-map
                         yunge-slime-thread-normal-bindings)
  (yunge-key-evil-define 'visual slime-thread-control-mode-map
                         yunge-slime-thread-visual-bindings)
  (yunge-key-evil-define 'normal slime-connection-list-mode-map
                         yunge-slime-connection-normal-bindings))

(defun yunge-slime--setup-trace-keys ()
  "Set up Evil bindings in SLIME trace views."
  (dolist (mode '(slime-trace-dialog-mode
                  slime-trace-dialog--detail-mode))
    (evil-set-initial-state mode 'normal))
  (yunge-key-evil-define 'normal slime-trace-dialog-mode-map
                         yunge-slime-trace-dialog-normal-bindings)
  (yunge-key-evil-define 'normal slime-trace-dialog--detail-mode-map
                         yunge-slime-trace-detail-normal-bindings))

(with-eval-after-load 'evil
  ;; SLIME resolves definitions asynchronously, so Evil must record the
  ;; origin before the command returns rather than compare locations later.
  (evil-add-command-properties 'slime-edit-definition :jump t)
  (with-eval-after-load 'lisp-mode
    (yunge-slime--setup-source-keys))
  (with-eval-after-load 'slime
    (yunge-slime--setup-view-keys))
  (with-eval-after-load 'slime-trace-dialog
    (yunge-slime--setup-trace-keys))
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
   yunge-slime-trace-map yunge-slime-trace-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-repl-clear-map yunge-slime-repl-clear-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-repl-command-map yunge-slime-repl-command-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-inspector-command-map
   yunge-slime-inspector-command-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-xref-command-map yunge-slime-xref-command-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-process-map yunge-slime-process-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-trace-copy-map yunge-slime-trace-copy-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-slime-trace-dialog-command-map
   yunge-slime-trace-dialog-command-bindings))

(elpaca slime
  (let ((directory
         (yunge-var-subdirectory "slime")))
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
    (add-hook 'lisp-mode-hook #'slime-lisp-mode-hook)
    (yunge-slime--enable-existing-lisp-buffers)))

(provide 'yunge-slime)

;;; yunge-slime.el ends here
