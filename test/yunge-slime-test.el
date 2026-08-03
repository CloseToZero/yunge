;;; yunge-slime-test.el --- Common Lisp tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-get-auxiliary-keymap "evil-core")
(declare-function evil-normal-state "evil-states")
(declare-function slime-inspector-mode "slime")
(declare-function slime-mode "slime")
(declare-function slime-repl-mode "slime-repl")
(declare-function slime-setup "slime")
(declare-function slime-xref-mode "slime")
(declare-function yunge-slime--default-implementation "yunge-slime")

(defvar lisp-mode-map)
(defvar slime-completion-at-point-functions)
(defvar slime-default-lisp)
(defvar slime-lisp-implementations)
(defvar slime-repl-history-file)
(defvar slime-repl-history-size)
(defvar slime-repl-map-mode)

(yunge-test-deftest-lazy-load yunge-slime
  (slime slime-fancy slime-repl))

(ert-deftest yunge-slime-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-slime 'slime
   :setup '(setq lisp-mode-hook nil
                 slime-completion-at-point-functions nil
                 slime-default-lisp nil
                 slime-lisp-implementations nil
                 slime-repl-history-file nil
                 slime-repl-history-size 200)
   :before-ready
   '(when (or slime-completion-at-point-functions
              (memq 'slime-lisp-mode-hook lisp-mode-hook)
              slime-default-lisp
              slime-lisp-implementations
              slime-repl-history-file
              (/= slime-repl-history-size 200))
      (error "SLIME was configured before its package became ready"))
   :after-ready
   '(unless
        (and (equal slime-completion-at-point-functions
                    '(yunge-slime-completion-at-point
                      slime-filename-completion))
             (eq slime-default-lisp
                 (yunge-slime--default-implementation))
             (equal slime-lisp-implementations
                    '((roswell ("ros" "run"))
                      (sbcl ("sbcl"))))
             (equal slime-repl-history-file
                    (expand-file-name "var/slime/repl-history.eld"
                                      user-emacs-directory))
             (= slime-repl-history-size 1000)
             (memq 'slime-lisp-mode-hook lisp-mode-hook)
             (autoloadp (symbol-function 'slime-lisp-mode-hook))
             (file-directory-p
              (expand-file-name "var/slime/" user-emacs-directory)))
      (error "SLIME configuration was not applied"))))

(ert-deftest yunge-slime-selects-an-available-default-lisp ()
  (yunge-test-load-package-config 'yunge-slime)
  (let (available)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (and (member program available) program))))
      (setq available '("ros" "sbcl"))
      (should (eq (yunge-slime--default-implementation) 'roswell))
      (setq available '("sbcl"))
      (should (eq (yunge-slime--default-implementation) 'sbcl)))))

(ert-deftest yunge-slime-skips-remote-completion-while-disconnected ()
  (require 'slime-autoloads)
  (yunge-test-load-package-config 'yunge-slime)
  (require 'slime)
  (slime-setup)
  (with-temp-buffer
    (lisp-mode)
    (insert "defun")
    (should-not
     (run-hook-with-args-until-success
      'completion-at-point-functions))))

(ert-deftest yunge-slime-binds-source-commands ()
  (yunge-test-enable-evil)
  (require 'slime-autoloads)
  (yunge-test-load-package-config 'yunge-slime)
  (require 'slime)
  (require 'which-key)
  (should
   (eq (lookup-key
        (evil-get-auxiliary-keymap lisp-mode-map 'normal)
        [localleader])
       yunge-slime-command-map))
  ;; Starting SLIME must remain available in a Common Lisp buffer before
  ;; `slime-mode' has been enabled or a Lisp process has been started.
  (let ((lisp-mode-hook nil))
    (with-temp-buffer
      (lisp-mode)
      (should-not (bound-and-true-p slime-mode))
      (yunge-test-evil-keys
       'normal
       '(("SPC m s" . slime)
         ("SPC m r" . yunge-slime-repl)
         ("SPC m h a" . slime-apropos)
         ("SPC m h f" . slime-describe-function)
         ("SPC m h h" . slime-documentation-lookup)
         ("SPC m h s" . slime-describe-symbol)
         ("SPC m m a" . slime-macroexpand-all)
         ("SPC m m o" . slime-macroexpand-1)
         ("SPC m q q" . slime-quit-lisp)
         ("SPC m q r" . slime-restart-inferior-lisp)
         ("SPC m e b" . slime-eval-buffer)
         ("SPC m e d" . slime-eval-defun)
         ("SPC m e e" . slime-eval-last-expression)
         ("SPC m e r" . slime-eval-region)
         ("SPC m c d" . slime-compile-defun)
         ("SPC m c f" . slime-compile-and-load-file)
         ("SPC m c r" . slime-compile-region)
         ("gd" . slime-edit-definition)
         ("K" . slime-describe-symbol)))
      (yunge-test-which-key-prefix
       "SPC m"
       '(("c" nil "+compile")
         ("e" nil "+evaluate")
         ("h" nil "+help")
         ("m" nil "+macro")
         ("q" nil "+quit")
         ("r" nil "REPL")
         ("s" nil "start")))
      (yunge-test-which-key-prefix
       "SPC m e"
       yunge-slime-eval-bindings)
      (yunge-test-which-key-prefix
       "SPC m c"
       yunge-slime-compile-bindings)
      (yunge-test-which-key-prefix
       "SPC m h"
       yunge-slime-help-bindings)
      (yunge-test-which-key-prefix
       "SPC m m"
       yunge-slime-macro-bindings)
      (yunge-test-which-key-prefix
       "SPC m q"
       yunge-slime-quit-bindings))))

(ert-deftest yunge-slime-integrates-repl-with-evil ()
  (yunge-test-enable-evil)
  (require 'slime-autoloads)
  (yunge-test-load-package-config 'yunge-slime)
  (require 'slime)
  (require 'slime-repl)
  (require 'which-key)
  (with-temp-buffer
    (slime-repl-mode)
    (should slime-repl-map-mode)
    (yunge-test-evil-keys
     'insert
     '(("M-p" . slime-repl-previous-input)
       ("M-n" . slime-repl-next-input)
       ("RET" . slime-repl-return)
       ("<return>" . slime-repl-return)
       ("C-j" . slime-repl-next-prompt)
       ("C-k" . slime-repl-previous-prompt)))
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("M-p" . slime-repl-previous-input)
       ("M-n" . slime-repl-next-input)
       ("RET" . slime-repl-return)
       ("<return>" . slime-repl-return)
       ("C-j" . slime-repl-next-prompt)
       ("C-k" . slime-repl-previous-prompt)
       ("gd" . slime-edit-definition)
       ("K" . slime-describe-symbol)
       ("SPC m c b" . slime-repl-clear-buffer)
       ("SPC m c o" . slime-repl-clear-output)
       ("SPC m h s" . slime-describe-symbol)
       ("SPC m i" . slime-repl-inspect)
       ("SPC m m o" . slime-macroexpand-1)
       ("SPC m p" . slime-repl-set-package)
       ("SPC m q q" . slime-quit-lisp)
       ("SPC m q r" . slime-restart-inferior-lisp)))
    (yunge-test-which-key-prefix
     "SPC m"
     '(("c" nil "+clear")
       ("h" nil "+help")
       ("i" nil "inspect")
       ("m" nil "+macro")
       ("p" nil "set package")
       ("q" nil "+quit")))
    (yunge-test-which-key-prefix
     "SPC m c"
     yunge-slime-repl-clear-bindings)))

(ert-deftest yunge-slime-integrates-browsing-views-with-evil ()
  (yunge-test-enable-evil)
  (require 'slime-autoloads)
  (yunge-test-load-package-config 'yunge-slime)
  (require 'slime)
  (require 'which-key)

  (yunge-test-evil-normal-keys
   'slime-apropos-mode
   '(("RET" . push-button)
     ("q" . quit-window)
     ("C-j" . slime-apropos-next-symbol)
     ("C-k" . slime-apropos-previous-symbol)
     ("g]" . forward-button)
     ("g[" . backward-button)
     ("<tab>" . forward-button)
     ("S-TAB" . backward-button)))

  (yunge-test-evil-normal-keys
   'slime-inspector-mode
   '(("RET" . slime-inspector-operate-on-point)
     ("q" . slime-inspector-quit)
     ("C-j" . slime-inspector-next-inspectable-object)
     ("C-k" . slime-inspector-previous-inspectable-object)
     ("gh" . slime-inspector-pop)
     ("gl" . slime-inspector-next)
     ("gf" . slime-inspector-show-source)
     ("gr" . slime-inspector-reinspect)
     ("K" . slime-inspector-describe)
     ("SPC m e" . slime-inspector-eval)
     ("SPC m f" . slime-inspector-fetch-all)
     ("SPC m h" . slime-inspector-history)
     ("SPC m p" . slime-inspector-pprint)
     ("SPC m v" . slime-inspector-toggle-verbose)))

  (let ((lisp-mode-hook nil))
    (yunge-test-evil-normal-keys
     'slime-xref-mode
     '(("RET" . slime-goto-xref)
       ("q" . quit-window)
       ("C-j" . slime-xref-next-line)
       ("C-k" . slime-xref-prev-line)
       ("gf" . slime-show-xref)
       ("SPC m c" . slime-recompile-xref)
       ("SPC m C" . slime-recompile-all-xrefs))))

  (with-temp-buffer
    (slime-inspector-mode)
    (yunge-test-which-key-prefix
     "SPC m" yunge-slime-inspector-command-bindings))

  (let ((lisp-mode-hook nil))
    (with-temp-buffer
      (slime-xref-mode)
      (yunge-test-which-key-prefix
       "SPC m" yunge-slime-xref-command-bindings))))

;;; yunge-slime-test.el ends here
