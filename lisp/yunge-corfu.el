;;; yunge-corfu.el --- In-buffer completion -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function corfu-quit "corfu")
(declare-function global-corfu-mode "corfu")
(declare-function pcomplete-from-help "pcomplete")

(defvar corfu-auto)
(defvar corfu-auto-delay)
(defvar corfu-auto-prefix)
(defvar corfu-cycle)
(defvar corfu-map)
(defvar corfu-mode)
(defvar corfu-preview-current)
(defvar text-mode-ispell-word-completion)
(defvar vc-git-program)

;; Ispell launches an external dictionary search for prose completion, which
;; is too costly to run automatically while typing.
(setq text-mode-ispell-word-completion nil)

(defun yunge-corfu--pcomplete-git-short-help (arguments)
  "Use non-interactive Git help when completing subcommand options.
Git for Windows opens a browser for `git help SUBCOMMAND', while
`git SUBCOMMAND -h' prints the option summary that `pcomplete-from-help'
expects.  Filter only that exact command shape and preserve all parser
keyword arguments."
  (let ((command (car arguments)))
    (if (and (listp command)
             (= (length command) 3)
             (equal (car command) vc-git-program)
             (equal (cadr command) "help")
             (stringp (caddr command))
             (not (string-prefix-p "-" (caddr command))))
        (cons (list (car command) (caddr command) "-h")
              (cdr arguments))
      arguments)))

(with-eval-after-load 'pcmpl-git
  (advice-add 'pcomplete-from-help :filter-args
              #'yunge-corfu--pcomplete-git-short-help))

(defconst yunge-corfu-popup-bindings
  '(("C-j" corfu-next "next candidate")
    ("C-k" corfu-previous "previous candidate")
    ("TAB" corfu-complete "complete candidate")
    ("<tab>" corfu-complete nil)))

(defvar-keymap yunge-corfu--popup-map
  :doc "Keymap active while a Corfu popup owns completion.")

(yunge-key-define yunge-corfu--popup-map yunge-corfu-popup-bindings)

(defvar-local yunge-corfu--popup-keys-active nil
  "Non-nil while this buffer owns an active Corfu popup.")

(defvar yunge-corfu--emulation-map-alist
  `((yunge-corfu--popup-keys-active . ,yunge-corfu--popup-map)))

(defun yunge-corfu--start-popup-keys (&rest _)
  "Give the current Corfu popup's keys precedence over Evil."
  (setq-local yunge-corfu--popup-keys-active t)
  (setq emulation-mode-map-alists
        (cons 'yunge-corfu--emulation-map-alist
              (remove 'yunge-corfu--emulation-map-alist
                      emulation-mode-map-alists))))

(defun yunge-corfu--finish-popup-keys (buffer)
  "Restore the ordinary keys in Corfu popup owner BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq yunge-corfu--popup-keys-active nil))))

(defun yunge-corfu--quit-popup-on-mode-disable ()
  "End this buffer's Corfu popup when `corfu-mode' is disabled."
  (when (and yunge-corfu--popup-keys-active (not corfu-mode))
    (corfu-quit)))

(with-eval-after-load 'corfu
  ;; Return belongs to the surrounding interface; Tab accepts completion.
  (keymap-unset corfu-map "RET")
  (advice-add 'corfu--setup :after #'yunge-corfu--start-popup-keys)
  (advice-add 'corfu--teardown :after #'yunge-corfu--finish-popup-keys)
  (add-hook 'corfu-mode-hook #'yunge-corfu--quit-popup-on-mode-disable))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-corfu--popup-map yunge-corfu-popup-bindings))

(elpaca corfu
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preview-current nil)
  (global-corfu-mode 1))

(provide 'yunge-corfu)

;;; yunge-corfu.el ends here
