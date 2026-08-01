;;; yunge-evil.el --- Evil modal editing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-jump)
(require 'yunge-minibuffer)

(declare-function evil-make-intercept-map "evil-core")

(defgroup yunge nil
  "Personal Emacs configuration."
  :group 'emacs)

(defvar-keymap yunge-leader-map
  :doc "Global leader map.")

(defvar-keymap yunge-buffer-map
  :doc "Global buffer command map.")

(defvar-keymap yunge-file-map
  :doc "Global file command map.")

(defvar-keymap yunge-jump-map
  :doc "Global jump command map.")

(defvar-keymap yunge-quit-map
  :doc "Global quit command map.")

(defvar-keymap yunge-search-map
  :doc "Global search command map.")

(defvar-keymap yunge-window-map
  :doc "Global window command map.")

(defvar-keymap yunge-evil--empty-localleader-map
  :doc "Fallback map when the current mode has no local leader.")

(defconst yunge-buffer-bindings
  '(("b" switch-to-buffer "switch buffer")
    ("n" next-buffer "next buffer")
    ("p" previous-buffer "previous buffer")
    ("q" kill-current-buffer "close buffer")
    ("r" revert-buffer "revert buffer")))

(defconst yunge-file-bindings
  '(("d" dired "open directory")
    ("f" find-file "find file")
    ("s" save-buffer "save file")))

(defconst yunge-quit-bindings
  '(("f" delete-frame "delete frame")
    ("q" save-buffers-kill-terminal "quit")
    ("r" restart-emacs "restart Emacs")))

(defconst yunge-leader-map-bindings
  `(("SPC" execute-extended-command "execute command")
    ("b" ,yunge-buffer-map "buffer")
    ("f" ,yunge-file-map "file")
    ("j" ,yunge-jump-map "jump")
    ("q" ,yunge-quit-map "quit")
    ("s" ,yunge-search-map "search")
    ("w" ,yunge-window-map "window")))

(yunge-key-define yunge-leader-map
                  yunge-leader-map-bindings)
(yunge-key-define yunge-buffer-map yunge-buffer-bindings)
(yunge-key-define yunge-file-map yunge-file-bindings)
(yunge-key-define yunge-quit-map yunge-quit-bindings)

(defun yunge-evil--localleader-binding (_binding)
  "Return the current mode's labelled local leader binding."
  (cons "mode"
        (or (key-binding [localleader])
            yunge-evil--empty-localleader-map)))

(keymap-set
 yunge-leader-map "m"
 '(menu-item "mode" nil :filter yunge-evil--localleader-binding))

(defvar-keymap yunge-leader-mode-map)

(define-minor-mode yunge-leader-mode
  "Keep the global leader above ordinary mode-specific Evil maps."
  :global t
  :group 'yunge
  :keymap yunge-leader-mode-map)

(defconst yunge-evil-leader-bindings
  `(("SPC" ,yunge-leader-map nil)))

(defun yunge-evil--setup-leader ()
  "Set up the leader after Evil has loaded."
  (yunge-key-evil-define '(normal visual)
                         yunge-leader-mode-map
                         yunge-evil-leader-bindings)
  ;; Mode-specific Evil maps must not replace the global leader.
  (evil-make-intercept-map yunge-leader-mode-map 'normal t)
  (evil-make-intercept-map yunge-leader-mode-map 'visual t)
  (yunge-leader-mode 1))

(with-eval-after-load 'evil
  (yunge-evil--setup-leader))

(with-eval-after-load 'which-key
  (yunge-key-which-key-describe-map
   yunge-leader-map yunge-leader-map-bindings)
  (yunge-key-which-key-describe-map
   yunge-buffer-map yunge-buffer-bindings)
  (yunge-key-which-key-describe-map
   yunge-file-map yunge-file-bindings)
  (yunge-key-which-key-describe-map
   yunge-quit-map yunge-quit-bindings))

(elpaca evil
  ;; Keep Evil's command semantics, but own all mode-specific bindings.
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-search-module 'evil-search
        evil-symbol-word-search t
        evil-undo-system 'undo-redo
        evil-want-C-u-delete t
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil)
  (evil-mode 1))

(provide 'yunge-evil)

;;; yunge-evil.el ends here
