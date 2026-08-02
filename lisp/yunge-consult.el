;;; yunge-consult.el --- Search and navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-evil)

(declare-function evil-add-command-properties "evil-common")

(defvar evil-command-line-map)
(defvar evil-eval-map)

(defconst yunge-consult-file-bindings
  '(("r" consult-recent-file "find recent file")))

(defconst yunge-consult-search-bindings
  '(("b" consult-line "search buffer")
    ("B" consult-line-multi "search project buffers")
    ("p" consult-ripgrep "search project")))

(defconst yunge-consult-jump-bindings
  '(("b" consult-bookmark "jump to bookmark")
    ("i" consult-imenu "jump to symbol")))

(defconst yunge-consult-history-bindings
  '(("M-r" consult-history "select history")))

(defconst yunge-consult-remap-bindings
  '(([remap switch-to-buffer] consult-buffer nil)
    ([remap imenu] consult-imenu nil)))

(defconst yunge-consult-jump-commands
  '(consult-bookmark consult-imenu consult-line consult-line-multi
    consult-recent-file consult-ripgrep))

(defun yunge-consult--setup-keys ()
  "Set up Consult command and remap bindings."
  (yunge-key-define yunge-file-map
                    yunge-consult-file-bindings)
  (yunge-key-define yunge-search-map
                    yunge-consult-search-bindings)
  (yunge-key-define yunge-jump-map
                    yunge-consult-jump-bindings)
  (yunge-key-define minibuffer-local-map
                    yunge-consult-history-bindings)
  (yunge-key-define global-map yunge-consult-remap-bindings))

(defun yunge-consult--setup-evil-history-keys ()
  "Set up history selection in Evil command-line maps."
  (dolist (map (list evil-command-line-map evil-eval-map))
    (yunge-key-define map yunge-consult-history-bindings)))

(defun yunge-consult--setup-evil ()
  "Give Consult navigation commands Evil jump semantics."
  (dolist (command yunge-consult-jump-commands)
    (evil-add-command-properties command :jump t :repeat nil)))

(defun yunge-consult--describe-keys ()
  "Describe Consult leader bindings to Which-Key."
  (yunge-key-add-which-key-descriptions
   yunge-file-map yunge-consult-file-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-search-map yunge-consult-search-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-jump-map yunge-consult-jump-bindings)
  (yunge-key-add-which-key-descriptions
   minibuffer-local-map yunge-consult-history-bindings))

(defun yunge-consult--describe-evil-history-keys ()
  "Describe history selection in Evil command-line maps."
  (dolist (map (list evil-command-line-map evil-eval-map))
    (yunge-key-add-which-key-descriptions
     map yunge-consult-history-bindings)))

(elpaca consult
  (yunge-consult--setup-keys)
  (with-eval-after-load 'evil
    (yunge-consult--setup-evil)
    (yunge-consult--setup-evil-history-keys))
  (with-eval-after-load 'which-key
    (yunge-consult--describe-keys)
    (with-eval-after-load 'evil
      (yunge-consult--describe-evil-history-keys)))
  ;; Use quoted `eval-after-load' instead of `with-eval-after-load' so the
  ;; non-autoloaded `consult-customize' macro is expanded only after Consult
  ;; has loaded.
  (eval-after-load 'consult
    '(consult-customize
      consult-ripgrep :preview-key '(:debounce 0.4 any))))

(provide 'yunge-consult)

;;; yunge-consult.el ends here
