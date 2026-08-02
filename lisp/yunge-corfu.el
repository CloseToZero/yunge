;;; yunge-corfu.el --- In-buffer completion -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function evil-get-minor-mode-keymap "evil-core")
(declare-function global-corfu-mode "corfu")

(defvar completion-in-region-mode)
(defvar corfu-map)
(defvar corfu-mode)
(defvar corfu-preview-current)

(defconst yunge-corfu-insert-bindings
  '(("C-n" corfu-next "next candidate")
    ("C-p" corfu-previous "previous candidate")
    ("TAB" corfu-complete "complete candidate")
    ("<tab>" corfu-complete nil)))

(define-minor-mode yunge-corfu--completion-mode
  "Give Corfu's active completion session precedence over Evil."
  :init-value nil
  :lighter nil)

(defun yunge-corfu--sync-completion-mode ()
  "Track whether Corfu owns the active completion session."
  (yunge-corfu--completion-mode
   (if (and completion-in-region-mode corfu-mode) 1 -1)))

(defun yunge-corfu--setup-keys ()
  "Set up bindings for Corfu."
  ;; Completion stays on TAB; RET keeps its editing or shell meaning.
  (keymap-unset corfu-map "RET")
  (add-hook 'completion-in-region-mode-hook
            #'yunge-corfu--sync-completion-mode))

(with-eval-after-load 'corfu
  (yunge-corfu--setup-keys))

(with-eval-after-load 'evil
  (with-eval-after-load 'corfu
    ;; This adapter follows Corfu's popup, so these bindings become active
    ;; without refreshing all of Evil's keymaps for every completion session.
    (yunge-key-evil-define-minor-mode
     'insert 'yunge-corfu--completion-mode
     yunge-corfu-insert-bindings)))

(with-eval-after-load 'which-key
  (with-eval-after-load 'evil
    (with-eval-after-load 'corfu
      (yunge-key-which-key-describe-map
       (evil-get-minor-mode-keymap 'insert 'yunge-corfu--completion-mode)
       yunge-corfu-insert-bindings))))

(elpaca corfu
  (setq tab-always-indent 'complete
        corfu-preview-current nil)
  (global-corfu-mode 1))

(provide 'yunge-corfu)

;;; yunge-corfu.el ends here
