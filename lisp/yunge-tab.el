;;; yunge-tab.el --- Tab-based task layouts -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)

;; Keep task layouts available without adding permanent frame chrome.
(setq tab-bar-show nil)

(defvar-keymap yunge-tab-map
  :doc "Global tab command map.")

(defface yunge-tab-mode-line-name
  '((t :inherit mode-line-emphasis))
  "Face for the current tab name in the mode line."
  :group 'yunge)

(defun yunge-tab--mode-line ()
  "Return the current tab name for the selected window's mode line."
  (when (mode-line-window-selected-p)
    (let ((tabs (tab-bar-tabs)))
      (when (cdr tabs)
        (when-let* ((tab (assq 'current-tab tabs))
                    (name (alist-get 'name tab)))
          (propertize (format "[%s] " (string-replace "%" "%%" name))
                      'face 'yunge-tab-mode-line-name
                      'help-echo "Current tab"))))))

(defconst yunge-tab-mode-line-format
  '(:eval (yunge-tab--mode-line))
  "Mode line construct showing the selected window's current tab.")

(let ((format
       (delete yunge-tab-mode-line-format
               (delq 'yunge-tab-mode-line-format
                     (copy-sequence
                      (default-value 'mode-line-format))))))
  (setq-default
   mode-line-format
   (apply #'append
          (mapcar
           (lambda (item)
             (if (eq item 'mode-line-buffer-identification)
                 (list yunge-tab-mode-line-format item)
               (list item)))
           format))))

(defun yunge-tab--name-default (frame)
  "Name FRAME's initial implicit tab `default'."
  (with-selected-frame frame
    (let* ((tabs (tab-bar-tabs))
           (tab (car tabs))
           (names (mapcar (lambda (item) (alist-get 'name item)) tabs)))
      (when (and (not (alist-get 'explicit-name tab))
                 (not (member "default" names)))
        (let ((inhibit-message t))
          (tab-rename "default" 1))))))

(yunge-tab--name-default (selected-frame))
(add-hook 'after-make-frame-functions #'yunge-tab--name-default)

(defun yunge-tab-new (name)
  "Create a new tab named NAME."
  (interactive "sNew tab name: ")
  (when (equal name "")
    (user-error "Tab name cannot be empty"))
  (when (member name
                (mapcar (lambda (tab) (alist-get 'name tab))
                        (tab-bar-tabs)))
    (user-error "A tab named %s already exists" name))
  (let ((inhibit-message t))
    (tab-new)
    (tab-rename name))
  (message "Created tab '%s'" name))

(defconst yunge-tab-bindings
  '(("TAB" tab-switch "switch tab")
    ("<tab>" tab-switch nil)
    ("n" yunge-tab-new "new tab")
    ("q" tab-close "close tab")
    ("r" tab-rename "rename tab")
    ("u" tab-undo "restore tab")))

(defconst yunge-tab-leader-bindings
  `(("TAB" ,yunge-tab-map "tab")
    ("<tab>" ,yunge-tab-map nil)))

(yunge-key-define yunge-tab-map yunge-tab-bindings)
(yunge-key-define yunge-leader-map yunge-tab-leader-bindings)

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-tab-map yunge-tab-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-leader-map yunge-tab-leader-bindings))

(provide 'yunge-tab)

;;; yunge-tab.el ends here
