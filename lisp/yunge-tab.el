;;; yunge-tab.el --- Tab-based task layouts -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-evil)
(require 'yunge-key)
(require 'subr-x)
(require 'tab-bar)

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
               (copy-sequence
                (default-value 'mode-line-format)))))
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

(defun yunge-tab--validate-name (name &optional renaming)
  "Return trimmed NAME if it is nonempty and available in this frame.
When RENAMING is non-nil, allow the current tab's name."
  (setq name (string-trim name))
  (when (string-empty-p name)
    (user-error "Tab name cannot be empty"))
  (dolist (tab (tab-bar-tabs))
    (when (and (equal name (alist-get 'name tab))
               (not (and renaming (eq (car tab) 'current-tab))))
      (user-error "A tab named %s already exists" name)))
  name)

(defun yunge-tab-new (name)
  "Create a tab with a nonempty, unique NAME in the current frame."
  (interactive "sNew tab name: ")
  (setq name (yunge-tab--validate-name name))
  (let ((inhibit-message t))
    (tab-new)
    (tab-rename name))
  (message "Created tab '%s'" name))

(defun yunge-tab-rename (name)
  "Rename the current tab to a nonempty, unique NAME in this frame."
  (interactive
   (list (read-string "Rename tab: "
                      (alist-get 'name (assq 'current-tab (tab-bar-tabs))))))
  (tab-rename (yunge-tab--validate-name name t)))

(defun yunge-tab-switch (name)
  "Select an existing tab by NAME in the current frame."
  (interactive
   (list (completing-read "Switch to tab: "
                          (mapcar (lambda (tab) (alist-get 'name tab))
                                  (tab-bar-tabs))
                          nil t)))
  (unless (member name (mapcar (lambda (tab) (alist-get 'name tab))
                              (tab-bar-tabs)))
    (user-error "No tab named %s" name))
  (tab-switch name))

(defconst yunge-tab-bindings
  '(("TAB" yunge-tab-switch "switch tab")
    ("<tab>" yunge-tab-switch nil)
    ("l" yunge-workspace-restore "restore workspace")
    ("n" yunge-tab-new "new tab")
    ("q" tab-close "close tab")
    ("r" yunge-tab-rename "rename tab")
    ("s" yunge-workspace-save "save workspace")
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
