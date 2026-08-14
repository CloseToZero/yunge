;;; yunge-consult.el --- Search and navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-jump-history)
(require 'yunge-evil)

(declare-function evil-add-command-properties "evil-common")
(declare-function evil-exit-visual-state "evil-states" (&optional later buffer))
(declare-function consult--buffer-pair "consult")
(declare-function consult--buffer-query "consult")
(declare-function consult-grep "consult" (&optional dir initial))
(declare-function consult-ripgrep "consult" (&optional dir initial))

(defvar evil-command-line-map)
(defvar evil-eval-map)
(defvar evil-state)

(defconst yunge-consult-file-bindings
  '(("r" consult-recent-file "find recent file")))

(defconst yunge-consult-search-bindings
  '(("b" consult-line "search buffer")
    ("B" consult-line-multi "search project buffers")
    ("p" yunge-consult-project-search "search project")
    ("P" yunge-consult-project-search-symbol
     "search symbol in project")))

(defconst yunge-consult-jump-bindings
  '(("b" consult-bookmark "jump to bookmark")
    ("i" consult-imenu "jump to symbol")))

(defconst yunge-consult-history-bindings
  '(("M-r" consult-history "select history")))

(defconst yunge-consult-remap-bindings
  '(([remap switch-to-buffer] consult-buffer nil)
    ([remap imenu] consult-imenu nil)))

(defconst yunge-consult-navigation-commands
  '(consult-bookmark consult-buffer consult-imenu consult-line
    consult-line-multi consult-recent-file consult-grep consult-ripgrep))

(defun yunge-consult--previous-window-buffer (&optional window)
  "Return the most recent live buffer previously shown in WINDOW."
  (let* ((window (or window (selected-window)))
         (current (window-buffer window)))
    (catch 'buffer
      (dolist (entry (window-prev-buffers window))
        (let ((buffer (car entry)))
          (when (and (buffer-live-p buffer)
                     (not (eq buffer current)))
            (throw 'buffer buffer)))))))

(defun yunge-consult--buffer-items ()
  "Return Consult buffer items with the selected window's history first."
  ;; Consult normally moves every visible buffer behind invisible buffers.
  ;; When two windows show the same buffer and one of them switches away,
  ;; that shared buffer remains visible, so pressing RET without typing in
  ;; `consult-buffer' can select an unrelated buffer instead of returning
  ;; to it.
  (let* ((previous (yunge-consult--previous-window-buffer))
         (items (consult--buffer-query :sort 'visibility
                                       :as #'consult--buffer-pair))
         previous-item)
    (when previous
      (setq previous-item
            (catch 'item
              (dolist (item items)
                (when (eq (cdr item) previous)
                  (throw 'item item))))))
    (if previous-item
        (cons previous-item (delq previous-item items))
      items)))

(defun yunge-consult--project-search-command ()
  "Return the available Consult command for searching project files."
  (cond
   ((executable-find "rg") #'consult-ripgrep)
   ((executable-find "grep") #'consult-grep)
   (t
    (user-error "Project search requires rg or grep"))))

(defun yunge-consult-project-search (&optional initial)
  "Search the current project, optionally starting with INITIAL."
  (interactive)
  (funcall-interactively
   (yunge-consult--project-search-command) nil initial))

(defun yunge-consult-project-search-symbol ()
  "Search the current project for the selection or symbol at point."
  (interactive)
  (let* ((text
          (if (use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end))
            (thing-at-point 'symbol t)))
         (initial (and text (regexp-quote text))))
    (when (eq evil-state 'visual)
      (evil-exit-visual-state))
    (deactivate-mark)
    (yunge-consult-project-search initial)))

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
  "Track successful Consult navigation without making it repeatable."
  (dolist (command yunge-consult-navigation-commands)
    (evil-add-command-properties command :jump nil :repeat nil)
    (yunge-jump-history-track-command command))
  (evil-add-command-properties
   'yunge-consult-project-search :jump nil :repeat nil)
  (evil-add-command-properties
   'yunge-consult-project-search-symbol :jump nil :repeat nil))

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
    '(progn
       (consult-customize
        consult-source-buffer :items #'yunge-consult--buffer-items)
       (consult-customize
        consult-grep consult-ripgrep yunge-consult-project-search
        yunge-consult-project-search-symbol
        :preview-key '(:debounce 0.4 any))
       (consult-customize
        consult-grep consult-ripgrep yunge-consult-project-search
        :initial
        (when (use-region-p)
          (prog1
              (regexp-quote
               (buffer-substring-no-properties
                (region-beginning) (region-end)))
            (deactivate-mark)))))))

(provide 'yunge-consult)

;;; yunge-consult.el ends here
