;;; yunge-dired.el --- Dired keybindings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function dired-copy-filename-as-kill "dired")

(defvar dired-movement-style)
(defvar dired-mode-map)
(defvar wdired-mode-map)

(defun yunge-dired-copy-filename ()
  "Copy the names of the selected files without their directories."
  (interactive)
  (dired-copy-filename-as-kill))

(defun yunge-dired-copy-absolute-path ()
  "Copy the absolute paths of the selected files."
  (interactive)
  (dired-copy-filename-as-kill 0))

(defun yunge-dired-copy-project-path ()
  "Copy project-relative paths of the selected files."
  (interactive)
  (dired-copy-filename-as-kill 1))

(defconst yunge-dired-mark-bindings
  '(("d" dired-mark-directories "directories")
    ("e" dired-mark-executables "executables")
    ("l" dired-mark-symlinks "symbolic links")
    ("s" dired-mark-subdir-files "subdirectory files")
    ("t" dired-toggle-marks "invert marks")
    ("u" dired-unmark-all-marks "clear marks")))

(defvar-keymap yunge-dired-mark-map
  :doc "Keymap for marking groups of Dired files.")

(yunge-key-define yunge-dired-mark-map yunge-dired-mark-bindings)

(defconst yunge-dired-regexp-bindings
  '(("c" dired-do-copy-regexp "copy")
    ("d" dired-flag-files-regexp "flag deletion")
    ("g" dired-mark-files-containing-regexp "mark contents")
    ("l" dired-downcase "downcase")
    ("m" dired-mark-files-regexp "mark names")
    ("r" dired-do-rename-regexp "rename")
    ("u" dired-upcase "upcase")))

(defvar-keymap yunge-dired-regexp-map
  :doc "Keymap for regexp-based Dired operations.")

(yunge-key-define yunge-dired-regexp-map yunge-dired-regexp-bindings)

(defconst yunge-dired-copy-bindings
  '(("a" yunge-dired-copy-absolute-path "absolute path")
    ("f" yunge-dired-copy-filename "filename")
    ("p" yunge-dired-copy-project-path "project path")))

(defvar-keymap yunge-dired-copy-map
  :doc "Keymap for copying Dired file names and paths.")

(yunge-key-define yunge-dired-copy-map yunge-dired-copy-bindings)

(defconst yunge-dired-attribute-bindings
  '(("g" dired-do-chgrp "change group")
    ("m" dired-do-chmod "change mode")
    ("o" dired-do-chown "change owner")
    ("t" dired-do-touch "change timestamp")))

(defvar-keymap yunge-dired-attribute-map
  :doc "Keymap for changing Dired file attributes.")

(yunge-key-define yunge-dired-attribute-map
                  yunge-dired-attribute-bindings)

(defconst yunge-dired-link-bindings
  '(("h" dired-do-hardlink "hard link")
    ("r" dired-do-relsymlink "relative symbolic link")
    ("s" dired-do-symlink "symbolic link")))

(defvar-keymap yunge-dired-link-map
  :doc "Keymap for creating links in Dired.")

(yunge-key-define yunge-dired-link-map yunge-dired-link-bindings)

(defconst yunge-dired-command-bindings
  `(("a" ,yunge-dired-attribute-map "attribute")
    ("d" dired-hide-details-mode "toggle details")
    ("l" ,yunge-dired-link-map "link")
    ("o" dired-do-open "open externally")))

(defvar-keymap yunge-dired-command-map
  :doc "Keymap for Dired commands.")

(yunge-key-define yunge-dired-command-map yunge-dired-command-bindings)

(defconst yunge-dired-normal-visual-bindings
  `(("%" ,yunge-dired-regexp-map "regexp")
    ("*" ,yunge-dired-mark-map "mark")
    ("d" dired-flag-file-deletion "flag deletion")
    ("m" dired-mark "mark")
    ("u" dired-unmark "unmark")
    ([localleader] ,yunge-dired-command-map nil)))

(defconst yunge-dired-normal-bindings
  `(("!" dired-do-shell-command "shell command")
    ("&" dired-do-async-shell-command "async shell command")
    ("+" dired-create-directory "create directory")
    ("-" dired-up-directory "parent directory")
    ("C" dired-do-copy "copy")
    ("R" dired-do-rename "rename")
    ("RET" dired-find-file "open")
    ("gr" revert-buffer "refresh")
    ("i" dired-toggle-read-only "edit filenames")
    ("j" dired-next-line nil)
    ("k" dired-previous-line nil)
    ("o" dired-find-file-other-window "open in other window")
    ("q" quit-window "quit")
    ("s" dired-sort-toggle-or-edit "sort")
    ("x" dired-do-flagged-delete "delete flagged files")
    ("y" ,yunge-dired-copy-map "copy")))

(defconst yunge-wdired-normal-bindings
  '(("ZQ" wdired-abort-changes "discard changes")
    ("ZZ" wdired-finish-edit "apply changes")))

(defun yunge-dired--setup-keys ()
  "Set up Evil bindings for Dired."
  (yunge-key-evil-define '(normal visual) dired-mode-map
                         yunge-dired-normal-visual-bindings)
  (yunge-key-evil-define 'normal dired-mode-map
                         yunge-dired-normal-bindings))

(defun yunge-dired--setup-wdired-keys ()
  "Set up Evil bindings for Wdired."
  (yunge-key-evil-define 'normal wdired-mode-map
                         yunge-wdired-normal-bindings))

(with-eval-after-load 'evil
  (with-eval-after-load 'dired
    (setq dired-movement-style 'bounded-files)
    (yunge-dired--setup-keys))
  (with-eval-after-load 'wdired
    (yunge-dired--setup-wdired-keys)))

(with-eval-after-load 'which-key
  (dolist (entry `((,yunge-dired-mark-map
                    ,yunge-dired-mark-bindings)
                   (,yunge-dired-regexp-map
                    ,yunge-dired-regexp-bindings)
                   (,yunge-dired-copy-map
                    ,yunge-dired-copy-bindings)
                   (,yunge-dired-attribute-map
                    ,yunge-dired-attribute-bindings)
                   (,yunge-dired-link-map
                    ,yunge-dired-link-bindings)
                   (,yunge-dired-command-map
                    ,yunge-dired-command-bindings)))
    (yunge-key-which-key-describe-map (car entry) (cadr entry)))
  (yunge-key-which-key-describe
   'dired-mode yunge-dired-normal-visual-bindings)
  (yunge-key-which-key-describe
   'dired-mode yunge-dired-normal-bindings)
  (yunge-key-which-key-describe
   'wdired-mode yunge-wdired-normal-bindings))

(provide 'yunge-dired)

;;; yunge-dired.el ends here
