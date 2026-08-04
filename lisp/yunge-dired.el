;;; yunge-dired.el --- Dired keybindings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function dbus-call-method "dbus"
                  (bus service path interface method &rest args))
(declare-function dired-copy-filename-as-kill "dired")
(declare-function dired-dnd-handle-file "dired" (uri action))
(declare-function dired-dnd-handle-local-file "dired" (uri action))
(declare-function dired-dwim-target-recent "dired-aux")
(declare-function dired-get-marked-files "dired"
                  (&optional localp arg filter distinguish-one-marked error))
(declare-function dnd-get-local-file-name "dnd"
                  (uri &optional must-exist))
(declare-function dnd-open-file "dnd" (uri action))
(declare-function project-current "project"
                  (&optional maybe-prompt directory))
(declare-function project-remember-project "project"
                  (project &optional no-write stable))
(declare-function shell-command-do-open "dired-aux" (files))
(declare-function url-encode-url "url-util" (url))
(declare-function w32-shell-execute "w32fns.c"
                  (operation document &optional parameters show-flag))
(declare-function x-popup-menu "menu.c" (position menu))

(defvar dnd-protocol-alist)
(defvar dired-dwim-target)
(defvar dired-movement-style)
(defvar dired-directory)
(defvar dired-mode-map)
(defvar wdired-mode-map)

(defun yunge-dired--drop-items-description (uris)
  "Return a short description of dropped file URIS."
  (if (cdr uris)
      (format "%d items" (length uris))
    (let ((file (or (dnd-get-local-file-name (car uris))
                    (car uris))))
      (format "`%s'"
              (file-name-nondirectory (directory-file-name file))))))

(defun yunge-dired--drop-action-menu (uris)
  "Ask which action to perform for dropped file URIS."
  (x-popup-menu
   t
   `(,(format "Drop %s" (yunge-dired--drop-items-description uris))
     (""
      ("Open" . open)
      ("Copy here" . copy)
      ("Move here" . move)
      ("Link here" . link)
      "--"
      ("Cancel" . nil)))))

(defun yunge-dired--transfer-dropped-file (uri action)
  "Perform transfer ACTION on the file designated by URI."
  (if (string-match-p "\\`file://[^/]" uri)
      (dired-dnd-handle-file uri action)
    (dired-dnd-handle-local-file uri action)))

(defun yunge-dired--perform-file-drop (window uris action)
  "Perform ACTION on file URIS dropped into WINDOW.
ACTION is `open', `copy', `move', or `link'."
  (require 'dnd)
  (with-selected-window window
    (let ((result (if (eq action 'open) 'private action)))
      (dolist (uri uris)
        (let ((performed
               (if (eq action 'open)
                   (dnd-open-file uri 'private)
                 (yunge-dired--transfer-dropped-file uri action))))
          (unless (eq performed result)
            (setq result 'private))))
      result)))

(defun yunge-dired-handle-file-drop (uris _source-action)
  "Ask how to handle a batch of file URIS dropped into Dired.
Ignore SOURCE-ACTION because drag-and-drop backends derive it
inconsistently from keyboard modifiers and the source application."
  (when-let* ((action (yunge-dired--drop-action-menu uris)))
    (yunge-dired--perform-file-drop (selected-window) uris action)))

(put 'yunge-dired-handle-file-drop 'dnd-multiple-handler t)

(defun yunge-dired--setup-dnd ()
  "Use the portable file-drop handler in the current Dired buffer."
  (when (boundp 'dnd-protocol-alist)
    (let (non-file-handlers)
      (dolist (entry dnd-protocol-alist)
        (unless (string-match-p (car entry) "file:///yunge-dired-drop")
          (push entry non-file-handlers)))
      (setq-local dnd-protocol-alist
                  (cons '("^file:" . yunge-dired-handle-file-drop)
                        (nreverse non-file-handlers))))))

(defun yunge-dired--remember-project ()
  "Remember the project containing the current Dired directory."
  (when (bound-and-true-p dired-directory)
    (when-let* ((project (project-current nil default-directory)))
      (project-remember-project project))))

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

(defun yunge-dired-open-directory-externally ()
  "Open the current Dired directory in the system file manager."
  (interactive)
  (shell-command-do-open (list default-directory)))

(defun yunge-dired--files-to-reveal ()
  "Return marked Dired files, or the file at point when none are marked."
  (dired-get-marked-files nil nil nil nil "No files specified"))

(defun yunge-dired--powershell-literal (string)
  "Return STRING as a single-quoted PowerShell literal."
  (concat "'"
          (replace-regexp-in-string "'" "''" string t t)
          "'"))

(defun yunge-dired--reveal-one-on-windows (file)
  "Reveal FILE in Windows Explorer."
  (w32-shell-execute
   "open" "explorer.exe"
   (format "/select,\"%s\""
           (subst-char-in-string ?/ ?\\ file))))

(defun yunge-dired--reveal-on-windows (files)
  "Reveal FILES in Windows Explorer."
  (if (null (cdr files))
      (yunge-dired--reveal-one-on-windows (car files))
    (let ((powershell (or (executable-find "pwsh")
                          (executable-find "powershell"))))
      (if (not powershell)
          (progn
            (yunge-dired--reveal-one-on-windows (car files))
            (message
             "Explorer can reveal only the first item without PowerShell"))
        (let* ((script
                (expand-file-name
                 "script/yunge-reveal.ps1"
                 user-emacs-directory))
               (command
                (concat
                 "& "
                 (mapconcat
                  #'yunge-dired--powershell-literal
                  (cons script files)
                  " ")))
               (encoded-command
                (base64-encode-string
                 (encode-coding-string command 'utf-16le)
                 t)))
          ;; Explorer's command line can select only one item.  Use the native
          ;; multi-select API through PowerShell only when it is necessary.
          ;; Hide PowerShell when it is created; the script makes only the
          ;; resulting Explorer window visible.
          (w32-shell-execute
           "open" powershell
           (concat
            "-NoLogo -NoProfile -NonInteractive -WindowStyle Hidden "
            "-ExecutionPolicy Bypass -EncodedCommand "
            encoded-command)
           0))))))

(defun yunge-dired--reveal-on-mac (files)
  "Reveal FILES in macOS Finder."
  (make-process
   :name "yunge-dired-reveal"
   :command (append '("open" "-R") files)
   :noquery t))

(defun yunge-dired--reveal-on-freedesktop (files)
  "Reveal FILES through the freedesktop file-manager interface."
  (require 'dbus)
  (require 'url-util)
  (condition-case error-data
      (dbus-call-method
       :session "org.freedesktop.FileManager1"
       "/org/freedesktop/FileManager1"
       "org.freedesktop.FileManager1" "ShowItems"
       :timeout 3000
       (cons :array
             (mapcar
              (lambda (file)
                (url-encode-url
                 (concat "file://" (expand-file-name file))))
              files))
       "")
    (dbus-error
     (user-error "The file manager cannot reveal files: %s"
                 (error-message-string error-data)))))

(defun yunge-dired-reveal-in-file-manager ()
  "Reveal Dired files in the system file manager.
If point is on a marked file, reveal all marked files where supported.
Otherwise, ignore marks and reveal only the file at point."
  (interactive)
  (let ((files (yunge-dired--files-to-reveal)))
    (dolist (file files)
      (when (file-remote-p file)
        (user-error "Cannot reveal a remote file externally")))
    (pcase system-type
      ('windows-nt (yunge-dired--reveal-on-windows files))
      ('darwin (yunge-dired--reveal-on-mac files))
      ((or 'gnu 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
       (yunge-dired--reveal-on-freedesktop files))
      (_ (user-error "Revealing files is unsupported on %s"
                     system-type)))))

(defconst yunge-dired-command-bindings
  `(("a" ,yunge-dired-attribute-map "attribute")
    ("d" dired-hide-details-mode "toggle details")
    ("e" yunge-dired-open-directory-externally "open in file manager")
    ("h" dired-summary "summary and errors")
    ("l" ,yunge-dired-link-map "link")
    ("o" dired-do-open "open externally")
    ("r" yunge-dired-reveal-in-file-manager "reveal in file manager")))

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
    ("C" dired-do-copy "copy")
    ("M-h" dired-up-directory "parent directory")
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

(with-eval-after-load 'dired
  (require 'dnd)
  (setq dired-dwim-target #'dired-dwim-target-recent)
  ;; Remove bindings left by the earlier Windows-only implementation.
  (dolist (event '([drag-n-drop]
                   [C-drag-n-drop]
                   [S-drag-n-drop]
                   [C-S-drag-n-drop]))
    (when (eq (lookup-key dired-mode-map event)
              #'yunge-dired-handle-file-drop)
      (define-key dired-mode-map event nil)))
  (add-hook 'dired-mode-hook #'yunge-dired--remember-project)
  (add-hook 'dired-mode-hook #'yunge-dired--setup-dnd)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'dired-mode)
        (yunge-dired--setup-dnd)))))

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
    (yunge-key-add-which-key-descriptions (car entry) (cadr entry))))

(provide 'yunge-dired)

;;; yunge-dired.el ends here
