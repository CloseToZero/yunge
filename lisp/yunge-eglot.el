;;; yunge-eglot.el --- Project Eglot integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-key)
(require 'yunge-state)

(declare-function eglot--lookup-mode "eglot" (mode))
(declare-function eglot--executable-find "eglot" (command remote))
(declare-function eglot-current-server "eglot" ())
(declare-function eglot-find-declaration "eglot" ())
(declare-function eglot-find-implementation "eglot" ())
(declare-function eglot-find-typeDefinition "eglot" ())
(declare-function eglot-ensure "eglot" ())
(declare-function eglot-hierarchy-center-on-node "eglot" ())
(declare-function eglot-managed-p "eglot" ())
(declare-function eglot-path-to-uri "eglot" (path &rest args))
(declare-function eglot-show-call-hierarchy "eglot" (direction))
(declare-function eglot-show-type-hierarchy "eglot" (direction))
(declare-function eglot-shutdown "eglot" (server &optional interactive
                                                 timeout preserve-buffers))
(declare-function eglot-uri-to-path "eglot" (uri))
(declare-function eldoc-box-help-at-point "eldoc-box" ())
(declare-function evil-add-command-properties "evil-common"
                  (command &rest properties))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function jsonrpc-request "jsonrpc"
                  (connection method params &rest args))
(declare-function project-current "project"
                  (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function xref-find-references "xref" (identifier))
(declare-function xref-find-apropos "xref" (pattern))

(defvar eglot-server-programs)
(defvar eglot-sync-connect)
(defvar eglot-hierarchy-mode-map)
(defvar eldoc-echo-area-use-multiline-p)
(defvar yunge-leader-map)

(defvar yunge-eglot-projects nil
  "Eglot language groups enabled for individual projects.
Each entry is a plist containing :root, :modes, and optionally
:compile-commands.")

(defvar yunge-eglot-state-file
  (yunge-var-file "eglot" "projects.eld")
  "File storing `yunge-eglot-projects'.")

(defconst yunge-eglot--clangd-modes
  '(c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
  "Major modes managed together by clangd.")

(defconst yunge-eglot--build-directory-regexp
  "\\(?:\\`\\|[-_]\\)build\\(?:\\'\\|[-_]\\)"
  "Regexp matching build as a distinct directory-name component.")

(defvar-keymap yunge-eglot-command-map
  :doc "Language server commands.")

(defun yunge-eglot--managed-command-binding (binding)
  "Return BINDING while Eglot manages the current buffer."
  (and (fboundp 'eglot-managed-p) (eglot-managed-p) binding))

(defconst yunge-eglot-command-bindings
  '(("a" (menu-item "code actions" eglot-code-actions
                    :filter yunge-eglot--managed-command-binding)
     "code actions")
    ("c" (menu-item "call hierarchy" eglot-show-call-hierarchy
                    :filter yunge-eglot--managed-command-binding)
     "call hierarchy")
    ("d" yunge-eglot-disable-project "disable")
    ("e" yunge-eglot-enable-project "enable")
    ("f" (menu-item "format" eglot-format
                    :filter yunge-eglot--managed-command-binding)
     "format")
    ("h" (menu-item "documentation" eldoc-box-help-at-point
                    :filter yunge-eglot--managed-command-binding)
     "documentation")
    ("i" (menu-item "implementation" eglot-find-implementation
                    :filter yunge-eglot--managed-command-binding)
     "implementation")
    ("o" (menu-item "organize imports" eglot-code-action-organize-imports
                    :filter yunge-eglot--managed-command-binding)
     "organize imports")
    ("r" (menu-item "rename" eglot-rename
                    :filter yunge-eglot--managed-command-binding)
     "rename")
    ("s" (menu-item "workspace symbols" xref-find-apropos
                    :filter yunge-eglot--managed-command-binding)
     "workspace symbols")
    ("t" (menu-item "type definition" eglot-find-typeDefinition
                    :filter yunge-eglot--managed-command-binding)
     "type definition")
    ("T" (menu-item "type hierarchy" eglot-show-type-hierarchy
                    :filter yunge-eglot--managed-command-binding)
     "type hierarchy")))

(defconst yunge-eglot-leader-bindings
  `(("l" ,yunge-eglot-command-map "LSP")))

(defconst yunge-eglot-managed-bindings
  `(("gD" eglot-find-declaration "declaration")
    ("gr" xref-find-references "references")
    ("ga" yunge-eglot-switch-source-header "alternate source/header")
    ("K" eldoc-box-help-at-point "documentation")))

(defconst yunge-eglot-hierarchy-normal-bindings
  `(("RET" push-button "visit or toggle")
    ("q" quit-window "quit")
    ("gc" eglot-hierarchy-center-on-node "reroot hierarchy")
    ,@yunge-key-button-navigation-bindings))

(defun yunge-eglot--load-state ()
  "Load project Eglot settings from `yunge-eglot-state-file'."
  (when (file-exists-p yunge-eglot-state-file)
    (condition-case error-data
        (with-temp-buffer
          (insert-file-contents yunge-eglot-state-file)
          (setq yunge-eglot-projects (read (current-buffer))))
      (error
       (display-warning
        'yunge-eglot
        (format "Cannot read %s: %s"
                yunge-eglot-state-file
                (error-message-string error-data)))))))

(defun yunge-eglot--save-state ()
  "Save `yunge-eglot-projects' to `yunge-eglot-state-file'."
  (make-directory (file-name-directory yunge-eglot-state-file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file yunge-eglot-state-file
      (prin1 yunge-eglot-projects (current-buffer))
      (insert "\n"))))

(defun yunge-eglot--normalize-root (root)
  "Return ROOT as an absolute directory name."
  (file-name-as-directory (expand-file-name root)))

(defun yunge-eglot--same-root-p (left right)
  "Return non-nil when LEFT and RIGHT name the same directory."
  (file-equal-p (yunge-eglot--normalize-root left)
                (yunge-eglot--normalize-root right)))

(defun yunge-eglot--mode-matches-p (mode modes)
  "Return non-nil when MODE belongs to one of MODES."
  (cl-some (lambda (parent)
             (provided-mode-derived-p mode parent))
           modes))

(defun yunge-eglot--entry (root mode)
  "Return the entry for ROOT whose language group contains MODE."
  (let ((normalized-root (yunge-eglot--normalize-root root)))
    (cl-find-if
     (lambda (entry)
       (and (yunge-eglot--same-root-p
             (plist-get entry :root) normalized-root)
            (yunge-eglot--mode-matches-p
             mode (plist-get entry :modes))))
     yunge-eglot-projects)))

(defun yunge-eglot--language-modes ()
  "Return the Eglot language group containing `major-mode'."
  (require 'eglot)
  (let ((lookup (eglot--lookup-mode major-mode)))
    (unless (cdr lookup)
      (user-error "No Eglot server is configured for %s" major-mode))
    (mapcar #'car (car lookup))))

(defun yunge-eglot--clangd-language-p (modes)
  "Return non-nil when MODES describe clangd's language group."
  (cl-some (lambda (mode)
             (memq mode yunge-eglot--clangd-modes))
           modes))

(defun yunge-eglot--compilation-database-p (file)
  "Return non-nil when FILE names an existing compilation database."
  (and (equal (file-name-nondirectory file) "compile_commands.json")
       (file-regular-p file)))

(defun yunge-eglot--compilation-databases (root)
  "Return automatically discoverable compilation databases below ROOT."
  (let ((root (yunge-eglot--normalize-root root))
        databases)
    (let ((database (expand-file-name "compile_commands.json" root)))
      (when (file-regular-p database)
        (push database databases)))
    (dolist (directory
             (directory-files root t
                              directory-files-no-dot-files-regexp))
      (when (and (file-directory-p directory)
                 (string-match-p
                  yunge-eglot--build-directory-regexp
                  (file-name-nondirectory directory)))
        (let ((database
               (expand-file-name "compile_commands.json" directory)))
          (when (file-regular-p database)
            (push database databases)))))
    (nreverse databases)))

(defun yunge-eglot--read-other-compilation-database (root default)
  "Read a compilation database below ROOT, initially offering DEFAULT."
  (let* ((directory (if default
                        (file-name-directory default)
                      root))
         (file (read-file-name "Compilation database: "
                               directory default t
                               "compile_commands.json")))
    (unless (yunge-eglot--compilation-database-p file)
      (user-error "Not a compile_commands.json file: %s" file))
    (expand-file-name file)))

(defun yunge-eglot--read-compilation-database (root &optional preferred)
  "Read the compilation database clangd should use for ROOT.
Offer PREFERRED first when it still names a compilation database."
  (let ((databases
         (delete-dups
          (append
           (when (and preferred
                      (yunge-eglot--compilation-database-p preferred))
             (list preferred))
           (yunge-eglot--compilation-databases root)))))
    (if (cdr databases)
        (let* ((other "Choose another file...")
               (choice
                (completing-read "Compilation database: "
                                 (append databases (list other))
                                 nil t nil nil (car databases))))
          (if (equal choice other)
              (yunge-eglot--read-other-compilation-database root nil)
            choice))
      (yunge-eglot--read-other-compilation-database
       root (car databases)))))

(defun yunge-eglot--set-entry (root modes compilation-database)
  "Enable MODES for ROOT using COMPILATION-DATABASE when non-nil."
  (let ((root (yunge-eglot--normalize-root root)))
    (setq yunge-eglot-projects
          (cl-delete-if
           (lambda (entry)
             (and (yunge-eglot--same-root-p
                   (plist-get entry :root) root)
                  (cl-intersection modes (plist-get entry :modes))))
           yunge-eglot-projects))
    (push (append (list :root root :modes modes)
                  (when compilation-database
                    (list :compile-commands compilation-database)))
          yunge-eglot-projects)))

(defun yunge-eglot--remove-entry (root modes)
  "Disable the language group MODES for ROOT."
  (let ((root (yunge-eglot--normalize-root root)))
    (setq yunge-eglot-projects
          (cl-delete-if
           (lambda (entry)
             (and (yunge-eglot--same-root-p
                   (plist-get entry :root) root)
                  (cl-intersection modes (plist-get entry :modes))))
           yunge-eglot-projects))))

(defun yunge-eglot--clangd-executable ()
  "Return the preferred clangd executable."
  (or (eglot--executable-find "clangd" t)
      (when-let* ((program-files (and (eq system-type 'windows-nt)
                                      (not (file-remote-p default-directory))
                                      (getenv "ProgramFiles")))
                  (clangd (expand-file-name "LLVM/bin/clangd.exe"
                                            program-files))
                  ((file-executable-p clangd)))
        clangd)
      "clangd"))

(defun yunge-eglot--clangd-contact (_interactive project)
  "Return the clangd command for PROJECT."
  (let* ((root (project-root project))
         (entry (yunge-eglot--entry root major-mode))
         (database (plist-get entry :compile-commands))
         (workers (max 1 (min 6 (/ (num-processors) 2)))))
    (append
     (list (yunge-eglot--clangd-executable)
           "--background-index"
           (format "-j=%d" workers))
     (when database
       (list
        (format "--compile-commands-dir=%s"
                (file-local-name
                 (directory-file-name
                  (file-name-directory database)))))))))

(defun yunge-eglot--current-project-and-modes ()
  "Return the current project root and Eglot language group."
  (let* ((project (project-current t))
         (root (project-root project))
         (modes (yunge-eglot--language-modes)))
    (list root modes)))

(defun yunge-eglot-enable-project ()
  "Enable Eglot for the current language in the current project."
  (interactive)
  (pcase-let* ((`(,root ,modes)
                (yunge-eglot--current-project-and-modes))
               (old-entry (yunge-eglot--entry root major-mode))
               (old-database (plist-get old-entry :compile-commands))
               (clangd-language
                (yunge-eglot--clangd-language-p modes))
               (database
                (when clangd-language
                  (yunge-eglot--read-compilation-database
                   root old-database)))
               (server (eglot-current-server))
               (restart
                (and server clangd-language
                     (not (equal old-database database)))))
    (yunge-eglot--set-entry root modes database)
    (yunge-eglot--save-state)
    (when restart
      (eglot-shutdown server))
    (eglot-ensure)
    (message "%s Eglot for %s in %s"
             (if restart "Restarted" "Enabled")
             major-mode root)))

(defun yunge-eglot-disable-project ()
  "Disable Eglot for the current language in the current project."
  (interactive)
  (pcase-let ((`(,root ,modes)
               (yunge-eglot--current-project-and-modes)))
    (yunge-eglot--remove-entry root modes)
    (yunge-eglot--save-state)
    (when-let* ((server (eglot-current-server)))
      (eglot-shutdown server))
    (message "Disabled Eglot for %s in %s" major-mode root)))

(defun yunge-eglot-switch-source-header ()
  "Visit the source or header corresponding to the current file via clangd."
  (interactive)
  (unless buffer-file-name
    (user-error "The current buffer is not visiting a file"))
  (unless (yunge-eglot--mode-matches-p
           major-mode yunge-eglot--clangd-modes)
    (user-error "Source/header switching requires a clangd language buffer"))
  (let* ((server (or (eglot-current-server)
                     (user-error "The current buffer is not managed by Eglot")))
         (uri (jsonrpc-request
               server :textDocument/switchSourceHeader
               `(:uri ,(eglot-path-to-uri buffer-file-name)))))
    (unless (and (stringp uri) (not (string-empty-p uri)))
      (user-error "clangd found no corresponding source or header"))
    (find-file (eglot-uri-to-path uri))))

(defun yunge-eglot--maybe-ensure ()
  "Start Eglot when this file's project and language are enabled."
  (when (and (fboundp 'eglot-ensure)
             buffer-file-name
             (cl-find-if
              (lambda (entry)
                (and (file-in-directory-p
                      buffer-file-name (plist-get entry :root))
                     (yunge-eglot--mode-matches-p
                      major-mode (plist-get entry :modes))))
              yunge-eglot-projects))
    (eglot-ensure)))

(defun yunge-eglot--ensure-existing-buffers ()
  "Start Eglot where existing file buffers match saved projects."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (yunge-eglot--maybe-ensure))))

(setq eldoc-echo-area-use-multiline-p nil
      message-truncate-lines t
      read-process-output-max (* 1024 1024))

(yunge-eglot--load-state)
(add-hook 'find-file-hook #'yunge-eglot--maybe-ensure)

(yunge-key-define yunge-eglot-command-map yunge-eglot-command-bindings)

(with-eval-after-load 'yunge-evil
  (yunge-key-define yunge-leader-map yunge-eglot-leader-bindings)
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-leader-map yunge-eglot-leader-bindings)))

(with-eval-after-load 'eglot
  (setq eglot-sync-connect 0)
  (add-to-list
   'eglot-server-programs
   (cons yunge-eglot--clangd-modes
         #'yunge-eglot--clangd-contact))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'eglot-hierarchy-mode 'normal)
    (yunge-key-evil-define-minor-mode
     'normal 'eglot--managed-mode yunge-eglot-managed-bindings)
    (yunge-key-evil-define 'normal eglot-hierarchy-mode-map
                           yunge-eglot-hierarchy-normal-bindings)
    (dolist (command '(eglot-find-declaration
                       eglot-find-implementation
                       eglot-find-typeDefinition
                       xref-find-references
                       yunge-eglot-switch-source-header))
      (evil-add-command-properties command :jump t))))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-eglot-command-map yunge-eglot-command-bindings))

(elpaca (eglot :type tar)
  (yunge-eglot--ensure-existing-buffers))
(elpaca eldoc-box)

(provide 'yunge-eglot)

;;; yunge-eglot.el ends here
