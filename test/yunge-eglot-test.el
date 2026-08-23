;;; yunge-eglot-test.el --- Project Eglot tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar yunge-test-eglot-elpaca-orders nil)

(cl-letf (((symbol-function 'elpaca)
           (cons 'macro
                 (lambda (order &rest _body)
                   (push order yunge-test-eglot-elpaca-orders)
                   nil))))
  (require 'yunge-eglot))

(declare-function eglot--lookup-mode "eglot" (mode))
(declare-function eglot-hierarchy-mode "eglot" ())

(yunge-test-deftest-lazy-load yunge-eglot
  (eglot eldoc-box project))

(ert-deftest yunge-eglot-downloads-the-gnu-elpa-tarball ()
  (should (member '(eglot :type tar)
                  yunge-test-eglot-elpaca-orders)))

(ert-deftest yunge-eglot-loads-the-locked-package ()
  (require 'eglot)
  (should
   (file-in-directory-p
    (locate-library "eglot")
    (yunge-test-package-directory 'eglot))))

(ert-deftest yunge-eglot-keeps-automatic-documentation-on-one-line ()
  (should-not eldoc-echo-area-use-multiline-p)
  (should message-truncate-lines))

(ert-deftest yunge-eglot-discovers-build-databases-with-word-boundaries ()
  (let ((root (make-temp-file "yunge-eglot-project-" t)))
    (unwind-protect
        (let ((names '("build" "build-clang" "build_asan"
                       "cmake-build-debug" "building" "builder"
                       "output")))
          (dolist (name names)
            (let ((directory (expand-file-name name root)))
              (make-directory directory)
              (write-region "[]" nil
                            (expand-file-name "compile_commands.json"
                                              directory)
                            nil 'silent)))
          (write-region "[]" nil
                        (expand-file-name "compile_commands.json" root)
                        nil 'silent)
          (should
           (equal
            (mapcar (lambda (file)
                      (file-relative-name file root))
                    (yunge-eglot--compilation-databases root))
            '("compile_commands.json"
              "build/compile_commands.json"
              "build-clang/compile_commands.json"
              "build_asan/compile_commands.json"
              "cmake-build-debug/compile_commands.json"))))
      (delete-directory root t))))

(ert-deftest yunge-eglot-allows-a-nonstandard-build-directory ()
  (let* ((root (make-temp-file "yunge-eglot-project-" t))
         (output (expand-file-name "output" root))
         (database (expand-file-name "compile_commands.json" output))
         default-seen)
    (unwind-protect
        (progn
          (make-directory output)
          (write-region "[]" nil database nil 'silent)
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (_prompt _directory default &rest _)
                       (setq default-seen default)
                       database)))
            (should
             (equal (yunge-eglot--read-compilation-database root)
                    database)))
          (should-not default-seen))
      (delete-directory root t))))

(ert-deftest yunge-eglot-prefers-the-recorded-compilation-database ()
  (let* ((root (make-temp-file "yunge-eglot-project-" t))
         (build (expand-file-name "build" root))
         (output (expand-file-name "output" root))
         (automatic (expand-file-name "compile_commands.json" build))
         (preferred (expand-file-name "compile_commands.json" output))
         default-seen)
    (unwind-protect
        (progn
          (make-directory build)
          (make-directory output)
          (write-region "[]" nil automatic nil 'silent)
          (write-region "[]" nil preferred nil 'silent)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt _collection &rest arguments)
                       (setq default-seen (nth 4 arguments))
                       preferred)))
            (should
             (equal (yunge-eglot--read-compilation-database
                     root preferred)
                    preferred)))
          (should (equal default-seen preferred)))
      (delete-directory root t))))

(ert-deftest yunge-eglot-persists-project-language-groups ()
  (let* ((root (make-temp-file "yunge-eglot-project-" t))
         (state-directory (make-temp-file "yunge-eglot-state-" t))
         (yunge-eglot-state-file
          (expand-file-name "projects.eld" state-directory))
         (yunge-eglot-projects nil)
         (database (expand-file-name "build/compile_commands.json" root))
         (modes '(c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)))
    (unwind-protect
        (progn
          (yunge-eglot--set-entry root modes database)
          (yunge-eglot--save-state)
          (setq yunge-eglot-projects nil)
          (yunge-eglot--load-state)
          (let ((entry (yunge-eglot--entry root 'c++-ts-mode)))
            (should entry)
            (should (equal (plist-get entry :compile-commands)
                           database)))
          (yunge-eglot--remove-entry root modes)
          (should-not (yunge-eglot--entry root 'c-mode)))
      (delete-directory root t)
      (delete-directory state-directory t))))

(ert-deftest yunge-eglot-clangd-contact-uses-the-database-directory ()
  (let* ((root (file-name-as-directory
                (make-temp-file "yunge-eglot-project-" t)))
         (database (expand-file-name "build-clang/compile_commands.json"
                                     root))
         (yunge-eglot-projects
          (list (list :root root
                      :modes yunge-eglot--clangd-modes
                      :compile-commands database))))
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'c++-mode)
          (cl-letf (((symbol-function 'project-root)
                     (lambda (_project) root))
                    ((symbol-function 'yunge-eglot--clangd-executable)
                     (lambda () "clangd"))
                    ((symbol-function 'num-processors)
                     (lambda () 24)))
            (should
             (equal
              (yunge-eglot--clangd-contact nil 'test-project)
              (list "clangd" "--background-index" "-j=6"
                    (format "--compile-commands-dir=%s"
                            (directory-file-name
                             (file-name-directory database))))))))
      (delete-directory root t))))

(ert-deftest yunge-eglot-registers-its-clangd-contact ()
  (require 'eglot)
  (let ((lookup (eglot--lookup-mode 'c++-mode)))
    (should (equal (mapcar #'car (car lookup))
                   yunge-eglot--clangd-modes))
    (should (eq (cdr lookup) #'yunge-eglot--clangd-contact))))

(ert-deftest yunge-eglot-runs-the-project-typescript-server-through-pnpm ()
  (require 'eglot)
  (let ((lookup (eglot--lookup-mode 'typescript-ts-mode)))
    (should (equal (mapcar #'car (car lookup))
                   yunge-eglot--typescript-modes))
    (should (equal (cdr lookup)
                   '("pnpm" "exec" "typescript-language-server"
                     "--stdio")))))

(ert-deftest yunge-eglot-keeps-local-clangd-off-remote-projects ()
  (let ((default-directory "/ssh:test:/project/"))
    (cl-letf (((symbol-function 'eglot--executable-find)
               (lambda (_command _remote) "/usr/bin/clangd"))
              ((symbol-function 'file-remote-p)
               (lambda (_file) t)))
      (should (equal (yunge-eglot--clangd-executable)
                     "/usr/bin/clangd")))))

(ert-deftest yunge-eglot-enables-the-current-project-language ()
  (let* ((root (file-name-as-directory
                (make-temp-file "yunge-eglot-project-" t)))
         (database (expand-file-name "output/compile_commands.json" root))
         (yunge-eglot-projects nil)
         saved ensured)
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'c++-mode)
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) 'test-project))
                    ((symbol-function 'project-root)
                     (lambda (_project) root))
                    ((symbol-function 'yunge-eglot--language-modes)
                     (lambda () yunge-eglot--clangd-modes))
                    ((symbol-function
                      'yunge-eglot--read-compilation-database)
                     (lambda (_root &optional _preferred) database))
                    ((symbol-function 'yunge-eglot--save-state)
                     (lambda () (setq saved t)))
                    ((symbol-function 'eglot-current-server)
                     (lambda () nil))
                    ((symbol-function 'eglot-ensure)
                     (lambda () (setq ensured t))))
            (yunge-eglot-enable-project)
            (should saved)
            (should ensured)
            (should
             (equal (plist-get
                     (yunge-eglot--entry root 'c-mode)
                     :compile-commands)
                    database))))
      (delete-directory root t))))

(ert-deftest yunge-eglot-restarts-clangd-after-database-change ()
  (let* ((root (file-name-as-directory
                (make-temp-file "yunge-eglot-project-" t)))
         (old-database
          (expand-file-name "build-old/compile_commands.json" root))
         (new-database
          (expand-file-name "build-new/compile_commands.json" root))
         (yunge-eglot-projects
          (list (list :root root
                      :modes yunge-eglot--clangd-modes
                      :compile-commands old-database)))
         preferred saved shutdown-server ensured)
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'c++-mode)
          (cl-letf (((symbol-function
                      'yunge-eglot--current-project-and-modes)
                     (lambda ()
                       (list root yunge-eglot--clangd-modes)))
                    ((symbol-function
                      'yunge-eglot--read-compilation-database)
                     (lambda (_root default)
                       (setq preferred default)
                       new-database))
                    ((symbol-function 'yunge-eglot--save-state)
                     (lambda () (setq saved t)))
                    ((symbol-function 'eglot-current-server)
                     (lambda () 'test-server))
                    ((symbol-function 'eglot-shutdown)
                     (lambda (server &rest _)
                       (setq shutdown-server server)))
                    ((symbol-function 'eglot-ensure)
                     (lambda () (setq ensured t))))
            (yunge-eglot-enable-project)
            (should (equal preferred old-database))
            (should saved)
            (should (eq shutdown-server 'test-server))
            (should ensured)
            (should
             (equal (plist-get
                     (yunge-eglot--entry root 'c-mode)
                     :compile-commands)
                    new-database))))
      (delete-directory root t))))

(ert-deftest yunge-eglot-keeps-clangd-when-database-is-unchanged ()
  (let* ((root (file-name-as-directory
                (make-temp-file "yunge-eglot-project-" t)))
         (database
          (expand-file-name "build/compile_commands.json" root))
         (yunge-eglot-projects
          (list (list :root root
                      :modes yunge-eglot--clangd-modes
                      :compile-commands database)))
         ensured)
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'c++-mode)
          (cl-letf (((symbol-function
                      'yunge-eglot--current-project-and-modes)
                     (lambda ()
                       (list root yunge-eglot--clangd-modes)))
                    ((symbol-function
                      'yunge-eglot--read-compilation-database)
                     (lambda (_root _default) database))
                    ((symbol-function 'yunge-eglot--save-state)
                     #'ignore)
                    ((symbol-function 'eglot-current-server)
                     (lambda () 'test-server))
                    ((symbol-function 'eglot-shutdown)
                     (lambda (&rest _)
                       (ert-fail "Unchanged clangd was restarted")))
                    ((symbol-function 'eglot-ensure)
                     (lambda () (setq ensured t))))
            (yunge-eglot-enable-project)
            (should ensured)))
      (delete-directory root t))))

(ert-deftest yunge-eglot-disables-only-the-current-project-language ()
  (let* ((root (file-name-as-directory
                (make-temp-file "yunge-eglot-project-" t)))
         (python-modes '(python-mode python-ts-mode))
         (yunge-eglot-projects
          (list (list :root root :modes yunge-eglot--clangd-modes)
                (list :root root :modes python-modes)))
         saved shutdown-server)
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'c++-mode)
          (cl-letf (((symbol-function
                      'yunge-eglot--current-project-and-modes)
                     (lambda ()
                       (list root yunge-eglot--clangd-modes)))
                    ((symbol-function 'yunge-eglot--save-state)
                     (lambda () (setq saved t)))
                    ((symbol-function 'eglot-current-server)
                     (lambda () 'test-server))
                    ((symbol-function 'eglot-shutdown)
                     (lambda (server &rest _)
                       (setq shutdown-server server))))
            (yunge-eglot-disable-project)
            (should saved)
            (should (eq shutdown-server 'test-server))
            (should-not (yunge-eglot--entry root 'c-mode))
            (should (yunge-eglot--entry root 'python-mode))))
      (delete-directory root t))))

(ert-deftest yunge-eglot-only-auto-starts-enabled-project-languages ()
  (let* ((root (file-name-as-directory
                (make-temp-file "yunge-eglot-project-" t)))
         (yunge-eglot-projects
          (list (list :root root
                      :modes yunge-eglot--clangd-modes)))
         ensured)
    (unwind-protect
        (cl-letf (((symbol-function 'eglot-ensure)
                   (lambda () (setq ensured t))))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "src/main.cpp" root)
                  major-mode 'c++-mode)
            (yunge-eglot--maybe-ensure)
            (should ensured))
          (setq ensured nil)
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "README.org" root)
                  major-mode 'org-mode)
            (yunge-eglot--maybe-ensure)
            (should-not ensured))
          (cl-letf (((symbol-function 'eglot-ensure) nil))
            (with-temp-buffer
              (setq buffer-file-name
                    (expand-file-name "src/early.cpp" root)
                    major-mode 'c++-mode)
              (should-not (yunge-eglot--maybe-ensure)))))
      (delete-directory root t))))

(ert-deftest yunge-eglot-catches-up-existing-project-buffers ()
  (let* ((root (file-name-as-directory
                (make-temp-file "yunge-eglot-project-" t)))
         (yunge-eglot-projects
          (list (list :root root :modes '(c++-mode))))
         (matching (generate-new-buffer " *yunge-eglot-matching*"))
         (other (generate-new-buffer " *yunge-eglot-other*"))
         ensured)
    (unwind-protect
        (progn
          (with-current-buffer matching
            (setq buffer-file-name
                  (expand-file-name "src/main.cpp" root)
                  major-mode 'c++-mode))
          (with-current-buffer other
            (setq buffer-file-name
                  (expand-file-name "README.org" root)
                  major-mode 'org-mode))
          (cl-letf (((symbol-function 'eglot-ensure)
                     (lambda () (push (current-buffer) ensured))))
            (yunge-eglot--ensure-existing-buffers))
          (should (equal ensured (list matching))))
      (dolist (buffer (list matching other))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory root t))))

(ert-deftest yunge-eglot-switches-between-source-and-header-with-clangd ()
  (require 'eglot)
  (let ((source "d:/project/include/example.h")
        (target "d:/project/src/example.cpp")
        request visited)
    (with-temp-buffer
      (setq buffer-file-name source
            major-mode 'c++-mode)
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda () 'test-server))
                ((symbol-function 'eglot-path-to-uri)
                 (lambda (path &rest _)
                   (should (equal path source))
                   "file:///project/include/example.h"))
                ((symbol-function 'jsonrpc-request)
                 (lambda (server method params &rest _)
                   (setq request (list server method params))
                   "file:///project/src/example.cpp"))
                ((symbol-function 'eglot-uri-to-path)
                 (lambda (uri)
                   (should (equal uri
                                  "file:///project/src/example.cpp"))
                   target))
                ((symbol-function 'find-file)
                 (lambda (file &rest _)
                   (setq visited file))))
        (yunge-eglot-switch-source-header)))
    (should
     (equal request
            '(test-server :textDocument/switchSourceHeader
                          (:uri "file:///project/include/example.h"))))
    (should (equal visited target))))

(ert-deftest yunge-eglot-rejects-an-empty-source-header-result ()
  (require 'eglot)
  (with-temp-buffer
    (setq buffer-file-name "d:/project/include/example.h"
          major-mode 'c++-mode)
    (cl-letf (((symbol-function 'eglot-current-server)
               (lambda () 'test-server))
              ((symbol-function 'eglot-path-to-uri)
               (lambda (&rest _) "file:///project/include/example.h"))
              ((symbol-function 'jsonrpc-request)
               (lambda (&rest _) "")))
      (should-error (yunge-eglot-switch-source-header)
                    :type 'user-error))))

(ert-deftest yunge-eglot-managed-keys-follow-buffer-management ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (fundamental-mode)
    ;; Reproduce Eglot first loading after this buffer entered Normal state.
    (require 'eglot)
    (require 'which-key)
    (setq-local eglot--managed-mode t)
    (yunge-test-evil-keys
     'normal
     '(("g D" . eglot-find-declaration)
       ("g r" . xref-find-references)
       ("g a" . yunge-eglot-switch-source-header)
       ("K" . eldoc-box-help-at-point)
       ("SPC l a" . eglot-code-actions)
       ("SPC l c" . eglot-show-call-hierarchy)
       ("SPC l f" . eglot-format)
       ("SPC l h" . eldoc-box-help-at-point)
       ("SPC l i" . eglot-find-implementation)
       ("SPC l o" . eglot-code-action-organize-imports)
       ("SPC l r" . eglot-rename)
       ("SPC l s" . xref-find-apropos)
       ("SPC l t" . eglot-find-typeDefinition)
       ("SPC l T" . eglot-show-type-hierarchy)))
    (yunge-test-which-key-prefix
     "SPC l"
     '(("a" nil "code actions")
       ("c" nil "call hierarchy")
       ("f" nil "format")
       ("h" nil "documentation")
       ("i" nil "implementation")
       ("o" nil "organize imports")
       ("r" nil "rename")
       ("s" nil "workspace symbols")
       ("t" nil "type definition")
       ("T" nil "type hierarchy")))
    (evil-visual-state)
    (yunge-test-evil-keys
     'visual
     '(("SPC l a" . eglot-code-actions)
       ("SPC l f" . eglot-format)))
    (evil-normal-state)
    (setq-local eglot--managed-mode nil)
    (should-not (key-binding (kbd "g D")))
    (should-not (key-binding (kbd "g r")))
    (should (eq (key-binding (kbd "g a"))
                'what-cursor-position))
    (should (eq (key-binding (kbd "K")) 'evil-lookup))
    (should-not (key-binding (kbd "SPC l a")))
    (should-not (key-binding (kbd "SPC l c")))
    (should-not (key-binding (kbd "SPC l f")))
    (should-not (key-binding (kbd "SPC l h")))
    (should-not (key-binding (kbd "SPC l i")))
    (should-not (key-binding (kbd "SPC l o")))
    (should-not (key-binding (kbd "SPC l r")))
    (should-not (key-binding (kbd "SPC l s")))
    (should-not (key-binding (kbd "SPC l t")))
    (should-not (key-binding (kbd "SPC l T")))
    (yunge-test-keys
     '(("SPC l e" . yunge-eglot-enable-project)
       ("SPC l d" . yunge-eglot-disable-project)))))

(ert-deftest yunge-eglot-binds-lifecycle-commands ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC l e" . yunge-eglot-enable-project)
     ("SPC l d" . yunge-eglot-disable-project)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC"
   '(("l" nil "+LSP")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC l"
   '(("e" nil "enable")
     ("d" nil "disable")))
  (with-temp-buffer
    (fundamental-mode)
    (should-not (key-binding (kbd "SPC l a")))
    (should-not (key-binding (kbd "SPC l c")))
    (should-not (key-binding (kbd "SPC l f")))
    (should-not (key-binding (kbd "SPC l h")))
    (should-not (key-binding (kbd "SPC l i")))
    (should-not (key-binding (kbd "SPC l o")))
    (should-not (key-binding (kbd "SPC l r")))
    (should-not (key-binding (kbd "SPC l s")))
    (should-not (key-binding (kbd "SPC l t")))
    (should-not (key-binding (kbd "SPC l T")))))

(ert-deftest yunge-eglot-integrates-hierarchy-buffers-with-evil ()
  (yunge-test-enable-evil)
  (require 'eglot)
  (with-temp-buffer
    (eglot-hierarchy-mode)
    (yunge-test-evil-keys
     'normal
     '(("RET" . push-button)
       ("q" . quit-window)
       ("gc" . eglot-hierarchy-center-on-node)
       ("g]" . forward-button)
       ("g[" . backward-button)
       ("<tab>" . forward-button)
       ("S-TAB" . backward-button)))))

;;; yunge-eglot-test.el ends here
