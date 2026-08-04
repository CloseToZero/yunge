;;; yunge-magit-test.el --- Magit tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(eval-when-compile
  (require 'magit-section))

(declare-function evil-normal-state "evil-states")
(declare-function evil-local-mode "evil-core" (&optional arg))
(declare-function git-rebase-mode "git-rebase")
(declare-function magit-blame-read-only-mode "magit-blame")
(declare-function magit-blob-mode "magit-files")
(declare-function magit-cherry-mode "magit-log")
(declare-function magit-diff-mode "magit-diff")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-insert-section--create "magit-section")
(declare-function magit-insert-section--finish "magit-section")
(declare-function magit-log-mode "magit-log")
(declare-function magit-log-select-mode "magit-log")
(declare-function magit-process-mode "magit-process")
(declare-function magit-repolist-mode "magit-repos")
(declare-function magit-reflog-mode "magit-reflog")
(declare-function magit-refs-mode "magit-refs")
(declare-function magit-region-values "magit-section")
(declare-function magit-revision-mode "magit-diff")
(declare-function magit-stash-mode "magit-stash")
(declare-function magit-stashes-mode "magit-stash")
(declare-function magit-status-mode "magit-status")
(declare-function magit-submodule-list-mode "magit-submodule")
(declare-function transient-get-suffix "transient" (prefix loc))
(declare-function yunge-magit--enter-insert-state-for-blank-commit-message
                  "yunge-magit")

(defvar evil-state)
(defvar git-commit-setup-hook)
(defvar magit-define-global-key-bindings)
(defvar magit-diff-section-map)
(defvar magit-module-section-map)
(defvar magit-root-section)
(defvar project-prefix-map)
(defvar project-switch-commands)
(defvar transient-map)
(defvar transient-popup-navigation-map)
(defvar transient-sticky-map)

(defconst yunge-magit-test-repository-bindings
  '(("A" . magit-cherry-pick)
    ("b" . magit-branch)
    ("c" . magit-commit)
    ("d" . magit-diff)
    ("f" . magit-fetch)
    ("F" . magit-pull)
    ("L" . magit-log)
    ("M" . magit-remote)
    ("m" . magit-merge)
    ("p" . magit-push)
    ("r" . magit-rebase)
    ("X" . magit-reset)
    ("Z" . magit-stash)))

(defconst yunge-magit-test-horizontal-bindings
  '(("h" . evil-backward-char)
    ("l" . evil-forward-char)))

(yunge-test-deftest-lazy-load yunge-magit
  (magit project transient))

(defun yunge-magit-test--visual-untracked-files (keys target)
  "Return the untracked files selected by KEYS before calling TARGET."
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit)

  (with-temp-buffer
    (magit-status-mode)
    (let ((inhibit-read-only t)
          selected)
      (erase-buffer)
      (setq magit-root-section nil)
      (magit-insert-section (status)
        (magit-insert-section (untracked)
          (magit-insert-heading "Untracked files")
          (dolist (file '("one" "two" "three" "four"))
            (magit-insert-section (file file)
              (magit-insert-heading file)))))
      (goto-char (point-min))
      (search-forward "one")
      (beginning-of-line)
      (evil-normal-state)
      (cl-letf (((symbol-function target)
                 (lambda (&rest _arguments)
                   (setq selected (magit-region-values nil t)))))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd keys))))
      selected)))

(defun yunge-magit-test--view-keys (mode quit-command)
  "Check common bindings in Magit view MODE, including QUIT-COMMAND."
  (yunge-test-evil-normal-keys
   mode
   (append
    `(("C-j" . magit-section-forward)
      ("C-k" . magit-section-backward)
      ("RET" . magit-visit-thing)
      ("<tab>" . magit-section-toggle)
      ("g<" . magit-process-buffer)
      ("q" . ,quit-command)
      ("gr" . magit-refresh)
      ("gR" . magit-refresh-all)
      ("y s" . magit-copy-section-value))
    yunge-magit-test-repository-bindings
    yunge-magit-test-horizontal-bindings
    '(("SPC m SPC" . magit-dispatch)
      ("C-i" . yunge-jump-history-forward)))))

(defun yunge-magit-test--minor-mode-keys (mode bindings)
  "Enable minor MODE and check its normal-state BINDINGS."
  (with-temp-buffer
    (fundamental-mode)
    (evil-local-mode 1)
    (evil-normal-state)
    (funcall mode 1)
    (yunge-test-evil-keys 'normal bindings)))

(defun yunge-magit-test--transient-command (prefix key)
  "Return the command bound to KEY in transient PREFIX."
  (plist-get (cdr (transient-get-suffix prefix key)) :command))

(defun yunge-magit-test--transient-key (prefix command)
  "Return the key bound to COMMAND in transient PREFIX."
  (plist-get (cdr (transient-get-suffix prefix command)) :key))

(ert-deftest yunge-magit-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-magit 'magit
   :before-ready
   '(progn
      (when (featurep 'magit)
        (error "Magit was loaded before its Elpaca body ran"))
      (when magit-define-global-key-bindings
        (error "Magit owns global keys before its package is ready"))
      (when (keymap-lookup yunge-file-map "g")
        (error "Magit file keys were bound before its Elpaca body ran"))
      (when (keymap-lookup yunge-go-map "g")
        (error "Magit keys were bound before its Elpaca body ran"))
      (when (keymap-lookup project-prefix-map "m")
        (error "Magit project key was bound before package readiness"))
      (when (and (boundp 'project-switch-commands)
                 (assq 'magit-project-status project-switch-commands))
        (error "Magit project action was added before package readiness")))
   :after-ready
   '(progn
      (unless (eq (keymap-lookup yunge-file-map "g")
                  'magit-file-dispatch)
        (error "Magit file keys were not bound after package readiness"))
      (unless (eq (keymap-lookup yunge-go-map "g") 'magit-status)
        (error "Magit keys were not bound after package readiness"))
      (unless (eq (keymap-lookup project-prefix-map "m")
                  'magit-project-status)
        (error "Magit project key was not bound after package readiness"))
      (when (featurep 'project)
        (error "Project was loaded by the Magit configuration"))
      (require 'project)
      (unless (equal (car (last project-switch-commands))
                     '(magit-project-status "Magit"))
        (error "Magit project action was not appended after readiness"))
      (dolist (binding '(("C-x g" . magit-status)
                         ("C-x M-g" . magit-dispatch)
                         ("C-c M-g" . magit-file-dispatch)))
        (when (eq (keymap-lookup global-map (car binding))
                  (cdr binding))
          (error "Magit installed global key %s" (car binding))))
      (when (featurep 'magit)
        (error "Magit was loaded by its configuration"))
      (when (featurep 'transient)
        (error "Transient was loaded by the Magit configuration")))))

(ert-deftest yunge-magit-configures-transient-navigation ()
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'transient)

  (should (eq (keymap-lookup transient-map "q")
              'transient-quit-one))
  (should (eq (keymap-lookup transient-sticky-map "q")
              'transient-quit-seq))
  (should (eq (keymap-lookup transient-popup-navigation-map "C-j")
              'transient-forward-button))
  (should (eq (keymap-lookup transient-popup-navigation-map "C-k")
              'transient-backward-button)))

(ert-deftest yunge-magit-normalizes-destructive-transient-keys ()
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (dolist (feature '(magit magit-branch magit-files magit-notes
                           magit-remote magit-sequence magit-stash
                           magit-submodule magit-tag magit-worktree))
    (require feature))

  (dolist (binding '((magit-dispatch magit-discard "x")
                     (magit-branch magit-branch-reset "X")
                     (magit-branch magit-branch-delete "x")
                     (magit-stash magit-stash-keep-index "k")
                     (magit-stash magit-stash-drop "x")
                     (magit-rebase magit-rebase-remove-commit "x")
                     (magit-remote magit-remote-remove "x")
                     (magit-submodule magit-submodule-remove "x")
                     (magit-tag magit-tag-delete "x")
                     (magit-worktree magit-worktree-delete "x")
                     (magit-notes magit-notes-remove "x")))
    (should (equal (yunge-magit-test--transient-key
                    (nth 0 binding) (nth 1 binding))
                   (nth 2 binding))))
  ;; File dispatch has separate layouts inside and outside file buffers.
  (dolist (binding '((", x" . magit-file-untrack)
                     ("x" . magit-file-untrack)
                     (", X" . magit-file-delete)
                     ("X" . magit-file-delete)))
    (should (eq (yunge-magit-test--transient-command
                 'magit-file-dispatch (car binding))
                (cdr binding)))))

(ert-deftest yunge-magit-integrates-with-evil ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)

  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC f g" . magit-file-dispatch)
     ("SPC g g" . magit-status)
     ("SPC p m" . magit-project-status)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC f"
   '(("g" nil "Git actions")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC g"
   '(("g" nil "Git status")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC p"
   '(("m" nil "Magit status")))

  (require 'magit)
  (yunge-test-evil-normal-keys
   'magit-status-mode
   (append
    '(("C-j" . magit-section-forward)
      ("C-k" . magit-section-backward)
      ("M-h" . magit-section-up)
      ("M-j" . magit-section-forward-sibling)
      ("M-k" . magit-section-backward-sibling)
      ("z1" . magit-section-show-level-1-all)
      ("z2" . magit-section-show-level-2-all)
      ("z3" . magit-section-show-level-3-all)
      ("z4" . magit-section-show-level-4-all)
      ("za" . magit-section-toggle)
      ("zc" . magit-section-hide)
      ("zC" . magit-section-hide-children)
      ("zo" . magit-section-show)
      ("zO" . magit-section-show-children)
      ("zz" . evil-scroll-line-to-center)
      ("1" . digit-argument)
      ("RET" . magit-visit-thing)
      ("<tab>" . magit-section-toggle)
      ("g<" . magit-process-buffer)
      ("q" . magit-mode-bury-buffer)
      ("gr" . magit-refresh)
      ("gR" . magit-refresh-all)
      ("gn" . magit-jump-to-untracked)
      ("gs" . magit-jump-to-staged)
      ("gu" . magit-jump-to-unstaged)
      ("gz" . magit-jump-to-stashes)
      ("s" . magit-stage-files)
      ("u" . magit-unstage-files)
      ("x" . magit-delete-thing)
      ("y s" . magit-copy-section-value))
    yunge-magit-test-repository-bindings
    yunge-magit-test-horizontal-bindings
    '(("SPC m SPC" . magit-dispatch)
      ("C-i" . yunge-jump-history-forward))))
  (yunge-test-which-key-prefix-bindings
   'magit-status-mode "SPC m"
   '(("SPC" nil "dispatch")))
  (yunge-test-which-key-prefix-bindings
   'magit-status-mode "y"
   '(("s" nil "section value")))
  (yunge-test-evil-visual-keys
   'magit-status-mode
   '(("s" . magit-stage-files)
     ("u" . magit-unstage-files)
     ("x" . magit-delete-thing)
     ("y" . evil-yank))))

(ert-deftest yunge-magit-integrates-history-views-with-evil ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit-log)
  (require 'magit-diff)
  (require 'magit-reflog)
  (require 'magit-refs)
  (require 'magit-stash)

  (dolist (mode '(magit-log-mode
                  magit-reflog-mode
                  magit-stashes-mode
                  magit-cherry-mode))
    (yunge-magit-test--view-keys mode 'magit-log-bury-buffer))
  (yunge-magit-test--view-keys
   'magit-refs-mode 'magit-mode-bury-buffer)
  (dolist (mode '(magit-diff-mode
                  magit-revision-mode
                  magit-stash-mode))
    (yunge-magit-test--view-keys mode 'magit-mode-bury-buffer))
  (yunge-test-evil-normal-keys
   'magit-log-select-mode
   (append
    '(("C-j" . magit-section-forward)
      ("C-k" . magit-section-backward)
      ("RET" . magit-log-select-pick)
      ("<tab>" . magit-section-toggle)
      ("q" . magit-log-select-quit)
      ("C-i" . yunge-jump-history-forward))
    yunge-magit-test-horizontal-bindings)))

(ert-deftest yunge-magit-log-ret-visits-the-commit-at-point ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit-log)

  (with-temp-buffer
    (magit-log-mode)
    (let ((inhibit-read-only t))
      (setq magit-root-section nil)
      (magit-insert-section (log)
        (magit-insert-section (commit "deadbeef")
          (magit-insert-heading "commit"))))
    (goto-char (point-min))
    (search-forward "commit")
    (beginning-of-line)
    (evil-normal-state)
    (yunge-test-key "RET" 'magit-show-commit)))

(ert-deftest yunge-magit-visual-stage-selects-exact-untracked-files ()
  (should
   (equal
    (yunge-magit-test--visual-untracked-files
     "V j j s" 'magit-stage-untracked)
    '("one" "two" "three"))))

(ert-deftest yunge-magit-visual-discard-selects-exact-untracked-files ()
  (should
   (equal
    (yunge-magit-test--visual-untracked-files
     "V j j x" 'magit-discard-files)
    '("one" "two" "three"))))

(ert-deftest yunge-magit-visual-drop-selects-exact-stashes ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit-stash)

  (with-temp-buffer
    (magit-status-mode)
    (let ((inhibit-read-only t)
          selected)
      (erase-buffer)
      (setq magit-root-section nil)
      (magit-insert-section (status)
        (magit-insert-section (stashes "refs/stash")
          (magit-insert-heading "Stashes")
          (dolist (stash '("one" "two" "three" "four"))
            (magit-insert-section (stash stash)
              (magit-insert-heading stash)))))
      (goto-char (point-min))
      (search-forward "one")
      (beginning-of-line)
      (evil-normal-state)
      (cl-letf (((symbol-function 'magit-stash-drop)
                 (lambda (&optional _stash)
                   (interactive)
                   (setq selected (magit-region-values 'stash)))))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (execute-kbd-macro (kbd "V j x"))))
      (should (equal selected '("one" "two"))))))

(ert-deftest yunge-magit-enters-insert-state-for-blank-commit-message ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'git-commit)
  (should
   (memq #'yunge-magit--enter-insert-state-for-blank-commit-message
         git-commit-setup-hook))

  (dolist (case '(("\n# Commit message" insert)
                  ("Existing subject\n" normal)))
    (with-temp-buffer
      (text-mode)
      (insert (car case))
      (goto-char (point-min))
      (evil-local-mode 1)
      (evil-normal-state)
      (yunge-magit--enter-insert-state-for-blank-commit-message)
      (should (eq evil-state (cadr case))))))

(ert-deftest yunge-magit-integrates-interactive-rebase-with-evil ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'git-rebase)

  (yunge-test-evil-normal-keys
   'git-rebase-mode
   (append
    '(("RET" . git-rebase-show-commit)
      ("M-j" . git-rebase-move-line-down)
      ("M-k" . git-rebase-move-line-up)
      ("p" . git-rebase-pick)
      ("r" . git-rebase-reword)
      ("e" . git-rebase-edit)
      ("s" . git-rebase-squash)
      ("f" . git-rebase-fixup)
      ("d" . git-rebase-drop)
      ("x" . git-rebase-exec)
      ("u" . git-rebase-undo)
      ("ZZ" . with-editor-finish)
      ("ZQ" . with-editor-cancel))
    yunge-magit-test-horizontal-bindings))
  (yunge-test-evil-visual-keys
   'git-rebase-mode
   '(("M-j" . git-rebase-move-line-down)
     ("M-k" . git-rebase-move-line-up)
     ("p" . git-rebase-pick)
     ("r" . git-rebase-reword)
     ("e" . git-rebase-edit)
     ("s" . git-rebase-squash)
     ("f" . git-rebase-fixup)
     ("d" . git-rebase-drop))))

(ert-deftest yunge-magit-integrates-read-only-tools-with-evil ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit-blame)
  (require 'magit-files)
  (require 'magit-process)
  (require 'magit-repos)
  (require 'magit-submodule)

  (yunge-magit-test--minor-mode-keys
   'magit-blame-read-only-mode
   (append
    '(("RET" . magit-show-commit)
      ("C-j" . magit-blame-next-chunk)
      ("C-k" . magit-blame-previous-chunk)
      ("M-j" . magit-blame-next-chunk-same-commit)
      ("M-k" . magit-blame-previous-chunk-same-commit)
      ("c" . magit-blame-cycle-style)
      ("q" . magit-blame-quit))
    yunge-magit-test-horizontal-bindings))
  (yunge-magit-test--minor-mode-keys
   'magit-blob-mode
   (append
    '(("C-j" . magit-blob-next)
      ("C-k" . magit-blob-previous)
      ("q" . magit-bury-or-kill-buffer))
    yunge-magit-test-horizontal-bindings))
  (yunge-test-evil-normal-keys
   'magit-process-mode
   (append
    '(("C-j" . magit-section-forward)
      ("C-k" . magit-section-backward)
      ("M-h" . magit-section-up)
      ("<tab>" . magit-section-toggle)
      ("q" . magit-mode-bury-buffer)
      ("x" . magit-process-kill))
    yunge-magit-test-horizontal-bindings))
  (dolist (mode '(magit-repolist-mode
                  magit-submodule-list-mode))
    (yunge-test-evil-normal-keys
     mode
     (append
      '(("RET" . magit-repolist-status)
        ("f" . magit-repolist-fetch)
        ("gr" . revert-buffer)
        ("m" . magit-repolist-mark)
        ("q" . quit-window)
        ("u" . magit-repolist-unmark))
      yunge-magit-test-horizontal-bindings))))

(ert-deftest yunge-magit-rebase-visual-action-excludes-the-next-line ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'git-rebase)

  (with-temp-buffer
    (insert "pick 1111111 one\npick 2222222 two\npick 3333333 three\n")
    (git-rebase-mode)
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "V j d")))
    (should
     (equal
      (mapcar (lambda (line) (car (split-string line)))
              (split-string (buffer-string) "\n" t))
      '("drop" "drop" "pick")))))

(ert-deftest yunge-magit-resolves-point-local-keys ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit)
  (require 'magit-submodule)

  (dolist (entry `((,magit-diff-section-map
                    magit-diff-visit-worktree-file
                    magit-discard)
                   (,magit-module-section-map
                    magit-submodule-visit
                    magit-delete-thing)))
    (with-temp-buffer
      (magit-status-mode)
      (let ((inhibit-read-only t))
        (insert (propertize "section" 'keymap (car entry))))
      (goto-char (point-min))
      (evil-normal-state)
      (yunge-test-keys
       `(("C-j" . magit-section-forward)
         ("C-k" . magit-section-backward)
         ("h" . evil-backward-char)
         ("l" . evil-forward-char)
         ("s" . magit-stage)
         ("u" . magit-unstage)
         ("x" . ,(nth 2 entry))
         ("C-<return>" . ,(nth 1 entry)))))))

;;; yunge-magit-test.el ends here
