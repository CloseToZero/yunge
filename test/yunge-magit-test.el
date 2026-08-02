;;; yunge-magit-test.el --- Magit tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(eval-when-compile
  (require 'magit-section))

(declare-function evil-normal-state "evil-states")
(declare-function evil-local-mode "evil-core" (&optional arg))
(declare-function git-rebase-mode "git-rebase")
(declare-function magit-diff-mode "magit-diff")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-insert-section--create "magit-section")
(declare-function magit-insert-section--finish "magit-section")
(declare-function magit-log-mode "magit-log")
(declare-function magit-log-select-mode "magit-log")
(declare-function magit-region-values "magit-section")
(declare-function magit-revision-mode "magit-diff")
(declare-function magit-status-mode "magit-status")
(declare-function yunge-magit--enter-insert-state-for-blank-commit-message
                  "yunge-magit")

(defvar evil-state)
(defvar git-commit-setup-hook)
(defvar magit-diff-section-map)
(defvar magit-module-section-map)
(defvar magit-root-section)

(defconst yunge-magit-test-repository-bindings
  '(("b" . magit-branch)
    ("c" . magit-commit)
    ("d" . magit-diff)
    ("f" . magit-fetch)
    ("F" . magit-pull)
    ("l" . magit-log-current)
    ("m" . magit-merge)
    ("p" . magit-push)
    ("r" . magit-rebase)
    ("Z" . magit-stash)))

(yunge-test-deftest-lazy-load yunge-magit
  (magit))

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
      ("q" . ,quit-command)
      ("gr" . magit-refresh)
      ("gR" . magit-refresh-all))
    yunge-magit-test-repository-bindings
    '(("SPC m SPC" . magit-dispatch)
      ("C-i" . yunge-jump-forward)))))

(ert-deftest yunge-magit-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-magit 'magit
   :before-ready
   '(progn
      (when (featurep 'magit)
        (error "Magit was loaded before its Elpaca body ran"))
      (when (keymap-lookup yunge-go-map "g")
        (error "Magit keys were bound before its Elpaca body ran")))
   :after-ready
   '(progn
      (unless (eq (keymap-lookup yunge-go-map "g") 'magit-status)
        (error "Magit keys were not bound after package readiness"))
      (when (featurep 'magit)
        (error "Magit was loaded by its configuration")))))

(ert-deftest yunge-magit-integrates-with-evil ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)

  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC g g" . magit-status)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC g"
   '(("g" nil "Git status")))

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
      ("q" . magit-mode-bury-buffer)
      ("gr" . magit-refresh)
      ("gR" . magit-refresh-all)
      ("s" . magit-stage-files)
      ("u" . magit-unstage-files)
      ("x" . magit-delete-thing))
    yunge-magit-test-repository-bindings
    '(("SPC m SPC" . magit-dispatch)
      ("C-i" . yunge-jump-forward))))
  (yunge-test-which-key-prefix-bindings
   'magit-status-mode "SPC m"
   '(("SPC" nil "dispatch")))
  (yunge-test-evil-visual-keys
   'magit-status-mode
   '(("s" . magit-stage-files)
     ("u" . magit-unstage-files)
     ("x" . magit-delete-thing))))

(ert-deftest yunge-magit-integrates-history-views-with-evil ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit-log)
  (require 'magit-diff)

  (yunge-magit-test--view-keys
   'magit-log-mode 'magit-log-bury-buffer)
  (dolist (mode '(magit-diff-mode magit-revision-mode))
    (yunge-magit-test--view-keys mode 'magit-mode-bury-buffer))
  (yunge-test-evil-normal-keys
   'magit-log-select-mode
   '(("C-j" . magit-section-forward)
     ("C-k" . magit-section-backward)
     ("RET" . magit-log-select-pick)
     ("<tab>" . magit-section-toggle)
     ("q" . magit-log-select-quit)
     ("C-i" . yunge-jump-forward))))

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
     ("ZQ" . with-editor-cancel)))
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
         ("s" . magit-stage)
         ("u" . magit-unstage)
         ("x" . ,(nth 2 entry))
         ("C-<return>" . ,(nth 1 entry)))))))

;;; yunge-magit-test.el ends here
