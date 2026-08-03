;;; yunge-magit.el --- Git interface -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-evil)

(declare-function evil-add-command-properties "evil-common")
(declare-function evil-insert-state "evil-states")
(declare-function transient-bind-q-to-quit "transient")
(declare-function transient-suffix-put "transient"
                  (prefix loc prop value))

(defvar git-commit-setup-hook)
(defvar git-rebase-mode-map)
(defvar magit-define-global-key-bindings)
(defvar magit-cherry-mode-map)
(defvar magit-diff-mode-map)
(defvar magit-diff-section-map)
(defvar magit-log-mode-map)
(defvar magit-log-select-mode-map)
(defvar magit-mode-map)
(defvar magit-module-section-map)
(defvar magit-process-mode-map)
(defvar magit-repolist-mode-map)
(defvar magit-refs-mode-map)
(defvar magit-status-mode-map)
(defvar project-prefix-map)
(defvar project-switch-commands)
(defvar transient-popup-navigation-map)

;; Global Magit entry points belong to this configuration's leader maps.
(setq magit-define-global-key-bindings nil)

(defconst yunge-magit-file-bindings
  '(("g" magit-file-dispatch "Git actions")))

(defconst yunge-magit-go-bindings
  '(("g" magit-status "Git status")))

(defconst yunge-magit-project-bindings
  '(("m" magit-project-status "Magit status")))

(defconst yunge-magit-command-bindings
  '(("SPC" magit-dispatch "dispatch")))

(defvar-keymap yunge-magit-command-map
  :doc "Keymap for Magit commands.")

(yunge-key-define yunge-magit-command-map
                  yunge-magit-command-bindings)

(defconst yunge-magit-section-normal-bindings
  '(("C-j" magit-section-forward "next section")
    ("C-k" magit-section-backward "previous section")
    ("M-h" magit-section-up "parent section")
    ("M-j" magit-section-forward-sibling "next sibling")
    ("M-k" magit-section-backward-sibling "previous sibling")
    ("z1" magit-section-show-level-1-all "show all through level 1")
    ("z2" magit-section-show-level-2-all "show all through level 2")
    ("z3" magit-section-show-level-3-all "show all through level 3")
    ("z4" magit-section-show-level-4-all "show all through level 4")
    ("za" magit-section-toggle "toggle section")
    ("zc" magit-section-hide "hide section")
    ("zC" magit-section-hide-children "hide child sections")
    ("zo" magit-section-show "show section")
    ("zO" magit-section-show-children "show child sections")))

(defconst yunge-magit-copy-bindings
  '(("s" magit-copy-section-value "section value")))

(defvar-keymap yunge-magit-copy-map
  :doc "Keymap for copying values from Magit views.")

(yunge-key-define yunge-magit-copy-map
                  yunge-magit-copy-bindings)

(defconst yunge-magit-view-normal-bindings
  `(("RET" magit-visit-thing "visit")
    ("<tab>" magit-section-toggle "toggle section")
    ("g<" magit-process-buffer "show process output")
    ("gr" magit-refresh "refresh")
    ("gR" magit-refresh-all "refresh all")
    ("y" ,yunge-magit-copy-map "copy")
    ([localleader] ,yunge-magit-command-map nil)))

(defconst yunge-magit-repository-normal-bindings
  '(("A" magit-cherry-pick "cherry-pick")
    ("b" magit-branch "branch")
    ("c" magit-commit "commit")
    ("d" magit-diff "diff")
    ("f" magit-fetch "fetch")
    ("F" magit-pull "pull")
    ("l" magit-log "log")
    ("m" magit-merge "merge")
    ("p" magit-push "push")
    ("r" magit-rebase "rebase")
    ("X" magit-reset "reset")
    ("Z" magit-stash "stash")))

(defconst yunge-magit-status-normal-bindings
  '(("gn" magit-jump-to-untracked "go to untracked")
    ("gs" magit-jump-to-staged "go to staged")
    ("gu" magit-jump-to-unstaged "go to unstaged")
    ("gz" magit-jump-to-stashes "go to stashes")
    ("q" magit-mode-bury-buffer "quit")
    ("s" magit-stage-files "stage")
    ("u" magit-unstage-files "unstage")
    ("x" magit-delete-thing "discard")))

(defconst yunge-magit-history-normal-bindings
  '(("q" magit-log-bury-buffer "quit")))

(defconst yunge-magit-log-select-normal-bindings
  '(("RET" magit-log-select-pick "select")
    ("<tab>" magit-section-toggle "toggle section")
    ("q" magit-log-select-quit "cancel")))

(defconst yunge-magit-mode-quit-normal-bindings
  '(("q" magit-mode-bury-buffer "quit")))

(defconst yunge-magit-status-visual-bindings
  '(("s" magit-stage-files "stage")
    ("u" magit-unstage-files "unstage")
    ("x" magit-delete-thing "discard")))

(defconst yunge-magit-rebase-normal-bindings
  '(("RET" git-rebase-show-commit "show commit")
    ("M-j" git-rebase-move-line-down "move line down")
    ("M-k" git-rebase-move-line-up "move line up")
    ("p" git-rebase-pick "pick")
    ("r" git-rebase-reword "reword")
    ("e" git-rebase-edit "edit")
    ("s" git-rebase-squash "squash")
    ("f" git-rebase-fixup "fixup")
    ("d" git-rebase-drop "drop")
    ("x" git-rebase-exec "execute")
    ("u" git-rebase-undo "undo")))

(defconst yunge-magit-rebase-visual-bindings
  '(("M-j" git-rebase-move-line-down "move selection down")
    ("M-k" git-rebase-move-line-up "move selection up")
    ("p" git-rebase-pick "pick")
    ("r" git-rebase-reword "reword")
    ("e" git-rebase-edit "edit")
    ("s" git-rebase-squash "squash")
    ("f" git-rebase-fixup "fixup")
    ("d" git-rebase-drop "drop")))

(defconst yunge-magit-blame-normal-bindings
  '(("RET" magit-show-commit "show commit")
    ("C-j" magit-blame-next-chunk "next chunk")
    ("C-k" magit-blame-previous-chunk "previous chunk")
    ("M-j" magit-blame-next-chunk-same-commit
     "next chunk from this commit")
    ("M-k" magit-blame-previous-chunk-same-commit
     "previous chunk from this commit")
    ("c" magit-blame-cycle-style "cycle style")
    ("q" magit-blame-quit "quit")))

(defconst yunge-magit-blob-normal-bindings
  '(("C-j" magit-blob-next "next revision")
    ("C-k" magit-blob-previous "previous revision")
    ("q" magit-bury-or-kill-buffer "quit")))

(defconst yunge-magit-process-normal-bindings
  '(("<tab>" magit-section-toggle "toggle section")
    ("q" magit-mode-bury-buffer "quit")
    ("x" magit-process-kill "kill process")))

(defconst yunge-magit-repolist-normal-bindings
  '(("RET" magit-repolist-status "show status")
    ("f" magit-repolist-fetch "fetch")
    ("gr" revert-buffer "refresh")
    ("m" magit-repolist-mark "mark")
    ("q" quit-window "quit")
    ("u" magit-repolist-unmark "unmark")))

(defun yunge-magit--enter-insert-state-for-blank-commit-message ()
  "Enter Insert state when a new commit message starts blank."
  (when (and (bound-and-true-p evil-local-mode)
             (bobp)
             (eolp))
    (evil-insert-state)))

(elpaca magit
  (yunge-key-define yunge-file-map yunge-magit-file-bindings)
  (yunge-key-define yunge-go-map yunge-magit-go-bindings)
  (yunge-key-define project-prefix-map yunge-magit-project-bindings)
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(magit-project-status "Magit") t))
  (with-eval-after-load 'transient
    (transient-bind-q-to-quit)
    ;; Keep popup navigation consistent with completion and Magit views.
    (keymap-set transient-popup-navigation-map "C-j"
                #'transient-forward-button)
    (keymap-set transient-popup-navigation-map "C-k"
                #'transient-backward-button))
  (with-eval-after-load 'magit
    (transient-suffix-put 'magit-dispatch 'magit-discard :key "x"))
  (with-eval-after-load 'magit-files
    ;; These are duplicate conditional suffixes, so change both locations.
    (transient-suffix-put 'magit-file-dispatch ", k" :key ", X")
    (transient-suffix-put 'magit-file-dispatch "k" :key "X"))
  (with-eval-after-load 'magit-branch
    (transient-suffix-put
     'magit-branch 'magit-branch-reset :key "X")
    (transient-suffix-put
     'magit-branch 'magit-branch-delete :key "x"))
  (with-eval-after-load 'magit-stash
    (transient-suffix-put
     'magit-stash 'magit-stash-keep-index :key "k")
    (transient-suffix-put
     'magit-stash 'magit-stash-drop :key "x"))
  (with-eval-after-load 'magit-sequence
    (transient-suffix-put
     'magit-rebase 'magit-rebase-remove-commit :key "x"))
  (with-eval-after-load 'magit-remote
    (transient-suffix-put
     'magit-remote 'magit-remote-remove :key "x"))
  (with-eval-after-load 'magit-submodule
    (transient-suffix-put
     'magit-submodule 'magit-submodule-remove :key "x"))
  (with-eval-after-load 'magit-tag
    (transient-suffix-put 'magit-tag 'magit-tag-delete :key "x"))
  (with-eval-after-load 'magit-worktree
    (transient-suffix-put
     'magit-worktree 'magit-worktree-delete :key "x"))
  (with-eval-after-load 'magit-notes
    (transient-suffix-put
     'magit-notes 'magit-notes-remove :key "x"))
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-file-map yunge-magit-file-bindings)
    (yunge-key-add-which-key-descriptions
     yunge-go-map yunge-magit-go-bindings)
    (yunge-key-add-which-key-descriptions
     project-prefix-map yunge-magit-project-bindings)
    (yunge-key-add-which-key-descriptions
     yunge-magit-copy-map yunge-magit-copy-bindings)
    (yunge-key-add-which-key-descriptions
     yunge-magit-command-map yunge-magit-command-bindings))
  ;; Section keymaps at point take precedence over Evil's mode bindings.
  ;; Remove their C-j bindings so navigation also works on file and module
  ;; rows.  C-RET retains the original visit commands.
  (with-eval-after-load 'magit-diff
    (keymap-unset magit-diff-section-map "C-j" t))
  (with-eval-after-load 'magit-submodule
    (keymap-unset magit-module-section-map "C-j" t))
  (with-eval-after-load 'git-commit
    (add-hook 'git-commit-setup-hook
              #'yunge-magit--enter-insert-state-for-blank-commit-message))
  (with-eval-after-load 'evil
    (with-eval-after-load 'magit-blame
      (yunge-key-evil-define-minor-mode
       'normal 'magit-blame-read-only-mode
       yunge-magit-blame-normal-bindings))
    (with-eval-after-load 'magit-files
      (yunge-key-evil-define-minor-mode
       'normal 'magit-blob-mode
       yunge-magit-blob-normal-bindings))
    (with-eval-after-load 'magit-process
      (yunge-key-evil-define
       'normal magit-process-mode-map
       (append yunge-magit-section-normal-bindings
               yunge-magit-process-normal-bindings)))
    (with-eval-after-load 'magit-repos
      (yunge-key-evil-define
       'normal magit-repolist-mode-map
       yunge-magit-repolist-normal-bindings))
    (with-eval-after-load 'git-rebase
      (yunge-key-evil-define
       'normal git-rebase-mode-map
       yunge-magit-rebase-normal-bindings)
      (yunge-key-evil-define
       'visual git-rebase-mode-map
       yunge-magit-rebase-visual-bindings)
      ;; Evil's linewise region includes its final newline.  Excluding it
      ;; prevents a rebase action from changing the following commit too.
      (dolist (command '(git-rebase-drop
                         git-rebase-edit
                         git-rebase-fixup
                         git-rebase-pick
                         git-rebase-reword
                         git-rebase-squash))
        (evil-add-command-properties command :exclude-newline t)))
    (with-eval-after-load 'magit-mode
      (yunge-key-evil-define
       'normal magit-mode-map
       yunge-magit-section-normal-bindings))
    (with-eval-after-load 'magit-status
      (yunge-key-evil-define
       'normal magit-status-mode-map
       (append yunge-magit-section-normal-bindings
               yunge-magit-view-normal-bindings
               yunge-magit-repository-normal-bindings
               yunge-magit-status-normal-bindings))
      (yunge-key-evil-define
       'visual magit-status-mode-map
       yunge-magit-status-visual-bindings)
      ;; A linewise selection includes its final newline, which makes Magit
      ;; treat the next section as selected too.
      (dolist (command '(magit-discard magit-stage magit-unstage))
        (evil-add-command-properties command :exclude-newline t)))
    (with-eval-after-load 'magit-stash
      ;; Keep a linewise stash selection from including the next stash.
      (evil-add-command-properties 'magit-stash-drop
                                   :exclude-newline t))
    (with-eval-after-load 'magit-log
      (dolist (map (list magit-log-mode-map
                         magit-cherry-mode-map))
        (yunge-key-evil-define
         'normal map
         (append yunge-magit-section-normal-bindings
                 yunge-magit-view-normal-bindings
                 yunge-magit-repository-normal-bindings
                 yunge-magit-history-normal-bindings)))
      (yunge-key-evil-define
       'normal magit-log-select-mode-map
       (append yunge-magit-section-normal-bindings
               yunge-magit-log-select-normal-bindings)))
    (with-eval-after-load 'magit-refs
      (yunge-key-evil-define
       'normal magit-refs-mode-map
       (append yunge-magit-section-normal-bindings
               yunge-magit-view-normal-bindings
               yunge-magit-repository-normal-bindings
               yunge-magit-mode-quit-normal-bindings)))
    (with-eval-after-load 'magit-diff
      ;; Revision mode inherits this map, so configure its shared parent.
      (yunge-key-evil-define
       'normal magit-diff-mode-map
       (append yunge-magit-section-normal-bindings
               yunge-magit-view-normal-bindings
               yunge-magit-repository-normal-bindings
               yunge-magit-mode-quit-normal-bindings)))))

(provide 'yunge-magit)

;;; yunge-magit.el ends here
