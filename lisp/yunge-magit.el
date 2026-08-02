;;; yunge-magit.el --- Git interface -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-evil)

(declare-function evil-add-command-properties "evil-common")
(declare-function evil-insert-state "evil-states")

(defvar git-commit-setup-hook)
(defvar git-rebase-mode-map)
(defvar magit-cherry-mode-map)
(defvar magit-diff-mode-map)
(defvar magit-diff-section-map)
(defvar magit-log-mode-map)
(defvar magit-log-select-mode-map)
(defvar magit-mode-map)
(defvar magit-module-section-map)
(defvar magit-process-mode-map)
(defvar magit-revision-mode-map)
(defvar magit-refs-mode-map)
(defvar magit-status-mode-map)

(defconst yunge-magit-go-bindings
  '(("g" magit-status "Git status")))

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

(defconst yunge-magit-view-normal-bindings
  `(("RET" magit-visit-thing "visit")
    ("<tab>" magit-section-toggle "toggle section")
    ("gr" magit-refresh "refresh")
    ("gR" magit-refresh-all "refresh all")
    ([localleader] ,yunge-magit-command-map nil)))

(defconst yunge-magit-repository-normal-bindings
  '(("A" magit-cherry-pick "cherry-pick")
    ("b" magit-branch "branch")
    ("c" magit-commit "commit")
    ("d" magit-diff "diff")
    ("f" magit-fetch "fetch")
    ("F" magit-pull "pull")
    ("l" magit-log-current "show current log")
    ("m" magit-merge "merge")
    ("p" magit-push "push")
    ("r" magit-rebase "rebase")
    ("Z" magit-stash "stash")))

(defconst yunge-magit-status-normal-bindings
  '(("q" magit-mode-bury-buffer "quit")
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

(defun yunge-magit--enter-insert-state-for-blank-commit-message ()
  "Enter Insert state when a new commit message starts blank."
  (when (and (bound-and-true-p evil-local-mode)
             (bobp)
             (eolp))
    (evil-insert-state)))

(elpaca magit
  (yunge-key-define yunge-go-map yunge-magit-go-bindings)
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-go-map yunge-magit-go-bindings)
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
    (with-eval-after-load 'magit
      (yunge-key-evil-define
       'normal magit-mode-map
       yunge-magit-section-normal-bindings)
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
      (dolist (map (list magit-diff-mode-map
                         magit-revision-mode-map))
        (yunge-key-evil-define
         'normal map
         (append yunge-magit-section-normal-bindings
                 yunge-magit-view-normal-bindings
                 yunge-magit-repository-normal-bindings
                 yunge-magit-mode-quit-normal-bindings))))))

(provide 'yunge-magit)

;;; yunge-magit.el ends here
