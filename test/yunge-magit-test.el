;;; yunge-magit-test.el --- Magit tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-normal-state "evil-states")
(declare-function magit-status-mode "magit-status")

(defvar magit-diff-section-map)
(defvar magit-module-section-map)

(yunge-test-deftest-lazy-load yunge-magit
  (magit))

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
     ("c" . magit-commit)
     ("RET" . magit-visit-thing)
     ("<tab>" . magit-section-toggle)
     ("q" . magit-mode-bury-buffer)
     ("gr" . magit-refresh)
     ("gR" . magit-refresh-all)
     ("s" . magit-stage-files)
     ("u" . magit-unstage-files)
     ("SPC m SPC" . magit-dispatch)
     ("C-i" . yunge-jump-forward)))
  (yunge-test-which-key-prefix-bindings
   'magit-status-mode "SPC m"
   '(("SPC" nil "dispatch"))))

(ert-deftest yunge-magit-resolves-point-local-keys ()
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit)
  (require 'magit-submodule)

  (dolist (entry `((,magit-diff-section-map
                    . magit-diff-visit-worktree-file)
                   (,magit-module-section-map
                    . magit-submodule-visit)))
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
         ("C-<return>" . ,(cdr entry)))))))

;;; yunge-magit-test.el ends here
