;;; yunge-elpaca.el --- Elpaca keybindings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function elpaca-ui-visit "elpaca-ui")
(declare-function evil-define-key* "evil-core")
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function tabulated-list-get-id "tabulated-list" (&optional pos))

(defvar elpaca-info-mode-map)
(defvar elpaca-log-mode-map)
(defvar elpaca-manager-mode-map)

(defun yunge-elpaca--move-entry-line (count)
  "Move COUNT physical lines when the destination is an Elpaca entry."
  (let ((origin (point)))
    (forward-line count)
    (if (tabulated-list-get-id)
        (back-to-indentation)
      (goto-char origin))))

(defun yunge-elpaca-next-line (count)
  "Move COUNT Elpaca entries forward."
  (interactive "p")
  (yunge-elpaca--move-entry-line count))

(defun yunge-elpaca-previous-line (count)
  "Move COUNT Elpaca entries backward."
  (interactive "p")
  (yunge-elpaca--move-entry-line (- count)))

(defun yunge-elpaca-visit-build ()
  "Visit the build directory of the package at point."
  (interactive)
  (elpaca-ui-visit 'build))

(defconst yunge-elpaca-command-bindings
  '(("c" elpaca-log-updates "check updates")
    ("i" elpaca-ui-mark-try "mark try")
    ("p" elpaca-ui-mark-pull "mark pull")
    ("r" elpaca-ui-mark-rebuild "mark rebuild")
    ("s" elpaca-ui-search "filter")))

(defvar-keymap yunge-elpaca-command-map
  :doc "Keymap for Elpaca commands.")

(yunge-key-define yunge-elpaca-command-map
                  yunge-elpaca-command-bindings)

(defconst yunge-elpaca-ui-normal-visual-bindings
  '(("d" elpaca-ui-mark-delete "mark delete")
    ("u" elpaca-ui-unmark "unmark")))

(defconst yunge-elpaca-ui-normal-bindings
  '(("j" yunge-elpaca-next-line "next line")
    ("k" yunge-elpaca-previous-line "previous line")
    ("RET" elpaca-ui-info "package info")
    ("q" quit-window "quit")
    ("x" elpaca-ui-execute-marks "execute marks")
    ("gr" elpaca-ui-search-refresh "refresh")
    ("gf" elpaca-ui-visit "visit source")
    ("gF" yunge-elpaca-visit-build "visit build")
    ("gx" elpaca-ui-browse-package "browse package")
    ("gl" elpaca-log "show log")
    ("gm" elpaca-manager "show manager")))

(defconst yunge-elpaca-log-normal-bindings
  '(("gd" elpaca-log-view-diff "view diff")))

(defconst yunge-elpaca-info-normal-bindings
  `(("RET" push-button "activate button")
    ("q" quit-window "quit")
    ("gr" revert-buffer "refresh")
    ,@yunge-key-button-navigation-bindings))

(defun yunge-elpaca--bind-ui-keys (map)
  "Add Evil bindings shared by Elpaca UI keymap MAP."
  (evil-define-key* '(normal visual) map
    [localleader] yunge-elpaca-command-map)
  (yunge-key-evil-define '(normal visual) map
                         yunge-elpaca-ui-normal-visual-bindings)
  (yunge-key-evil-define 'normal map
                         yunge-elpaca-ui-normal-bindings))

(with-eval-after-load 'evil
  (dolist (mode '(elpaca-info-mode
                  elpaca-log-mode
                  elpaca-manager-mode))
    (evil-set-initial-state mode 'normal))

  (with-eval-after-load 'elpaca-manager
    (yunge-elpaca--bind-ui-keys elpaca-manager-mode-map))

  (with-eval-after-load 'elpaca-log
    (yunge-elpaca--bind-ui-keys elpaca-log-mode-map)
    (yunge-key-evil-define 'normal elpaca-log-mode-map
                           yunge-elpaca-log-normal-bindings))

  (with-eval-after-load 'elpaca-info
    (yunge-key-evil-define 'normal elpaca-info-mode-map
                           yunge-elpaca-info-normal-bindings)))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-elpaca-command-map yunge-elpaca-command-bindings))

(provide 'yunge-elpaca)

;;; yunge-elpaca.el ends here
