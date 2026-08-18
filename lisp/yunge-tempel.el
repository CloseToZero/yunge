;;; yunge-tempel.el --- Inline templates -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-org)
(require 'yunge-state)

(declare-function tempel-expand "tempel")
(declare-function tempel-insert "tempel")

(defvar completion-at-point-functions)
(defvar tempel-path)

(defconst yunge-tempel-org-bindings
  '(("i" tempel-insert "insert template")))

(defun yunge-tempel--org-tab ()
  "Expand an exact Tempel trigger and report whether Org Tab was handled."
  (when (tempel-expand)
    (tempel-expand t)
    t))

(defun yunge-tempel--setup-org-capf ()
  "Make exact Tempel triggers complete before other Org candidates."
  (add-hook 'completion-at-point-functions #'tempel-expand nil t))

(defun yunge-tempel--setup-existing-org-buffers ()
  "Add Tempel completion to Org buffers opened before package readiness."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (yunge-tempel--setup-org-capf)))))

(defun yunge-tempel--setup ()
  "Configure Tempel templates and Org integration."
  (setq tempel-path
        (expand-file-name "template/*.eld" yunge-config-directory))
  (add-hook 'org-mode-hook #'yunge-tempel--setup-org-capf)
  (add-hook 'org-cycle-tab-first-hook #'yunge-tempel--org-tab)
  (yunge-tempel--setup-existing-org-buffers)
  (yunge-key-define yunge-org-command-map
                    yunge-tempel-org-bindings)
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-org-command-map yunge-tempel-org-bindings)))

(elpaca tempel
  (yunge-tempel--setup))

(provide 'yunge-tempel)

;;; yunge-tempel.el ends here
