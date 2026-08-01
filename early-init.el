;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

;; Keep text portable without overriding the Windows clipboard encoding.
(prefer-coding-system 'utf-8-unix)

;; Keep the initial frame clean before it becomes visible.  The fullscreen
;; entry belongs here (rather than `default-frame-alist') so later frames keep
;; their normal size.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(startup-redirect-eln-cache
 (expand-file-name "var/eln-cache/" user-emacs-directory))

;; Keep async native-comp diagnostics available without popping up *Warnings*.
(setq native-comp-async-report-warnings-errors 'silent)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      ring-bell-function #'ignore)

;;; early-init.el ends here
