;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(when (< emacs-major-version 31)
  (error "This configuration requires Emacs 31 or newer (found %s)"
         emacs-version))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'yunge-state)

;; Raise the GC threshold during startup, then restore Emacs's default.
(let ((threshold gc-cons-threshold))
  (setq gc-cons-threshold (* 128 1024 1024))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold threshold))))

;; Keep text portable without overriding the Windows clipboard encoding.
(prefer-coding-system 'utf-8-unix)

;; This configuration owns defaults and does not use platform resources.
(setq inhibit-default-init t
      inhibit-x-resources t)

;; Elpaca owns package activation.
(setq package-enable-at-startup nil)

;; Avoid rounding frame and window geometry to character-cell increments.
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

;; Avoid implicit resizing while startup changes fonts and frame chrome.
(setq frame-inhibit-implied-resize t)

(require 'yunge-font)
(require 'yunge-frame)

;; Keep maximization exclusive to the initial frame.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(startup-redirect-eln-cache
 (expand-file-name "eln-cache/" yunge-var-directory))

(defvar native-comp-async-report-warnings-errors)

;; Keep async native-comp diagnostics available without popping up *Warnings*.
(setq native-comp-async-report-warnings-errors 'silent)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Collapse every minor-mode lighter; symbols after `not' remain visible.
(setq mode-line-collapse-minor-modes '(not))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      ring-bell-function #'ignore)

;;; early-init.el ends here
