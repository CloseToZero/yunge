;;; yunge-frame.el --- Frame decoration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-state)

;; Our Windows build adds this primitive because the upstream helper makes
;; maximized W32 frames undecorated.  That removes the resize border and lets
;; the client area extend behind the taskbar.  The custom primitive removes
;; only the caption and constrains maximized geometry to the monitor work area.
(declare-function w32-set-title-bar-visible "w32fns.c" (frame visible))
(declare-function w32-shell-execute "w32fns.c"
                  (operation document &optional parameters show-flag))

(defun yunge-frame--update-windows-caption-with-powershell (frame)
  "Match FRAME's native caption to its maximized state using PowerShell."
  (when-let* ((powershell (or (executable-find "pwsh")
                              (executable-find "powershell")))
              (handle (frame-parameter frame 'window-id)))
    (let* ((script
            (expand-file-name "script/yunge-title-bar.ps1"
                              yunge-config-directory))
           (command (format "& '%s' -Handle %s" script handle))
           (encoded-command
            (base64-encode-string
             (encode-coding-string command 'utf-16le)
             t)))
      (w32-shell-execute
       "open" powershell
       (concat
        "-NoLogo -NoProfile -NonInteractive -WindowStyle Hidden "
        "-ExecutionPolicy Bypass -EncodedCommand "
        encoded-command)
       0)
      t)))

(defun yunge-frame--set-windows-title-bar-visible (frame visible)
  "Set FRAME's title bar visibility to VISIBLE when a backend is available."
  (if (fboundp 'w32-set-title-bar-visible)
      (progn
        (w32-set-title-bar-visible frame visible)
        t)
    ;; Stock Emacs lacks our primitive, so retain the process fallback.
    (yunge-frame--update-windows-caption-with-powershell frame)))

(defun yunge-frame-update-windows-title-bar (frame)
  "Hide FRAME's title bar only while it is maximized on Windows."
  (when (eq (framep frame) 'w32)
    (let ((fullscreen (frame-parameter frame 'fullscreen))
          (state (frame-parameter frame
                                  'yunge-frame--title-bar-state)))
      (if (memq fullscreen '(fullboth fullscreen))
          ;; Native fullscreen owns the complete frame style.
          (set-frame-parameter frame
                               'yunge-frame--title-bar-state nil)
        (let ((desired (if (eq fullscreen 'maximized)
                           'hidden
                         'shown)))
          (unless (eq state desired)
            (let ((needs-update (or state (eq desired 'hidden))))
              ;; A new restored frame already has its normal caption.
              (when (or (not needs-update)
                        (yunge-frame--set-windows-title-bar-visible
                         frame (eq desired 'shown)))
                (set-frame-parameter
                 frame 'yunge-frame--title-bar-state desired)))))))))

(defun yunge-frame-sync-mac-appearance (&optional _theme)
  "Match macOS frame chrome to the current theme."
  (let ((appearance (frame-parameter nil 'background-mode)))
    (dolist (frame (frame-list))
      (when (eq (framep frame) 'ns)
        (setq appearance (frame-parameter frame 'background-mode))
        (set-frame-parameter frame 'ns-appearance appearance)))
    (setf (alist-get 'ns-appearance default-frame-alist)
          appearance)))

(pcase system-type
  ('windows-nt
   (add-hook 'window-size-change-functions
             #'yunge-frame-update-windows-title-bar))
  ('darwin
   ;; Blend native chrome into Emacs without removing window controls.
   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
   (add-hook 'enable-theme-functions
             #'yunge-frame-sync-mac-appearance)
   (add-hook 'disable-theme-functions
             #'yunge-frame-sync-mac-appearance))
  (_
   (add-hook 'window-size-change-functions
             #'frame-hide-title-bar-when-maximized)))

(provide 'yunge-frame)

;;; yunge-frame.el ends here
