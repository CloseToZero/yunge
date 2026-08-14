;;; yunge-server.el --- Emacs server lifecycle -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function server-start "server" (&optional leave-dead inhibit-prompt))

(defvar server-process)

(defun yunge-server-start ()
  "Start the default server in the current Emacs when needed."
  (require 'server)
  ;; `server-running-p' can return `:other' for a stale TCP connection
  ;; file.  `server-start' already distinguishes that case from a live
  ;; server owned by another Emacs, so only inspect our own process here.
  (unless (process-live-p server-process)
    (server-start)))

;; Batch processes exit immediately and may run concurrently during tests.
(unless noninteractive
  (add-hook 'emacs-startup-hook #'yunge-server-start))

(provide 'yunge-server)

;;; yunge-server.el ends here
