;;; yunge-server.el --- Emacs server lifecycle -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function server-running-p "server" (&optional name))
(declare-function server-start "server" (&optional leave-dead inhibit-prompt))

(defun yunge-server-start ()
  "Start the default server when it is known not to be running."
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; Batch processes exit immediately and may run concurrently during tests.
(unless noninteractive
  (add-hook 'emacs-startup-hook #'yunge-server-start))

(provide 'yunge-server)

;;; yunge-server.el ends here
