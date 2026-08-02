;;; yunge-mac.el --- macOS keyboard modifiers -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defvar ns-command-modifier)
(defvar ns-option-modifier)

;; Match the physical modifier positions used on PC keyboards: Command is
;; next to Space on Apple keyboards and therefore acts as Meta, while Option
;; acts as Super.
(setq ns-command-modifier 'meta
      ns-option-modifier 'super)

(defun yunge-mac-toggle-modifiers ()
  "Swap the Meta and Super assignments of Command and Option."
  (interactive)
  (if (and (eq ns-command-modifier 'meta)
           (eq ns-option-modifier 'super))
      (progn
        (setq ns-command-modifier 'super
              ns-option-modifier 'meta)
        (message "Command=Super, Option=Meta"))
    (setq ns-command-modifier 'meta
          ns-option-modifier 'super)
    (message "Command=Meta, Option=Super")))

(provide 'yunge-mac)

;;; yunge-mac.el ends here
