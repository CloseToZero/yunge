;;; yunge-history.el --- History and saved locations -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-state)
(require 'recentf)
(require 'savehist)
(require 'saveplace)

(defconst yunge-history-autosave-interval (* 5 60)
  "Seconds between saves of persistent session state.")

(defvar yunge-reader-saved-places nil
  "Most recently used durable Yunge Reader places.")

;; Keep each history useful without allowing it to grow without bound.
(setq history-length 1000
      history-delete-duplicates t)

(setopt savehist-autosave-interval
        yunge-history-autosave-interval)
(add-to-list 'savehist-additional-variables
             'yunge-reader-saved-places)
(savehist-mode 1)

(setq recentf-max-saved-items 1000
      ;; Defer file-system probes until Emacs has been idle.
      recentf-auto-cleanup (* 10 60)
      recentf-show-messages nil)
(setopt recentf-autosave-interval
        yunge-history-autosave-interval)
(recentf-mode 1)

(setq save-place-limit 1000)
(setopt save-place-autosave-interval
        yunge-history-autosave-interval)
(save-place-mode 1)

(provide 'yunge-history)

;;; yunge-history.el ends here
