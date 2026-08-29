;;; yunge-history.el --- History and saved locations -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-state)
(require 'recentf)
(require 'savehist)
(require 'saveplace)

(defconst yunge-history-autosave-interval (* 5 60)
  "Seconds between saves of persistent session state.")

(defconst yunge-history-max-items 1000
  "Maximum number of entries retained by common histories.")

(defconst yunge-history-additional-variables
  '(kill-ring
    command-history
    search-ring
    regexp-search-ring
    kmacro-ring
    evil-ex-history
    evil-eval-history
    evil-ex-search-history
    evil-search-forward-history
    evil-search-backward-history)
  "Histories persisted explicitly by `savehist-mode'.")

(defvar yunge-reader-saved-document-state nil
  "Versioned persistent Yunge Reader document records.")

;; Keep each history useful without allowing it to grow without bound.
(setq history-length yunge-history-max-items
      history-delete-duplicates t
      kill-ring-max yunge-history-max-items
      search-ring-max yunge-history-max-items
      regexp-search-ring-max yunge-history-max-items
      kmacro-ring-max yunge-history-max-items)

(setopt savehist-autosave-interval
        yunge-history-autosave-interval)
(dolist (variable yunge-history-additional-variables)
  (add-to-list 'savehist-additional-variables
               (cons variable yunge-history-max-items)))
(add-to-list 'savehist-additional-variables
             'yunge-reader-saved-document-state)
(savehist-mode 1)

(setq recentf-max-saved-items yunge-history-max-items
      ;; Defer file-system probes until Emacs has been idle.
      recentf-auto-cleanup (* 10 60)
      recentf-show-messages nil)
(setopt recentf-autosave-interval
        yunge-history-autosave-interval)
(recentf-mode 1)

(setq save-place-limit yunge-history-max-items)
(setopt save-place-autosave-interval
        yunge-history-autosave-interval)
(save-place-mode 1)

(provide 'yunge-history)

;;; yunge-history.el ends here
