;;; yunge-theme.el --- Color theme configuration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-font)
(require 'yunge-key)

(declare-function elpaca-wait "elpaca")

(defvar modus-themes-after-load-theme-hook)
(defvar yunge-toggle-map)

(defconst yunge-theme-light 'modus-operandi
  "Light theme used by this configuration.")

(defconst yunge-theme-dark 'modus-vivendi
  "Dark theme used by this configuration.")

(defvar yunge-theme--immersion-restore-theme nil
  "Theme to restore when leaving immersion, or nil when inactive.")

(defvar-keymap yunge-theme-immersion-map
  :doc "Theme-specific immersion commands.")

(defun yunge-theme--enter-immersion (theme)
  "Enter fullscreen immersion using THEME."
  (let ((fullscreen
         (memq (frame-parameter nil 'fullscreen)
               '(fullboth fullscreen))))
    ;; A non-fullscreen entry starts a new immersion even if the preceding
    ;; session was left with F11 instead of `yunge-theme-exit-immersion'.
    (unless (and fullscreen yunge-theme--immersion-restore-theme)
      (setq yunge-theme--immersion-restore-theme
            (car custom-enabled-themes)))
    (unless (memq theme custom-enabled-themes)
      (modus-themes-load-theme theme))
    (unless fullscreen
      (toggle-frame-fullscreen))))

(defun yunge-theme-enter-light-immersion ()
  "Enter fullscreen immersion using the configured light theme."
  (interactive)
  (yunge-theme--enter-immersion yunge-theme-light))

(defun yunge-theme-enter-dark-immersion ()
  "Enter fullscreen immersion using the configured dark theme."
  (interactive)
  (yunge-theme--enter-immersion yunge-theme-dark))

(defun yunge-theme-exit-immersion ()
  "Leave fullscreen immersion and restore the preceding theme."
  (interactive)
  (when (memq (frame-parameter nil 'fullscreen)
              '(fullboth fullscreen))
    (toggle-frame-fullscreen))
  (when-let* ((theme yunge-theme--immersion-restore-theme))
    (unless (memq theme custom-enabled-themes)
      (modus-themes-load-theme theme))
    (setq yunge-theme--immersion-restore-theme nil)))

(defconst yunge-theme-immersion-bindings
  '(("d" yunge-theme-enter-dark-immersion "dark")
    ("l" yunge-theme-enter-light-immersion "light")
    ("q" yunge-theme-exit-immersion "exit")))

(defconst yunge-theme-toggle-bindings
  `(("d" modus-themes-toggle "dark/light theme")
    ("i" ,yunge-theme-immersion-map "immersion")))

(elpaca modus-themes
  (require 'modus-themes)

  ;; Keep the typography consistent with `yunge-font' while retaining visual
  ;; distinctions that carry meaning in code and prose.
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-variable-pitch-ui nil
        modus-themes-to-toggle
        (list yunge-theme-light yunge-theme-dark))

  ;; Match the deliberately minimal frame chrome configured in early-init.el.
  (setq modus-themes-common-palette-overrides
        '((fringe unspecified)))

  ;; Theme activation recalculates faces, so restore the configured Latin and
  ;; Han fonts after the initial load and every later theme switch.
  (add-hook 'modus-themes-after-load-theme-hook #'yunge-font-setup)
  (modus-themes-load-theme yunge-theme-light))

;; The theme is part of the first rendered frame, so unlike feature packages
;; it must be ready before initialization continues.
(elpaca-wait)

;; This library is loaded before Evil so that the first graphical frame is
;; themed immediately.  Install its leader binding once the map is available.
(with-eval-after-load 'yunge-evil
  (yunge-key-define yunge-theme-immersion-map
                    yunge-theme-immersion-bindings)
  (yunge-key-define yunge-toggle-map yunge-theme-toggle-bindings)
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-toggle-map yunge-theme-toggle-bindings)
    (yunge-key-add-which-key-descriptions
     yunge-theme-immersion-map yunge-theme-immersion-bindings)))

(provide 'yunge-theme)

;;; yunge-theme.el ends here
