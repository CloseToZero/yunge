;;; yunge-theme-test.el --- Theme tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar modus-themes-bold-constructs)
(defvar modus-themes-after-load-theme-hook)
(defvar modus-themes-common-palette-overrides)
(defvar modus-themes-italic-constructs)
(defvar modus-themes-mixed-fonts)
(defvar modus-themes-to-toggle)
(defvar modus-themes-variable-pitch-ui)
(defvar yunge-theme--immersion-restore-theme)
(defvar yunge-theme-immersion-map)

(defun yunge-theme-test--load-config ()
  "Load the theme configuration synchronously for tests."
  (cl-letf (((symbol-function 'elpaca-wait) #'ignore))
    (yunge-test-load-package-config 'yunge-theme)))

(ert-deftest yunge-theme-configures-modus-pair ()
  (yunge-test-run-package-config
   'yunge-theme 'modus-themes
   :setup '(defun elpaca-wait () nil)
   :before-ready
   '(when (featurep 'modus-themes)
      (error "Modus was loaded before package readiness"))
   :after-ready
   '(progn
      (unless (equal custom-enabled-themes '(modus-operandi))
        (error "The default light theme was not enabled"))
      (unless (equal modus-themes-to-toggle
                     '(modus-operandi modus-vivendi))
        (error "The Modus theme pair was not configured"))
      (unless (and modus-themes-bold-constructs
                   modus-themes-italic-constructs
                   (not modus-themes-mixed-fonts)
                   (not modus-themes-variable-pitch-ui))
        (error "Theme typography was not configured"))
      (unless (equal modus-themes-common-palette-overrides
                     '((fringe unspecified)))
        (error "Theme palette overrides were not configured"))
      (unless (memq 'yunge-font-setup
                    modus-themes-after-load-theme-hook)
        (error "Font restoration was not registered")))))

(ert-deftest yunge-theme-integrates-with-toggle-map ()
  (yunge-theme-test--load-config)
  (yunge-test-enable-evil)
  (require 'which-key)

  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC t d" . modus-themes-toggle)
     ("SPC t i d" . yunge-theme-enter-dark-immersion)
     ("SPC t i l" . yunge-theme-enter-light-immersion)
     ("SPC t i q" . yunge-theme-exit-immersion)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC t"
   '(("d" nil "dark/light theme")
     ("i" nil "+immersion")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC t i"
   '(("d" nil "dark")
     ("l" nil "light")
     ("q" nil "exit"))))

(ert-deftest yunge-theme-toggles-between-light-and-dark ()
  (yunge-theme-test--load-config)
  (let ((font-setup (symbol-function 'yunge-font-setup))
        (font-setup-calls 0))
    (cl-letf (((symbol-function 'yunge-font-setup)
               (lambda ()
                 (cl-incf font-setup-calls)
                 (funcall font-setup))))
      (call-interactively #'modus-themes-toggle)
      (should (equal custom-enabled-themes '(modus-vivendi)))
      (should (= font-setup-calls 1))
      (call-interactively #'modus-themes-toggle)
      (should (equal custom-enabled-themes '(modus-operandi)))
      (should (= font-setup-calls 2)))))

(ert-deftest yunge-theme-immersion-keeps-fullscreen-and-selects-theme ()
  (yunge-theme-test--load-config)
  (let ((custom-enabled-themes '(modus-vivendi))
        (yunge-theme--immersion-restore-theme nil)
        (fullscreen 'maximized)
        (fullscreen-calls 0)
        theme-loads)
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (and (eq parameter 'fullscreen) fullscreen)))
              ((symbol-function 'toggle-frame-fullscreen)
               (lambda (&optional _frame)
                 (cl-incf fullscreen-calls)
                 (setq fullscreen
                       (if (eq fullscreen 'fullboth)
                           'maximized
                         'fullboth))))
              ((symbol-function 'modus-themes-load-theme)
               (lambda (theme)
                 (push theme theme-loads)
                 (setq custom-enabled-themes (list theme)))))
      (yunge-theme-enter-light-immersion)
      (should (eq fullscreen 'fullboth))
      (should (= fullscreen-calls 1))
      (should (equal theme-loads '(modus-operandi)))
      (should (eq yunge-theme--immersion-restore-theme
                  'modus-vivendi))

      (yunge-theme-enter-light-immersion)
      (should (= fullscreen-calls 1))
      (should (equal theme-loads '(modus-operandi)))

      (yunge-theme-enter-dark-immersion)
      (should (= fullscreen-calls 1))
      (should (equal theme-loads
                     '(modus-vivendi modus-operandi)))
      (should (eq yunge-theme--immersion-restore-theme
                  'modus-vivendi))

      (yunge-theme-enter-light-immersion)
      (yunge-theme-exit-immersion)
      (should (eq fullscreen 'maximized))
      (should (= fullscreen-calls 2))
      (should (equal theme-loads
                     '(modus-vivendi modus-operandi
                       modus-vivendi modus-operandi)))
      (should-not yunge-theme--immersion-restore-theme)

      ;; Leaving through F11 cannot clear our state directly.  A later entry
      ;; from a non-fullscreen frame must therefore start a fresh snapshot.
      (yunge-theme-enter-dark-immersion)
      (setq fullscreen 'maximized
            custom-enabled-themes '(modus-operandi))
      (yunge-theme-enter-dark-immersion)
      (should (eq yunge-theme--immersion-restore-theme
                  'modus-operandi))
      (yunge-theme-exit-immersion)
      (should (equal custom-enabled-themes '(modus-operandi)))
      (should-not yunge-theme--immersion-restore-theme))))

;;; yunge-theme-test.el ends here
