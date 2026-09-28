;;; yunge-theme-test.el --- Theme tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar yunge-theme--theme-before-immersion)
(defvar yunge-theme-immersion-map)

(defun yunge-theme-test--load-config ()
  "Load the theme configuration synchronously for tests."
  (cl-letf (((symbol-function 'elpaca-wait) #'ignore))
    (yunge-test-load-package-config 'yunge-theme)))

(ert-deftest yunge-theme-starts-with-light-theme-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-theme 'modus-themes
   :setup '(defun elpaca-wait () nil)
   :before-ready
   '(when (featurep 'modus-themes)
      (error "Modus was loaded before package readiness"))
   :after-ready
   '(unless (equal custom-enabled-themes '(modus-operandi))
      (error "The default light theme was not enabled"))))

(ert-deftest yunge-theme-binds-toggle-and-immersion-keys ()
  (yunge-theme-test--load-config)
  (yunge-test-enable-evil)
  (require 'which-key)

  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC t d" . modus-themes-toggle)
     ("SPC t i d" . yunge-theme-enter-dark-immersion)
     ("SPC t i l" . yunge-theme-enter-light-immersion)
     ("SPC t i q" . yunge-theme-exit-immersion))))

(ert-deftest yunge-theme-toggle-switches-between-light-and-dark ()
  (yunge-theme-test--load-config)
  (call-interactively #'modus-themes-toggle)
  (should (equal custom-enabled-themes '(modus-vivendi)))
  (call-interactively #'modus-themes-toggle)
  (should (equal custom-enabled-themes '(modus-operandi))))

(ert-deftest yunge-theme-immersion-restores-prior-theme-and-fullscreen ()
  (yunge-theme-test--load-config)
  (let ((custom-enabled-themes '(modus-vivendi))
        (yunge-theme--theme-before-immersion nil)
        (fullscreen 'maximized))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (and (eq parameter 'fullscreen) fullscreen)))
              ((symbol-function 'toggle-frame-fullscreen)
               (lambda (&optional _frame)
                 (setq fullscreen
                       (if (eq fullscreen 'fullboth)
                           'maximized
                         'fullboth))))
              ((symbol-function 'modus-themes-load-theme)
               (lambda (theme)
                 (setq custom-enabled-themes (list theme)))))
      (yunge-theme-enter-light-immersion)
      (should (eq fullscreen 'fullboth))
      (should (equal custom-enabled-themes '(modus-operandi)))

      (yunge-theme-enter-light-immersion)
      (should (eq fullscreen 'fullboth))
      (should (equal custom-enabled-themes '(modus-operandi)))

      (yunge-theme-enter-dark-immersion)
      (should (eq fullscreen 'fullboth))
      (should (equal custom-enabled-themes '(modus-vivendi)))

      (yunge-theme-enter-light-immersion)
      (yunge-theme-exit-immersion)
      (should (eq fullscreen 'maximized))
      (should (equal custom-enabled-themes '(modus-vivendi)))

      ;; Simulate leaving through F11, then starting a new immersion.
      (yunge-theme-enter-dark-immersion)
      (setq fullscreen 'maximized
            custom-enabled-themes '(modus-operandi))
      (yunge-theme-enter-dark-immersion)
      (should (eq fullscreen 'fullboth))
      (should (equal custom-enabled-themes '(modus-vivendi)))
      (yunge-theme-exit-immersion)
      (should (eq fullscreen 'maximized))
      (should (equal custom-enabled-themes '(modus-operandi))))))

;;; yunge-theme-test.el ends here
