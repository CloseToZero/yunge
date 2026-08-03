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
   '(("SPC t d" . modus-themes-toggle)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC t"
   '(("d" nil "dark/light theme"))))

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

;;; yunge-theme-test.el ends here
