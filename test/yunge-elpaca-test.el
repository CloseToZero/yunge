;;; yunge-elpaca-test.el --- Elpaca tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-elpaca
  (evil which-key elpaca-ui elpaca-log elpaca-info))

(ert-deftest yunge-elpaca-binds-keys ()
  (require 'yunge-elpaca)
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'elpaca-manager)
  (require 'elpaca-log)
  (require 'elpaca-info)

  (let ((elpaca-menu-functions nil))
    (yunge-test-evil-normal-keys
     'elpaca-manager-mode
     '(("RET" . elpaca-ui-info)
       ("q" . quit-window)
       ("d" . elpaca-ui-mark-delete)
       ("u" . elpaca-ui-unmark)
       ("x" . elpaca-ui-execute-marks)
       ("m" . evil-set-marker)
       ("gr" . elpaca-ui-search-refresh)
       ("gf" . elpaca-ui-visit)
       ("gF" . yunge-elpaca-visit-build)
       ("gx" . elpaca-ui-browse-package)
       ("gl" . elpaca-log)
       ("gm" . elpaca-manager)
       ("SPC m c" . elpaca-log-updates)
       ("SPC m i" . elpaca-ui-mark-try)
       ("SPC m p" . elpaca-ui-mark-pull)
       ("SPC m r" . elpaca-ui-mark-rebuild)
       ("SPC m s" . elpaca-ui-search)))

    (yunge-test-evil-visual-keys
     'elpaca-manager-mode
     '(("d" . elpaca-ui-mark-delete)
       ("u" . elpaca-ui-unmark)
       ("SPC m p" . elpaca-ui-mark-pull))))

  (yunge-test-evil-normal-keys
   'elpaca-log-mode
   '(("d" . elpaca-ui-mark-delete)
     ("gd" . elpaca-log-view-diff)))

  (yunge-test-evil-normal-keys
   'elpaca-info-mode
   '(("RET" . push-button)
     ("q" . quit-window)
     ("gr" . revert-buffer)
     ("g]" . forward-button)
     ("g[" . backward-button)
     ("TAB" . forward-button)
     ("S-TAB" . backward-button)))

  (dolist (mode '(elpaca-manager-mode elpaca-log-mode))
    (yunge-test-which-key-bindings
     mode yunge-elpaca-ui-normal-visual-bindings)
    (yunge-test-which-key-bindings
     mode yunge-elpaca-ui-normal-bindings))
  (let ((elpaca-menu-functions nil))
    (dolist (mode '(elpaca-manager-mode elpaca-log-mode))
      (yunge-test-which-key-prefix-bindings
       mode "SPC m" yunge-elpaca-command-bindings)))
  (yunge-test-which-key-bindings
   'elpaca-log-mode yunge-elpaca-log-normal-bindings)
  (yunge-test-which-key-bindings
   'elpaca-info-mode yunge-elpaca-info-normal-bindings))

;;; yunge-elpaca-test.el ends here
