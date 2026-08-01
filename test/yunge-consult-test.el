;;; yunge-consult-test.el --- Consult tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-get-command-property "evil-common")
(declare-function evil-has-command-property-p "evil-common")

(yunge-test-deftest-lazy-load yunge-consult
  (consult consult-imenu))

(ert-deftest yunge-consult-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-consult 'consult
   :before-ready
   '(progn
      (when (featurep 'consult)
        (error "Consult was loaded before its Elpaca body ran"))
      (when (or (keymap-lookup yunge-buffer-map "b")
                (keymap-lookup yunge-jump-map "i")
                (keymap-lookup yunge-search-map "b"))
        (error "Consult keys were bound before its Elpaca body ran")))
   :after-ready
   '(progn
      (unless (and (eq (keymap-lookup yunge-buffer-map "b")
                       'consult-buffer)
                   (eq (keymap-lookup yunge-jump-map "i")
                       'consult-imenu)
                   (eq (keymap-lookup yunge-search-map "b")
                       'consult-line))
        (error "Consult keys were not bound after package readiness"))
      (when (featurep 'consult)
        (error "Consult was loaded by its configuration")))))

(ert-deftest yunge-consult-integrates-with-evil ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'consult-autoloads)
  (yunge-test-load-package-config 'yunge-consult)

  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC b b" . consult-buffer)
     ("SPC s b" . consult-line)
     ("SPC s B" . consult-line-multi)
     ("SPC s p" . consult-ripgrep)
     ("SPC j i" . consult-imenu)))

  (with-temp-buffer
    (should (eq (command-remapping 'switch-to-buffer)
                'consult-buffer))
    (should (eq (command-remapping 'imenu) 'consult-imenu)))

  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC b"
   '(("b" nil "switch buffer")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC s"
   '(("b" nil "search buffer")
     ("B" nil "search project buffers")
     ("p" nil "search project")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC j"
   '(("i" nil "jump to symbol")))

  (dolist (command '(consult-imenu consult-line
                     consult-line-multi consult-ripgrep))
    (should (eq (evil-get-command-property command :jump) t))
    (should (evil-has-command-property-p command :repeat))
    (should-not (evil-get-command-property command :repeat))))

;;; yunge-consult-test.el ends here
