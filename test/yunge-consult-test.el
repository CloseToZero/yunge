;;; yunge-consult-test.el --- Consult tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-get-command-property "evil-common")
(declare-function yunge-jump--track-navigation "yunge-jump")

(defvar evil-command-line-map)
(defvar evil-eval-map)

(yunge-test-deftest-lazy-load yunge-consult
  (consult consult-imenu))

(ert-deftest yunge-consult-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-consult 'consult
   :before-ready
   '(progn
      (when (featurep 'consult)
        (error "Consult was loaded before its Elpaca body ran"))
      (unless (eq (lookup-key yunge-buffer-map (kbd "b"))
                  'switch-to-buffer)
        (error "Core buffer binding is missing"))
      (when (or (keymap-lookup yunge-file-map "r")
                (keymap-lookup yunge-jump-map "b")
                (keymap-lookup yunge-jump-map "i")
                (keymap-lookup yunge-search-map "b"))
        (error "Consult keys were bound before its Elpaca body ran")))
   :after-ready
   '(progn
      (unless (and (eq (keymap-lookup yunge-file-map "r")
                       'consult-recent-file)
                   (eq (keymap-lookup yunge-jump-map "b")
                       'consult-bookmark)
                   (eq (keymap-lookup yunge-jump-map "i")
                       'consult-imenu)
                   (eq (keymap-lookup yunge-search-map "b")
                       'consult-line)
                   (eq (keymap-lookup minibuffer-local-map "M-r")
                       'consult-history)
                   (eq (command-remapping 'switch-to-buffer)
                       'consult-buffer))
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
     ("SPC f r" . consult-recent-file)
     ("SPC j b" . consult-bookmark)
     ("SPC s b" . consult-line)
     ("SPC s B" . consult-line-multi)
     ("SPC s p" . consult-ripgrep)
     ("SPC j i" . consult-imenu)))

  (yunge-test-keymap-keys
   minibuffer-local-map
   '(("M-r" . consult-history)))
  (yunge-test-keymap-keys
   evil-command-line-map
   '(("M-r" . consult-history)))
  (yunge-test-keymap-keys
   evil-eval-map
   '(("M-r" . consult-history)))

  (with-temp-buffer
    (should (eq (command-remapping 'switch-to-buffer)
                'consult-buffer))
    (should (eq (command-remapping 'imenu) 'consult-imenu)))

  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC b"
   '(("b" nil "switch buffer")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC f"
   '(("r" nil "find recent file")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC s"
   '(("b" nil "search buffer")
     ("B" nil "search project buffers")
     ("p" nil "search project")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC j"
   '(("b" nil "jump to bookmark")
     ("i" nil "jump to symbol")))

  (dolist (command '(consult-bookmark consult-buffer consult-imenu
                     consult-line consult-line-multi consult-recent-file
                     consult-ripgrep))
    (should-not (evil-get-command-property command :jump))
    (should-not (evil-get-command-property command :repeat t))
    (should
     (advice-member-p #'yunge-jump--track-navigation command))))

;;; yunge-consult-test.el ends here
