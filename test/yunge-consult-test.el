;;; yunge-consult-test.el --- Consult tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function consult--customize-args "consult"
                  (options &rest defaults))
(declare-function evil-get-command-property "evil-common")
(declare-function evil-visual-state "evil-states")
(declare-function yunge-jump-history--track-navigation "yunge-jump-history")

(defvar evil-command-line-map)
(defvar evil-eval-map)
(defvar evil-state)
(defvar consult-source-buffer)

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
     (advice-member-p #'yunge-jump-history--track-navigation command))))

(ert-deftest yunge-consult-prefers-the-selected-window-history ()
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (let ((shared (generate-new-buffer "*yunge-consult-shared*"))
        (current (generate-new-buffer "*yunge-consult-current*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer shared)
          (set-window-buffer (split-window-right) shared)
          (switch-to-buffer current)
          (should (get-buffer-window shared))
          (should (eq (yunge-consult--previous-window-buffer) shared))
          (should (eq (plist-get consult-source-buffer :items)
                      #'yunge-consult--buffer-items))
          (should (eq (cdar (yunge-consult--buffer-items)) shared)))
      (dolist (buffer (list shared current))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest yunge-consult-ripgrep-starts-from-visual-selection ()
  (yunge-test-enable-evil)
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (with-temp-buffer
    (insert "foo.bar")
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (evil-visual-state)
    (let ((this-command 'consult-ripgrep))
      (should
       (equal
        (plist-get (consult--customize-args nil) :initial)
        "foo\\.bar")))
    (should (eq evil-state 'normal))
    (should-not (use-region-p))
    (let ((this-command 'consult-ripgrep))
      (should-not
       (plist-get (consult--customize-args nil) :initial)))))

;;; yunge-consult-test.el ends here
