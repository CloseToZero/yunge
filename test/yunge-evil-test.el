;;; yunge-evil-test.el --- Evil tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-visual-state "evil-states")

(defvar evil-state)

(defvar-keymap yunge-test-localleader-map
  "p" #'backward-char)

(defvar-keymap yunge-test-buffer-mode-map)

(define-derived-mode yunge-test-buffer-mode fundamental-mode "Yunge test")

(defvar-keymap yunge-test-space-mode-map)

(define-minor-mode yunge-test-space-mode
  "Bind Space in an ordinary Evil mode map."
  :keymap yunge-test-space-mode-map)

(yunge-test-deftest-lazy-load yunge-evil
  (evil))

(ert-deftest yunge-evil-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-evil 'evil
   :before-ready
   '(when (or (featurep 'evil)
              (bound-and-true-p evil-mode))
      (error "Evil was enabled before its Elpaca body ran"))
   :after-ready
   '(unless
        (and (featurep 'evil)
             evil-mode
             yunge-leader-mode
             (equal
              (list evil-emacs-state-modes
                    evil-insert-state-modes
                    evil-motion-state-modes
                    evil-search-module
                    evil-symbol-word-search
                    evil-undo-system
                    evil-want-C-u-delete
                    evil-want-C-u-scroll
                    evil-want-Y-yank-to-eol
                    evil-want-integration
                    evil-want-keybinding)
              '(nil nil nil evil-search t undo-redo
                    t t t t nil)))
      (error "Unexpected Evil configuration"))))

(ert-deftest yunge-evil-routes-leader-keys ()
  (yunge-test-enable-evil)
  (require 'which-key)

  (yunge-key-evil-define
   '(normal visual) yunge-test-buffer-mode-map
   `(([localleader] ,yunge-test-localleader-map nil)))
  (yunge-key-evil-define
   '(normal visual) yunge-test-space-mode-map
   '(("SPC" ignore nil)))

  (keymap-set yunge-leader-map "t" #'forward-char)
  (unwind-protect
      (with-temp-buffer
        (yunge-test-buffer-mode)
        (should (eq evil-state 'normal))
        (yunge-test-space-mode 1)

        (yunge-test-key "SPC t" 'forward-char)
        (yunge-test-key "SPC m p" 'backward-char)

        (evil-visual-state)
        (should (eq evil-state 'visual))
        (yunge-test-key "SPC t" 'forward-char)
        (yunge-test-key "SPC m p" 'backward-char)

        (let ((overriding-terminal-local-map
               (define-keymap "SPC" #'forward-line)))
          (yunge-test-key "SPC" 'forward-line)))
    (keymap-unset yunge-leader-map "t"))

  (yunge-test-which-key-prefix-bindings
   'yunge-test-buffer-mode "SPC"
   '(("b" nil "+buffer")
     ("j" nil "+jump")
     ("m" nil "+mode")
     ("s" nil "+search"))))

;;; yunge-evil-test.el ends here
