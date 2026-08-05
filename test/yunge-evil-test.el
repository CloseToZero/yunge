;;; yunge-evil-test.el --- Evil tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-ex-make-search-pattern "evil-search" (regexp))
(declare-function evil-ex-pattern-regex "evil-search" (pattern))
(declare-function evil-normal-state "evil-states")
(declare-function evil-visual-state "evil-states")

(defvar evil-ex-search-direction)
(defvar evil-ex-search-history)
(defvar evil-ex-search-pattern)
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
  (evil project pyim pyim-cregexp))

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
                    evil-want-minibuffer
                    evil-search-module
                    evil-symbol-word-search
                    evil-undo-system
                    evil-want-C-u-delete
                    evil-want-C-u-scroll
                    evil-want-Y-yank-to-eol
                    evil-want-integration
                    evil-want-keybinding)
              '(nil nil nil t evil-search t undo-redo
                    t t t t nil))
             (advice-member-p #'yunge-evil--pinyin-search-pattern
                              'evil-ex-make-search-pattern))
      (error "Unexpected Evil configuration"))))

(ert-deftest yunge-evil-search-patterns-do-not-expand-pinyin-by-default ()
  (yunge-test-enable-evil)
  (should (equal (evil-ex-pattern-regex
                  (evil-ex-make-search-pattern "bl"))
                 "bl"))
  (should (equal (evil-ex-pattern-regex
                  (evil-ex-make-search-pattern "b.*l"))
                 "b.*l")))

(ert-deftest yunge-evil-slash-search-expands-pinyin ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "bl \u4fdd\u7559 bl")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (yunge-test-key "/" 'yunge-evil-pinyin-search-forward)
      (yunge-test-key "?" 'yunge-evil-pinyin-search-backward)
      (execute-kbd-macro (kbd "/ b l RET"))
      (should (= (point) 4))
      (should
       (string-match-p
        (evil-ex-pattern-regex evil-ex-search-pattern)
        "\u4fdd\u7559"))
      (execute-kbd-macro (kbd "n"))
      (should (= (point) 7))
      (should (equal (evil-ex-pattern-regex
                      (evil-ex-make-search-pattern "bl"))
                     "bl")))))

(ert-deftest yunge-evil-star-searches-the-word-literally ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "bl \u4fdd\u7559 bl")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "*"))
      (should (= (point) 7))
      (should (equal (evil-ex-pattern-regex evil-ex-search-pattern)
                     "\\_<bl\\_>")))))

(ert-deftest yunge-evil-visual-star-searches-the-selected-text-literally ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "bl \u4fdd\u7559 bl bl")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "v l *"))
      (should (eq evil-state 'normal))
      (should (= (point) 7))
      (should (equal (evil-ex-pattern-regex evil-ex-search-pattern)
                     "bl"))
      (should (eq evil-ex-search-direction 'forward))
      (should (equal (car evil-ex-search-history) "bl"))
      (execute-kbd-macro (kbd "n"))
      (should (= (point) 10))
      (execute-kbd-macro (kbd "N"))
      (should (= (point) 7)))))

(ert-deftest yunge-evil-visual-star-quotes-regular-expression-syntax ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "a.b axb a.b")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "v l l *"))
      (should (= (point) 9))
      (should (equal (evil-ex-pattern-regex evil-ex-search-pattern)
                     "a\\.b")))))

(ert-deftest yunge-evil-visual-star-rejects-block-selections ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "ab\ncd")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "C-v l"))
      (should-error
       (call-interactively #'yunge-evil-visual-search-forward)
       :type 'user-error)
      (should (eq evil-state 'visual)))))

(ert-deftest yunge-evil-opens-config-directory ()
  (yunge-test-enable-evil)
  (let ((user-emacs-directory "C:/config/emacs/")
        opened-directory)
    (cl-letf (((symbol-function 'dired)
               (lambda (directory)
                 (setq opened-directory directory))))
      (call-interactively #'yunge-open-config-directory))
    (should (equal opened-directory user-emacs-directory))))

(ert-deftest yunge-evil-routes-leader-keys ()
  (yunge-test-enable-evil)
  (require 'which-key)

  (yunge-key-evil-define
   '(normal visual) yunge-test-buffer-mode-map
   `(([localleader] ,yunge-test-localleader-map nil)))
  (yunge-key-evil-define
   '(normal visual) yunge-test-space-mode-map
   '(("SPC" ignore nil)))

  (keymap-set yunge-leader-map "z" #'forward-char)
  (unwind-protect
      (with-temp-buffer
        (yunge-test-buffer-mode)
        (should (eq evil-state 'normal))
        (yunge-test-space-mode 1)

        (yunge-test-key "SPC SPC" 'execute-extended-command)
        (yunge-test-key "SPC b j" 'next-buffer)
        (yunge-test-key "SPC b k" 'previous-buffer)
        (yunge-test-key "SPC b q" 'kill-current-buffer)
        (yunge-test-key "SPC b r" 'revert-buffer)
        (yunge-test-key "SPC f c" 'yunge-open-config-directory)
        (yunge-test-key "SPC f d" 'dired)
        (yunge-test-key "SPC f f" 'find-file)
        (yunge-test-key "SPC f s" 'save-buffer)
        (yunge-test-key "SPC h f" 'describe-function)
        (yunge-test-key "SPC p p" 'project-switch-project)
        (yunge-test-key "SPC q f" 'delete-frame)
        (yunge-test-key "SPC q q" 'save-buffers-kill-terminal)
        (yunge-test-key "SPC q r" 'restart-emacs)
        (should (keymapp (key-binding (kbd "SPC t"))))
        (yunge-test-key "SPC z" 'forward-char)
        (yunge-test-key "SPC m p" 'backward-char)

        (evil-visual-state)
        (should (eq evil-state 'visual))
        (yunge-test-key "SPC z" 'forward-char)
        (yunge-test-key "SPC m p" 'backward-char)

        (let ((overriding-terminal-local-map
               (define-keymap "SPC" #'forward-line)))
          (yunge-test-key "SPC" 'forward-line)))
    (keymap-unset yunge-leader-map "z"))

  (should (eq (lookup-key yunge-buffer-map (kbd "b"))
              'switch-to-buffer))

  (yunge-test-which-key-prefix-bindings
   'yunge-test-buffer-mode "SPC"
   '(("SPC" nil "execute command")
     ("b" nil "+buffer")
     ("f" nil "+file")
     ("g" nil "+go")
     ("h" nil "+help")
     ("j" nil "+jump")
     ("m" nil "+mode")
     ("p" nil "+project")
     ("q" nil "+quit")
     ("s" nil "+search")
     ("t" nil "+toggle/terminal")
     ("w" nil "+window")))

  (yunge-test-which-key-prefix-bindings
   'yunge-test-buffer-mode "SPC b"
   '(("b" nil "switch buffer")
     ("j" nil "next buffer")
     ("k" nil "previous buffer")
     ("q" nil "close buffer")
     ("r" nil "revert buffer")))

  (yunge-test-which-key-prefix-bindings
   'yunge-test-buffer-mode "SPC f"
   '(("c" nil "open config directory")
     ("d" nil "open directory")
     ("f" nil "find file")
     ("s" nil "save file")))

  (yunge-test-which-key-prefix-bindings
   'yunge-test-buffer-mode "SPC q"
   '(("f" nil "delete frame")
     ("q" nil "quit")
     ("r" nil "restart Emacs"))))

;;; yunge-evil-test.el ends here
