;;; yunge-evil-test.el --- Evil tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-ex-make-search-pattern "evil-search" (regexp))
(declare-function evil-ex-pattern-regex "evil-search" (pattern))
(declare-function evil-ex-search-next "evil-commands" (count))
(declare-function evil-ex-split-search-pattern "evil-search"
                  (pattern direction))
(declare-function evil-force-normal-state "evil-states" ())
(declare-function evil-emacs-state "evil-states")
(declare-function evil-insert-state "evil-states")
(declare-function evil-normal-state "evil-states")
(declare-function evil-replace-state "evil-states")
(declare-function evil-visual-state "evil-states")

(defvar evil-ex-search-direction)
(defvar evil-ex-search-history)
(defvar evil-ex-search-pattern)
(defvar evil-ex-active-highlights-alist)
(defvar evil-command-line-map)
(defvar evil-state)
(defvar yunge-evil--pinyin-search)

(defvar-keymap yunge-test-localleader-map
  "p" #'backward-char)

(defvar-keymap yunge-test-buffer-mode-map)

(define-derived-mode yunge-test-buffer-mode fundamental-mode "Yunge test")

(defvar-keymap yunge-test-space-mode-map)

(define-minor-mode yunge-test-space-mode
  "Bind Space in an ordinary Evil mode map."
  :keymap yunge-test-space-mode-map)

(defvar-keymap yunge-test-marker-mode-map)

(define-derived-mode yunge-test-marker-mode fundamental-mode
  "Yunge marker test")

(yunge-test-deftest-lazy-load yunge-evil
  (evil project pyim pyim-cregexp yunge-comment))

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
                              'evil-ex-make-search-pattern)
             (advice-member-p #'yunge-evil--split-pinyin-search-pattern
                              'evil-ex-split-search-pattern)
             (advice-member-p
              #'yunge-evil--handle-interactive-search-failure
              'evil-ex-search-next)
             (advice-member-p
              #'yunge-evil--handle-interactive-search-failure
              'evil-ex-search-previous))
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

(ert-deftest yunge-evil-pinyin-search-treats-slash-literally ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "x font_test font/src")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro (kbd "/ f o n t / RET"))
      (should (= (point) 13))
      (let ((regexp (evil-ex-pattern-regex evil-ex-search-pattern)))
        (should (string-match-p regexp "font/src"))
        (should-not (string-match-p regexp "font_test"))))))

(ert-deftest yunge-evil-command-line-navigates-history-with-meta-keys ()
  (yunge-test-enable-evil)
  (should (eq (keymap-lookup evil-command-line-map "M-n")
              'next-history-element))
  (should (eq (keymap-lookup evil-command-line-map "M-p")
              'previous-history-element))
  (with-temp-buffer
    (insert "older target")
    (goto-char (point-min))
    (evil-normal-state)
    (let ((evil-ex-search-history '("target" "older")))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro (kbd "/ M-p M-p M-n RET"))
        (should (= (point) 7))))))

(ert-deftest yunge-evil-repeat-search-failure-is-concise-only-interactively ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "present")
    (goto-char (point-min))
    (evil-normal-state)
    (setq evil-ex-search-direction 'forward
          evil-ex-search-pattern (evil-ex-make-search-pattern "missing")
          evil-ex-search-history '("missing"))
    (let (shown-message)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq shown-message
                         (apply #'format format-string arguments)))))
        (dolist (binding '(("n" . evil-ex-search-next)
                           ("N" . evil-ex-search-previous)))
          (yunge-test-key (car binding) (cdr binding))
          (should
           (eq (condition-case nil
                   (progn
                     (call-interactively (key-binding (kbd (car binding))))
                     'reported)
                 (search-failed 'signaled))
               'reported))
          (should (equal shown-message "Search failed: missing")))))
    (should-error (evil-ex-search-next 1) :type 'search-failed)))

(ert-deftest yunge-evil-pinyin-search-requires-a-regexp-prefix ()
  (yunge-test-enable-evil)
  (let ((yunge-evil--pinyin-search t))
    (let ((regexp
           (evil-ex-pattern-regex
            (evil-ex-make-search-pattern "a.b"))))
      (should (string-match-p regexp "a.b"))
      (should-not (string-match-p regexp "axb")))
    (should
     (equal
      (evil-ex-pattern-regex
       (evil-ex-make-search-pattern ":re:a.b"))
      "a.b"))
    (should
     (equal (evil-ex-split-search-pattern ":re:font/" 'forward)
            '(":re:font" "" nil)))))

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

(ert-deftest yunge-evil-force-normal-clears-highlight-interactively ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "word word")
    (goto-char (point-min))
    (evil-normal-state)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (yunge-test-key "<escape>" 'evil-force-normal-state)
      (execute-kbd-macro (kbd "*"))
      (should (assq 'evil-ex-search evil-ex-active-highlights-alist))
      (evil-insert-state)
      (execute-kbd-macro (kbd "<escape>"))
      (should (eq evil-state 'normal))
      (should (assq 'evil-ex-search evil-ex-active-highlights-alist))
      (evil-force-normal-state)
      (should (assq 'evil-ex-search evil-ex-active-highlights-alist))
      (execute-kbd-macro (kbd "<escape>"))
      (should-not
       (assq 'evil-ex-search evil-ex-active-highlights-alist)))))

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
  (let ((yunge-config-directory "C:/config/emacs/")
        opened-directory)
    (cl-letf (((symbol-function 'dired)
               (lambda (directory)
                 (setq opened-directory directory))))
      (call-interactively #'yunge-open-config-directory))
    (should (equal opened-directory yunge-config-directory))))

(ert-deftest yunge-evil-leader-exposes-overridden-marker-command ()
  (yunge-test-enable-evil)
  (yunge-key-evil-define
   'normal yunge-test-marker-mode-map
   '(("m" ignore nil)))
  (with-temp-buffer
    (yunge-test-marker-mode)
    (insert "one\ntwo\n")
    (goto-char (point-min))
    (yunge-test-key "m" 'ignore)
    (yunge-test-key "SPC j m s" 'evil-set-marker)
    (yunge-test-key "SPC j m j" 'evil-goto-mark)
    (yunge-test-key "SPC j m l" 'evil-goto-mark-line)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (goto-char (1+ (point-min)))
      (execute-kbd-macro (kbd "SPC j m s a"))
      (forward-line)
      (execute-kbd-macro (kbd "SPC j m j a"))
      (should (= (point) (1+ (point-min))))
      (forward-line)
      (execute-kbd-macro (kbd "SPC j m l a"))
      (should (= (point) (point-min))))))

(ert-deftest yunge-evil-routes-leader-keys ()
  (yunge-test-enable-evil)
  (require 'which-key)

  (yunge-key-evil-define
   '(normal visual) yunge-test-buffer-mode-map
   `(([localleader] ,yunge-test-localleader-map nil)))
  (yunge-key-evil-define
   '(normal visual) yunge-test-space-mode-map
   '(("SPC" ignore nil)))

  (keymap-set yunge-leader-map "x" #'ignore)
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

        (dolist (state '((evil-insert-state . insert)
                         (evil-replace-state . replace)
                         (evil-emacs-state . emacs)))
          (funcall (car state))
          (should (eq evil-state (cdr state)))
          (yunge-test-key "M-m z" 'forward-char)
          (yunge-test-key "M-m m p" 'backward-char))

        (evil-insert-state)
        (insert "text")
        (let ((position (point)))
          (call-interactively (key-binding (kbd "M-m x")))
          (should (eq evil-state 'insert))
          (should (= (point) position)))

        (let ((overriding-terminal-local-map
               (define-keymap "SPC" #'forward-line)))
          (yunge-test-key "SPC" 'forward-line)))
    (keymap-unset yunge-leader-map "x")
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
   'yunge-test-buffer-mode "SPC j"
   '(("m" nil "+marker")))

  (yunge-test-which-key-prefix-bindings
   'yunge-test-buffer-mode "SPC j m"
   '(("j" nil "jump to marker")
     ("l" nil "jump to marker line")
     ("s" nil "set marker")))

  (yunge-test-which-key-prefix-bindings
   'yunge-test-buffer-mode "SPC q"
   '(("f" nil "delete frame")
     ("q" nil "quit")
     ("r" nil "restart Emacs"))))

;;; yunge-evil-test.el ends here
