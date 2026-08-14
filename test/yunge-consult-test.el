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
                (keymap-lookup yunge-search-map "b")
                (keymap-lookup yunge-search-map "P"))
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
                   (eq (keymap-lookup yunge-search-map "p")
                       'yunge-consult-project-search)
                   (eq (keymap-lookup yunge-search-map "P")
                       'yunge-consult-project-search-symbol)
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
     ("SPC s p" . yunge-consult-project-search)
     ("SPC s P" . yunge-consult-project-search-symbol)
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
     ("p" nil "search project")
     ("P" nil "search symbol in project")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC j"
   '(("b" nil "jump to bookmark")
     ("i" nil "jump to symbol")))

  (dolist (command '(consult-bookmark consult-buffer consult-imenu
                     consult-line consult-line-multi consult-recent-file
                     consult-grep consult-ripgrep))
    (should-not (evil-get-command-property command :jump))
    (should-not (evil-get-command-property command :repeat t))
    (should
     (advice-member-p #'yunge-jump-history--track-navigation command)))

  (dolist (command '(yunge-consult-project-search
                     yunge-consult-project-search-symbol))
    (should-not (evil-get-command-property command :jump))
    (should-not (evil-get-command-property command :repeat t))))

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

(ert-deftest yunge-consult-project-search-starts-from-visual-selection ()
  (yunge-test-enable-evil)
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (with-temp-buffer
    (insert "foo.bar")
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (evil-visual-state)
    (let ((this-command 'yunge-consult-project-search))
      (should
       (equal
        (plist-get (consult--customize-args nil) :initial)
        "foo\\.bar")))
    (should (eq evil-state 'normal))
    (should-not (use-region-p))
    (let ((this-command 'yunge-consult-project-search))
      (should-not
       (plist-get (consult--customize-args nil) :initial)))))

(ert-deftest yunge-consult-project-symbol-prefers-visual-selection ()
  (yunge-test-enable-evil)
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (with-temp-buffer
    (c++-mode)
    (insert "glyph_data_format()")
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (evil-visual-state)
    (let (arguments)
      (cl-letf (((symbol-function 'yunge-consult-project-search)
                 (lambda (&optional initial)
                   (setq arguments initial))))
        (yunge-consult-project-search-symbol))
      (should (equal arguments
                     (regexp-quote "glyph_data_format()")))
      (should (eq evil-state 'normal))
      (should-not (use-region-p)))))

(ert-deftest yunge-consult-project-symbol-uses-literal-symbol-at-point ()
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo+bar")
    (goto-char (point-min))
    (let (initial)
      (cl-letf (((symbol-function 'yunge-consult-project-search)
                 (lambda (&optional value)
                   (setq initial value))))
        (yunge-consult-project-search-symbol))
      (should (equal initial (regexp-quote "foo+bar"))))))

(ert-deftest yunge-consult-project-symbol-allows-an-empty-input ()
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (with-temp-buffer
    (insert " ")
    (let ((initial 'unset))
      (cl-letf (((symbol-function 'yunge-consult-project-search)
                 (lambda (&optional value)
                   (setq initial value))))
        (yunge-consult-project-search-symbol))
      (should-not initial))))

(ert-deftest yunge-consult-project-search-prefers-ripgrep ()
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (let (called)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "rg") "rg")))
              ((symbol-function 'consult-ripgrep)
               (lambda (&optional directory initial)
                 (setq called (list directory initial)))))
      (yunge-consult-project-search "needle"))
    (should (equal called '(nil "needle")))))

(ert-deftest yunge-consult-project-search-falls-back-to-grep ()
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (let (called)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "grep") "grep")))
              ((symbol-function 'consult-grep)
               (lambda (&optional directory initial)
                 (setq called (list directory initial)))))
      (yunge-consult-project-search "needle"))
    (should (equal called '(nil "needle")))))

(ert-deftest yunge-consult-project-search-requires-a-search-program ()
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (should-error (yunge-consult-project-search) :type 'user-error)))

;;; yunge-consult-test.el ends here
