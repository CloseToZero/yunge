;;; yunge-consult-test.el --- Consult tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function consult--customize-args "consult"
                  (options &rest defaults))
(declare-function consult--async-min-input "consult" (&optional min-input))
(declare-function consult--file-preview "consult")
(declare-function consult-bookmark "consult" (name))
(declare-function evil-get-command-property "evil-common")
(declare-function evil-visual-state "evil-states")
(declare-function yunge-jump-history-backward "yunge-jump-history")

(defvar evil-command-line-map)
(defvar evil-eval-map)
(defvar evil-state)
(defvar bookmark-alist)
(defvar bookmark-default-file)
(defvar bookmark-save-flag)
(defvar consult-source-buffer)

(yunge-test-deftest-lazy-load yunge-consult
  (consult consult-imenu))

(ert-deftest yunge-consult-binds-keys-only-after-package-ready ()
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

(ert-deftest yunge-consult-binds-navigation-keys-without-evil-jumps ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'consult-autoloads)
  (yunge-test-load-package-config 'yunge-consult)
  (require 'consult)

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

  (dolist (command '(consult-bookmark consult-buffer consult-imenu
                     consult-line consult-line-multi consult-recent-file
                     consult-grep consult-ripgrep))
    (should-not (evil-get-command-property command :jump))
    (should-not (evil-get-command-property command :repeat t)))

  (dolist (command '(yunge-consult-project-search
                     yunge-consult-project-search-symbol))
    (should-not (evil-get-command-property command :jump))
    (should-not (evil-get-command-property command :repeat t))))

(ert-deftest yunge-consult-previews-text-without-opening-reader-files ()
  (yunge-test-enable-evil)
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (let* ((root (make-temp-file "yunge-consult-preview-" t))
         (epub (expand-file-name "book.epub" root))
         (pdf (expand-file-name "PAPER.PDF" root))
         (text-file (expand-file-name "notes.txt" root))
         (preview-buffer (generate-new-buffer " *yunge-consult-preview*"))
         (original-buffer (window-buffer))
         opened)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'consult--find-file-temporarily)
                     (lambda (file)
                       (push file opened)
                       preview-buffer)))
            (let ((preview (consult--file-preview)))
              (funcall preview 'preview epub)
              (funcall preview 'preview pdf)
              (should-not opened)
              (funcall preview 'preview text-file)
              (should (equal (mapcar #'expand-file-name opened)
                             (list text-file)))
              (should (eq (window-buffer) preview-buffer))
              (funcall preview 'preview epub)
              (should (equal (mapcar #'expand-file-name opened)
                             (list text-file)))
              (should (eq (window-buffer) original-buffer))
              (funcall preview 'exit nil))))
      (when (buffer-live-p preview-buffer)
        (kill-buffer preview-buffer))
      (delete-directory root t))))

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
          (should (eq (cdar (funcall (plist-get consult-source-buffer :items)))
                      shared)))
      (dolist (buffer (list shared current))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest yunge-consult-bookmark-navigation-can-jump-back ()
  (yunge-test-enable-evil)
  (require 'consult)
  (require 'bookmark)
  (yunge-test-load-package-config 'yunge-consult)
  (let* ((root (make-temp-file "yunge-consult-bookmark-" t))
         (file (expand-file-name "target.txt" root))
         (bookmark-default-file (expand-file-name "bookmarks" root))
         (bookmark-save-flag nil)
         (bookmark-alist nil)
         (origin (generate-new-buffer " *yunge-consult-origin*"))
         destination)
    (unwind-protect
        (progn
          (with-temp-file file (insert "target"))
          (setq destination (find-file-noselect file))
          (save-window-excursion
            (with-current-buffer destination
              (goto-char 3)
              (bookmark-set "yunge-consult-target"))
            (set-window-parameter nil 'yunge-jump-history nil)
            (switch-to-buffer origin)
            (consult-bookmark "yunge-consult-target")
            (should (eq (current-buffer) destination))
            (yunge-jump-history-backward)
            (should (eq (current-buffer) origin))))
      (set-window-parameter nil 'yunge-jump-history nil)
      (dolist (buffer (list origin destination))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory root t))))

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

(ert-deftest yunge-consult-async-searches-explain-short-queries ()
  (yunge-test-enable-evil)
  (require 'consult)
  (yunge-test-load-package-config 'yunge-consult)
  (with-temp-buffer
    (let* ((query (string #x4f8b #x5b50))
           submitted
           (stage
            (funcall (consult--async-min-input)
                     (lambda (action)
                       (when (stringp action)
                         (push action submitted))))))
      (funcall stage 'setup)
      (let ((overlay (car (append (car (overlay-lists))
                                  (cdr (overlay-lists))))))
        (should overlay)
        (funcall stage "x")
        (let ((notice (overlay-get overlay 'after-string)))
          (should
           (equal notice
                  " [Type at least 2 characters to start search]"))
          (should (eq (get-text-property 1 'face notice) 'warning)))
        (should-not submitted)
        (funcall stage query)
        (should (equal submitted (list query)))
        (should-not (overlay-get overlay 'after-string))
        (funcall stage (propertize "x" 'consult--force t))
        (should-not (overlay-get overlay 'after-string))
        (should (equal submitted (list "x" query)))
        (funcall stage 'destroy)
        (should-not (overlay-buffer overlay))))))

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
