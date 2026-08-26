;;; yunge-ghostel-test.el --- Ghostel tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-normal-state "evil-states")
(declare-function evil-visual-state "evil-states")
(declare-function ghostel--set-title "ghostel" (title))
(declare-function ghostel-mode "ghostel")
(declare-function yunge-ghostel--windows-shell-spec "yunge-ghostel")
(declare-function yunge-ghostel-next-input "yunge-ghostel")
(declare-function yunge-ghostel-previous-input "yunge-ghostel")
(declare-function yunge-ghostel-project "yunge-ghostel" (&optional arg))
(declare-function yunge-ghostel-redo "yunge-ghostel" (count))
(declare-function yunge-ghostel-undo "yunge-ghostel" (count))

(defvar ghostel-semi-char-mode-map)
(defvar ghostel-buffer-name-function)
(defvar ghostel--managed-buffer-name)
(defvar ghostel-mode-hook)
(defvar ghostel-module-auto-install)
(defvar ghostel-module-directory)
(defvar ghostel-readonly-fake-cursor)
(defvar ghostel-shell)
(defvar project-prefix-map)
(defvar project-switch-commands)
(defvar yunge-toggle-map)

(defun yunge-ghostel-test--load-config ()
  "Load the Ghostel configuration synchronously for command tests."
  (yunge-test-enable-evil)
  (require 'evil-ghostel-autoloads)
  (yunge-test-load-package-config 'yunge-ghostel))

(yunge-test-deftest-lazy-load yunge-ghostel
  (evil evil-ghostel ghostel project which-key))

(ert-deftest yunge-ghostel-configures-core-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-ghostel 'ghostel
   :before-ready
   '(progn
      (when (keymap-lookup yunge-toggle-map "t")
        (error "Ghostel terminal key was bound before package readiness"))
      (when (keymap-lookup project-prefix-map "t")
        (error "Ghostel project key was bound before package readiness")))
   :after-ready
   '(progn
      (unless (eq ghostel-module-auto-install 'download)
        (error "Ghostel module download was not configured"))
      (unless (equal ghostel-module-directory
                     (expand-file-name "ghostel/"
                                       yunge-var-directory))
        (error "Ghostel module directory was not redirected"))
      (when ghostel-readonly-fake-cursor
        (error "Ghostel read-only hint cursor was not disabled"))
      (when (featurep 'project)
        (error "Project was loaded by the Ghostel configuration"))
      (require 'project)
      (when (or (keymap-lookup yunge-toggle-map "t")
                (keymap-lookup project-prefix-map "t")
                (member '(yunge-ghostel-project "Ghostel")
                        project-switch-commands))
        (error "Ghostel was exposed before its Evil integration was ready"))
      (when (featurep 'ghostel)
        (error "Ghostel was loaded by its configuration")))))

(ert-deftest yunge-ghostel-enables-existing-buffers-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-ghostel 'evil-ghostel
   :setup
   '(progn
      (require 'ghostel)
      (setq ghostel-mode-hook nil
            yunge-ghostel-test-buffer
            (generate-new-buffer " *yunge-ghostel-existing*"))
      (with-current-buffer yunge-ghostel-test-buffer
        (ghostel-mode)))
   :before-ready
   '(with-current-buffer yunge-ghostel-test-buffer
      (when (bound-and-true-p evil-ghostel-mode)
        (error "Evil Ghostel was enabled before package readiness")))
   :after-ready
   '(progn
      (unless (memq 'evil-ghostel-mode ghostel-mode-hook)
        (error "Evil Ghostel hook was not installed"))
      (with-current-buffer yunge-ghostel-test-buffer
        (unless (bound-and-true-p evil-ghostel-mode)
          (error "Existing Ghostel buffer missed Evil integration")))
      (unless (eq (keymap-lookup yunge-toggle-map "t") 'ghostel)
        (error "Ghostel terminal key was not bound"))
      (unless (eq (keymap-lookup project-prefix-map "t")
                  'yunge-ghostel-project)
        (error "Ghostel project key was not bound"))
      (require 'project)
      (unless (member '(yunge-ghostel-project "Ghostel")
                      project-switch-commands)
        (error "Ghostel project action was not added")))))

(ert-deftest yunge-ghostel-binds-entry-points ()
  (yunge-ghostel-test--load-config)
  (require 'which-key)

  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC t t" . ghostel)
     ("SPC p t" . yunge-ghostel-project))))

(ert-deftest yunge-ghostel-project-keeps-its-project-name ()
  (yunge-ghostel-test--load-config)
  (require 'ghostel)
  (let ((buffer (generate-new-buffer "*sample-ghostel*"))
        received-arg)
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel-project)
                   (lambda (&optional arg)
                     (setq received-arg arg)
                     buffer)))
          (should (eq (yunge-ghostel-project '(4)) buffer))
          (should (equal received-arg '(4)))
          (with-current-buffer buffer
            (setq-local ghostel--managed-buffer-name (buffer-name))
            (ghostel--set-title "C:/Program Files/PowerShell/pwsh.exe")
            (should (equal (buffer-name) "*sample-ghostel*"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-ghostel-integrates-input-with-evil ()
  (yunge-ghostel-test--load-config)
  (require 'evil-ghostel)
  (yunge-test-keymap-keys
   ghostel-semi-char-mode-map
   '(("M-n" . yunge-ghostel-next-input)
     ("M-p" . yunge-ghostel-previous-input)))
  (with-temp-buffer
    (ghostel-mode)
    (evil-normal-state)
    (yunge-test-evil-keys
     'normal
     '(("p" . evil-ghostel-paste-after)
       ("P" . evil-ghostel-paste-before)
       ("u" . yunge-ghostel-undo)
       ("C-r" . yunge-ghostel-redo)
       ("<down-mouse-1>" . ghostel-mouse-press-or-copy-mode)))
    (evil-visual-state)
    (yunge-test-evil-keys
     'visual
     '(("<down-mouse-1>" . ghostel-mouse-press-or-copy-mode)))))

(ert-deftest yunge-ghostel-uses-psreadline-undo-and-redo ()
  (yunge-ghostel-test--load-config)
  (let ((ghostel-shell '("C:/Windows/PowerShell.exe"))
        sent)
    (cl-letf (((symbol-function 'evil-ghostel--prompt-active-p)
               (lambda () t))
              ((symbol-function 'ghostel--send-encoded)
               (lambda (key modifiers &optional _utf8)
                 (push (list key modifiers) sent)))
              ((symbol-function 'evil-ghostel-undo)
               (lambda (_count) (ert-fail "Used readline undo")))
              ((symbol-function 'evil-ghostel-redo)
               (lambda (_count) (ert-fail "Used upstream redo"))))
      (yunge-ghostel-undo 2)
      (yunge-ghostel-redo 1))
    (should (equal (nreverse sent)
                   '(("z" "ctrl") ("z" "ctrl") ("y" "ctrl"))))))

(ert-deftest yunge-ghostel-keeps-upstream-undo-outside-powershell ()
  (yunge-ghostel-test--load-config)
  (let ((ghostel-shell "/bin/bash")
        calls)
    (cl-letf (((symbol-function 'evil-ghostel--prompt-active-p)
               (lambda () t))
              ((symbol-function 'evil-ghostel-undo)
               (lambda (count) (push (list 'undo count) calls)))
              ((symbol-function 'evil-ghostel-redo)
               (lambda (count) (push (list 'redo count) calls)))
              ((symbol-function 'ghostel--send-encoded)
               (lambda (&rest _arguments)
                 (ert-fail "Replaced the non-PowerShell line editor"))))
      (yunge-ghostel-undo 2)
      (yunge-ghostel-redo 3))
    (should (equal (nreverse calls) '((undo 2) (redo 3))))))

(ert-deftest yunge-ghostel-history-keys-drive-the-shell-line-editor ()
  (yunge-ghostel-test--load-config)
  (let (sent)
    (cl-letf (((symbol-function 'ghostel-alt-screen-p) #'ignore)
              ((symbol-function 'ghostel--send-encoded)
               (lambda (key modifiers &optional _utf8)
                 (push (list key modifiers) sent))))
      (yunge-ghostel-previous-input)
      (yunge-ghostel-next-input))
    (should (equal (nreverse sent) '(("up" "") ("down" ""))))))

(ert-deftest yunge-ghostel-history-keys-pass-through-in-full-screen-apps ()
  (yunge-ghostel-test--load-config)
  (let (sent)
    (cl-letf (((symbol-function 'ghostel-alt-screen-p)
               (lambda () t))
              ((symbol-function 'ghostel--send-event)
               (lambda () (setq sent last-command-event)))
              ((symbol-function 'ghostel--send-encoded)
               (lambda (&rest _)
                 (ert-fail "History key was rewritten"))))
      (let ((last-command-event ?\M-p))
        (yunge-ghostel-previous-input)))
    (should (eq sent ?\M-p))))

(ert-deftest yunge-ghostel-prefers-modern-powershell ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program)
               (pcase program
                 ("pwsh.exe" "C:/PowerShell/pwsh.exe")
                 ("powershell.exe" "C:/Windows/powershell.exe")))))
    (should
     (equal (yunge-ghostel--windows-shell-spec)
            (list "C:/PowerShell/pwsh.exe"
                  "-NoLogo" "-NoExit" "-Command"
                  yunge-ghostel-powershell-command)))))

(ert-deftest yunge-ghostel-falls-back-to-command-prompt ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program)
               (and (equal program "cmd.exe") "C:/Windows/cmd.exe"))))
    (should
     (equal (yunge-ghostel--windows-shell-spec)
            '("C:/Windows/cmd.exe")))))

(ert-deftest yunge-ghostel-powershell-reports-its-directory ()
  (let ((powershell (or (executable-find "pwsh.exe")
                        (executable-find "powershell.exe"))))
    (unless powershell
      (ert-skip "PowerShell is unavailable"))
    (with-temp-buffer
      (let* ((process-environment (copy-sequence process-environment))
             (directory (directory-file-name temporary-file-directory))
             (expected (replace-regexp-in-string "\\\\" "/" directory))
             (command
              (concat yunge-ghostel-powershell-command
                      ";Set-Location -LiteralPath "
                      "$env:YUNGE_GHOSTEL_TEST_DIRECTORY;prompt")))
        (setenv "YUNGE_GHOSTEL_TEST_DIRECTORY" directory)
        (should (zerop
                 (call-process powershell nil t nil
                               "-NoLogo" "-NoProfile" "-Command"
                               command)))
        (should
         (string-match-p
          (regexp-quote
           (concat (string 27) "]7;file:///" expected (string 7)))
          (buffer-string)))))))

;;; yunge-ghostel-test.el ends here
