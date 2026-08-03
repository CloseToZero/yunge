;;; yunge-ghostel-test.el --- Ghostel tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function yunge-ghostel--windows-shell-spec "yunge-ghostel")

(defvar ghostel-mode-hook)
(defvar ghostel-module-auto-install)
(defvar ghostel-module-directory)
(defvar ghostel-shell)
(defvar project-prefix-map)
(defvar project-switch-commands)
(defvar yunge-toggle-map)

(yunge-test-deftest-lazy-load yunge-ghostel
  (evil evil-ghostel ghostel project which-key))

(ert-deftest yunge-ghostel-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-ghostel 'evil-ghostel
   :before-ready
   '(progn
      (when (keymap-lookup yunge-toggle-map "t")
        (error "Ghostel terminal key was bound before package readiness"))
      (when (keymap-lookup project-prefix-map "t")
        (error "Ghostel project key was bound before package readiness")))
   :after-ready
   '(progn
      (unless (eq (keymap-lookup yunge-toggle-map "t") 'ghostel)
        (error "Ghostel terminal key was not bound"))
      (unless (eq (keymap-lookup project-prefix-map "t")
                  'ghostel-project)
        (error "Ghostel project key was not bound"))
      (unless (eq ghostel-module-auto-install 'download)
        (error "Ghostel module download was not configured"))
      (unless (equal ghostel-module-directory
                     (expand-file-name "var/ghostel/"
                                       user-emacs-directory))
        (error "Ghostel module directory was not redirected"))
      (unless (memq 'evil-ghostel-mode ghostel-mode-hook)
        (error "Evil Ghostel integration was not enabled"))
      (when (featurep 'project)
        (error "Project was loaded by the Ghostel configuration"))
      (require 'project)
      (unless (member '(ghostel-project "Ghostel")
                      project-switch-commands)
        (error "Ghostel project action was not added"))
      (when (featurep 'ghostel)
        (error "Ghostel was loaded by its configuration"))
      (when (featurep 'evil-ghostel)
        (error "Evil Ghostel was loaded by its configuration")))))

(ert-deftest yunge-ghostel-binds-entry-points ()
  (require 'evil-ghostel-autoloads)
  (yunge-test-load-package-config 'yunge-ghostel)
  (yunge-test-enable-evil)
  (require 'which-key)

  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC t t" . ghostel)
     ("SPC p t" . ghostel-project)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC t"
   '(("t" nil "terminal")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC p"
   '(("t" nil "terminal"))))

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
