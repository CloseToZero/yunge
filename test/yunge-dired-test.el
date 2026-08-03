;;; yunge-dired-test.el --- Dired tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function project-known-project-roots "project")
(declare-function wdired-abort-changes "wdired")
(declare-function yunge-dired--remember-project "yunge-dired")

(defvar dired-directory)
(defvar dired-movement-style)
(defvar project--list)
(defvar project-list-file)

(yunge-test-deftest-lazy-load yunge-dired
  (dired evil project wdired which-key))

(ert-deftest yunge-dired-copy-commands-select-path-kinds ()
  (require 'yunge-dired)
  (let (arguments)
    (cl-letf (((symbol-function 'dired-copy-filename-as-kill)
               (lambda (&optional argument)
                 (push argument arguments))))
      (yunge-dired-copy-filename)
      (yunge-dired-copy-absolute-path)
      (yunge-dired-copy-project-path))
    (should (equal (nreverse arguments) '(nil 0 1)))))

(ert-deftest yunge-dired-binds-keys ()
  (require 'yunge-dired)
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'dired)

  ;; Key resolution uses synthetic Dired buffers and must not persist them.
  (cl-letf (((symbol-function 'yunge-dired--remember-project) #'ignore))
    (yunge-test-evil-normal-keys
     'dired-mode
     '(("RET" . dired-find-file)
       ("M-h" . dired-up-directory)
       ("i" . dired-toggle-read-only)
       ("j" . dired-next-line)
       ("k" . dired-previous-line)
       ("m" . dired-mark)
       ("d" . dired-flag-file-deletion)
       ("u" . dired-unmark)
       ("x" . dired-do-flagged-delete)
       ("t" . evil-find-char-to)
       ("* t" . dired-toggle-marks)
       ("% r" . dired-do-rename-regexp)
       ("w" . evil-forward-word-begin)
       ("y f" . yunge-dired-copy-filename)
       ("y a" . yunge-dired-copy-absolute-path)
       ("y p" . yunge-dired-copy-project-path)
       ("SPC m a m" . dired-do-chmod)
       ("SPC m l s" . dired-do-symlink)))

    (yunge-test-evil-visual-keys
     'dired-mode
     '(("m" . dired-mark)
       ("d" . dired-flag-file-deletion)
       ("u" . dired-unmark)
       ("t" . evil-find-char-to)
       ("* t" . dired-toggle-marks)
       ("% m" . dired-mark-files-regexp)
       ("y" . evil-yank)))

    (should (eq dired-movement-style 'bounded-files))

    (yunge-test-which-key-prefix-bindings
     'dired-mode "*" '(("t" nil "invert marks")))
    (yunge-test-which-key-prefix-bindings
     'dired-mode "%" '(("m" nil "mark names")))
    (yunge-test-which-key-prefix-bindings
     'dired-mode "y" '(("a" nil "absolute path")))
    (yunge-test-which-key-prefix-bindings
     'dired-mode "SPC m" '(("a" nil "+attribute")
                            ("l" nil "+link")))))

(ert-deftest yunge-dired-remembers-containing-project ()
  (require 'yunge-dired)
  (require 'dired)
  (let* ((root (make-temp-file "yunge-dired-project-" t))
         (directory (expand-file-name "src/" root))
         (project-list-file (expand-file-name "projects.eld" root))
         (project--list 'unset)
         buffer)
    (make-directory (expand-file-name ".git/" root))
    (make-directory directory)
    (unwind-protect
        (progn
          (setq buffer (dired-noselect directory))
          (should (seq-some
                   (lambda (known-root)
                     (file-equal-p known-root root))
                   (project-known-project-roots))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest yunge-dired-ignores-directories-without-projects ()
  (let ((default-directory temporary-file-directory)
        (dired-directory temporary-file-directory))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'project-remember-project)
               (lambda (&rest _arguments)
                 (ert-fail "Remembered a non-project directory"))))
      (yunge-dired--remember-project))))

(ert-deftest yunge-dired-remembers-remote-projects ()
  (let ((default-directory "/ssh:test:/repo/src/")
        (dired-directory "/ssh:test:/repo/src/")
        (project '(vc Git "/ssh:test:/repo/"))
        remembered)
    (cl-letf (((symbol-function 'project-current)
               (lambda (maybe-prompt directory)
                 (should-not maybe-prompt)
                 (should (equal directory default-directory))
                 project))
              ((symbol-function 'project-remember-project)
               (lambda (candidate &rest _arguments)
                 (setq remembered candidate))))
      (yunge-dired--remember-project))
    (should (eq remembered project))))

(ert-deftest yunge-wdired-integrates-with-evil-editing ()
  (require 'yunge-dired)
  (yunge-test-enable-evil)
  (require 'which-key)
  (let* ((directory (make-temp-file "yunge-wdired-" t))
         (buffer (dired-noselect directory)))
    (unwind-protect
        (with-current-buffer buffer
          (call-interactively (key-binding (kbd "i")))
          (yunge-test-evil-keys
           'normal
           '(("i" . evil-insert)
             ("ZQ" . wdired-abort-changes)
             ("ZZ" . wdired-finish-edit)))
          (wdired-abort-changes))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

;;; yunge-dired-test.el ends here
