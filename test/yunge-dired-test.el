;;; yunge-dired-test.el --- Dired tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function dired-goto-file "dired" (file))
(declare-function dired-mark "dired" (arg &optional interactive))
(declare-function dired-unmark-all-marks "dired")
(declare-function dired-dwim-target-directory "dired-aux")
(declare-function dired-dwim-target-recent "dired-aux")
(declare-function project-known-project-roots "project")
(declare-function wdired-abort-changes "wdired")
(declare-function yunge-dired--files-to-reveal "yunge-dired")
(declare-function yunge-dired--perform-file-drop
                  "yunge-dired" (window uris action))
(declare-function yunge-dired--reveal-on-windows "yunge-dired" (files))
(declare-function yunge-dired--remember-project "yunge-dired")
(declare-function yunge-dired--setup-dnd "yunge-dired")

(defvar dired-directory)
(defvar dired-dwim-target)
(defvar dired-mode-map)
(defvar dired-movement-style)
(defvar dnd-protocol-alist)
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
       ("SPC m e" . yunge-dired-open-directory-externally)
       ("SPC m l s" . dired-do-symlink)
       ("SPC m r" . yunge-dired-reveal-in-file-manager)))

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
    (should (eq dired-dwim-target #'dired-dwim-target-recent))

    (dolist (event '([drag-n-drop]
                     [C-drag-n-drop]
                     [S-drag-n-drop]
                     [C-S-drag-n-drop]))
      (should-not (eq (lookup-key dired-mode-map event)
                      #'yunge-dired-handle-file-drop)))

    (yunge-test-which-key-prefix-bindings
     'dired-mode "*" '(("t" nil "invert marks")))
    (yunge-test-which-key-prefix-bindings
     'dired-mode "%" '(("m" nil "mark names")))
    (yunge-test-which-key-prefix-bindings
     'dired-mode "y" '(("a" nil "absolute path")))
    (yunge-test-which-key-prefix-bindings
     'dired-mode "SPC m" '(("a" nil "+attribute")
                            ("e" nil "open in file manager")
                            ("l" nil "+link")
                            ("r" nil "reveal in file manager")))))

(ert-deftest yunge-dired-opens-current-directory-externally ()
  (require 'yunge-dired)
  (let ((default-directory "C:/project/")
        opened-files)
    (cl-letf (((symbol-function 'shell-command-do-open)
               (lambda (files)
                 (setq opened-files files))))
      (call-interactively #'yunge-dired-open-directory-externally))
    (should (equal opened-files (list default-directory)))))

(ert-deftest yunge-dired-reveal-prefers-marks-over-point ()
  (require 'yunge-dired)
  (require 'dired)
  (let* ((directory (make-temp-file "yunge-dired-reveal-" t))
         (first (expand-file-name "first" directory))
         (second (expand-file-name "second" directory))
         (third (expand-file-name "third" directory))
         buffer)
    (unwind-protect
        (progn
          (dolist (file (list first second third))
            (write-region "" nil file nil 'silent))
          (setq buffer (dired-noselect directory))
          (with-current-buffer buffer
            (dired-goto-file first)
            (dired-mark 1)
            (dired-mark 1)
            (should (equal (yunge-dired--files-to-reveal)
                           (list first second)))
            (let ((inhibit-message t))
              (dired-unmark-all-marks))
            (should (equal (yunge-dired--files-to-reveal)
                           (list third)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest yunge-dired-windows-reveal-selects-one-file-directly ()
  (require 'yunge-dired)
  (let (arguments)
    (cl-letf (((symbol-function 'w32-shell-execute)
               (lambda (&rest args)
                 (setq arguments args))))
      (yunge-dired--reveal-on-windows '("C:/a directory/file")))
    (should (equal arguments
                   '("open" "explorer.exe"
                     "/select,\"C:\\a directory\\file\"")))))

(ert-deftest yunge-dired-windows-reveal-uses-api-for-many-files ()
  (require 'yunge-dired)
  (let ((user-emacs-directory "C:/Config Dir/")
        arguments)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "pwsh"))
              ((symbol-function 'w32-shell-execute)
               (lambda (&rest args)
                 (setq arguments args))))
      (yunge-dired--reveal-on-windows
       '("D:/测试/first file.txt" "D:/测试/second's.txt")))
    (pcase-let* ((`("open" "pwsh" ,parameters 0) arguments)
                 (encoded (car (last (split-string parameters))))
                 (command
                  (decode-coding-string
                   (base64-decode-string encoded)
                   'utf-16le)))
      (should
       (equal
        command
        (concat
         "& 'c:/Config Dir/script/yunge-reveal.ps1' "
         "'D:/测试/first file.txt' 'D:/测试/second''s.txt'"))))))

(ert-deftest yunge-dired-windows-reveal-falls-back-without-powershell ()
  (require 'yunge-dired)
  (let (arguments)
    (cl-letf (((symbol-function 'executable-find) #'ignore)
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'w32-shell-execute)
               (lambda (&rest args)
                 (setq arguments args))))
      (yunge-dired--reveal-on-windows
       '("C:/first" "C:/second")))
    (should (equal arguments
                   '("open" "explorer.exe"
                     "/select,\"C:\\first\"")))))

(ert-deftest yunge-dired-installs-portable-file-drop-handler ()
  (require 'yunge-dired)
  (with-temp-buffer
    (setq-local dnd-protocol-alist
                '(("^file:///" . dnd-open-local-file)
                  ("^file:" . dired-dnd-handle-local-file)
                  ("^https?://" . dnd-open-file)))
    (yunge-dired--setup-dnd)
    (should (equal dnd-protocol-alist
                   '(("^file:" . yunge-dired-handle-file-drop)
                     ("^https?://" . dnd-open-file))))
    (should (get 'yunge-dired-handle-file-drop 'dnd-multiple-handler))))

(ert-deftest yunge-dired-performs-portable-file-drop-actions ()
  (require 'yunge-dired)
  (let ((uris '("file:///source/one.txt" "file:///source/two.txt"))
        opened transferred menu-action (menu-count 0))
    (cl-letf (((symbol-function 'dnd-open-file)
               (lambda (uri action)
                 (push (list uri action) opened)
                 'private))
              ((symbol-function 'dired-dnd-handle-local-file)
               (lambda (uri action)
                 (push (list uri action) transferred)
                 action))
              ((symbol-function 'dnd-get-local-file-name)
               (lambda (uri &rest _arguments) uri))
              ((symbol-function 'x-popup-menu)
               (lambda (&rest _arguments)
                 (cl-incf menu-count)
                 menu-action)))
      (setq menu-action 'open)
      (should (eq (yunge-dired-handle-file-drop uris 'move) 'private))
      (should (equal (nreverse opened)
                     '(("file:///source/one.txt" private)
                       ("file:///source/two.txt" private))))

      (setq menu-action 'copy)
      (should (eq (yunge-dired-handle-file-drop uris 'private) 'copy))
      (should (equal (nreverse transferred)
                     '(("file:///source/one.txt" copy)
                       ("file:///source/two.txt" copy))))

      (setq menu-action nil
            transferred nil)
      (should-not (yunge-dired-handle-file-drop uris 'copy))
      (should-not transferred)
      (should (= menu-count 3)))))

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

(ert-deftest yunge-dired-prefers-last-selected-target-window ()
  (require 'yunge-dired)
  (require 'dired)
  (let* ((root (make-temp-file "yunge-dired-target-" t))
         (source-directory (expand-file-name "source/" root))
         (older-directory (expand-file-name "older/" root))
         (target-directory (expand-file-name "target/" root))
         buffers)
    (dolist (directory
             (list source-directory older-directory target-directory))
      (make-directory directory))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((source-window (selected-window))
                 (older-window (split-window-right))
                 (target-window
                  (split-window older-window nil 'below)))
            (dolist (entry
                     (list (cons source-window source-directory)
                           (cons older-window older-directory)
                           (cons target-window target-directory)))
              (let ((buffer (dired-noselect (cdr entry))))
                (push buffer buffers)
                (set-window-buffer (car entry) buffer)))
            (select-window older-window)
            (select-window target-window)
            (select-window source-window)
            (should
             (equal (dired-dwim-target-directory)
                    target-directory))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (delete-directory root t))))

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
