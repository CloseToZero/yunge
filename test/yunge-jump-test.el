;;; yunge-jump-test.el --- Jump history tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-set-jump "evil-jumps")
(declare-function yunge-jump-backward "yunge-jump")
(declare-function yunge-jump-forward "yunge-jump")
(declare-function yunge-jump-track-command "yunge-jump")

(defvar evil--jumps-jump-command)

(defun yunge-jump-test--buffer (name)
  "Return a non-file buffer named NAME with test content."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert "0123456789"))
    buffer))

(defun yunge-jump-test--record (buffer position)
  "Record POSITION in BUFFER as an Evil jump."
  (evil-set-jump
   (with-current-buffer buffer
     (copy-marker position))))

(defun yunge-jump-test--reset ()
  "Clear jump state from the selected test window."
  (set-window-parameter nil 'yunge-jump-history nil)
  (setq evil--jumps-jump-command nil))

(defun yunge-jump-test--kill (&rest buffers)
  "Kill every live buffer in BUFFERS."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun yunge-jump-test--navigate (action)
  "Run ACTION as a navigation command."
  (funcall action))

(ert-deftest yunge-jump-tracks-successful-navigation ()
  (yunge-test-enable-evil)
  (yunge-jump-test--reset)
  (let ((origin (yunge-jump-test--buffer " *yunge-jump-origin*"))
        (preview (yunge-jump-test--buffer " *yunge-jump-preview*"))
        (destination
         (yunge-jump-test--buffer " *yunge-jump-destination*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (goto-char 4)
          (yunge-jump-track-command 'yunge-jump-test--navigate)

          (yunge-jump-test--navigate
           (lambda ()
             (switch-to-buffer preview)
             (goto-char 6)
             (evil-set-jump)
             (switch-to-buffer destination)
             (goto-char 8)))

          (yunge-jump-backward)
          (should (eq (current-buffer) origin))
          (should (= (point) 4))
          (should-error (yunge-jump-backward) :type 'user-error)

          (yunge-jump-forward)
          (should (eq (current-buffer) destination))
          (should (= (point) 8)))
      (yunge-jump-test--kill origin preview destination)
      (yunge-jump-test--reset))))

(ert-deftest yunge-jump-preserves-forward-history-after-cancel ()
  (yunge-test-enable-evil)
  (yunge-jump-test--reset)
  (let ((older (yunge-jump-test--buffer " *yunge-jump-older*"))
        (latest (yunge-jump-test--buffer " *yunge-jump-latest*"))
        (preview (yunge-jump-test--buffer " *yunge-jump-preview*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer latest)
          (goto-char 7)
          (yunge-jump-test--record older 3)
          (yunge-jump-backward)
          (yunge-jump-track-command 'yunge-jump-test--navigate)

          (let (cancelled)
            (condition-case nil
                (yunge-jump-test--navigate
                 (lambda ()
                   (switch-to-buffer preview)
                   (evil-set-jump)
                   (switch-to-buffer older)
                   (goto-char 3)
                   (signal 'quit nil)))
              (quit (setq cancelled t)))
            (should cancelled))

          (yunge-jump-forward)
          (should (eq (current-buffer) latest))
          (should (= (point) 7)))
      (yunge-jump-test--kill older latest preview)
      (yunge-jump-test--reset))))

(ert-deftest yunge-jump-records-navigation-in-destination-window ()
  (yunge-test-enable-evil)
  (yunge-jump-test--reset)
  (let ((origin (yunge-jump-test--buffer " *yunge-jump-origin*"))
        (destination
         (yunge-jump-test--buffer " *yunge-jump-destination*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer origin)
          (goto-char 4)
          (let ((destination-window (split-window-right)))
            (set-window-buffer destination-window destination)
            (set-window-point destination-window 8)
            (yunge-jump-track-command 'yunge-jump-test--navigate)

            (yunge-jump-test--navigate
             (lambda ()
               (select-window destination-window)))

            (yunge-jump-backward)
            (should (eq (selected-window) destination-window))
            (should (eq (current-buffer) origin))
            (should (= (point) 4))

            (yunge-jump-forward)
            (should (eq (current-buffer) destination))
            (should (= (point) 8))))
      (yunge-jump-test--kill origin destination)
      (yunge-jump-test--reset))))

(ert-deftest yunge-jump-crosses-a-live-non-file-buffer ()
  (yunge-test-enable-evil)
  (yunge-jump-test--reset)
  (let ((origin (yunge-jump-test--buffer " *yunge-jump-origin*"))
        (destination
         (yunge-jump-test--buffer " *yunge-jump-destination*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer destination)
          (goto-char 3)
          (yunge-jump-test--record origin 5)

          (should (eq (key-binding (kbd "C-o"))
                      'yunge-jump-backward))
          (should (eq (key-binding (kbd "C-i"))
                      'yunge-jump-forward))

          (yunge-jump-backward)
          (should (eq (current-buffer) origin))
          (should (= (point) 5))

          (yunge-jump-forward)
          (should (eq (current-buffer) destination))
          (should (= (point) 3)))
      (yunge-jump-test--kill origin destination)
      (yunge-jump-test--reset))))

(ert-deftest yunge-jump-skips-a-dead-non-file-buffer ()
  (yunge-test-enable-evil)
  (yunge-jump-test--reset)
  (let ((older (yunge-jump-test--buffer " *yunge-jump-older*"))
        (dead (yunge-jump-test--buffer " *yunge-jump-dead*"))
        (current (yunge-jump-test--buffer " *yunge-jump-current*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer current)
          (yunge-jump-test--record older 4)
          (yunge-jump-test--record dead 6)
          (kill-buffer dead)

          (yunge-jump-backward)
          (should (eq (current-buffer) older))
          (should (= (point) 4)))
      (yunge-jump-test--kill older dead current)
      (yunge-jump-test--reset))))

(ert-deftest yunge-jump-reopens-a-dead-file-buffer ()
  (yunge-test-enable-evil)
  (yunge-jump-test--reset)
  (let* ((file (make-temp-file "yunge-jump-" nil nil "0123456789"))
         (origin (find-file-noselect file))
         (current (yunge-jump-test--buffer " *yunge-jump-current*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer current)
          (yunge-jump-test--record origin 5)
          (kill-buffer origin)

          (yunge-jump-backward)
          (should (equal (buffer-file-name) file))
          (should (= (point) 5)))
      (yunge-jump-test--kill current (get-file-buffer file))
      (delete-file file)
      (yunge-jump-test--reset))))

(ert-deftest yunge-jump-discards-the-forward-branch ()
  (yunge-test-enable-evil)
  (yunge-jump-test--reset)
  (let ((older (yunge-jump-test--buffer " *yunge-jump-older*"))
        (middle (yunge-jump-test--buffer " *yunge-jump-middle*"))
        (latest (yunge-jump-test--buffer " *yunge-jump-latest*"))
        (branch (yunge-jump-test--buffer " *yunge-jump-branch*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer latest)
          (yunge-jump-test--record older 3)
          (yunge-jump-test--record middle 5)
          (yunge-jump-backward)

          (evil-set-jump)
          (switch-to-buffer branch)
          (goto-char 7)

          (yunge-jump-backward)
          (should (eq (current-buffer) middle))
          (yunge-jump-forward)
          (should (eq (current-buffer) branch))
          (should-error (yunge-jump-forward) :type 'user-error))
      (yunge-jump-test--kill older middle latest branch)
      (yunge-jump-test--reset))))

(ert-deftest yunge-jump-copies-history-when-splitting-a-window ()
  (yunge-test-enable-evil)
  (yunge-jump-test--reset)
  (let ((origin (yunge-jump-test--buffer " *yunge-jump-origin*"))
        (current (yunge-jump-test--buffer " *yunge-jump-current*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer current)
          (yunge-jump-test--record origin 5)
          (let* ((source (selected-window))
                 (new (split-window source)))
            (select-window new)
            (yunge-jump-backward)
            (should (eq (current-buffer) origin))

            (select-window source)
            (yunge-jump-backward)
            (should (eq (current-buffer) origin))))
      (yunge-jump-test--kill origin current)
      (yunge-jump-test--reset))))

;;; yunge-jump-test.el ends here
