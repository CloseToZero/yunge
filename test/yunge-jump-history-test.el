;;; yunge-jump-history-test.el --- Jump history tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-set-jump "evil-jumps")
(declare-function yunge-jump-history-backward "yunge-jump-history")
(declare-function yunge-jump-history-forward "yunge-jump-history")
(declare-function yunge-jump-history-track-command "yunge-jump-history")

(defvar evil--jumps-jump-command)

(defun yunge-jump-history-test--buffer (name)
  "Return a non-file buffer named NAME with test content."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert "0123456789"))
    buffer))

(defun yunge-jump-history-test--record (buffer position)
  "Record POSITION in BUFFER as an Evil jump."
  (evil-set-jump
   (with-current-buffer buffer
     (copy-marker position))))

(defun yunge-jump-history-test--reset ()
  "Clear jump state from the selected test window."
  (set-window-parameter nil 'yunge-jump-history nil)
  (setq evil--jumps-jump-command nil))

(defun yunge-jump-history-test--kill (&rest buffers)
  "Kill every live buffer in BUFFERS."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun yunge-jump-history-test--navigate (action)
  "Run ACTION as a navigation command."
  (funcall action))

(ert-deftest yunge-jump-history-tracks-successful-navigation ()
  (yunge-test-enable-evil)
  (yunge-jump-history-test--reset)
  (let ((origin (yunge-jump-history-test--buffer " *yunge-jump-origin*"))
        (preview (yunge-jump-history-test--buffer " *yunge-jump-preview*"))
        (destination
         (yunge-jump-history-test--buffer " *yunge-jump-destination*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (goto-char 4)
          (yunge-jump-history-track-command 'yunge-jump-history-test--navigate)

          (yunge-jump-history-test--navigate
           (lambda ()
             (switch-to-buffer preview)
             (goto-char 6)
             (evil-set-jump)
             (switch-to-buffer destination)
             (goto-char 8)))

          (yunge-jump-history-backward)
          (should (eq (current-buffer) origin))
          (should (= (point) 4))
          (should-error (yunge-jump-history-backward) :type 'user-error)

          (yunge-jump-history-forward)
          (should (eq (current-buffer) destination))
          (should (= (point) 8)))
      (yunge-jump-history-test--kill origin preview destination)
      (yunge-jump-history-test--reset))))

(ert-deftest yunge-jump-history-preserves-forward-history-after-cancel ()
  (yunge-test-enable-evil)
  (yunge-jump-history-test--reset)
  (let ((older (yunge-jump-history-test--buffer " *yunge-jump-older*"))
        (latest (yunge-jump-history-test--buffer " *yunge-jump-latest*"))
        (preview (yunge-jump-history-test--buffer " *yunge-jump-preview*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer latest)
          (goto-char 7)
          (yunge-jump-history-test--record older 3)
          (yunge-jump-history-backward)
          (yunge-jump-history-track-command 'yunge-jump-history-test--navigate)

          (let (cancelled)
            (condition-case nil
                (yunge-jump-history-test--navigate
                 (lambda ()
                   (switch-to-buffer preview)
                   (evil-set-jump)
                   (switch-to-buffer older)
                   (goto-char 3)
                   (signal 'quit nil)))
              (quit (setq cancelled t)))
            (should cancelled))

          (yunge-jump-history-forward)
          (should (eq (current-buffer) latest))
          (should (= (point) 7)))
      (yunge-jump-history-test--kill older latest preview)
      (yunge-jump-history-test--reset))))

(ert-deftest yunge-jump-history-records-navigation-in-destination-window ()
  (yunge-test-enable-evil)
  (yunge-jump-history-test--reset)
  (let ((origin (yunge-jump-history-test--buffer " *yunge-jump-origin*"))
        (destination
         (yunge-jump-history-test--buffer " *yunge-jump-destination*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer origin)
          (goto-char 4)
          (let ((destination-window (split-window-right)))
            (set-window-buffer destination-window destination)
            (set-window-point destination-window 8)
            (yunge-jump-history-track-command 'yunge-jump-history-test--navigate)

            (yunge-jump-history-test--navigate
             (lambda ()
               (select-window destination-window)))

            (yunge-jump-history-backward)
            (should (eq (selected-window) destination-window))
            (should (eq (current-buffer) origin))
            (should (= (point) 4))

            (yunge-jump-history-forward)
            (should (eq (current-buffer) destination))
            (should (= (point) 8))))
      (yunge-jump-history-test--kill origin destination)
      (yunge-jump-history-test--reset))))

(ert-deftest yunge-jump-history-crosses-a-live-non-file-buffer ()
  (yunge-test-enable-evil)
  (yunge-jump-history-test--reset)
  (let ((origin (yunge-jump-history-test--buffer " *yunge-jump-origin*"))
        (destination
         (yunge-jump-history-test--buffer " *yunge-jump-destination*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer destination)
          (goto-char 3)
          (yunge-jump-history-test--record origin 5)

          (should (eq (key-binding (kbd "C-o"))
                      'yunge-jump-history-backward))
          (should (eq (key-binding (kbd "C-i"))
                      'yunge-jump-history-forward))

          (yunge-jump-history-backward)
          (should (eq (current-buffer) origin))
          (should (= (point) 5))

          (yunge-jump-history-forward)
          (should (eq (current-buffer) destination))
          (should (= (point) 3)))
      (yunge-jump-history-test--kill origin destination)
      (yunge-jump-history-test--reset))))

(ert-deftest yunge-jump-history-skips-a-dead-non-file-buffer ()
  (yunge-test-enable-evil)
  (yunge-jump-history-test--reset)
  (let ((older (yunge-jump-history-test--buffer " *yunge-jump-older*"))
        (dead (yunge-jump-history-test--buffer " *yunge-jump-dead*"))
        (current (yunge-jump-history-test--buffer " *yunge-jump-current*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer current)
          (yunge-jump-history-test--record older 4)
          (yunge-jump-history-test--record dead 6)
          (kill-buffer dead)

          (yunge-jump-history-backward)
          (should (eq (current-buffer) older))
          (should (= (point) 4)))
      (yunge-jump-history-test--kill older dead current)
      (yunge-jump-history-test--reset))))

(ert-deftest yunge-jump-history-reopens-a-dead-file-buffer ()
  (yunge-test-enable-evil)
  (yunge-jump-history-test--reset)
  (let* ((file (make-temp-file "yunge-jump-" nil nil "0123456789"))
         (origin (find-file-noselect file))
         (current (yunge-jump-history-test--buffer " *yunge-jump-current*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer current)
          (yunge-jump-history-test--record origin 5)
          (kill-buffer origin)

          (yunge-jump-history-backward)
          (should (equal (buffer-file-name) file))
          (should (= (point) 5)))
      (yunge-jump-history-test--kill current (get-file-buffer file))
      (delete-file file)
      (yunge-jump-history-test--reset))))

(ert-deftest yunge-jump-history-discards-the-forward-branch ()
  (yunge-test-enable-evil)
  (yunge-jump-history-test--reset)
  (let ((older (yunge-jump-history-test--buffer " *yunge-jump-older*"))
        (middle (yunge-jump-history-test--buffer " *yunge-jump-middle*"))
        (latest (yunge-jump-history-test--buffer " *yunge-jump-latest*"))
        (branch (yunge-jump-history-test--buffer " *yunge-jump-branch*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer latest)
          (yunge-jump-history-test--record older 3)
          (yunge-jump-history-test--record middle 5)
          (yunge-jump-history-backward)

          (evil-set-jump)
          (switch-to-buffer branch)
          (goto-char 7)

          (yunge-jump-history-backward)
          (should (eq (current-buffer) middle))
          (yunge-jump-history-forward)
          (should (eq (current-buffer) branch))
          (should-error (yunge-jump-history-forward) :type 'user-error))
      (yunge-jump-history-test--kill older middle latest branch)
      (yunge-jump-history-test--reset))))

(ert-deftest yunge-jump-history-copies-history-when-splitting-a-window ()
  (yunge-test-enable-evil)
  (yunge-jump-history-test--reset)
  (let ((origin (yunge-jump-history-test--buffer " *yunge-jump-origin*"))
        (current (yunge-jump-history-test--buffer " *yunge-jump-current*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer current)
          (yunge-jump-history-test--record origin 5)
          (let* ((source (selected-window))
                 (new (split-window source)))
            (select-window new)
            (yunge-jump-history-backward)
            (should (eq (current-buffer) origin))

            (select-window source)
            (yunge-jump-history-backward)
            (should (eq (current-buffer) origin))))
      (yunge-jump-history-test--kill origin current)
      (yunge-jump-history-test--reset))))

;;; yunge-jump-history-test.el ends here
