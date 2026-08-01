;;; yunge-window-test.el --- Window management tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar evil-state)

(ert-deftest yunge-window-routes-one-shot-and-control-bindings ()
  (yunge-test-enable-evil)
  (require 'yunge-window)
  (require 'which-key)

  (with-temp-buffer
    (fundamental-mode)
    (should (eq evil-state 'normal))
    (yunge-test-key "SPC w h" 'windmove-left)
    (yunge-test-key "j" 'evil-next-line)

    (unwind-protect
        (progn
          (yunge-window-control)
          (yunge-test-key "j" 'windmove-down)
          (yunge-test-key "SPC" 'yunge-key-control-quit))
      (yunge-key-control-quit))

    (yunge-test-key "j" 'evil-next-line))

  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC w"
   '(("SPC" nil "window control")
     ("+" nil "increase height")
     ("-" nil "decrease height")
     ("<" nil "decrease width")
     ("=" nil "balance")
     (">" nil "increase width")
     ("h" nil "select left")
     ("j" nil "select below")
     ("k" nil "select above")
     ("l" nil "select right")
     ("o" nil "keep only this window")
     ("q" nil "close window")
     ("s" nil "split below")
     ("v" nil "split right")
     ("w" nil "next window"))))

(ert-deftest yunge-window-splits-and-selects-new-window ()
  (yunge-test-enable-evil)
  (require 'yunge-window)
  (dolist (command '(yunge-window-split-below
                     yunge-window-split-right))
    (save-window-excursion
      (delete-other-windows)
      (let ((original (selected-window)))
        (funcall command)
        (should (= (length (window-list)) 2))
        (should-not (eq (selected-window) original))))))

;;; yunge-window-test.el ends here
