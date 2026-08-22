;;; yunge-window-test.el --- Window tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar evil-state)

(yunge-test-deftest-lazy-load yunge-window
  (ace-window))

(ert-deftest yunge-window-configures-ace-window-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-window 'ace-window
   :before-ready
   '(progn
      (when (featurep 'ace-window)
        (error "Ace Window was loaded before its Elpaca body ran"))
      (unless (eq (keymap-lookup yunge-window-map "a") 'ace-window)
        (error "Ace Window binding is missing")))
   :after-ready
   '(when (featurep 'ace-window)
      (error "Ace Window was loaded by its configuration"))))

(ert-deftest yunge-window-routes-one-shot-and-control-bindings ()
  (yunge-test-enable-evil)
  (yunge-test-load-package-config 'yunge-window)
  (require 'which-key)

  (with-temp-buffer
    (fundamental-mode)
    (should (eq evil-state 'normal))
    (yunge-test-key "SPC w h" 'windmove-left)
    (yunge-test-key "SPC w a" 'ace-window)
    (yunge-test-key "SPC w u" 'tab-bar-history-back)
    (yunge-test-key "SPC w r" 'tab-bar-history-forward)
    (yunge-test-key "SPC w w" 'other-window)
    (yunge-test-key "j" 'evil-next-line)

    (unwind-protect
        (progn
          (yunge-window-control)
          (yunge-test-key "j" 'windmove-down)
          (yunge-test-key "u" 'tab-bar-history-back)
          (yunge-test-key "r" 'tab-bar-history-forward)
          (yunge-test-key "w" 'other-window)
          (yunge-test-key "SPC" 'yunge-key-control-quit))
      (yunge-key-control-quit))

    (yunge-test-key "j" 'evil-next-line))

  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC w"
   '(("SPC" nil "window control")
     ("a" nil "select window")
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
     ("r" nil "redo layout")
     ("s" nil "split below")
     ("u" nil "undo layout")
     ("v" nil "split right")
     ("w" nil "next window"))))

(ert-deftest yunge-window-enables-per-tab-layout-history ()
  (yunge-test-enable-evil)
  (yunge-test-load-package-config 'yunge-window)
  (should tab-bar-history-mode)
  (should (memq #'tab-bar--history-pre-change
                (default-value 'pre-command-hook)))
  (should (memq #'tab-bar--history-change
                (default-value 'window-configuration-change-hook))))

(ert-deftest yunge-window-undoes-and-redoes-layout-changes ()
  (yunge-test-enable-evil)
  (yunge-test-load-package-config 'yunge-window)
  (let ((tab-bar-history-back (make-hash-table))
        (tab-bar-history-forward (make-hash-table))
        (tab-bar-history-old nil)
        (tab-bar-history-pre-command nil)
        (tab-bar-history-done-command nil))
    (save-window-excursion
      (delete-other-windows)
      (setq this-command 'yunge-window-test-split)
      (tab-bar--history-pre-change)
      (yunge-window-split-right)
      (tab-bar--history-change)
      (should (= (length (window-list)) 2))

      (setq this-command 'tab-bar-history-back)
      (tab-bar--history-pre-change)
      (tab-bar-history-back)
      (should (one-window-p))

      (setq this-command 'tab-bar-history-forward)
      (tab-bar--history-pre-change)
      (tab-bar-history-forward)
      (should (= (length (window-list)) 2)))))

(ert-deftest yunge-window-splits-and-selects-new-window ()
  (yunge-test-enable-evil)
  (yunge-test-load-package-config 'yunge-window)
  (dolist (command '(yunge-window-split-below
                     yunge-window-split-right))
    (save-window-excursion
      (delete-other-windows)
      (let ((original (selected-window)))
        (funcall command)
        (should (= (length (window-list)) 2))
        (should-not (eq (selected-window) original))))))

;;; yunge-window-test.el ends here
