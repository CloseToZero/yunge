;;; yunge-tab-test.el --- Tab tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar evil-state)

(ert-deftest yunge-tab-creates-named-tabs-without-showing-the-tab-bar ()
  (yunge-test-enable-evil)
  (require 'yunge-tab)
  (let ((initial-count (length (tab-bar-tabs))))
    (should-not tab-bar-show)
    (should-not tab-bar-mode)
    (should (equal (alist-get 'name (assq 'current-tab (tab-bar-tabs)))
                   "default"))
    (unwind-protect
        (progn
          (yunge-tab-new "yunge-tab-test-one")
          (yunge-tab-new "yunge-tab-test-two")
          (should (= (length (tab-bar-tabs)) (+ initial-count 2)))
          (let ((names (mapcar (lambda (tab) (alist-get 'name tab))
                               (tab-bar-tabs))))
            (should (member "yunge-tab-test-one" names))
            (should (member "yunge-tab-test-two" names)))
          (should-not tab-bar-mode))
      (while (> (length (tab-bar-tabs)) initial-count)
        (tab-close)))))

(ert-deftest yunge-tab-shows-the-current-tab-in-the-selected-mode-line ()
  (yunge-test-enable-evil)
  (require 'yunge-tab)
  (let ((initial-count (length (tab-bar-tabs))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mode-line-window-selected-p)
                     (lambda () t)))
            (should-not (yunge-tab--mode-line)))
          (yunge-tab-new "yunge-tab-mode-line-test")
          (cl-letf (((symbol-function 'mode-line-window-selected-p)
                     (lambda () t)))
            (should
             (equal (substring-no-properties (yunge-tab--mode-line))
                    "[yunge-tab-mode-line-test] ")))
          (cl-letf (((symbol-function 'mode-line-window-selected-p)
                     (lambda () nil)))
            (should-not (yunge-tab--mode-line))))
      (while (> (length (tab-bar-tabs)) initial-count)
        (tab-close))))
  (let ((tail (member yunge-tab-mode-line-format
                      (default-value 'mode-line-format))))
    (should (eq (cadr tail) 'mode-line-buffer-identification))))

(ert-deftest yunge-tab-routes-leader-bindings ()
  (yunge-test-enable-evil)
  (require 'yunge-tab)
  (require 'which-key)

  (with-temp-buffer
    (fundamental-mode)
    (should (eq evil-state 'normal))
    (yunge-test-keys
     '(("C-i" . yunge-jump-history-forward)
       ("SPC TAB TAB" . tab-switch)
       ("SPC <tab> <tab>" . tab-switch)
       ("SPC TAB n" . yunge-tab-new)
       ("SPC TAB q" . tab-close)
       ("SPC TAB r" . tab-rename)
       ("SPC TAB u" . tab-undo))))

  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC"
   '(("TAB" nil "+tab")))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC TAB"
   '(("TAB" nil "switch tab")
     ("n" nil "new tab")
     ("q" nil "close tab")
     ("r" nil "rename tab")
     ("u" nil "restore tab"))))

;;; yunge-tab-test.el ends here
