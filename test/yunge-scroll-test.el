;;; yunge-scroll-test.el --- Scrolling tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-scroll)

(ert-deftest yunge-scroll-remaps-the-built-in-wheel-command ()
  (should
   (eq (lookup-key global-map [remap mwheel-scroll])
       #'yunge-scroll-mwheel)))

(ert-deftest yunge-scroll-keeps-a-visible-point-at-a-wheel-boundary ()
  (with-temp-buffer
    (insert "before\nafter\n")
    (goto-char 3)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((window (selected-window))
            (mouse-wheel-follow-mouse t)
            (mwheel-scroll-up-function 'scroll-up)
            (mwheel-scroll-down-function 'scroll-down))
        (cl-letf (((symbol-function 'mwheel-event-window)
                   (lambda (_event) window))
                  ((symbol-function 'mwheel-scroll)
                   (lambda (_event &optional _argument)
                     (goto-char (point-max))))
                  ((symbol-function 'pos-visible-in-window-p)
                   (lambda (position target &optional partially)
                     (should (= position 3))
                     (should (eq target window))
                     (should partially)
                     t)))
          (yunge-scroll-mwheel 'wheel-event)
          (should (= (point) 3)))))))

(ert-deftest yunge-scroll-allows-point-to-follow-an-offscreen-scroll ()
  (with-temp-buffer
    (insert "before\nafter\n")
    (goto-char 3)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((window (selected-window))
            (mouse-wheel-follow-mouse t)
            (mwheel-scroll-up-function 'scroll-up)
            (mwheel-scroll-down-function 'scroll-down))
        (cl-letf (((symbol-function 'mwheel-event-window)
                   (lambda (_event) window))
                  ((symbol-function 'mwheel-scroll)
                   (lambda (_event &optional _argument)
                     (goto-char (point-max))))
                  ((symbol-function 'pos-visible-in-window-p)
                   (lambda (&rest _arguments) nil)))
          (yunge-scroll-mwheel 'wheel-event)
          (should (= (point) (point-max))))))))

(ert-deftest yunge-scroll-leaves-custom-wheel-backends-alone ()
  (with-temp-buffer
    (insert "before\nafter\n")
    (goto-char 3)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((window (selected-window))
            (mouse-wheel-follow-mouse t)
            (mwheel-scroll-up-function 'pixel-scroll-up)
            (mwheel-scroll-down-function 'pixel-scroll-down))
        (cl-letf (((symbol-function 'mwheel-event-window)
                   (lambda (_event) window))
                  ((symbol-function 'mwheel-scroll)
                   (lambda (_event &optional _argument)
                     (goto-char (point-max))))
                  ((symbol-function 'pos-visible-in-window-p)
                   (lambda (&rest _arguments)
                     (ert-fail "Custom wheel point must not be inspected"))))
          (yunge-scroll-mwheel 'wheel-event)
          (should (= (point) (point-max))))))))

(provide 'yunge-scroll-test)

;;; yunge-scroll-test.el ends here
