;;; yunge-navigation-test.el --- Jump landing tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-navigation)

(ert-deftest yunge-navigation-adapts-text-landing-to-visibility ()
  (with-temp-buffer
    (insert "target\n")
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let (recentered (pulses 0) visible)
        (cl-letf (((symbol-function 'pos-visible-in-window-group-p)
                   (lambda (&rest _arguments) visible))
                  ((symbol-function 'recenter-window-group)
                   (lambda (&rest _arguments) (setq recentered t)))
                  ((symbol-function 'yunge-navigation--pulse)
                   (lambda (&rest _arguments) (cl-incf pulses))))
          (setq visible t)
          (yunge-navigation-land)
          (should-not recentered)
          (should (= pulses 1))

          (setq visible nil
                recentered nil)
          (yunge-navigation-land)
          (should recentered)
          (should (= pulses 2)))))))

(ert-deftest yunge-navigation-leaves-owned-viewports-alone ()
  (with-temp-buffer
    (setq-local yunge-navigation-landing-policy 'viewport)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (cl-letf (((symbol-function 'pos-visible-in-window-group-p)
                 (lambda (&rest _arguments)
                   (ert-fail "Owned viewport visibility was inspected")))
                ((symbol-function 'yunge-navigation--pulse)
                 (lambda (&rest _arguments)
                   (ert-fail "Owned viewport was pulsed"))))
        (yunge-navigation-land)))))

(provide 'yunge-navigation-test)

;;; yunge-navigation-test.el ends here
