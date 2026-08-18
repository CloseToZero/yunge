;;; yunge-history-test.el --- History tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-history)

(defvar yunge-history-test--entries nil)

(ert-deftest yunge-history-configures-persistent-state ()
  (should (= history-length 1000))
  (should history-delete-duplicates)
  (should savehist-mode)
  (should (= savehist-autosave-interval (* 5 60)))
  (should
   (memq 'yunge-reader-saved-places
         savehist-additional-variables))
  (should
   (memq 'yunge-reader-saved-appearance-overrides
         savehist-additional-variables))
  (should recentf-mode)
  (should (= recentf-max-saved-items 1000))
  (should (= recentf-auto-cleanup (* 10 60)))
  (should-not recentf-show-messages)
  (should (= recentf-autosave-interval (* 5 60)))
  (should save-place-mode)
  (should (= save-place-limit 1000))
  (should (= save-place-autosave-interval (* 5 60))))

(ert-deftest yunge-history-removes-older-duplicates ()
  (let ((yunge-history-test--entries nil))
    (add-to-history 'yunge-history-test--entries "A")
    (add-to-history 'yunge-history-test--entries "B")
    (add-to-history 'yunge-history-test--entries "A")
    (should (equal yunge-history-test--entries '("A" "B")))))

;;; yunge-history-test.el ends here
