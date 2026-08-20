;;; yunge-history-test.el --- History tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar yunge-history-test--entries nil)

(ert-deftest yunge-history-configures-persistent-state ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (defvar yunge-var-directory)
       (let* ((root (make-temp-file "yunge-history-" t))
              (user-emacs-directory (file-name-as-directory root))
              (yunge-var-directory
               (file-name-as-directory (expand-file-name "var/" root))))
         (unwind-protect
             (progn
               (require 'yunge-history)
               (unless (and (= history-length 1000)
                            history-delete-duplicates
                            savehist-mode
                            (= savehist-autosave-interval (* 5 60))
                            recentf-mode
                            (= recentf-max-saved-items 1000)
                            (= recentf-auto-cleanup (* 10 60))
                            (not recentf-show-messages)
                            (= recentf-autosave-interval (* 5 60))
                            save-place-mode
                            (= save-place-limit 1000)
                            (= save-place-autosave-interval (* 5 60)))
                 (error "History configuration is incomplete"))
               (unless
                   (memq 'yunge-reader-saved-document-state
                         savehist-additional-variables)
                 (error "Unified Reader state is not persistent"))
               (dolist (obsolete
                        '(yunge-reader-saved-places
                          yunge-reader-saved-appearance-overrides
                          yunge-reader-saved-marks))
                 (when (memq obsolete savehist-additional-variables)
                   (error "Obsolete Reader state remains: %S" obsolete)))
               (savehist-mode -1)
               (recentf-mode -1)
               (save-place-mode -1))
           (delete-directory root t)))))))

(ert-deftest yunge-history-removes-older-duplicates ()
  (let ((history-delete-duplicates t)
        (yunge-history-test--entries nil))
    (add-to-history 'yunge-history-test--entries "A")
    (add-to-history 'yunge-history-test--entries "B")
    (add-to-history 'yunge-history-test--entries "A")
    (should (equal yunge-history-test--entries '("A" "B")))))

;;; yunge-history-test.el ends here
