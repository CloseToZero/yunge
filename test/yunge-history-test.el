;;; yunge-history-test.el --- History tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

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
               (unless (and (= yunge-history-max-items 1000)
                            (= history-length 1000)
                            history-delete-duplicates
                            (= kill-ring-max 1000)
                            (= search-ring-max 1000)
                            (= regexp-search-ring-max 1000)
                            (= kmacro-ring-max 1000)
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

(ert-deftest yunge-history-round-trips-explicit-histories ()
  (let* ((root (make-temp-file "yunge-history-round-trip-" t))
         (user-directory (file-name-as-directory root))
         (var-directory
          (file-name-as-directory (expand-file-name "var/" root)))
         (histories
          '((kill-ring "kill")
            (command-history (find-file "example"))
            (search-ring "literal")
            (regexp-search-ring "regexp")
            (kmacro-ring "macro")
            (evil-ex-history "write")
            (evil-eval-history "(+ 1 2)")
            (evil-ex-search-history "search")
            (evil-search-forward-history "forward")
            (evil-search-backward-history "backward"))))
    (unwind-protect
        (progn
          ;; The first Emacs records history and relies on normal shutdown
          ;; to persist it.
          (yunge-test-run-emacs
           "--eval"
           (prin1-to-string
            `(progn
               (defvar yunge-var-directory)
               (let ((user-emacs-directory ,user-directory)
                     (yunge-var-directory ,var-directory))
                 (require 'yunge-history)
                 (dolist (entry ',histories)
                   (set (car entry) (copy-tree (cdr entry))))))))
          ;; A separate Emacs must restore the same values during startup.
          (yunge-test-run-emacs
           "--eval"
           (prin1-to-string
            `(progn
               (defvar yunge-var-directory)
               (let ((user-emacs-directory ,user-directory)
                     (yunge-var-directory ,var-directory))
                 (require 'yunge-history)
                 (dolist (entry ',histories)
                   (unless (equal (symbol-value (car entry)) (cdr entry))
                     (error "%S history did not survive an Emacs restart"
                            (car entry)))))))))
      (delete-directory root t))))

;;; yunge-history-test.el ends here
