;;; yunge-state-test.el --- Mutable state tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-state-configures-mutable-storage ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(let* ((root (make-temp-file "yunge-state-" t))
            (config-root (make-temp-file "yunge-config-" t))
            (user-emacs-directory (file-name-as-directory root))
            (yunge-config-directory
             (file-name-as-directory config-root))
            (file (expand-file-name "file.txt" root)))
       (unwind-protect
           (progn
             (require 'yunge-state)
             (unless (equal yunge-config-directory
                            (file-name-as-directory config-root))
               (error "The tracked configuration root was not preserved"))
             (unless (equal yunge-var-directory
                            (file-name-as-directory
                             (expand-file-name "var/" root)))
               (error "The shared var directory was not configured"))
             (let ((unused
                    (yunge-var-file "unused" "state.eld")))
               (when (file-exists-p (file-name-directory unused))
                 (error "Computing a var path created its directory")))
             (with-temp-file file
               (insert "saved"))
             (with-current-buffer (find-file-noselect file)
               (auto-save-mode 1)
               (goto-char (point-max))
               (insert " recovery")
               (do-auto-save t t)
               (unless (and buffer-auto-save-file-name
                            (file-exists-p
                             buffer-auto-save-file-name))
                 (error "Auto-save recovery file was not written"))
               (set-buffer-modified-p nil)
               (kill-buffer))
             (unless (file-directory-p tramp-auto-save-directory)
               (error "TRAMP auto-save directory was not created"))
             (unless (equal server-auth-dir
                            (expand-file-name "server/"
                                              yunge-var-directory))
               (error "Server authentication was not redirected under var"))
             (unless (equal
                      (list bookmark-default-file
                            custom-file
                            org-clock-persist-file
                            org-id-locations-file
                            org-persist-directory
                            org-publish-timestamp-directory
                            project-list-file
                            recentf-save-file
                            save-place-file
                            savehist-file
                            tramp-persistency-file-name)
                      (mapcar
                       (lambda (file)
                         (expand-file-name file yunge-var-directory))
                       '("bookmark/bookmarks.eld"
                         "custom/custom.el"
                         "org-clock/clock-save.el"
                         "org-id/locations.eld"
                         "org-persist/"
                         "org-publish/timestamps/"
                         "project/projects.eld"
                         "recentf/recentf.eld"
                         "save-place/places.eld"
                         "savehist/savehist.eld"
                         "tramp/persistency.eld")))
               (error "Mutable files were not grouped by owner"))
             (unless (equal
                      (list transient-history-file
                            transient-levels-file
                            transient-values-file)
                      (mapcar
                       (lambda (file)
                         (expand-file-name file yunge-var-directory))
                       '("transient/history.el"
                         "transient/levels.el"
                         "transient/values.el")))
               (error "Transient state was not redirected under var")))
         (delete-directory root t)
         (delete-directory config-root t))))))

(provide 'yunge-state-test)

;;; yunge-state-test.el ends here
