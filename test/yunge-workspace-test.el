;;; yunge-workspace-test.el --- Persistent workspace tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(ert-deftest yunge-workspace-round-trips-only-when-restored ()
  (let* ((root (make-temp-file "yunge-workspace-" t))
         (user-directory (file-name-as-directory root))
         (var-directory
          (file-name-as-directory (expand-file-name "var/" root)))
         (file-a (expand-file-name "a.txt" root))
         (file-b (expand-file-name "b.txt" root))
         (file-c (expand-file-name "c.txt" root))
         (file-d (expand-file-name "d.txt" root))
         (special-name "*yunge-workspace-special*"))
    (unwind-protect
        (progn
          ;; Save two snapshots from the first Emacs process.
          (yunge-test-run-emacs
           "--eval"
           (prin1-to-string
            `(progn
               (defvar yunge-var-directory)
               (let ((user-emacs-directory ,user-directory)
                     (yunge-var-directory ,var-directory))
                 (require 'yunge-workspace)
                 (unless yunge-workspace-auto-save-mode
                   (error "Default workspace auto-save is not enabled"))
                 (dolist (file (list ,file-a ,file-b ,file-c ,file-d))
                   (with-temp-file file
                     (insert (file-name-base file))))

                 (find-file ,file-a)
                 (delete-other-windows)
                 (split-window-right)
                 (set-window-buffer (next-window)
                                    (find-file-noselect ,file-b))
                 (tab-rename "code" 1)
                 (tab-new)
                 (tab-rename "notes")
                 (switch-to-buffer (find-file-noselect ,file-c))
                 ;; File buffers are workspace members even when no tab shows
                 ;; them.
                 (find-file-noselect ,file-d)
                 (with-current-buffer (get-buffer-create ,special-name)
                   (insert "ephemeral"))
                 (set-window-buffer (split-window-right) ,special-name)

                 (yunge-workspace-save "coding")
                 ;; Exercise the interactive exit hook with a distinct default
                 ;; state; batch Emacs deliberately skips automatic saves.
                 (tab-rename "latest")
                 (let ((noninteractive nil))
                   (run-hooks 'kill-emacs-hook))
                 (yunge-workspace-auto-save-mode -1)))))

          ;; A fresh Emacs starts clean, then restores the selected snapshot.
          (yunge-test-run-emacs
           "--eval"
           (prin1-to-string
            `(progn
               (defvar yunge-var-directory)
               (let ((user-emacs-directory ,user-directory)
                     (yunge-var-directory ,var-directory))
                 (require 'yunge-workspace)
                 (unless (equal (yunge-workspace-names)
                                '("coding" "default"))
                   (error "Saved workspaces were not discoverable"))
                 (when (or (get-file-buffer ,file-a)
                           (get-file-buffer ,file-b)
                           (get-file-buffer ,file-c)
                           (get-file-buffer ,file-d)
                           (> (length (tab-bar-tabs)) 1))
                   (error "A workspace was restored during startup"))

                 ;; An unrelated buffer may already own a saved buffer name.
                 (with-current-buffer (get-buffer-create "a.txt")
                   (insert "unsaved scratch text"))
                 (with-current-buffer (get-buffer-create ,special-name)
                   (insert "current session"))
                 (yunge-workspace-restore "default")
                 (yunge-workspace-save "restored")
                 (unless
                     (equal
                      (mapcar (lambda (tab) (alist-get 'name tab))
                              (tab-bar-tabs))
                      '("code" "latest"))
                   (error "The default workspace was not saved on exit"))

                 (with-current-buffer (get-file-buffer ,file-b)
                   (goto-char (point-max))
                   (insert " draft"))
                 (yunge-workspace-restore "coding")
                 (let ((names
                        (mapcar (lambda (tab) (alist-get 'name tab))
                                (tab-bar-tabs))))
                   (unless (equal names '("code" "notes"))
                     (error "Tab names were not restored: %S" names)))
                 (unless
                     (equal
                      (alist-get 'name (assq 'current-tab (tab-bar-tabs)))
                      "notes")
                   (error "The selected tab was not restored"))
                 (dolist (file (list ,file-a ,file-b ,file-c ,file-d))
                   (unless (get-file-buffer file)
                     (error "File buffer was not restored: %s" file)))
                 (with-current-buffer ,special-name
                   (unless (equal (buffer-string) "current session")
                     (error "Restore changed an excluded buffer")))
                 (unless
                     (and (= (length (window-list)) 1)
                          (equal (buffer-file-name (window-buffer)) ,file-c))
                   (error "The notes layout was not restored"))

                 (tab-bar-select-tab 1)
                 (let ((files
                        (sort
                         (mapcar
                          (lambda (window)
                            (buffer-file-name (window-buffer window)))
                          (window-list))
                         #'string<)))
                   (unless
                       (and (= (length (window-list)) 2)
                            (equal files
                                   (sort (list ,file-a ,file-b) #'string<)))
                     (error "The code layout was not restored: %S" files)))
                 (with-current-buffer "a.txt"
                   (unless (and (not buffer-file-name)
                                (buffer-modified-p)
                                (equal (buffer-string) "unsaved scratch text"))
                     (error "Restore changed an unrelated buffer")))
                 (with-current-buffer (get-file-buffer ,file-b)
                   (unless (and (buffer-modified-p)
                                (equal (buffer-string) "b draft"))
                     (error "Restore discarded unsaved file edits")))
                 (yunge-workspace-auto-save-mode -1))))))
      (delete-directory root t))))

;;; yunge-workspace-test.el ends here
