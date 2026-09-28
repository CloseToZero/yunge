;;; yunge-magit-staging-test.el --- Magit staging flows -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function magit-diff-staged "magit-diff")
(declare-function magit-diff-unstaged "magit-diff")
(declare-function magit-status "magit-status")

(defun yunge-magit-staging-test--git (root &rest arguments)
  "Run Git in ROOT with ARGUMENTS and return its output."
  (with-temp-buffer
    (let* ((default-directory root)
           (status (apply #'process-file "git" nil t nil arguments)))
      (unless (equal status 0)
        (ert-fail (format "git %S failed: %s" arguments (buffer-string))))
      (buffer-string))))

(defun yunge-magit-staging-test--with-repository (body)
  "Call BODY in a temporary modified Git repository."
  (unless (executable-find "git")
    (ert-skip "Git is unavailable"))
  (yunge-test-enable-evil)
  (require 'magit-autoloads)
  (yunge-test-load-package-config 'yunge-magit)
  (require 'magit)
  (let* ((root (file-name-as-directory
                (make-temp-file "yunge-magit-staging-" t)))
         (file (expand-file-name "notes.txt" root))
         (buffers-before (buffer-list))
         (base (concat "top\nold first\n"
                       (mapconcat (lambda (number) (format "middle %02d" number))
                                  (number-sequence 1 12) "\n")
                       "\nold second\nbottom\n")))
    (unwind-protect
        (save-window-excursion
          (yunge-magit-staging-test--git root "init" "--quiet")
          (with-temp-file file (insert base))
          (yunge-magit-staging-test--git root "add" "notes.txt")
          (yunge-magit-staging-test--git
           root "-c" "user.name=Yunge Test"
           "-c" "user.email=yunge-test@example.invalid"
           "-c" "commit.gpgSign=false" "commit" "--quiet" "-m" "Initial")
          (with-temp-file file
            (insert (replace-regexp-in-string
                     "old second" "new second"
                     (replace-regexp-in-string
                      "old first\n"
                      "old first\nnew first\nnew extra\nnew third\n"
                      base))))
          (funcall body root))
      (dolist (buffer (buffer-list))
        (unless (memq buffer buffers-before)
          (when (buffer-live-p buffer) (kill-buffer buffer))))
      (delete-directory root t))))

(defun yunge-magit-staging-test--at (buffer text)
  "Select BUFFER at the line containing TEXT."
  (switch-to-buffer buffer)
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line)
  (evil-normal-state))

(ert-deftest yunge-magit-stages-and-unstages-a-file-from-status ()
  (yunge-magit-staging-test--with-repository
   (lambda (root)
     (let* ((default-directory root)
            (status (magit-status root))
            (baseline (yunge-magit-staging-test--git
                       root "show" "HEAD:notes.txt"))
            (worktree (with-temp-buffer
                        (insert-file-contents (expand-file-name "notes.txt" root))
                        (buffer-string))))
       (yunge-magit-staging-test--at status "notes.txt")
       (execute-kbd-macro (kbd "s"))
       (should (equal (yunge-magit-staging-test--git root "show" ":notes.txt")
                      worktree))
       (yunge-magit-staging-test--at status "notes.txt")
       (execute-kbd-macro (kbd "u"))
       (should (equal (yunge-magit-staging-test--git root "show" ":notes.txt")
                      baseline))
       (should (string-empty-p
                (yunge-magit-staging-test--git
                 root "diff" "--cached" "--" "notes.txt")))
       (with-temp-buffer
         (insert-file-contents (expand-file-name "notes.txt" root))
         (should (equal (buffer-string) worktree)))))))

(ert-deftest yunge-magit-stages-and-unstages-one-hunk-from-diff ()
  (yunge-magit-staging-test--with-repository
   (lambda (root)
     (let* ((default-directory root)
            (baseline (yunge-magit-staging-test--git
                       root "show" "HEAD:notes.txt"))
            (first-hunk
             (replace-regexp-in-string
              "old first\n" "old first\nnew first\nnew extra\nnew third\n"
              baseline)))
       (yunge-magit-staging-test--at (magit-diff-unstaged) "+new first")
       (execute-kbd-macro (kbd "s"))
       (should (equal (yunge-magit-staging-test--git root "show" ":notes.txt")
                      first-hunk))
       (yunge-magit-staging-test--at (magit-diff-staged) "+new first")
       (execute-kbd-macro (kbd "u"))
       (should (equal (yunge-magit-staging-test--git root "show" ":notes.txt")
                      baseline))))))

(ert-deftest yunge-magit-visual-stage-and-unstage-select-exact-lines ()
  (yunge-magit-staging-test--with-repository
   (lambda (root)
     (let* ((default-directory root)
            (baseline (yunge-magit-staging-test--git
                       root "show" "HEAD:notes.txt"))
            (selected-lines
             (replace-regexp-in-string
              "old first\n" "old first\nnew first\nnew extra\n"
              baseline)))
       (yunge-magit-staging-test--at (magit-diff-unstaged) "+new first")
       (execute-kbd-macro (kbd "V j s"))
       (should (equal (yunge-magit-staging-test--git root "show" ":notes.txt")
                      selected-lines))
       (yunge-magit-staging-test--at (magit-diff-staged) "+new first")
       (execute-kbd-macro (kbd "V j u"))
       (should (equal (yunge-magit-staging-test--git root "show" ":notes.txt")
                      baseline))))))

(provide 'yunge-magit-staging-test)

;;; yunge-magit-staging-test.el ends here

