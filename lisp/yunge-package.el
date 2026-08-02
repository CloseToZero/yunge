;;; yunge-package.el --- Package management -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory
  (expand-file-name "var/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory
  (expand-file-name "build/" elpaca-directory))
(defvar elpaca-sources-directory
  (expand-file-name "source/" elpaca-directory))
(defvar elpaca-lock-file
  (expand-file-name "elpaca-lock.el" user-emacs-directory))

(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref "6530ffa73b18ccee858e7c471415ab7e0c0d8ce1"
           :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca-activate)))

(let* ((repo (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer
                   (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop
                    (apply #'call-process
                           `("git" nil ,buffer t "clone"
                             ,@(when-let* ((depth
                                           (plist-get order :depth)))
                                 (list (format "--depth=%d" depth)
                                       "--no-single-branch"))
                             ,(plist-get order :repo) ,repo))))
                  ((zerop
                    (call-process "git" nil buffer t "checkout"
                                  (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop
                    (call-process
                     emacs nil buffer nil "-Q" "-L" "." "--batch"
                     "--eval"
                     "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn
              (message "%s" (buffer-string))
              (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error)
       (warn "%s" err)
       (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil))
      (load "./elpaca-autoloads"))))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(when (eq system-type 'windows-nt)
  ;; Avoid requiring symlink privileges on Windows.
  (elpaca-no-symlink-mode 1))

(provide 'yunge-package)

;;; yunge-package.el ends here
