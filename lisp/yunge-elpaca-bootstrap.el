;;; yunge-elpaca-bootstrap.el --- Install and activate Elpaca -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-state)

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory
  (yunge-var-subdirectory "elpaca"))
(defvar elpaca-builds-directory
  (expand-file-name "build/" elpaca-directory))
(defvar elpaca-sources-directory
  (expand-file-name "source/" elpaca-directory))
(defvar elpaca-lock-file
  (expand-file-name "elpaca-lock.el" yunge-config-directory))

(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref "6530ffa73b18ccee858e7c471415ab7e0c0d8ce1"
           :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca-activate)))

(defun yunge-elpaca-bootstrap--run (buffer program &rest arguments)
  "Run PROGRAM with ARGUMENTS, recording output and failures in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert (mapconcat #'identity (cons program arguments) " ") "\n"))
  (let ((status (apply #'call-process program nil (list buffer t) nil arguments)))
    (unless (eq status 0)
      (error "%s exited with %s" program status))))

(defun yunge-elpaca-bootstrap--install (repo)
  "Install the pinned Elpaca source in REPO after preparing its autoloads."
  (make-directory elpaca-sources-directory t)
  (let* ((staging (make-temp-file (expand-file-name ".elpaca-" elpaca-sources-directory) t))
         (default-directory (file-name-as-directory staging))
         (buffer (get-buffer-create "*elpaca-bootstrap*"))
         (order (cdr elpaca-order)))
    (with-current-buffer buffer (erase-buffer))
    (message "Installing Elpaca...")
    (unwind-protect
        (condition-case err
            (progn
              (yunge-elpaca-bootstrap--run buffer "git" "clone"
                                          (plist-get order :repo) ".")
              (yunge-elpaca-bootstrap--run buffer "git" "checkout" (plist-get order :ref))
              (yunge-elpaca-bootstrap--run
               buffer (expand-file-name invocation-name invocation-directory)
               "--batch" "-Q" "-L" "." "-l" "elpaca" "--eval"
               "(elpaca-generate-autoloads \"elpaca\" default-directory)")
              (rename-file staging (directory-file-name repo))
              (message "Installed Elpaca"))
          (error
           (display-buffer buffer)
           (error "Elpaca installation failed: %s; see *elpaca-bootstrap* and restart to retry"
                  (error-message-string err))))
      (when (file-exists-p staging)
        (delete-directory staging t)))))

(let* ((repo (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory)))
  (unless (file-exists-p repo)
    (yunge-elpaca-bootstrap--install repo))
  (unless (file-readable-p (expand-file-name "elpaca.el" repo))
    (error "Incomplete Elpaca source in %s; move it aside and restart to reinstall" repo))
  (let* ((directory
          (if (and (file-readable-p (expand-file-name "elpaca.el" build))
                   (file-readable-p (expand-file-name "elpaca-autoloads.el" build)))
              build
            repo))
         (autoloads (expand-file-name "elpaca-autoloads.el" directory)))
    (add-to-list 'load-path directory)
    (unless (file-readable-p autoloads)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" directory))
    (load autoloads nil 'nomessage)))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(when (eq system-type 'windows-nt)
  ;; Avoid requiring symlink privileges on Windows.
  (elpaca-no-symlink-mode 1))

(provide 'yunge-elpaca-bootstrap)

;;; yunge-elpaca-bootstrap.el ends here
