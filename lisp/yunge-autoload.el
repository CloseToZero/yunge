;;; yunge-autoload.el --- Generated autoload cache -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'subr-x)
(require 'yunge-state)

(defvar autoload-compute-prefixes)

(declare-function loaddefs-generate "loaddefs-gen"
                  (dirs output-file &optional excluded-files
                        extra-data include-package-version generate-full))

(defvar yunge-autoload-source-directory
  (expand-file-name "lisp/" user-emacs-directory)
  "Directory scanned when generating configuration autoloads.")

(defvar yunge-autoload-cache-directory
  (expand-file-name "autoload/" yunge-var-directory)
  "Directory containing generated configuration autoloads.")

(defvar yunge-autoload-loaddefs-file
  (expand-file-name "yunge-loaddefs.el"
                    yunge-autoload-cache-directory)
  "Generated configuration autoload file.")

(defvar yunge-autoload-repository-hash-file
  (expand-file-name "autoloads.sha256" user-emacs-directory)
  "Tracked hash of the current configuration autoloads.")

(defvar yunge-autoload-cache-hash-file
  (expand-file-name "yunge-loaddefs.sha256"
                    yunge-autoload-cache-directory)
  "Hash recorded when the local autoload cache was generated.")

(defun yunge-autoload--read-hash (file)
  "Return the hash stored in FILE, or nil when FILE does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (string-trim (buffer-string)))))

(defun yunge-autoload--loaddefs-hash (file)
  "Return a SHA-256 digest of the Lisp forms in loaddefs FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((trailer
           `(provide ',(intern (file-name-base file))))
          forms)
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (unless (equal form trailer)
                (push form forms))))
        (end-of-file))
      (secure-hash
       'sha256
       (encode-coding-string
        (prin1-to-string (nreverse forms)) 'utf-8-unix)))))

(defun yunge-autoload--generate-file (file)
  "Generate configuration autoloads in FILE."
  (require 'loaddefs-gen)
  (make-directory (file-name-directory file) t)
  (let ((autoload-compute-prefixes nil)
        (coding-system-for-write 'utf-8-unix)
        (inhibit-message t))
    (loaddefs-generate
     yunge-autoload-source-directory file nil nil nil t)))

(defun yunge-autoload--write-hash (file hash)
  "Write HASH to FILE."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert hash "\n"))))

(defun yunge-autoload--generate-cache ()
  "Generate the local autoload cache and return its hash."
  (yunge-autoload--generate-file yunge-autoload-loaddefs-file)
  (let ((hash
         (yunge-autoload--loaddefs-hash
          yunge-autoload-loaddefs-file)))
    (yunge-autoload--write-hash yunge-autoload-cache-hash-file hash)
    hash))

(defun yunge-autoload-load ()
  "Load cached configuration autoloads and warn when they are stale."
  (unless (file-exists-p yunge-autoload-loaddefs-file)
    (yunge-autoload--generate-cache)
    (message "Generated initial configuration autoload cache"))
  (let ((repository-hash
         (yunge-autoload--read-hash
          yunge-autoload-repository-hash-file))
        (cache-hash
         (yunge-autoload--read-hash
          yunge-autoload-cache-hash-file))
        (loaddefs-exists
         (file-exists-p yunge-autoload-loaddefs-file)))
    (when loaddefs-exists
      (add-to-list 'load-path yunge-autoload-cache-directory)
      (load yunge-autoload-loaddefs-file nil 'nomessage))
    (unless (and loaddefs-exists
                 repository-hash
                 (equal repository-hash cache-hash))
      (display-warning
       'yunge-autoload
       (concat
        "The configuration autoload cache is missing or stale; "
        "run M-x yunge-autoload-generate")
       :warning))))

(defun yunge-autoload-generate ()
  "Generate the configuration autoload cache and update its hashes."
  (interactive)
  (let ((hash (yunge-autoload--generate-cache)))
    (yunge-autoload--write-hash
     yunge-autoload-repository-hash-file hash))
  (yunge-autoload-load)
  (message "Generated configuration autoloads"))

(provide 'yunge-autoload)

;;; yunge-autoload.el ends here
