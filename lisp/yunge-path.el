;;; yunge-path.el --- Paths associated with buffers -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

(defvar yunge-path-buffer-path-functions nil
  "Functions that return the current buffer's associated path.
Each function takes no arguments and returns a file or directory name,
or nil when it does not handle this buffer.  Directory names must end
in a directory separator.  The first non-nil result is used.")

(defun yunge-path--buffer-path ()
  "Return the absolute file or directory path associated with this buffer.
Indirect buffers use their base buffer's path.  Terminal and compilation
buffers use their recorded working directory."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((path
           (or (run-hook-with-args-until-success
                'yunge-path-buffer-path-functions)
               buffer-file-name
               (when (derived-mode-p 'comint-mode 'eshell-mode 'term-mode
                                     'vterm-mode 'ghostel-mode 'compilation-mode)
                 (file-name-as-directory default-directory)))))
      (unless path
        (user-error "This buffer has no associated file or directory"))
      (expand-file-name path))))

(defun yunge-copy-buffer-absolute-path ()
  "Copy the absolute path associated with the current buffer.
File buffers use their file.  Dired uses its opened directory, Magit
views use their repository root, and terminals use their working directory."
  (interactive)
  (let ((path (yunge-path--buffer-path)))
    (kill-new path)
    (message "Copied buffer path: %s" path)))

(defun yunge-copy-buffer-project-path ()
  "Copy the current buffer's associated path relative to its project root.
Use the same target as `yunge-copy-buffer-absolute-path'.  The project root
itself is represented by \".\".  Signal an error if no project is found."
  (interactive)
  (let* ((path (yunge-path--buffer-path))
         (project (project-current nil (file-name-directory path))))
    (unless project
      (user-error "This buffer's path is not in a project"))
    (let ((relative (file-relative-name path (project-root project))))
      (when (equal relative "./")
        (setq relative "."))
      (kill-new relative)
      (message "Copied project path: %s" relative))))

(provide 'yunge-path)

;;; yunge-path.el ends here
