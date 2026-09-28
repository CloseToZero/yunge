;;; fangcun-test-helper.el --- Fangcun test notes -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'fangcun)

(defun fangcun-test--write-file (file contents)
  "Write CONTENTS to FILE, creating its parent directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert contents)))

(defun fangcun-test--kill-buffers-below (directory)
  "Kill buffers visiting files below DIRECTORY."
  (dolist (buffer (buffer-list))
    (when-let* ((file (buffer-file-name buffer)))
      (when (file-in-directory-p file directory)
        (kill-buffer buffer)))))

(defmacro fangcun-test-with-notes (&rest body)
  "Evaluate BODY with two temporary Fangcun yiyu roots."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "fangcun-test-" t))
          (personal-root (expand-file-name "personal/" root))
          (work-root (expand-file-name "work/" root))
          (personal-file
           (expand-file-name "theorems.org" personal-root))
          (work-file
           (expand-file-name "projects/status.org" work-root))
          (fangcun-yiyus
           `((personal :name "Personal" :root ,personal-root)
             (work :name "Work" :root ,work-root)))
          (fangcun-native-helper-enabled nil)
          (fangcun--session-active-p nil)
          (fangcun--session-yiyus nil)
          (fangcun--native-build-process nil)
          (fangcun--native-watch-process nil)
          (fangcun--native-event-timer nil)
          (fangcun--native-pending-files
           (make-hash-table :test #'equal))
          (fangcun--native-pending-full-sync-p nil)
          (fangcun-database-file
           (expand-file-name "fangcun.sqlite" root)))
     (unwind-protect
         (progn
           (fangcun-test--write-file
            personal-file
            (concat
             ":PROPERTIES:\n"
             ":ID: personal-file\n"
             ":END:\n"
             "#+title: [[https://example.com][Personal Notes]]\n\n"
             "* [[id:source][A theorem]]\n"
             ":PROPERTIES:\n"
             ":ID: theorem\n"
             ":END:\n\n"
             "* \n"
             ":PROPERTIES:\n"
             ":ID: untitled-heading\n"
             ":END:\n"))
           (fangcun-test--write-file
            work-file
            (concat
             ":PROPERTIES:\n"
             ":ID: work-file\n"
             ":END:\n"))
           ,@body)
       (fangcun-test--kill-buffers-below root)
       (delete-directory root t))))

(provide 'fangcun-test-helper)

;;; fangcun-test-helper.el ends here
