;;; yunge-edit-test.el --- Editing default tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-edit)
(require 'replace)

(ert-deftest yunge-edit-result-session-saves-touched-source-buffers ()
  (let* ((file (make-temp-file "yunge-edit-"))
         (source (find-file-noselect file))
         (marker (with-current-buffer source
                   (goto-char (point-min))
                   (point-marker)))
         finished)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "changed"))
          (with-temp-buffer
            (insert "result")
            (add-text-properties
             (point-min) (point-max)
             `(occur-target ((,marker . ,marker))))
            (yunge-edit-setup-result-session
             (lambda () (setq finished t)))
            (goto-char (point-max))
            (insert "!")
            (yunge-edit-finish-result-session))
          (should finished)
          (should-not (buffer-modified-p source))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "changed"))))
      (when (buffer-live-p source)
        (kill-buffer source))
      (delete-file file))))

(ert-deftest yunge-edit-result-session-does-not-save-untouched-buffers ()
  (let* ((file (make-temp-file "yunge-edit-"))
         (source (find-file-noselect file)))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "changed"))
          (with-temp-buffer
            (yunge-edit-setup-result-session #'ignore)
            (yunge-edit-finish-result-session))
          (should (buffer-modified-p source)))
      (when (buffer-live-p source)
        (with-current-buffer source
          (set-buffer-modified-p nil))
        (kill-buffer source))
      (delete-file file))))

(ert-deftest yunge-edit-result-session-refuses-a-false-abort ()
  (should-error (yunge-edit-refuse-result-abort) :type 'user-error))

;;; yunge-edit-test.el ends here
