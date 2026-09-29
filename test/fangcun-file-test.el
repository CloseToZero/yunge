;;; fangcun-file-test.el --- Saved Fangcun file behavior -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'fangcun-test-helper)
(require 'fangcun-file)

(ert-deftest fangcun-file-locates-saved-text-with-an-unsaved-buffer ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((buffer (find-file-noselect personal-file))
          position beginning end)
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "* [[id:source][A theorem]]")
        (beginning-of-line)
        (insert "Unsaved text before the node.\n")
        (goto-char (point-min))
        (narrow-to-region (point-min) (line-end-position))
        (setq position (point)
              beginning (point-min)
              end (point-max)))
      (let ((location (fangcun-file-locate-node "theorem")))
        (should (equal (fangcun-node-id (plist-get location :node)) "theorem"))
        (should (file-equal-p (plist-get location :file) personal-file))
        (should (eq (plist-get location :kind) 'heading))
        (should (= (plist-get location :start-line) 6))
        (should (= (plist-get location :end-line) 10))
        (should (equal (plist-get location :outline-path) '("A theorem")))
        (should (plist-get location :modified-p)))
      (with-current-buffer buffer
        (should (= (point) position))
        (should (= (point-min) beginning))
        (should (= (point-max) end))
        (save-restriction
          (widen)
          (should (search-forward "Unsaved text before the node." nil t))))
      (with-temp-buffer
        (insert-file-contents personal-file)
        (should-not (search-forward "Unsaved text before the node." nil t))))))

(ert-deftest fangcun-file-refreshes-clean-buffers-but-refuses-unsaved-edits ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((buffer (find-file-noselect personal-file)))
      (with-temp-buffer
        (insert-file-contents personal-file)
        (goto-char (point-max))
        (insert "\n* Added outside Emacs\n")
        (write-region (point-min) (point-max) personal-file nil 'silent))
      (set-file-times personal-file
                      (time-add (current-time) (seconds-to-time 5)))
      (let ((node (fangcun-file-create-heading-node
                   "personal" "theorems.org" '("Added outside Emacs"))))
        (should (equal (fangcun-node-id (fangcun-node-from-id (fangcun-node-id node)))
                       (fangcun-node-id node)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^\\* Added outside Emacs$")
          (should (equal (org-entry-get (point) "ID")
                         (fangcun-node-id node)))
          (should-not (buffer-modified-p))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "Unsaved text.\n"))
      (with-temp-buffer
        (insert-file-contents personal-file)
        (goto-char (point-max))
        (insert "* Another external heading\n")
        (write-region (point-min) (point-max) personal-file nil 'silent))
      (set-file-times personal-file
                      (time-add (current-time) (seconds-to-time 5)))
      (should-error
       (fangcun-file-create-heading-node
        "personal" "theorems.org" '("Added outside Emacs"))
       :type 'user-error)
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should (string-suffix-p "Unsaved text.\n" (buffer-string))))
      (with-temp-buffer
        (insert-file-contents personal-file)
        (should-not (search-forward "Unsaved text." nil t))
        (goto-char (point-min))
        (should (search-forward "Another external heading" nil t))))))

(ert-deftest fangcun-file-never-overwrites-a-concurrent-creation ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let* ((file (expand-file-name "raced.org" personal-root))
           (write-region-annotate-functions
            (list
             (lambda (_start _end)
               (let ((write-region-annotate-functions nil)
                     (coding-system-for-write 'utf-8-unix))
                 (with-temp-file file
                   (insert "#+title: Another writer\n")))
               nil))))
      (should-error
       (fangcun-file-create-node "personal" "raced.org" "Proposed title"))
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string) "#+title: Another writer\n")))
      (should-not
       (seq-find
        (lambda (node) (equal (fangcun-node-title node) "Proposed title"))
        (fangcun-node-list))))))

(ert-deftest fangcun-file-keeps-saved-text-when-indexing-fails ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (with-temp-buffer
      (insert-file-contents personal-file)
      (goto-char (point-max))
      (insert (concat
               "\n* Conflicting heading\n:PROPERTIES:\n"
               ":ID: work-file\n:END:\n* New heading\n"))
      (write-region (point-min) (point-max) personal-file nil 'silent))
    (cl-letf (((symbol-function 'org-id-new)
               (lambda (&optional _prefix) "new-heading")))
      (let ((error-data
             (should-error
              (fangcun-file-create-heading-node
               "personal" "theorems.org" '("New heading"))
              :type 'user-error)))
        (should (string-match-p "Saved .*indexing failed"
                                (error-message-string error-data)))
        (should (string-match-p "fangcun-db-sync"
                                (error-message-string error-data)))))
    (with-temp-buffer
      (insert-file-contents personal-file)
      (org-mode)
      (should (search-forward "* New heading" nil t))
      (should (equal (org-entry-get (point) "ID") "new-heading")))
    (should-not (fangcun-node-from-id "new-heading"))
    (should (equal (fangcun-node-file (fangcun-node-from-id "work-file"))
                   "projects/status.org"))
    (with-temp-buffer
      (insert-file-contents personal-file)
      (goto-char (point-min))
      (search-forward "* Conflicting heading")
      (search-forward ":ID: work-file")
      (replace-match ":ID: repaired-heading" t t)
      (write-region (point-min) (point-max) personal-file nil 'silent))
    (fangcun-db-sync)
    (should (equal (fangcun-node-file (fangcun-node-from-id "repaired-heading"))
                   "theorems.org"))
    (should (equal (fangcun-node-file (fangcun-node-from-id "new-heading"))
                   "theorems.org"))))

(provide 'fangcun-file-test)

;;; fangcun-file-test.el ends here
