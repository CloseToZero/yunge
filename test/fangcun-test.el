;;; fangcun-test.el --- Fangcun tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
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

(defun fangcun-test--node (id nodes)
  "Return the node named ID from NODES."
  (seq-find
   (lambda (node)
     (equal (fangcun-node-id node) id))
   nodes))

(ert-deftest fangcun-syncs-multiple-yiyus ()
  (fangcun-test-with-notes
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 2 :nodes 4)))
    (let* ((nodes (fangcun-node-list))
           (personal (fangcun-test--node
                      "personal-file" nodes))
           (theorem (fangcun-test--node "theorem" nodes))
           (untitled (fangcun-test--node
                      "untitled-heading" nodes))
           (work (fangcun-test--node "work-file" nodes)))
      (should (= (length nodes) 4))
      (should (equal (fangcun-node-title personal)
                     "Personal Notes"))
      (should (equal (fangcun-node-title theorem) "A theorem"))
      (should (equal (fangcun-node-title untitled)
                     "untitled-heading"))
      (should (equal (fangcun-node-yiyu-id work) "work"))
      (should (equal (fangcun-node-yiyu-name work) "Work"))
      (should
       (equal
        (fangcun-node-title work)
        (file-name-sans-extension
         (file-relative-name work-file work-root)))))))

(ert-deftest fangcun-parsing-inhibits-org-startup ()
  (fangcun-test-with-notes
    (let ((original-org-mode (symbol-function 'org-mode))
          called)
      (cl-letf
          (((symbol-function 'org-mode)
            (lambda (&rest arguments)
              (setq called t)
              (should org-inhibit-startup)
              (apply original-org-mode arguments))))
        (fangcun-db-sync))
      (should called))))

(ert-deftest fangcun-sync-uses-nonvisiting-buffers-for-disk-files ()
  (fangcun-test-with-notes
    (let ((original-collector
           (symbol-function
            'fangcun--collect-nodes-from-buffer))
          collected)
      (cl-letf
          (((symbol-function
             'fangcun--collect-nodes-from-buffer)
            (lambda (buffer &rest arguments)
              (setq collected t)
              (should-not (buffer-file-name buffer))
              (apply original-collector buffer arguments))))
        (fangcun-db-sync))
      (should collected))))

(ert-deftest fangcun-sync-reuses-matching-visited-buffer ()
  (fangcun-test-with-notes
    (let ((buffer (find-file-noselect personal-file))
          (original-collector
           (symbol-function
            'fangcun--collect-nodes-from-buffer))
          reused)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "A theorem")
        (org-back-to-heading)
        (narrow-to-region (point) (point-max))
        (let ((saved-point (point))
              (saved-min (point-min))
              (saved-max (point-max)))
          (cl-letf
              (((symbol-function
                 'fangcun--collect-nodes-from-buffer)
                (lambda (&rest arguments)
                  (when (eq (car arguments) buffer)
                    (setq reused t))
                  (apply original-collector arguments))))
            (should
             (equal (fangcun-db-sync)
                    '(:yiyus 2 :files 2 :nodes 4))))
          (should reused)
          (should (= (point) saved-point))
          (should (= (point-min) saved-min))
          (should (= (point-max) saved-max)))))))

(ert-deftest fangcun-sync-ignores-unsaved-buffer-changes ()
  (fangcun-test-with-notes
    (let ((buffer (find-file-noselect personal-file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert
               (concat
                "\n* Unsaved node\n"
                ":PROPERTIES:\n"
                ":ID: fangcun-test-unsaved\n"
                ":END:\n")))
            (should (eq (find-buffer-visiting personal-file) buffer))
            (should (buffer-modified-p buffer))
            (should-not
             (with-current-buffer buffer
               (and (derived-mode-p 'org-mode)
                    (not (buffer-modified-p))
                    (verify-visited-file-modtime buffer))))
            (should
             (equal (fangcun-db-sync)
                    '(:yiyus 2 :files 2 :nodes 4)))
            (should-not
             (fangcun-test--node
              "fangcun-test-unsaved" (fangcun-node-list))))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))))))

(ert-deftest fangcun-sync-reads-disk-when-visited-buffer-is-stale ()
  (fangcun-test-with-notes
    (let ((buffer (find-file-noselect personal-file)))
      (fangcun-test--write-file
       personal-file
       (concat
        ":PROPERTIES:\n"
        ":ID: personal-file\n"
        ":END:\n"
        "#+title: Changed on disk\n\n"
        "* Disk node\n"
        ":PROPERTIES:\n"
        ":ID: fangcun-test-disk-node\n"
        ":END:\n"))
      (set-file-times personal-file
                      (time-add (current-time) 2))
      (should (eq (find-buffer-visiting personal-file) buffer))
      (should-not (verify-visited-file-modtime buffer))
      (should-not
       (with-current-buffer buffer
         (and (derived-mode-p 'org-mode)
              (not (buffer-modified-p))
              (verify-visited-file-modtime buffer))))
      (should
       (equal (fangcun-db-sync)
              '(:yiyus 2 :files 2 :nodes 3)))
      (let ((nodes (fangcun-node-list)))
        (should
         (equal
          (fangcun-node-title
           (fangcun-test--node "personal-file" nodes))
          "Changed on disk"))
        (should
         (fangcun-test--node "fangcun-test-disk-node" nodes))
        (should-not (fangcun-test--node "theorem" nodes))))))

(ert-deftest fangcun-sync-rebuilds-database ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (fangcun--call-with-database
     (lambda (database)
       (sqlite-execute database "CREATE TABLE obsolete (value)")))
    (delete-file personal-file)
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 1 :nodes 1)))
    (should-not
     (fangcun--call-with-database
      (lambda (database)
        (sqlite-select
         database
         (concat
          "SELECT name FROM sqlite_master "
          "WHERE type = 'table' AND name = 'obsolete'")))))
    (should
     (equal (mapcar #'fangcun-node-id (fangcun-node-list))
            '("work-file")))))

(ert-deftest fangcun-find-shows-owner-and-locates-id ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (fangcun-test--write-file
     personal-file
     (concat
      "A line inserted after synchronization.\n"
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":END:\n"
      "#+title: Personal Notes\n\n"
      "* A theorem\n"
      ":PROPERTIES:\n"
      ":ID: theorem\n"
      ":END:\n"))
    (let (annotation-function candidate)
      (cl-letf
          (((symbol-function 'completing-read)
            (lambda (_prompt collection &rest _arguments)
              (setq annotation-function
                    (plist-get completion-extra-properties
                               :annotation-function)
                    candidate
                    (seq-find
                     (lambda (item)
                       (string-prefix-p "A theorem" (car item)))
                     collection))
              (car candidate))))
        (let ((node (fangcun-node-find)))
          (should (equal (fangcun-node-id node) "theorem"))
          (should (equal (buffer-file-name) personal-file))
          (should (looking-at-p "\\* A theorem"))))
      (let ((annotation
             (funcall annotation-function (car candidate))))
        (should
         (equal (substring-no-properties annotation)
                "  Personal › theorems.org"))))))

(ert-deftest fangcun-candidates-hide-and-distinguish-node-ids ()
  (let* ((first
          (make-fangcun-node
           :id "first"
           :yiyu-name "Personal"
           :file "notes.org"
           :title "Shared title"))
         (second
          (make-fangcun-node
           :id "second"
           :yiyu-name "Work"
           :file "notes.org"
           :title "Shared title"))
         (first-candidate (fangcun--node-candidate first))
         (second-candidate (fangcun--node-candidate second))
         (title-length (length (fangcun-node-title first))))
    (should-not (equal first-candidate second-candidate))
    (should (equal (substring first-candidate 0 title-length)
                   "Shared title"))
    (should (get-text-property title-length
                               'invisible first-candidate))
    (should-not
     (string-match-p
      "first"
      (fangcun--node-annotation first-candidate)))))

(ert-deftest fangcun-visit-widens-narrowed-target-buffer ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let* ((node (fangcun-test--node
                  "theorem" (fangcun-node-list)))
           (buffer (find-file-noselect personal-file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (narrow-to-region (point-min) (line-end-position))
        (fangcun-node-visit node)
        (should-not (buffer-narrowed-p))
        (should (equal (org-id-get) "theorem"))))))

;;; fangcun-test.el ends here
