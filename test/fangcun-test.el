;;; fangcun-test.el --- Fangcun tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'fangcun)

(declare-function yunge-jump-history--track-navigation
                  "yunge-jump-history")
(declare-function yunge-jump-history-backward "yunge-jump-history")
(declare-function yunge-jump-history-forward "yunge-jump-history")

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

(ert-deftest fangcun-database-file-requires-an-absolute-path ()
  (let ((symbol (make-symbol "fangcun-test-database-file"))
        (file
         (expand-file-name "fangcun.sqlite" temporary-file-directory)))
    (should-error
     (fangcun--set-database-file symbol "fangcun.sqlite"))
    (fangcun--set-database-file symbol file)
    (should (equal (default-value symbol) file))
    (should
     (eq (get 'fangcun-database-file 'custom-set)
         #'fangcun--set-database-file))))

(ert-deftest fangcun-creates-schema-only-for-a-new-database ()
  (let* ((root (make-temp-file "fangcun-database-test-" t))
         (fangcun-database-file
          (expand-file-name "state/fangcun.sqlite" root)))
    (unwind-protect
        (progn
          (should-not (fangcun-node-list))
          (should (file-exists-p fangcun-database-file))
          (should-not (fangcun-node-list)))
      (delete-directory root t))))

(ert-deftest fangcun-syncs-multiple-yiyus ()
  (fangcun-test-with-notes
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 2 :nodes 4 :links 1)))
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
                    '(:yiyus 2 :files 2 :nodes 4 :links 1))))
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
                    '(:yiyus 2 :files 2 :nodes 4 :links 1)))
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
              '(:yiyus 2 :files 2 :nodes 3 :links 0)))
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
            '(:yiyus 2 :files 1 :nodes 1 :links 0)))
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

(ert-deftest fangcun-updates-one-file-and-its-outgoing-links ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     work-file
     (concat
      ":PROPERTIES:\n"
      ":ID: work-file\n"
      ":END:\n"
      "[[id:theorem][Incoming link]]\n"))
    (fangcun-db-sync)
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":END:\n"
      "#+title: Updated Personal Notes\n\n"
      "* Replacement\n"
      ":PROPERTIES:\n"
      ":ID: replacement\n"
      ":END:\n"
      "[[id:work-file][New outgoing link]]\n"))
    (should
     (equal (fangcun-db-update-file personal-file)
            '(:nodes 2 :links 1)))
    (let ((nodes (fangcun-node-list)))
      (should
       (equal
        (sort (mapcar #'fangcun-node-id nodes) #'string-lessp)
        '("personal-file" "replacement" "work-file")))
      (should
       (equal
        (fangcun-node-title
         (fangcun-test--node "personal-file" nodes))
        "Updated Personal Notes")))
    (fangcun--call-with-database
     (lambda (database)
       (should
        (equal
         (mapcar
          (lambda (row)
            (list (elt row 0) (elt row 1)))
          (sqlite-select
           database
           (concat
            "SELECT source_id, target_id FROM links "
            "ORDER BY source_id, target_id")))
         '(("replacement" "work-file")
           ("work-file" "theorem"))))))))

(ert-deftest fangcun-update-requires-a-saved-buffer ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((buffer (find-file-noselect personal-file)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "Unsaved"))
            (should-error
             (fangcun-db-update-file personal-file)
             :type 'user-error)
            (should (= (length (fangcun-node-list)) 4)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))))))

(ert-deftest fangcun-update-requires-an-initial-full-sync ()
  (fangcun-test-with-notes
    (should-error
     (fangcun-db-update-file personal-file)
     :type 'user-error)
    (should-not (file-exists-p fangcun-database-file))))

(ert-deftest fangcun-save-hook-waits-for-an-initial-full-sync ()
  (fangcun-test-with-notes
    (let ((fangcun-db-update-on-save t)
          (buffer (find-file-noselect personal-file)))
      (with-current-buffer buffer
        (should (memq #'fangcun--update-after-save after-save-hook))
        (goto-char (point-max))
        (insert "\nSaved before syncing.\n")
        (save-buffer))
      (should-not (file-exists-p fangcun-database-file)))))

(ert-deftest fangcun-save-hook-updates-managed-files-silently ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((fangcun-db-update-on-save t)
          (buffer (find-file-noselect personal-file))
          messages)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "A theorem")
        (replace-match "Updated after saving")
        (cl-letf
            (((symbol-function 'message)
              (lambda (format-string &rest arguments)
                (push (apply #'format-message
                             format-string arguments)
                      messages))))
          (save-buffer)))
      (should-not
       (seq-some
        (lambda (text)
          (string-prefix-p "Fangcun indexed" text))
        messages))
      (should
       (equal
        (fangcun-node-title
         (fangcun-test--node "theorem" (fangcun-node-list)))
        "Updated after saving"))
      (let ((fangcun-db-update-on-save nil))
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "Updated after saving")
          (replace-match "Automatic updates disabled")
          (save-buffer)))
      (should
       (equal
        (fangcun-node-title
         (fangcun-test--node "theorem" (fangcun-node-list)))
        "Updated after saving")))))

(ert-deftest fangcun-syncs-id-link-occurrences-and-owners ()
  (fangcun-test-with-notes
    (let ((source-file
           (expand-file-name "references.org" personal-root))
          (unowned-file
           (expand-file-name "unowned.org" personal-root)))
      (fangcun-test--write-file
       source-file
       (concat
        ":PROPERTIES:\n"
        ":ID: source-file\n"
        ":END:\n"
        "#+title: Source file\n\n"
        "[[id:work-file][File link]]\n\n"
        "* Parent\n"
        ":PROPERTIES:\n"
        ":ID: parent\n"
        ":END:\n"
        "** Child without an ID\n"
        "[[id:work-file][First child link]]\n"
        "[[id:work-file][Second child link]]\n"))
      (fangcun-test--write-file
       unowned-file
       "* No ID\n[[id:work-file][Unowned link]]\n")
      (should
       (equal (fangcun-db-sync)
              '(:yiyus 2 :files 4 :nodes 6 :links 4)))
      (fangcun--call-with-database
       (lambda (database)
         (let ((rows
                (sqlite-select
                 database
                 (concat
                  "SELECT source_id, target_id, position "
                  "FROM links WHERE target_id = 'work-file' "
                  "ORDER BY position"))))
           (should
            (equal (mapcar
                    (lambda (row)
                      (list (elt row 0) (elt row 1)))
                    rows)
                   '(("source-file" "work-file")
                     ("parent" "work-file")
                     ("parent" "work-file"))))
           (should
            (equal (mapcar (lambda (row) (elt row 2)) rows)
                   (sort
                    (mapcar (lambda (row) (elt row 2)) rows)
                    #'<))))
         (should
          (equal
           (mapcar
            (lambda (row)
              (list (elt row 0) (elt row 1)))
            (sqlite-select
             database
             (concat
              "SELECT source_id, target_id FROM links "
              "WHERE target_id = 'source'")))
           '(("theorem" "source"))))
         (should
          (sqlite-select
           database
           (concat
            "SELECT name FROM sqlite_master "
            "WHERE type = 'index' AND name = 'links_target_id'"))))))))

(ert-deftest fangcun-finds-a-backlink-at-its-first-occurrence ()
  (fangcun-test-with-notes
    (let ((source-file
           (expand-file-name "references.org" personal-root)))
      (fangcun-test--write-file
       source-file
       (concat
        ":PROPERTIES:\n"
        ":ID: source-file\n"
        ":END:\n"
        "#+title: Source file\n\n"
        "* Parent\n"
        ":PROPERTIES:\n"
        ":ID: parent\n"
        ":END:\n"
        "[[id:work-file][First reference]]\n"
        "[[id:work-file][Second reference]]\n"))
      (fangcun-db-sync)
      (let ((backlinks (fangcun-backlink-list "work-file")))
        (should (= (length backlinks) 1))
        (should
         (equal
          (fangcun-node-id
           (fangcun-backlink-node (car backlinks)))
          "parent")))
      (find-file work-file)
      (goto-char (point-max))
      (cl-letf
          (((symbol-function 'completing-read)
            (lambda (_prompt collection &rest _arguments)
              (caar collection))))
        (let ((backlink (fangcun-backlink-find)))
          (should
           (equal
            (fangcun-node-id
             (fangcun-backlink-node backlink))
            "parent"))
          (should (equal (buffer-file-name) source-file))
          (should
           (looking-at-p
            "\\[\\[id:work-file\\]\\[First reference\\]\\]")))))))

(ert-deftest fangcun-jumps-participate-in-window-history ()
  (dolist (command '(fangcun-node-find fangcun-backlink-find))
    (should
     (advice-member-p
      #'yunge-jump-history--track-navigation command)))
  (yunge-test-enable-evil)
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((origin (generate-new-buffer " *fangcun-jump-origin*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer origin)
            (insert "0123456789")
            (goto-char 4)
            (set-window-parameter nil 'yunge-jump-history nil)

            (let (cancelled)
              (cl-letf
                  (((symbol-function 'completing-read)
                    (lambda (&rest _arguments)
                      (signal 'quit nil))))
                (condition-case nil
                    (fangcun-node-find)
                  (quit (setq cancelled t))))
              (should cancelled))
            (should (eq (current-buffer) origin))
            (should (= (point) 4))
            (should-error
             (yunge-jump-history-backward) :type 'user-error)

            (set-window-parameter nil 'yunge-jump-history nil)
            (cl-letf
                (((symbol-function 'completing-read)
                  (lambda (_prompt collection &rest _arguments)
                    (car
                     (seq-find
                      (lambda (item)
                        (string-prefix-p "A theorem" (car item)))
                      collection)))))
              (fangcun-node-find))
            (should (equal (buffer-file-name) personal-file))
            (should (equal (org-id-get) "theorem"))

            (yunge-jump-history-backward)
            (should (eq (current-buffer) origin))
            (should (= (point) 4))

            (yunge-jump-history-forward)
            (should (equal (buffer-file-name) personal-file))
            (should (equal (org-id-get) "theorem")))
        (when (buffer-live-p origin)
          (kill-buffer origin))
        (set-window-parameter nil 'yunge-jump-history nil)))))

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

(ert-deftest fangcun-inserts-id-link-with-node-title ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (with-temp-buffer
      (org-mode)
      (cl-letf
          (((symbol-function 'completing-read)
            (lambda (_prompt collection &rest _arguments)
              (car
               (seq-find
                (lambda (item)
                  (string-prefix-p "A theorem" (car item)))
                collection)))))
        (let ((node (fangcun-node-insert)))
          (should (equal (fangcun-node-id node) "theorem"))
          (should
           (equal (buffer-string)
                  "[[id:theorem][A theorem]]")))))))

(ert-deftest fangcun-insert-requires-org-mode ()
  (with-temp-buffer
    (should-error (fangcun-node-insert) :type 'user-error)))

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
