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
  (should
   (equal fangcun-database-file
          (expand-file-name "fangcun/fangcun.sqlite"
                            yunge-var-directory)))
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

(ert-deftest fangcun-detects-nonportable-file-names ()
  (dolist (name
           '("colon:name.org"
             "question?.org"
             "back\\slash.org"
             " leading.org"
             "trailing.org "
             "trailing."
             "NUL.org"
             "com1.notes.org"))
    (should (fangcun--portable-file-name-error name)))
  (should
   (fangcun--portable-file-name-error
    (concat "control" (string 1) ".org")))
  (dolist (name
           '("中文笔记.org" "C++.org" "two words.org" ".NET.org"))
    (should-not
     (fangcun--portable-file-name-error name))))

(ert-deftest fangcun-validates-new-file-boundaries ()
  (fangcun-test-with-notes
    (should-not
     (fangcun--new-file-name-error "new.org" personal-root))
    (dolist (name
             '("theorems.org"
               "new.txt"
               "missing/new.org"
               "../outside.org"))
      (should
       (fangcun--new-file-name-error name personal-root)))))

(ert-deftest fangcun-creates-an-unsaved-file-node ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((origin (find-file-noselect personal-file))
          (target (expand-file-name "c-cpp.org" personal-root))
          (org-id-locations nil)
          (org-startup-folded 'showeverything)
          (configured-function
           (symbol-function 'fangcun--configured-yiyus))
          (configured-calls 0)
          prompted-initial)
      (switch-to-buffer origin)
      (cl-letf
          (((symbol-function 'fangcun--configured-yiyus)
            (lambda ()
              (cl-incf configured-calls)
              (funcall configured-function)))
           ((symbol-function 'read-string)
            (lambda (prompt &optional initial &rest _arguments)
              (if (string-prefix-p "Node title" prompt)
                  "C/C++"
                (setq prompted-initial initial)
                "c-cpp.org")))
           ((symbol-function 'completing-read)
            (lambda (&rest _arguments)
              (ert-fail "Current yiyu should be selected automatically")))
           ((symbol-function 'org-id-new)
            (lambda (&optional _prefix) "created-node"))
           ((symbol-function 'org-id-locations-load)
            (lambda ()
              (ert-fail "Creating a node should not load Org ID state"))))
        (should (equal (fangcun-file-node-create) "created-node")))
      (should (= configured-calls 1))
      (should (equal prompted-initial "C/C++.org"))
      (should (equal (buffer-file-name) target))
      (should (buffer-modified-p))
      (should-not (file-exists-p target))
      (goto-char (point-min))
      (should (equal (org-id-get) "created-node"))
      (re-search-forward "^:ID:")
      (should-not (org-invisible-p (match-beginning 0)))
      (should
       (equal (cdr (assoc "TITLE" (org-collect-keywords '("title"))))
              '("C/C++")))
      (save-buffer)
      (let ((node
             (fangcun-test--node
              "created-node" (fangcun-node-list))))
        (should node)
        (should (equal (fangcun-node-title node) "C/C++"))
        (should (equal (fangcun-node-file node) "c-cpp.org"))))))

(ert-deftest fangcun-create-selects-yiyu-and-reprompts-invalid-name ()
  (fangcun-test-with-notes
    (let ((target (expand-file-name "status.org" work-root))
          (answers '("status.txt" "status.org"))
          (org-id-locations nil)
          prompts initials created-buffer)
      (with-temp-buffer
        (cl-letf
            (((symbol-function 'completing-read)
              (lambda (_prompt collection &rest _arguments)
                (should (assoc "Work" collection))
                "Work"))
             ((symbol-function 'read-string)
              (lambda (prompt &optional initial &rest _arguments)
                (if (string-prefix-p "Node title" prompt)
                    "Status"
                  (push prompt prompts)
                  (push initial initials)
                  (pop answers))))
             ((symbol-function 'org-id-new)
              (lambda (&optional _prefix) "work-status"))
             ((symbol-function 'org-id-locations-load)
              (lambda ()
                (ert-fail "Creating a node should not load Org ID state"))))
          (should (equal (fangcun-file-node-create) "work-status"))
          (setq created-buffer (current-buffer))))
      (should-not answers)
      (should (equal (nreverse initials)
                     '("Status.org" "status.txt")))
      (should
       (string-match-p "must end with \\.org"
                       (car prompts)))
      (should
       (equal (buffer-file-name created-buffer) target))
      (should-not (file-exists-p target)))))

(ert-deftest fangcun-resolves-org-ids-through-its-database ()
  (should (advice-member-p #'fangcun--id-find 'org-id-find))
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((org-id-locations nil))
      (cl-letf
          (((symbol-function 'org-id-locations-load)
            (lambda ()
              (ert-fail "Fangcun IDs should not load Org ID state"))))
        (let ((location (org-id-find "theorem")))
          (should (file-equal-p (car location) personal-file))
          (with-current-buffer (find-file-noselect personal-file)
            (goto-char (cdr location))
            (should (equal (org-id-get) "theorem"))))
        (let ((marker (org-id-find "theorem" t)))
          (unwind-protect
              (progn
                (should (markerp marker))
                (should
                 (file-equal-p
                  (buffer-file-name (marker-buffer marker))
                  personal-file))
                (with-current-buffer (marker-buffer marker)
                  (goto-char marker)
                  (should (equal (org-id-get) "theorem"))))
            (move-marker marker nil)))))))

(ert-deftest fangcun-lets-org-resolve-ids-outside-its-database ()
  (fangcun-test-with-notes
    (let ((outside-file (expand-file-name "outside.org" root))
          (org-id-locations (make-hash-table :test #'equal)))
      (fangcun-test--write-file
       outside-file
       ":PROPERTIES:\n:ID: outside\n:END:\n")
      (puthash "outside" outside-file org-id-locations)
      (should-not (file-exists-p fangcun-database-file))
      (let ((location (org-id-find "outside")))
        (should (file-equal-p (car location) outside-file)))
      (should-not (file-exists-p fangcun-database-file)))))

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

(ert-deftest fangcun-compares-yiyus-with-one-ordering ()
  (let ((yiyus
         (list
          (make-fangcun-yiyu
           :id "personal" :name "Personal" :root "personal/")
          (make-fangcun-yiyu
           :id "work" :name "Work" :root "work/"))))
    (cl-letf
        (((symbol-function 'sqlite-select)
          (lambda (_database _query)
            '(("work" "Work" "work/")
              ("personal" "Personal" "personal/")))))
      (should (fangcun--database-yiyus-match-p nil yiyus)))))

(ert-deftest fangcun-syncs-multiple-yiyus ()
  (fangcun-test-with-notes
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 2 :nodes 4 :aliases 0 :links 1)))
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

(ert-deftest fangcun-records-files-without-nodes ()
  (fangcun-test-with-notes
    (let ((empty-file
           (expand-file-name "empty.org" personal-root)))
      (fangcun-test--write-file empty-file "")
      (should
       (equal (fangcun-db-sync)
              '(:yiyus 2 :files 3 :nodes 4 :aliases 0 :links 1)))
      (fangcun--call-with-database
       (lambda (database)
         (let ((rows
                (sqlite-select
                 database
                 (concat
                  "SELECT yiyu_id, file, mtime, size FROM files "
                  "ORDER BY yiyu_id, file"))))
           (should (= (length rows) 3))
           (let ((row
                  (seq-find
                   (lambda (candidate)
                     (equal (elt candidate 1) "empty.org"))
                   rows)))
             (should (equal (elt row 0) "personal"))
             (should (numberp (elt row 2)))
             (should (= (elt row 3) 0)))))))))

(ert-deftest fangcun-file-owns-nodes-aliases-and-outgoing-links ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":ALIASES: Personal\n"
      ":END:\n"
      "[[id:work-file][Outgoing]]\n"))
    (fangcun-test--write-file
     work-file
     (concat
      ":PROPERTIES:\n"
      ":ID: work-file\n"
      ":END:\n"
      "[[id:personal-file][Incoming]]\n"))
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 2 :nodes 2 :aliases 1 :links 2)))
    (fangcun--call-with-database
     (lambda (database)
       (sqlite-execute
        database
        (concat
         "DELETE FROM files "
         "WHERE yiyu_id = ? AND file = ?")
        (vector "personal" "theorems.org"))
       (should
        (equal
         (sqlite-select database "SELECT id FROM nodes")
         '(("work-file"))))
       (should-not (sqlite-select database "SELECT alias FROM aliases"))
       (should
        (equal
         (sqlite-select
          database
          "SELECT source_id, target_id FROM links")
         '(("work-file" "personal-file"))))))))

(ert-deftest fangcun-indexes-and-replaces-node-aliases ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":ALIASES: PN \"Personal knowledge\" PN\n"
      ":END:\n"
      "#+title: Personal Notes\n\n"
      "* Fixed-point theorem\n"
      ":PROPERTIES:\n"
      ":ID: theorem\n"
      ":ALIASES: FPT \"Fixed point theorem\"\n"
      ":END:\n"))
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 2 :nodes 3 :aliases 4 :links 0)))
    (let ((personal
           (fangcun-test--node "personal-file" (fangcun-node-list))))
      (should
       (equal (fangcun-node-aliases personal)
              '("Personal knowledge" "PN"))))
    (should
     (equal
      (fangcun-node-aliases (fangcun-node-from-id "theorem"))
      '("Fixed point theorem" "FPT")))
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":ALIASES: \"Current name\"\n"
      ":END:\n"
      "#+title: Personal Notes\n"))
    (should
     (equal (fangcun-db-update-file personal-file)
            '(:nodes 1 :aliases 1 :links 0)))
    (should
     (equal
      (fangcun-node-aliases (fangcun-node-from-id "personal-file"))
      '("Current name")))
    (should-not (fangcun-node-from-id "theorem"))))

(ert-deftest fangcun-indexes-only-ids-in-property-drawers ()
  (with-temp-buffer
    (insert
     (concat
      ":PROPERTIES:\n"
      ":ID: file-node\n"
      ":ALIASES: File \"File alias\"\n"
      ":END:\n"
      "#+title: Edge\n\n"
      "* Normal\n"
      ":PROPERTIES:\n"
      ":ID: normal\n"
      ":ALIASES: One \"Two words\"\n"
      ":END:\n"
      "* Lowercase\n"
      ":properties:\n"
      ":id: lowercase\n"
      ":end:\n"
      "* Planning\n"
      "SCHEDULED: <2026-08-10 Mon>\n"
      ":PROPERTIES:\n"
      ":ID: planned\n"
      ":END:\n"
      "* Plain text\n"
      ":ID: ignored-plain\n"
      "* Other drawer\n"
      ":LOGBOOK:\n"
      ":ID: ignored-drawer\n"
      ":END:\n"
      "* Source block\n"
      "#+begin_src text\n"
      ":ID: ignored-source\n"
      "#+end_src\n"
      "* Comment block\n"
      "#+begin_comment\n"
      ":ID: ignored-comment\n"
      "#+end_comment\n"))
    (org-mode)
    (let* ((yiyu
            (make-fangcun-yiyu
             :id 'test :name "Test" :root default-directory))
           (nodes
            (fangcun--collect-nodes-from-buffer
             (current-buffer) yiyu "edge.org")))
      (should
       (equal
        (mapcar
         (lambda (node)
           (list (fangcun-node-id node)
                 (fangcun-node-title node)
                 (fangcun-node-aliases node)))
         nodes)
        '(("file-node" "Edge" ("File" "File alias"))
          ("normal" "Normal" ("One" "Two words"))
          ("lowercase" "Lowercase" nil)
          ("planned" "Planning" nil)))))))

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
                     '(:yiyus 2 :files 2 :nodes 4 :aliases 0
                       :links 1))))
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
                     '(:yiyus 2 :files 2 :nodes 4 :aliases 0
                       :links 1)))
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
               '(:yiyus 2 :files 2 :nodes 3 :aliases 0 :links 0)))
      (let ((nodes (fangcun-node-list)))
        (should
         (equal
          (fangcun-node-title
           (fangcun-test--node "personal-file" nodes))
          "Changed on disk"))
        (should
         (fangcun-test--node "fangcun-test-disk-node" nodes))
        (should-not (fangcun-test--node "theorem" nodes))))))

(ert-deftest fangcun-rebuild-replaces-database ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (fangcun--call-with-database
     (lambda (database)
       (sqlite-execute database "CREATE TABLE obsolete (value)")))
    (delete-file personal-file)
    (should
     (equal (fangcun-db-rebuild)
             '(:yiyus 2 :files 1 :nodes 1 :aliases 0 :links 0)))
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

(ert-deftest fangcun-sync-skips-unchanged-files ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (cl-letf
        (((symbol-function 'fangcun--parse-file)
          (lambda (&rest _arguments)
            (ert-fail "An unchanged file was parsed"))))
      (should
       (equal (fangcun-db-sync)
              '(:yiyus 2 :files 2 :nodes 4 :aliases 0 :links 1))))))

(ert-deftest fangcun-syncs-added-changed-and-deleted-files ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (delete-file work-file)
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":END:\n"
      "* Replacement\n"
      ":PROPERTIES:\n"
      ":ID: replacement\n"
      ":END:\n"))
    (let ((new-file (expand-file-name "new.org" personal-root))
          (original-parser (symbol-function 'fangcun--parse-file))
          parsed)
      (fangcun-test--write-file
       new-file
       ":PROPERTIES:\n:ID: new-file\n:END:\n")
      (cl-letf
          (((symbol-function 'fangcun--parse-file)
            (lambda (yiyu file)
              (push (file-name-nondirectory file) parsed)
              (funcall original-parser yiyu file))))
        (should
         (equal (fangcun-db-sync)
                '(:yiyus 2 :files 2 :nodes 3 :aliases 0 :links 0))))
      (should
       (equal (sort parsed #'string-lessp)
              '("new.org" "theorems.org")))
      (should
       (equal
        (sort (mapcar #'fangcun-node-id (fangcun-node-list))
              #'string-lessp)
        '("new-file" "personal-file" "replacement"))))))

(ert-deftest fangcun-sync-rolls-back-a-failed-update ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":END:\n"
      "* Duplicate\n"
      ":PROPERTIES:\n"
      ":ID: work-file\n"
      ":END:\n"))
    (should-error (fangcun-db-sync))
    (should
     (equal
      (sort (mapcar #'fangcun-node-id (fangcun-node-list))
            #'string-lessp)
      '("personal-file" "theorem" "untitled-heading" "work-file")))))

(ert-deftest fangcun-sync-keeps-an-unavailable-yiyu ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (delete-directory work-root t)
    (should-error (fangcun-db-sync) :type 'user-error)
    (should (fangcun-node-from-id "work-file"))))

(ert-deftest fangcun-sync-rebuilds-after-yiyu-configuration-changes ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((fangcun-yiyus
           `((personal :name "Renamed" :root ,personal-root)
             (work :name "Work" :root ,work-root))))
      (fangcun-db-sync)
      (should
       (equal
        (fangcun-node-yiyu-name
         (fangcun-node-from-id "personal-file"))
        "Renamed")))))

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
             '(:nodes 2 :aliases 0 :links 1)))
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
          (configured-function
           (symbol-function 'fangcun--configured-yiyus))
          (configured-calls 0)
          messages)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "A theorem")
        (replace-match "Updated after saving")
        (cl-letf
            (((symbol-function 'fangcun--configured-yiyus)
              (lambda ()
                (cl-incf configured-calls)
                (funcall configured-function)))
             ((symbol-function 'message)
              (lambda (format-string &rest arguments)
                (push (apply #'format-message
                             format-string arguments)
                      messages))))
          (save-buffer)))
      (should (= configured-calls 1))
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
        "#+caption: [[id:ignored-keyword][Keyword link]]\n\n"
        "[[id:work-file][File link]]\n\n"
        "[[id:work-file::*Status][Heading search]]\n"
        "[[id:work-file::target][Target search]]\n\n"
        "* Parent\n"
        ":PROPERTIES:\n"
        ":ID: parent\n"
        ":IGNORED: [[id:ignored-property][Property link]]\n"
        ":END:\n"
        "#+begin_src text\n"
        "[[id:ignored-source][Source block link]]\n"
        "#+end_src\n"
        "#+begin_comment\n"
        "[[id:ignored-comment][Comment block link]]\n"
        "#+end_comment\n"
        "# [[id:ignored-line-comment][Comment line link]]\n"
        "** Child without an ID\n"
        "[[id:work-file][First child link]]\n"
        "[[id:work-file][Second child link]]\n"))
      (fangcun-test--write-file
       unowned-file
       "* No ID\n[[id:work-file][Unowned link]]\n")
      (should
       (equal (fangcun-db-sync)
               '(:yiyus 2 :files 4 :nodes 6 :aliases 0 :links 6)))
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
                     ("source-file" "work-file")
                     ("source-file" "work-file")
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
            "WHERE type = 'index' AND name = 'links_target_id'")))
         (should-not
          (sqlite-select
           database
           (concat
            "SELECT target_id FROM links "
            "WHERE target_id LIKE 'ignored-%'"))))))))

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
        "[[id:work-file::*Status][First reference]]\n"
        "[[id:work-file][Second reference]]\n"))
      (fangcun-db-sync)
      (let ((backlinks (fangcun-backlink-list "work-file")))
        (should (= (length backlinks) 1))
        (should
         (equal
          (fangcun-node-id
           (fangcun-backlink-node (car backlinks)))
          "parent")))
      (let ((occurrences
             (fangcun-backlink-occurrence-list "work-file")))
        (should (= (length occurrences) 2))
        (should
         (< (fangcun-backlink-position (car occurrences))
            (fangcun-backlink-position (cadr occurrences)))))
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
            "\\[\\[id:work-file::\\*Status\\]\\[First reference\\]\\]")))))))

(ert-deftest fangcun-displays-backlink-occurrences-with-previews ()
  (fangcun-test-with-notes
    (let ((source-file
           (expand-file-name "references.org" personal-root))
          (buffer (get-buffer-create fangcun-backlinks-buffer-name)))
      (unwind-protect
          (progn
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
              "[[id:work-file::*Status][First reference]]\n"
              "[[id:work-file][Second reference]]\n"))
            (fangcun-db-sync)
            (find-file work-file)
            (goto-char (point-max))
            (save-window-excursion
              (fangcun-backlinks)
              (should (eq major-mode 'fangcun-backlinks-mode))
              (should (equal fangcun-backlinks-target-id "work-file"))
              (should (eq revert-buffer-function
                          #'fangcun-backlinks-refresh))
              (revert-buffer)
              (should
               (= (how-many "^Parent  Personal — references.org$"
                            (point-min) (point-max))
                  1))
              (let ((first (next-button (point-min))))
                (should first)
                (let ((second (next-button (button-end first))))
                  (should second)
                  (should-not (next-button (button-end second)))
                  (should
                   (equal (button-label first) "First reference"))
                  (should
                   (equal (button-label second) "Second reference"))
                  (goto-char (button-start second))
                  (let ((origin (point)))
                    (set-window-parameter nil 'yunge-jump-history nil)
                    (call-interactively #'fangcun-backlink-visit)
                    (should (equal (buffer-file-name) source-file))
                    (should
                     (looking-at-p
                      "\\[\\[id:work-file\\]\\[Second reference\\]\\]"))
                    (yunge-jump-history-backward)
                    (should (eq (current-buffer) buffer))
                    (should (= (point) origin))
                    (yunge-jump-history-forward)
                    (should (equal (buffer-file-name) source-file))
                    (set-window-parameter
                     nil 'yunge-jump-history nil))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (set-window-parameter nil 'yunge-jump-history nil)))))

(ert-deftest fangcun-backlink-previews-read-each-source-once ()
  (fangcun-test-with-notes
    (let ((source-file
           (expand-file-name "references.org" personal-root))
          (buffer (get-buffer-create fangcun-backlinks-buffer-name)))
      (unwind-protect
          (progn
            (fangcun-test--write-file
             source-file
             (concat
              ":PROPERTIES:\n"
              ":ID: source-file\n"
              ":END:\n"
              "#+title: Source file\n\n"
              "[[id:work-file][First reference]]\n"
              "[[id:work-file][Second reference]]\n"))
            (fangcun-db-sync)
            (let ((original (symbol-function 'insert-file-contents))
                  (reads 0))
              (cl-letf
                  (((symbol-function 'insert-file-contents)
                    (lambda (file &rest arguments)
                      (when (file-equal-p file source-file)
                        (cl-incf reads))
                      (apply original file arguments))))
                (with-current-buffer buffer
                  (fangcun-backlinks-mode)
                  (setq fangcun-backlinks-target-id "work-file")
                  (fangcun-backlinks-refresh)))
              (should (= reads 1))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest fangcun-backlink-previews-reuse-only-matching-buffer ()
  (fangcun-test-with-notes
    (let* ((source-file
            (expand-file-name "references.org" personal-root))
           (backlinks-buffer
            (get-buffer-create fangcun-backlinks-buffer-name))
           source-buffer)
      (unwind-protect
          (progn
            (fangcun-test--write-file
             source-file
             (concat
              ":PROPERTIES:\n"
              ":ID: source-file\n"
              ":END:\n"
              "#+title: Source file\n\n"
              "[[id:work-file][Saved reference]]\n"))
            (fangcun-db-sync)
            (setq source-buffer (find-file-noselect source-file))
            (let ((original (symbol-function 'insert-file-contents))
                  (reads 0))
              (cl-letf
                  (((symbol-function 'insert-file-contents)
                    (lambda (file &rest arguments)
                      (when (file-equal-p file source-file)
                        (cl-incf reads))
                      (apply original file arguments))))
                (with-current-buffer backlinks-buffer
                  (fangcun-backlinks-mode)
                  (setq fangcun-backlinks-target-id "work-file")
                  (fangcun-backlinks-refresh)
                  (should (search-forward "Saved reference" nil t)))
                (should (= reads 0))
                (with-current-buffer source-buffer
                  (goto-char (point-min))
                  (search-forward "Saved reference")
                  (replace-match "Unsaved reference"))
                (with-current-buffer backlinks-buffer
                  (fangcun-backlinks-refresh)
                  (goto-char (point-min))
                  (should (search-forward "Saved reference" nil t))
                  (goto-char (point-min))
                  (should-not
                   (search-forward "Unsaved reference" nil t)))
                (should (= reads 1)))))
        (when (buffer-live-p source-buffer)
          (with-current-buffer source-buffer
            (set-buffer-modified-p nil)))
        (when (buffer-live-p backlinks-buffer)
          (kill-buffer backlinks-buffer))))))

(ert-deftest fangcun-jumps-participate-in-window-history ()
  (dolist (command '(fangcun-node-find fangcun-backlink-visit))
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

(ert-deftest fangcun-inserts-id-link-with-selected-alias ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      "* Fixed-point theorem\n"
      ":PROPERTIES:\n"
      ":ID: theorem\n"
      ":ALIASES: FPT \"Fixed point theorem\"\n"
      ":END:\n"))
    (fangcun-db-sync)
    (with-temp-buffer
      (org-mode)
      (cl-letf
          (((symbol-function 'completing-read)
            (lambda (_prompt collection &rest _arguments)
              (car
               (seq-find
                (lambda (item)
                  (string-prefix-p "Fixed point theorem" (car item)))
                collection)))))
        (let ((node (fangcun-node-insert)))
          (should (equal (fangcun-node-id node) "theorem"))
          (should (equal (fangcun-node-title node)
                         "Fixed point theorem"))
          (should
           (equal (buffer-string)
                  "[[id:theorem][Fixed point theorem]]")))))))

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
