;;; fangcun-test.el --- Fangcun tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'fangcun)

(require 'fangcun-test-helper)

(defun fangcun-test--node (id nodes)
  "Return the node named ID from NODES."
  (seq-find
   (lambda (node)
     (equal (fangcun-node-id node) id))
   nodes))

(ert-deftest fangcun-indexes-without-yunge-state-or-history ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(let* ((root (make-temp-file "fangcun-independent-" t))
            (notes (expand-file-name "notes/" root))
            (file (expand-file-name "example.org" notes))
            (state (expand-file-name "state/" root)))
       (unwind-protect
           (progn
             (make-directory notes t)
             (with-temp-file file
               (insert ":PROPERTIES:\n:ID: independent\n:END:\n"
                       "#+title: Independent\n"))
             (setq fangcun-state-directory state
                   fangcun-yiyus `((notes :name "Notes" :root ,notes))
                   fangcun-native-helper-enabled nil)
             (require 'fangcun)
             (fangcun-db-sync t)
             (unless (and (file-exists-p
                           (expand-file-name "fangcun.sqlite" state))
                          (equal (fangcun-node-title
                                  (fangcun-node-from-id "independent"))
                                 "Independent"))
               (error "Standalone Fangcun did not index its configured state"))
             (when (or (featurep 'yunge-state)
                       (featurep 'yunge-jump-history))
               (error "Standalone Fangcun loaded Yunge state or history")))
         (delete-directory root t))))))

(ert-deftest fangcun-database-customization-requires-an-absolute-path ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'fangcun)
       (let ((file (expand-file-name
                    "fangcun.sqlite" temporary-file-directory)))
         (unless (condition-case nil
                     (progn
                       (customize-set-variable
                        'fangcun-database-file "fangcun.sqlite")
                       nil)
                   (error t))
           (error "Customize accepted a relative Fangcun database path"))
         (customize-set-variable 'fangcun-database-file file)
         (unless (equal fangcun-database-file file)
           (error "Customize did not set the database path")))))))

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
    (let ((note-directory (expand-file-name "note" personal-root)))
      (should-not
       (fangcun--new-file-name-error
        (expand-file-name "逻辑.org" note-directory)
        personal-root)))
    (should-not
     (fangcun--new-file-name-error
      (expand-file-name "new.org" personal-root)
      personal-root))
    (dolist (name
             '("theorems.org"
               "new.txt"
               "CON/new.org"
               "trailing./new.org"
               "../outside.org"))
      (should
       (fangcun--new-file-name-error
        (expand-file-name name personal-root)
        personal-root)))))

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
            (lambda (&rest _arguments) "C/C++"))
           ((symbol-function 'read-file-name)
            (lambda (_prompt directory &optional _default _mustmatch
                             initial &rest _arguments)
              (should (equal directory personal-root))
              (setq prompted-initial initial)
              "c-cpp.org"))
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

(ert-deftest fangcun-creates-a-local-heading-node ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (with-current-buffer (find-file-noselect personal-file)
      (goto-char (point-min))
      (re-search-forward "^\\* $")
      (beginning-of-line)
      (insert "** Child\nChild body\n\n")
      (search-backward "Child body")
      (save-excursion
        (org-back-to-heading t)
        (should-not (org-id-get))
        (should (equal (org-entry-get nil "ID" t) "theorem")))
      (let ((new-id-calls 0))
        (cl-letf
            (((symbol-function 'org-id-new)
              (lambda (&optional _prefix)
                (cl-incf new-id-calls)
                "child-node"))
             ((symbol-function 'org-id-locations-load)
              (lambda ()
                (ert-fail
                 "Creating a node should not load Org ID state"))))
          (should
           (equal (fangcun-heading-node-create) "child-node"))
          (should (looking-at "Child body"))
          (should
           (equal (fangcun-heading-node-create) "child-node"))
          (should (= new-id-calls 1))))
      (save-excursion
        (org-back-to-heading t)
        (should (equal (org-id-get) "child-node")))
      (save-buffer)
      (let ((node (fangcun-node-from-id "child-node")))
        (should node)
        (should (equal (fangcun-node-title node) "Child"))))))

(ert-deftest fangcun-create-selects-yiyu-and-reprompts-invalid-name ()
  (fangcun-test-with-notes
    (let ((target (expand-file-name "note/status.org" work-root))
          (answers
           (list (expand-file-name "status.txt" work-root)
                 "note/status.org"))
          (org-id-locations nil)
          prompts initials created-buffer)
      (with-temp-buffer
        (cl-letf
            (((symbol-function 'completing-read)
              (lambda (_prompt collection &rest _arguments)
                (should (assoc "Work" collection))
                "Work"))
             ((symbol-function 'read-string)
              (lambda (&rest _arguments) "Status"))
             ((symbol-function 'read-file-name)
              (lambda (prompt directory &optional _default _mustmatch
                              initial &rest _arguments)
                (should (equal directory work-root))
                (push prompt prompts)
                (push initial initials)
                (pop answers)))
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
      (should (file-directory-p (file-name-directory target)))
      (should-not (file-exists-p target)))))

(ert-deftest fangcun-resolves-org-ids-through-its-database ()
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

(ert-deftest fangcun-follows-org-id-links-through-its-database ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((org-id-locations (make-hash-table :test #'equal)))
      (with-temp-buffer
        (org-mode)
        (insert "[[id:theorem][A theorem]]")
        (goto-char (point-min))
        (search-forward "A theorem")
        (backward-char)
        (cl-letf (((symbol-function 'org-id-locations-load)
                   (lambda ()
                     (ert-fail
                      "Following a Fangcun link loaded Org ID state"))))
          (org-open-at-point))
        (should (file-equal-p (buffer-file-name) personal-file))
        (should (equal (org-id-get) "theorem"))))))

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

(ert-deftest fangcun-syncs-multiple-yiyus ()
  (fangcun-test-with-notes
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 2 :nodes 4 :aliases 0 :tags 0 :links 1)))
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
      (should (equal (fangcun-node-outline-path personal) nil))
      (should (equal (fangcun-node-outline-path theorem)
                     '("A theorem")))
      (should (equal (fangcun-node-title untitled)
                     "untitled-heading"))
      (should (equal (fangcun-node-yiyu-id work) "work"))
      (should (equal (fangcun-node-yiyu-name work) "Work"))
      (should
       (equal
        (fangcun-node-title work)
        (file-name-sans-extension
         (file-relative-name work-file work-root)))))))

(ert-deftest fangcun-rejects-remote-yiyu-roots ()
  (let ((fangcun-yiyus
         '((remote :name "Remote" :root "/ssh:host:/notes"))))
    (should-error (fangcun--configured-yiyus) :type 'user-error)))

(ert-deftest fangcun-rejects-duplicate-yiyu-ids-and-overlapping-roots ()
  (let* ((root (make-temp-file "fangcun-yiyu-test-" t))
         (nested (expand-file-name "nested/" root)))
    (unwind-protect
        (progn
          (make-directory nested)
          (let ((fangcun-yiyus
                 `((notes :name "Notes" :root ,root)
                   (notes :name "Other" :root ,nested))))
            (should-error
             (fangcun--configured-yiyus) :type 'user-error))
          (let ((fangcun-yiyus
                 `((notes :name "Notes" :root ,root)
                   (nested :name "Nested" :root ,nested))))
            (should-error
             (fangcun--configured-yiyus) :type 'user-error))
          (let ((fangcun-yiyus
                 `((notes :name "Notes" :root ,root)
                   (same :name "Same" :root ,root))))
            (should-error
             (fangcun--configured-yiyus) :type 'user-error)))
      (delete-directory root t))))

(ert-deftest fangcun-adds-and-persists-a-yiyu ()
  (let* ((root (make-temp-file "fangcun-yiyu-test-" t))
         (expected-root (file-name-as-directory root))
         (fangcun-yiyus nil)
         saved
         applied)
    (unwind-protect
        (cl-letf
            (((symbol-function 'customize-save-variable)
              (lambda (symbol value &optional _comment)
                (should (eq symbol 'fangcun-yiyus))
                (setq fangcun-yiyus value
                      saved value)))
             ((symbol-function 'fangcun--apply-yiyu-configuration)
              (lambda () (setq applied t))))
          (fangcun-yiyu-add "jingwei" "经纬" root)
          (should
           (equal
            saved
            `((jingwei :name "经纬" :root ,expected-root))))
          (should applied))
      (delete-directory root t))))

(ert-deftest fangcun-removes-a-yiyu-without-deleting-its-root ()
  (let* ((root (make-temp-file "fangcun-yiyu-test-" t))
         (other-root (make-temp-file "fangcun-yiyu-test-" t))
         (fangcun-yiyus
          `((personal :name "Personal" :root ,root)
            (work :name "Work" :root ,other-root)))
         saved
         applied)
    (unwind-protect
        (cl-letf
            (((symbol-function 'customize-save-variable)
              (lambda (symbol value &optional _comment)
                (should (eq symbol 'fangcun-yiyus))
                (setq fangcun-yiyus value
                      saved value)))
             ((symbol-function 'fangcun--apply-yiyu-configuration)
              (lambda () (setq applied t))))
          (fangcun-yiyu-remove 'personal)
          (should
           (equal saved `((work :name "Work" :root ,other-root))))
          (should applied)
          (should (file-directory-p root)))
      (delete-directory root t)
      (delete-directory other-root t))))

(ert-deftest fangcun-removing-the-last-yiyu-clears-the-index ()
  (fangcun-test-with-notes
    (setq fangcun-yiyus
          `((personal :name "Personal" :root ,personal-root)))
    (fangcun-db-sync)
    (should (fangcun-node-from-id "personal-file"))
    (cl-letf
        (((symbol-function 'customize-save-variable)
          (lambda (_symbol value &optional _comment)
            (setq fangcun-yiyus value))))
      (fangcun-yiyu-remove 'personal))
    (should-not (fangcun-node-list))
    (should-not (fangcun-node-from-id "personal-file"))
    (should-not (fangcun-backlink-list "personal-file"))
    (should (file-exists-p personal-file))))

(ert-deftest fangcun-records-files-without-nodes ()
  (fangcun-test-with-notes
    (let ((empty-file
           (expand-file-name "empty.org" personal-root)))
      (fangcun-test--write-file empty-file "")
      (should
       (equal (fangcun-db-sync)
              '(:yiyus 2 :files 3 :nodes 4 :aliases 0 :tags 0 :links 1))))))

(ert-deftest fangcun-file-owns-nodes-aliases-tags-and-outgoing-links ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":ALIASES: Personal\n"
      ":END:\n"
      "#+filetags: :personal:\n"
      "[[id:work-file][Outgoing]]\n"))
    (fangcun-test--write-file
     work-file
     (concat
      ":PROPERTIES:\n"
      ":ID: work-file\n"
      ":END:\n"
      "#+filetags: :work:\n"
      "[[id:personal-file][Incoming]]\n"))
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 2 :nodes 2 :aliases 1 :tags 2 :links 2)))
    (delete-file personal-file)
    (should-not (fangcun-node-from-id "personal-file"))
    (should
     (equal (mapcar #'fangcun-node-id (fangcun-node-list))
            '("work-file")))
    (should
     (equal (fangcun-node-tags (fangcun-node-from-id "work-file"))
            '("work")))
    (should-not (fangcun-backlink-list "work-file"))
    (let ((backlinks (fangcun-backlink-list "personal-file")))
      (should (= (length backlinks) 1))
      (should
       (equal
        (fangcun-node-id (fangcun-backlink-node (car backlinks)))
        "work-file")))))

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
            '(:yiyus 2 :files 2 :nodes 3 :aliases 4 :tags 0 :links 0)))
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
            '(:nodes 1 :aliases 1 :tags 0 :links 0)))
    (should
     (equal
      (fangcun-node-aliases (fangcun-node-from-id "personal-file"))
      '("Current name")))
    (should-not (fangcun-node-from-id "theorem"))))

(ert-deftest fangcun-indexes-effective-node-tags ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n"
      ":ID: personal-file\n"
      ":END:\n"
      "#+filetags: :notes:shared:\n\n"
      "* Parent :math:\n"
      ":PROPERTIES:\n"
      ":ID: parent\n"
      ":END:\n"
      "** Child :proof:\n"
      ":PROPERTIES:\n"
      ":ID: child\n"
      ":END:\n"))
    (let ((org-use-tag-inheritance t))
      (should
       (equal (fangcun-db-sync)
              '(:yiyus 2 :files 2 :nodes 4 :aliases 0 :tags 9
                       :links 0))))
    (let ((nodes (fangcun-node-list)))
      (should
       (equal (fangcun-node-tags
               (fangcun-test--node "personal-file" nodes))
              '("notes" "shared")))
      (should
       (equal (fangcun-node-tags
               (fangcun-test--node "parent" nodes))
              '("math" "notes" "shared")))
      (should
       (equal (fangcun-node-tags
               (fangcun-test--node "child" nodes))
              '("math" "notes" "proof" "shared"))))))

(ert-deftest fangcun-sets-file-and-heading-tags ()
  (with-temp-buffer
    (insert
     (concat
      ":PROPERTIES:\n"
      ":ID: file-node\n"
      ":END:\n"
      "#+title: Notes\n\n"
      "* Child\n"
      ":PROPERTIES:\n"
      ":ID: child\n"
      ":END:\n"))
    (org-mode)
    (cl-letf (((symbol-function 'fangcun--ensure-session) #'ignore))
      (goto-char (point-min))
      (fangcun-node-set-tags '("notes" "中文"))
      (should
       (equal org-file-tags '("notes" "中文")))
      (should
       (equal
        (buffer-substring-no-properties
         (point-min)
         (save-excursion
           (goto-char (point-min))
           (re-search-forward "^#\\+title:")
           (line-beginning-position)))
        (concat
         ":PROPERTIES:\n"
         ":ID: file-node\n"
         ":END:\n"
         "#+filetags: :notes:中文:\n")))
      (re-search-forward "^\\* Child")
      (fangcun-node-set-tags '("proof"))
      (should (equal (org-get-tags nil t) '("proof")))
      (should-error
       (fangcun-node-set-tags '("not-valid"))
       :type 'user-error))))

(ert-deftest fangcun-tag-input-reprompts-after-invalid-input ()
  (let ((answers '(("not-valid") ("notes" "中文")))
        prompts)
    (cl-letf
        (((symbol-function 'fangcun--tag-completions) #'ignore)
         ((symbol-function 'completing-read-multiple)
          (lambda (prompt &rest _arguments)
            (push prompt prompts)
            (pop answers))))
      (should
       (equal (fangcun--read-tags '("old"))
              '("notes" "中文")))
      (should (= (length prompts) 2))
      (should (string-match-p "invalid" (car prompts))))))

(ert-deftest fangcun-sync-preserves-visited-buffer-position-and-narrowing ()
  (fangcun-test-with-notes
    (let ((buffer (find-file-noselect personal-file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "A theorem")
        (org-back-to-heading)
        (narrow-to-region (point) (point-max))
        (let ((saved-point (point))
              (saved-min (point-min))
              (saved-max (point-max)))
          (should
           (equal (fangcun-db-sync)
                  '(:yiyus 2 :files 2 :nodes 4 :aliases 0 :tags 0
                           :links 1)))
          (should (equal (fangcun-node-title
                          (fangcun-node-from-id "theorem"))
                         "A theorem"))
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
                    '(:yiyus 2 :files 2 :nodes 4 :aliases 0 :tags 0
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
              '(:yiyus 2 :files 2 :nodes 3 :aliases 0 :tags 0 :links 0)))
      (let ((nodes (fangcun-node-list)))
        (should
         (equal
          (fangcun-node-title
           (fangcun-test--node "personal-file" nodes))
          "Changed on disk"))
        (should
         (fangcun-test--node "fangcun-test-disk-node" nodes))
        (should-not (fangcun-test--node "theorem" nodes))))))

(ert-deftest fangcun-rebuild-replaces-indexed-state ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (delete-file personal-file)
    (should
     (equal (fangcun-db-rebuild)
            '(:yiyus 2 :files 1 :nodes 1 :aliases 0 :tags 0 :links 0)))
    (should
     (equal (mapcar #'fangcun-node-id (fangcun-node-list))
            '("work-file")))))

(ert-deftest fangcun-rebuild-preserves-a-valid-database-on-failure ()
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
    (should-error (fangcun-db-rebuild))
    (should
     (equal
      (sort (mapcar #'fangcun-node-id (fangcun-node-list))
            #'string-lessp)
      '("personal-file" "theorem" "untitled-heading" "work-file")))
    (should-not
     (directory-files
      (file-name-directory fangcun-database-file)
      nil "\\`.fangcun-rebuild-"))))

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
    (fangcun-test--write-file
     (expand-file-name "new.org" personal-root)
     ":PROPERTIES:\n:ID: new-file\n:END:\n")
    (should
     (equal (fangcun-db-sync)
            '(:yiyus 2 :files 2 :nodes 3 :aliases 0 :tags 0 :links 0)))
    (should
     (equal
      (sort (mapcar #'fangcun-node-id (fangcun-node-list))
            #'string-lessp)
      '("new-file" "personal-file" "replacement")))))

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
    (rename-file work-root
                 (expand-file-name "work-unavailable" root))
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
            '(:nodes 2 :aliases 0 :tags 0 :links 1)))
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
    (should
     (equal
      (mapcar
       (lambda (backlink)
         (fangcun-node-id (fangcun-backlink-node backlink)))
       (fangcun-backlink-list "work-file"))
      '("replacement")))
    (should
     (equal
      (mapcar
       (lambda (backlink)
         (fangcun-node-id (fangcun-backlink-node backlink)))
       (fangcun-backlink-list "theorem"))
      '("work-file")))))

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

(ert-deftest fangcun-rename-file-updates-the-index ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((renamed
           (expand-file-name "renamed.org" personal-root)))
      (rename-file personal-file renamed)
      (let ((node (fangcun-node-from-id "personal-file")))
        (should node)
        (should (equal (fangcun-node-file node) "renamed.org"))))))

(ert-deftest fangcun-delete-file-removes-its-indexed-data ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (delete-file work-file)
    (should-not (fangcun-node-from-id "work-file"))))

(ert-deftest fangcun-native-scan-decodes-file-states ()
  (fangcun-test-with-notes
    (let* ((yiyus (fangcun--configured-yiyus))
           (native-file
            (expand-file-name "native.org" personal-root))
           command)
      (cl-letf (((symbol-function 'process-file)
                 (lambda (program _input _destination _display
                                  &rest arguments)
                   (setq command (cons program arguments))
                   (insert
                    (json-serialize
                     '((kind . "ready")
                       (build-id . "test-build")))
                    "\n"
                    (json-serialize
                     `((kind . "state")
                       (yiyu . "personal")
                       (file . ,native-file)
                       (mtime . 42.5)
                       (size . 17)))
                    "\n")
                   0))
                ((symbol-function
                  'fangcun--native-helper-build-id)
                 (lambda () "test-build")))
        (let ((states
               (fangcun--native-scan-file-states yiyus)))
          (should (= (length states) 1))
          (let ((state (car states)))
            (should
             (equal
              (fangcun-yiyu-id (fangcun-file-state-yiyu state))
              "personal"))
            (should
             (equal (fangcun-file-state-relative-file state)
                    "native.org"))
            (should (= (fangcun-file-state-mtime state) 42.5))
            (should (= (fangcun-file-state-size state) 17))))
        (should
         (equal
          (cdr command)
          (list "scan"
                "personal" personal-root
                "work" work-root)))))))

(ert-deftest fangcun-native-ready-validates-the-source-build-id ()
  (cl-letf (((symbol-function 'fangcun--native-helper-build-id)
             (lambda () "expected")))
    (should-not
     (fangcun--validate-native-ready-message
      '((kind . "ready") (build-id . "expected"))))
    (should-error
     (fangcun--validate-native-ready-message
      '((kind . "ready") (build-id . "old")))
     :type 'fangcun-native-helper-outdated)
    (should-error
     (fangcun--validate-native-ready-message
      '((kind . "state")))
     :type 'fangcun-native-helper-outdated)))

(ert-deftest fangcun-native-mismatch-builds-and-falls-back-to-emacs ()
  (let ((fangcun-native-helper-enabled t)
        (yiyus
         (list
          (make-fangcun-yiyu
           :id "test" :name "Test" :root default-directory)))
        built)
    (cl-letf (((symbol-function 'fangcun--native-helper-available-p)
               (lambda () t))
              ((symbol-function 'fangcun--native-scan-file-states)
               (lambda (_yiyus)
                 (signal 'fangcun-native-helper-outdated '("old"))))
              ((symbol-function 'fangcun--build-native-helper)
               (lambda ()
                 (setq built t)))
              ((symbol-function 'fangcun--elisp-scan-file-states)
               (lambda (scan-yiyus)
                 (when scan-yiyus
                   '(fallback)))))
      (should (equal (fangcun--scan-file-states yiyus) '(fallback)))
      (should built))))

(ert-deftest fangcun-native-build-reports-success-without-active-session ()
  (let ((fangcun--native-build-process 'build-process)
        (fangcun--session-active-p nil)
        messages
        watch-started)
    (cl-letf (((symbol-function 'process-status)
               (lambda (_process) 'exit))
              ((symbol-function 'process-exit-status)
               (lambda (_process) 0))
              ((symbol-function 'fangcun--native-helper-available-p)
               (lambda () t))
              ((symbol-function 'fangcun--start-native-watch)
               (lambda (_yiyus)
                 (setq watch-started t)))
              ((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (push (apply #'format format-string arguments) messages))))
      (fangcun--native-build-sentinel 'build-process "finished\n"))
    (should-not fangcun--native-build-process)
    (should-not watch-started)
    (should (member "Built Fangcun native helper" messages))))

(ert-deftest fangcun-native-ignores-an-obsolete-build-completion ()
  (let ((fangcun--native-build-process 'current-build)
        reacted)
    (cl-letf (((symbol-function 'process-status)
               (lambda (_process) 'exit))
              ((symbol-function 'process-exit-status)
               (lambda (_process) 0))
              ((symbol-function 'fangcun--native-helper-available-p)
               (lambda ()
                 (setq reacted t)))
              ((symbol-function 'fangcun--native-warning)
               (lambda (&rest _arguments)
                 (setq reacted t))))
      (fangcun--native-build-sentinel 'old-build "finished\n"))
    (should (eq fangcun--native-build-process 'current-build))
    (should-not reacted)))

(ert-deftest fangcun-update-starts-the-fangcun-session ()
  (fangcun-test-with-notes
    (fangcun-db-update-file personal-file t)
    (should (file-exists-p fangcun-database-file))
    (should fangcun--session-active-p)
    (should (= (length (fangcun-node-list)) 4))))

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

(ert-deftest fangcun-native-events-preserve-index-while-a-root-is-unavailable ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((unavailable (expand-file-name "work-unavailable" root)))
      (rename-file work-root unavailable)
      (fangcun-test--write-file
       personal-file
       (concat ":PROPERTIES:\n:ID: personal-file\n:END:\n"
               "#+title: Updated personal notes\n"))
      (fangcun--queue-native-files (list personal-file work-file))
      (cancel-timer fangcun--native-event-timer)
      (let (warning)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (_type message &rest _arguments)
                     (setq warning message))))
          (fangcun--process-native-events))
        (should (fangcun-node-from-id "work-file"))
        (should (equal (fangcun-node-title
                        (fangcun-node-from-id "personal-file"))
                       "Personal Notes"))
        (should (string-match-p (regexp-quote work-root) warning)))
      ;; Restore the root, then remove a note while its directory is available.
      (rename-file unavailable (directory-file-name work-root))
      (let ((fangcun--session-active-p nil))
        (delete-file work-file))
      (fangcun-db-sync)
      (should-not (fangcun-node-from-id "work-file"))
      (should (equal (fangcun-node-title
                      (fangcun-node-from-id "personal-file"))
                     "Updated personal notes")))))

(ert-deftest fangcun-native-events-move-ids-between-existing-files ()
  (fangcun-test-with-notes
    (fangcun-test--write-file
     work-file
     (concat
      ":PROPERTIES:\n:ID: work-file\n:END:\n"
      "* Work heading\n:PROPERTIES:\n:ID: work-heading\n:END:\n"))
    (fangcun-db-sync)
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n:ID: personal-file\n:END:\n"
      "#+title: Personal\n"
      "* Moved work heading\n:PROPERTIES:\n:ID: work-heading\n:END:\n"
      "[[id:theorem][Moved reference]]\n"))
    (fangcun-test--write-file
     work-file
     (concat
      ":PROPERTIES:\n:ID: work-file\n:END:\n"
      "* Moved theorem\n:PROPERTIES:\n:ID: theorem\n:END:\n"))
    (set-file-times personal-file (time-add (current-time) 2))
    (set-file-times work-file (time-add (current-time) 2))
    (fangcun--queue-native-files (list work-file personal-file))
    (cancel-timer fangcun--native-event-timer)
    (fangcun--process-native-events)
    (let ((theorem (fangcun-node-from-id "theorem"))
          (work-heading (fangcun-node-from-id "work-heading")))
      (should (equal (fangcun-node-file theorem) "projects/status.org"))
      (should (equal (fangcun-node-title theorem) "Moved theorem"))
      (should (equal (fangcun-node-file work-heading) "theorems.org"))
      (should (equal (fangcun-node-title work-heading)
                     "Moved work heading")))
    (should
     (equal (mapcar (lambda (item)
                      (fangcun-node-id
                       (fangcun-backlink-node item)))
                    (fangcun-backlink-list "theorem"))
            '("work-heading")))))

(ert-deftest fangcun-native-events-roll-back-a-failed-batch ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (fangcun-test--write-file
     personal-file
     (concat
      ":PROPERTIES:\n:ID: personal-file\n:END:\n"
      "#+title: Changed but unindexed\n"
      "[[id:work-file][New reference]]\n"))
    (fangcun-test--write-file
     work-file
     (concat
      ":PROPERTIES:\n:ID: work-file\n:END:\n"
      "* Duplicate\n:PROPERTIES:\n:ID: personal-file\n:END:\n"))
    (set-file-times personal-file (time-add (current-time) 2))
    (set-file-times work-file (time-add (current-time) 2))
    (fangcun--queue-native-files (list personal-file work-file))
    (cancel-timer fangcun--native-event-timer)
    (let (warning)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _arguments)
                   (setq warning message))))
        (fangcun--process-native-events))
      (should (string-match-p "reconciliation failed" warning)))
    (should (equal (fangcun-node-title
                    (fangcun-node-from-id "personal-file"))
                   "Personal Notes"))
    (should (equal (fangcun-node-file
                    (fangcun-node-from-id "theorem"))
                   "theorems.org"))
    (should-not (fangcun-backlink-list "work-file"))))

(ert-deftest fangcun-native-events-reconcile-a-renamed-file-before-its-new-path ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((renamed
           (expand-file-name "renamed.org" personal-root)))
      ;; Simulate a rename performed outside Emacs, so only the native event
      ;; batch reconciles the database.  Insert the paths in the order that
      ;; previously made the new path run before the vanished old path.
      (let ((fangcun--session-active-p nil))
        (rename-file personal-file renamed))
      (fangcun-test--write-file
       renamed
       (concat
        ":PROPERTIES:\n"
        ":ID: personal-file\n"
        ":END:\n"
        "#+title: Renamed personal notes\n"))
      (puthash personal-file t fangcun--native-pending-files)
      (puthash renamed t fangcun--native-pending-files)
      (fangcun--process-native-events)
      (let ((node (fangcun-node-from-id "personal-file")))
        (should node)
        (should (equal (fangcun-node-file node) "renamed.org"))
        (should (equal (fangcun-node-title node)
                       "Renamed personal notes"))))))

(ert-deftest fangcun-native-monitor-output-belongs-to-its-process ()
  (let ((fangcun--native-watch-process 'current-monitor)
        (properties (make-hash-table :test #'equal))
        handled)
    (cl-letf (((symbol-function 'process-get)
               (lambda (process property)
                 (gethash (cons process property) properties)))
              ((symbol-function 'process-put)
               (lambda (process property value)
                 (puthash (cons process property) value properties)))
              ((symbol-function 'fangcun--handle-native-message)
               (lambda (process line)
                 (push (cons process line) handled))))
      (fangcun--native-watch-filter 'current-monitor "partial")
      (setq fangcun--native-watch-process 'new-monitor)
      (fangcun--native-watch-filter 'current-monitor " stale\n")
      (fangcun--native-watch-filter 'new-monitor "fresh\n"))
    (should
     (equal
      (gethash (cons 'current-monitor 'fangcun-output) properties)
      "partial"))
    (should (equal handled '((new-monitor . "fresh"))))))

(ert-deftest fangcun-first-use-synchronizes-once ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (setq fangcun--session-active-p nil)
    (with-temp-buffer
      (insert-file-contents personal-file)
      (goto-char (point-min))
      (re-search-forward "A theorem")
      (replace-match "Changed between sessions")
      (write-region nil nil personal-file nil 'silent))
    (let ((sync-function (symbol-function 'fangcun--sync-yiyus))
          (sync-calls 0))
      (cl-letf (((symbol-function 'fangcun--sync-yiyus)
                 (lambda (yiyus no-message)
                   (cl-incf sync-calls)
                   (funcall sync-function yiyus no-message))))
        (fangcun--ensure-session)
        (fangcun--ensure-session))
      (should (= sync-calls 1)))
    (should
     (equal
      (fangcun-node-title
       (fangcun-test--node "theorem" (fangcun-node-list)))
      "Changed between sessions"))))

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

(ert-deftest fangcun-revert-hook-updates-managed-files ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((buffer (find-file-noselect personal-file)))
      (with-current-buffer buffer
        (should (memq #'fangcun--update-after-revert after-revert-hook)))
      (with-temp-buffer
        (insert-file-contents personal-file)
        (goto-char (point-min))
        (re-search-forward "A theorem")
        (replace-match "Updated after reverting")
        (write-region nil nil personal-file nil 'silent))
      (with-current-buffer buffer
        (revert-buffer t t))
      (should
       (equal
        (fangcun-node-title
         (fangcun-test--node "theorem" (fangcun-node-list)))
        "Updated after reverting")))))

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
              '(:yiyus 2 :files 4 :nodes 6 :aliases 0 :tags 0 :links 6)))
      (let* ((backlinks (fangcun-backlink-list "work-file"))
             (source
              (seq-find
               (lambda (backlink)
                 (equal
                  (fangcun-node-id (fangcun-backlink-node backlink))
                  "source-file"))
               backlinks))
             (parent
              (seq-find
               (lambda (backlink)
                 (equal
                  (fangcun-node-id (fangcun-backlink-node backlink))
                  "parent"))
               backlinks))
             (occurrences
              (fangcun-backlink-occurrence-list "work-file")))
        (should (= (length backlinks) 2))
        (should (= (fangcun-backlink-count source) 3))
        (should (= (fangcun-backlink-count parent) 2))
        (should (= (length occurrences) 5)))
      (let ((backlinks (fangcun-backlink-list "source")))
        (should (= (length backlinks) 1))
        (should
         (equal
          (fangcun-node-id (fangcun-backlink-node (car backlinks)))
          "theorem")))
      (dolist (id '("ignored-keyword"
                    "ignored-property"
                    "ignored-source"
                    "ignored-comment"
                    "ignored-line-comment"))
        (should-not (fangcun-backlink-list id))))))

(ert-deftest fangcun-check-reports-note-graph-problems ()
  (fangcun-test-with-notes
    (let ((unowned-file
           (expand-file-name "unowned.org" personal-root))
          (buffer (get-buffer-create fangcun-check-buffer-name))
          (org-id-locations (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (fangcun-test--write-file
             personal-file
             ":PROPERTIES:\n:ID: duplicate-node\n:END:\n")
            (fangcun-test--write-file
             work-file
             ":PROPERTIES:\n:ID: duplicate-node\n:END:\n")
            (fangcun-test--write-file
             unowned-file
             (concat
              "#+title: Unowned links\n\n"
              "* Section without an ID\n"
              "[[id:missing-check-target][Missing target]]\n"))
            (save-window-excursion
              (fangcun-check)
              (should (eq major-mode 'fangcun-check-mode))
              (should
               (= (how-many "Duplicate Fangcun node ID"
                            (point-min) (point-max))
                  2))
              (should
               (save-excursion
                 (goto-char (point-min))
                 (search-forward "has no owning Fangcun node" nil t)))
              (should
               (save-excursion
                 (goto-char (point-min))
                 (search-forward "Unresolved Org ID link target" nil t)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

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
        (should (= (fangcun-backlink-count (car backlinks)) 2))
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
                  (call-interactively #'fangcun-backlink-visit)
                  (should (equal (buffer-file-name) source-file))
                  (should
                   (looking-at-p
                    "\\[\\[id:work-file\\]\\[Second reference\\]\\]"))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest fangcun-backlink-previews-ignore-unsaved-buffer-edits ()
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
            (with-current-buffer backlinks-buffer
              (fangcun-backlinks-mode)
              (setq fangcun-backlinks-target-id "work-file")
              (fangcun-backlinks-refresh)
              (should (search-forward "Saved reference" nil t)))
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
               (search-forward "Unsaved reference" nil t))))
        (when (buffer-live-p source-buffer)
          (with-current-buffer source-buffer
            (set-buffer-modified-p nil)))
        (when (buffer-live-p backlinks-buffer)
          (kill-buffer backlinks-buffer))))))

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
                "  Personal › theorems.org › A theorem"))))))

(ert-deftest fangcun-candidates-distinguish-file-and-heading-locations ()
  (let* ((file-node
          (make-fangcun-node
           :id "file"
           :yiyu-name "Personal"
           :file "notes.org"
           :title "Shared title"))
         (heading-node
          (make-fangcun-node
           :id "heading"
           :yiyu-name "Personal"
           :file "notes.org"
           :title "Shared title"
           :outline-path '("Shared title")))
         (file-annotation
          (fangcun--node-annotation
           (fangcun--node-candidate file-node)))
         (heading-annotation
          (fangcun--node-annotation
           (fangcun--node-candidate heading-node))))
    (should
     (equal (substring-no-properties file-annotation)
            "  Personal › notes.org (file)"))
    (should
     (equal (substring-no-properties heading-annotation)
            "  Personal › notes.org › Shared title"))))

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

(ert-deftest fangcun-candidates-display-searchable-tags ()
  (let* ((node
          (make-fangcun-node
           :id "theorem"
           :title "A theorem"
           :tags '("math" "中文")))
         (candidate (fangcun--node-candidate node))
         (visible "A theorem  #math #中文"))
    (should
     (equal (substring-no-properties candidate 0 (length visible))
            visible))
    (should (eq (get-text-property 12 'face candidate) 'org-tag))
    (should
     (get-text-property (length visible) 'invisible candidate))))

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

(ert-deftest fangcun-replaces-an-active-region-with-an-id-link ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (with-temp-buffer
      (org-mode)
      (insert "the contraction argument")
      (set-mark (point-min))
      (activate-mark)
      (let ((transient-mark-mode t)
            initial-input)
        (cl-letf
            (((symbol-function 'completing-read)
              (lambda (_prompt collection &optional _predicate
                                _require-match initial &rest _arguments)
                (setq initial-input initial)
                (car
                 (seq-find
                  (lambda (item)
                    (string-prefix-p "A theorem" (car item)))
                  collection)))))
          (let ((node (fangcun-node-insert)))
            (should (equal (fangcun-node-id node) "theorem"))))
        (should-not initial-input)
        (should
         (equal
          (buffer-string)
          "[[id:theorem][the contraction argument]]"))
        (should-not mark-active)))))

(ert-deftest fangcun-retargets-an-id-link-without-changing-its-description ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (with-temp-buffer
      (org-mode)
      (insert "[[id:personal-file][the original wording]]")
      (search-backward "original")
      (let (initial-input)
        (cl-letf
            (((symbol-function 'completing-read)
              (lambda (_prompt collection &optional _predicate
                                _require-match initial &rest _arguments)
                (setq initial-input initial)
                (car
                 (seq-find
                  (lambda (item)
                    (string-prefix-p "A theorem" (car item)))
                  collection)))))
          (fangcun-node-insert))
        (should (equal initial-input "the original wording"))
        (should
         (equal
          (buffer-string)
          "[[id:theorem][the original wording]]"))))))

(ert-deftest fangcun-preserves-region-text-when-insertion-quits ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (with-temp-buffer
      (org-mode)
      (insert "selected wording")
      (set-mark (point-min))
      (activate-mark)
      (let ((transient-mark-mode t)
            quit-seen)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _arguments) (keyboard-quit))))
          (condition-case nil
              (fangcun-node-insert)
            (quit (setq quit-seen t))))
        (should quit-seen)
        (should (equal (buffer-string) "selected wording"))
        (should-not mark-active)))))

(ert-deftest fangcun-completes-org-id-links-with-fangcun-nodes ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let (original-called)
      (cl-letf
          (((symbol-function 'completing-read)
            (lambda (_prompt collection &rest _arguments)
              (car
               (seq-find
                (lambda (item)
                  (and (consp item)
                       (string-prefix-p "A theorem" (car item))))
                collection)))))
        (should
         (equal
          (fangcun--id-complete
           (lambda (&optional _argument)
             (setq original-called t)
             "id:outside")
           nil)
          "id:theorem")))
      (should-not original-called))))

(ert-deftest fangcun-org-id-completion-retains-the-native-org-path ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let (original-called)
      (cl-letf
          (((symbol-function 'completing-read)
            (lambda (_prompt collection &rest _arguments)
              (seq-find
               (lambda (item)
                 (and (stringp item)
                      (string-prefix-p "Org heading" item)))
               collection))))
        (should
         (equal
          (fangcun--id-complete
           (lambda (&optional _argument)
             (setq original-called t)
             "id:outside")
           nil)
          "id:outside")))
      (should original-called))))

(ert-deftest fangcun-describes-file-node-ids-for-org-link-insertion ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (should
     (equal (org-id-description "id:personal-file" nil)
            "Personal Notes"))
    (should
     (equal
      (org-id-description "id:personal-file" "Selected wording")
      "Selected wording"))))

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
