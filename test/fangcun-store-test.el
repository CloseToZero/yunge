;;; fangcun-store-test.el --- Fangcun SQLite behavior -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'fangcun-store)

(ert-deftest fangcun-store-moves-nodes-and-rolls-back-a-failed-batch ()
  (let* ((root (make-temp-file "fangcun-store-" t))
         (database-file (expand-file-name "index.sqlite" root))
         (yiyu (make-fangcun-yiyu :id "notes" :name "Notes" :root root))
         (a (make-fangcun-file-state
             :yiyu yiyu :relative-file "a.org" :mtime 1 :size 1))
         (b (make-fangcun-file-state
             :yiyu yiyu :relative-file "b.org" :mtime 2 :size 2))
         (c (make-fangcun-file-state
             :yiyu yiyu :relative-file "c.org" :mtime 3 :size 3))
         (shared (make-fangcun-node
                  :id "shared" :yiyu-id "notes" :file "a.org"
                  :title "Original" :aliases '("Alias") :tags '("topic")))
         (source (make-fangcun-node
                  :id "source" :yiyu-id "notes" :file "b.org"
                  :title "Source"))
         (link (make-fangcun-link
                :source-id "source" :target-id "shared" :position 12)))
    (unwind-protect
        (progn
          (fangcun-store-rebuild
           database-file (list yiyu)
           (list (cons a (list :nodes (list shared) :links nil))
                 (cons b (list :nodes (list source) :links (list link)))))
          (should (equal (fangcun-node-aliases
                          (fangcun-store-node-from-id database-file "shared"))
                         '("Alias")))
          (should (equal (fangcun-node-tags
                          (fangcun-store-node-from-id database-file "shared"))
                         '("topic")))
          (should (equal (fangcun-node-id
                          (fangcun-backlink-node
                           (car (fangcun-store-backlink-list
                                 database-file "shared"))))
                         "source"))
          (setf (fangcun-node-file shared) "b.org")
          (fangcun-store-call-with-database
           database-file
           (lambda (database)
             (should (equal
                      (plist-get
                       (fangcun-store-replace-files
                        database
                        (list (cons a (list :nodes nil :links nil))
                              (cons b (list :nodes (list source shared)
                                            :links (list link))))
                        nil)
                       :nodes)
                      2))))
          (should (equal (fangcun-node-file
                          (fangcun-store-node-from-id database-file "shared"))
                         "b.org"))
          (let ((duplicate (make-fangcun-node
                            :id "shared" :yiyu-id "notes" :file "c.org"
                            :title "Duplicate")))
            (setf (fangcun-node-title source) "Should roll back")
            (should-error
             (fangcun-store-call-with-database
              database-file
              (lambda (database)
                (fangcun-store-replace-files
                 database
                 (list (cons b (list :nodes (list source shared)
                                     :links (list link)))
                       (cons c (list :nodes (list duplicate) :links nil)))
                 nil))))
            (should (equal (fangcun-node-title
                            (fangcun-store-node-from-id database-file "source"))
                           "Source"))
            (fangcun-store-call-with-database
             database-file
             (lambda (database)
               (should-not (fangcun-store-file-state
                            database yiyu "c.org"))))))
      (delete-directory root t))))

(ert-deftest fangcun-store-failed-rebuild-preserves-the-previous-index ()
  (let* ((root (make-temp-file "fangcun-store-" t))
         (database-file (expand-file-name "index.sqlite" root))
         (yiyu (make-fangcun-yiyu :id "notes" :name "Notes" :root root))
         (a (make-fangcun-file-state
             :yiyu yiyu :relative-file "a.org" :mtime 1 :size 1))
         (b (make-fangcun-file-state
             :yiyu yiyu :relative-file "b.org" :mtime 2 :size 2))
         (original (make-fangcun-node
                    :id "same" :yiyu-id "notes" :file "a.org"
                    :title "Previous index"))
         (duplicate (make-fangcun-node
                     :id "same" :yiyu-id "notes" :file "b.org"
                     :title "Conflicting rebuild")))
    (unwind-protect
        (progn
          (fangcun-store-rebuild
           database-file (list yiyu)
           (list (cons a (list :nodes (list original) :links nil))))
          (should-error
           (fangcun-store-rebuild
            database-file (list yiyu)
            (list (cons a (list :nodes (list original) :links nil))
                  (cons b (list :nodes (list duplicate) :links nil)))))
          (should (equal (fangcun-node-title
                          (fangcun-store-node-from-id database-file "same"))
                         "Previous index"))
          (should (= (length (fangcun-store-node-list database-file)) 1)))
      (delete-directory root t))))

(provide 'fangcun-store-test)

;;; fangcun-store-test.el ends here
