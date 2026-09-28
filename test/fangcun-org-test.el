;;; fangcun-org-test.el --- Fangcun Org parsing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'ert)
(require 'fangcun-org)

(ert-deftest fangcun-org-reads-only-ids-in-property-drawers ()
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
      "[[id:file-node][Link]]\n"
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
           (data
            (fangcun-org-read-buffer
             (current-buffer) yiyu "edge.org"))
           (nodes (plist-get data :nodes))
           (links (plist-get data :links)))
      (should
       (equal
        (mapcar
         (lambda (node)
           (list (fangcun-node-id node)
                 (fangcun-node-title node)
                 (fangcun-node-outline-path node)
                 (fangcun-node-aliases node)))
         nodes)
        '(("file-node" "Edge" nil ("File" "File alias"))
          ("normal" "Normal" ("Normal") ("One" "Two words"))
          ("lowercase" "Lowercase" ("Lowercase") nil)
          ("planned" "Planning" ("Planning") nil))))
      (should (= (length links) 1))
      (should (equal (fangcun-link-source-id (car links)) "normal"))
      (should (equal (fangcun-link-target-id (car links)) "file-node")))))

(provide 'fangcun-org-test)

;;; fangcun-org-test.el ends here
