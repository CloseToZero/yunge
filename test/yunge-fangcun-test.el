;;; yunge-fangcun-test.el --- Fangcun integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-fangcun
  (fangcun org which-key))

(ert-deftest yunge-fangcun-loads-on-the-first-org-id-lookup ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (defmacro elpaca (&rest _body) nil)
       (require 'org-id)
       (require 'yunge-fangcun)
       (when (featurep 'fangcun)
         (error "Fangcun loaded before an ID lookup"))
       (unless (autoloadp (symbol-function 'fangcun--id-find))
         (error "The Fangcun ID resolver is not autoloaded"))
       (let ((org-id-locations (make-hash-table :test #'equal)))
         (org-id-find "not-a-fangcun-id"))
       (unless (featurep 'fangcun)
         (error "The first ID lookup did not load Fangcun"))))))

(ert-deftest yunge-fangcun-loads-for-yiyu-but-not-ordinary-org-files ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (defmacro elpaca (&rest _body) nil)
       (require 'yunge-fangcun)
       (let* ((root (make-temp-file "fangcun-loader-test-" t))
              (note (expand-file-name "note.org" root))
              (outside (make-temp-file "outside-yiyu-" nil ".org"))
              (fangcun-yiyus
               `((notes :name "Notes" :root ,root))))
         (unwind-protect
             (progn
               (with-temp-buffer
                 (setq buffer-file-name outside)
                 (org-mode)
                 (when (featurep 'fangcun)
                   (error "An ordinary Org file loaded Fangcun")))
               (with-temp-buffer
                 (setq buffer-file-name note)
                 (org-mode)
                 (unless (featurep 'fangcun)
                   (error "A yiyu Org file did not load Fangcun"))
                 (unless (memq #'fangcun--update-after-save
                               after-save-hook)
                   (error "The first yiyu buffer lacks save updates")))
               (with-temp-buffer
                 (setq buffer-file-name outside)
                 (org-mode)
                 (when (memq #'fangcun--update-after-save
                             after-save-hook)
                   (error "An ordinary Org file has Fangcun updates"))))
           (delete-file outside)
           (delete-directory root t)))))))

(ert-deftest yunge-fangcun-binds-note-entry-points ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'which-key)

  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC n b" . fangcun-backlink-find)
     ("SPC n f" . fangcun-node-find)
     ("SPC n i" . fangcun-node-insert)
     ("SPC n n" . fangcun-file-node-create)
     ("SPC n t" . fangcun-node-set-tags)
     ("SPC n v" . fangcun-backlinks)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC n"
   '(("b" nil "find backlink")
     ("f" nil "find node")
     ("i" nil "insert node link")
     ("n" nil "new file node")
     ("t" nil "set node tags")
     ("v" nil "view backlinks"))))

(ert-deftest yunge-fangcun-integrates-the-backlinks-buffer-with-evil ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'fangcun)
  (require 'which-key)

  (yunge-test-evil-normal-keys
   'fangcun-backlinks-mode
   '(("RET" . fangcun-backlink-visit)
     ("C-j" . forward-button)
     ("C-k" . backward-button)
     ("q" . quit-window)
     ("gr" . revert-buffer)
     ("g]" . forward-button)
     ("g[" . backward-button)
     ("<tab>" . forward-button)
     ("S-TAB" . backward-button)))
  (with-temp-buffer
    (fangcun-backlinks-mode)
    (yunge-test-which-key-prefix
     "g" '(("]" nil "next button")
           ("[" nil "previous button")
           ("r" nil "refresh")))))

(ert-deftest yunge-fangcun-inserts-after-the-normal-state-eol-character ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'fangcun)

  (should
   (advice-member-p #'yunge-evil-call-after-normal-state-eol
                    'fangcun-node-insert))
  (with-temp-buffer
    (org-mode)
    (insert "Theorem:")
    (backward-char)
    (evil-normal-state)
    (cl-letf (((symbol-function 'fangcun--read-node)
               (lambda ()
                 (make-fangcun-node
                  :id "theorem" :title "A theorem")))
              ((symbol-function 'fangcun--ensure-session) #'ignore))
      (fangcun-node-insert))
    (should
     (equal (buffer-string)
            "Theorem:[[id:theorem][A theorem]]"))))

;;; yunge-fangcun-test.el ends here
