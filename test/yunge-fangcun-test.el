;;; yunge-fangcun-test.el --- Fangcun integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function yunge-jump-history-backward "yunge-jump-history")

(yunge-test-deftest-lazy-load yunge-fangcun
  (fangcun org which-key))

(ert-deftest yunge-fangcun-uses-yunge-state-and-preserves-custom-paths ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(let ((root (make-temp-file "yunge-fangcun-state-" t)))
       (unwind-protect
           (progn
             (defmacro elpaca (&rest _body) nil)
             (setq yunge-var-directory (expand-file-name "var/" root))
             (require 'yunge-fangcun)
             (require 'fangcun)
             (unless (and (equal fangcun-state-directory
                                 (expand-file-name "fangcun/"
                                                   yunge-var-directory))
                          (equal fangcun-database-file
                                 (expand-file-name "fangcun/fangcun.sqlite"
                                                   yunge-var-directory)))
               (error "Yunge did not supply Fangcun's database location")))
         (delete-directory root t)))))
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(let ((root (make-temp-file "yunge-fangcun-custom-" t)))
       (unwind-protect
           (progn
             (defmacro elpaca (&rest _body) nil)
             (setq yunge-var-directory (expand-file-name "var/" root)
                   fangcun-state-directory (expand-file-name "custom/" root)
                   fangcun-database-file
                   (expand-file-name "chosen.sqlite" root))
             (require 'yunge-fangcun)
             (require 'fangcun)
             (unless (and (equal fangcun-state-directory
                                 (expand-file-name "custom/" root))
                          (equal fangcun-database-file
                                 (expand-file-name "chosen.sqlite" root)))
               (error "Yunge changed explicit Fangcun state settings")))
         (delete-directory root t))))))

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
       (dolist (function
                '(fangcun--id-complete
                  fangcun--id-description
                  fangcun--id-find))
         (unless (autoloadp (symbol-function function))
           (error "The Fangcun Org ID adapter is not autoloaded: %s"
                  function)))
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
   '(("SPC n b f" . fangcun-backlink-find)
     ("SPC n b v" . fangcun-backlinks)
     ("SPC n C" . fangcun-check)
     ("SPC n c" . fangcun-heading-node-create)
     ("SPC n f" . fangcun-node-find)
     ("SPC n i" . fangcun-node-insert)
     ("SPC n n" . fangcun-file-node-create)
     ("SPC n t" . fangcun-node-set-tags))))

(ert-deftest yunge-fangcun-node-find-records-only-successful-jumps ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'fangcun)
  (let* ((root (make-temp-file "yunge-fangcun-jump-" t))
         (notes (expand-file-name "notes/" root))
         (file (expand-file-name "theorem.org" notes))
         (fangcun-yiyus `((notes :name "Notes" :root ,notes)))
         (fangcun-database-file (expand-file-name "fangcun.sqlite" root))
         (fangcun-native-helper-enabled nil)
         (fangcun--session-active-p nil)
         (fangcun--session-yiyus nil)
         (fangcun--native-build-process nil)
         (fangcun--native-watch-process nil)
         (fangcun--native-event-timer nil)
         (fangcun--native-pending-files (make-hash-table :test #'equal))
         (fangcun--native-pending-full-sync-p nil)
         (origin (generate-new-buffer " *yunge-fangcun-origin*")))
    (unwind-protect
        (progn
          (make-directory notes t)
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: note-file\n:END:\n"
                    "#+title: Notes\n\n"
                    "* A theorem\n:PROPERTIES:\n:ID: theorem\n:END:\n"))
          (fangcun-db-sync t)
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer origin)
            (insert "0123456789")
            (goto-char 4)
            (set-window-parameter nil 'yunge-jump-history nil)
            (let (cancelled)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _arguments)
                           (signal 'quit nil))))
                (condition-case nil
                    (fangcun-node-find)
                  (quit (setq cancelled t))))
              (should cancelled))
            (should (eq (current-buffer) origin))
            (should (= (point) 4))
            (should-error (yunge-jump-history-backward) :type 'user-error)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _arguments)
                         (car (seq-find
                               (lambda (item)
                                 (string-prefix-p "A theorem" (car item)))
                               collection)))))
              (fangcun-node-find))
            (should (equal (buffer-file-name) file))
            (should (equal (org-id-get) "theorem"))
            (yunge-jump-history-backward)
            (should (eq (current-buffer) origin))
            (should (= (point) 4))
            (yunge-jump-history-forward)
            (should (equal (buffer-file-name) file))
            (should (equal (org-id-get) "theorem"))))
      (set-window-parameter nil 'yunge-jump-history nil)
      (when (buffer-live-p origin)
        (kill-buffer origin))
      (dolist (buffer (buffer-list))
        (when-let* ((visited (buffer-file-name buffer)))
          (when (file-in-directory-p visited root)
            (kill-buffer buffer))))
      (delete-directory root t))))

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
     ("S-TAB" . backward-button))))

(ert-deftest yunge-fangcun-integrates-the-check-buffer-with-evil ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'fangcun)
  (require 'which-key)

  (yunge-test-evil-normal-keys
   'fangcun-check-mode
   '(("RET" . fangcun-check-visit)
     ("C-j" . forward-button)
     ("C-k" . backward-button)
     ("q" . quit-window)
     ("gr" . revert-buffer)
     ("g]" . forward-button)
     ("g[" . backward-button)
     ("<tab>" . forward-button)
     ("S-TAB" . backward-button))))

(ert-deftest yunge-fangcun-inserts-after-the-normal-state-eol-character ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'fangcun)

  (with-temp-buffer
    (org-mode)
    (insert "Theorem:")
    (backward-char)
    (evil-normal-state)
    (cl-letf (((symbol-function 'fangcun--read-node)
               (lambda (&optional _initial-input)
                 (make-fangcun-node
                  :id "theorem" :title "A theorem")))
              ((symbol-function 'fangcun--ensure-session) #'ignore))
      (fangcun-node-insert))
    (should
     (equal (buffer-string)
            "Theorem:[[id:theorem][A theorem]]"))))

(ert-deftest yunge-fangcun-replaces-an-evil-visual-selection ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'fangcun)

  (with-temp-buffer
    (org-mode)
    (insert "contraction argument")
    (set-mark (point-min))
    (activate-mark)
    (evil-visual-state)
    (cl-letf (((symbol-function 'fangcun--read-node)
               (lambda (&optional initial-input)
                 (should-not initial-input)
                 (make-fangcun-node
                  :id "theorem" :title "A theorem")))
              ((symbol-function 'fangcun--ensure-session) #'ignore))
      (fangcun-node-insert))
    (should
     (equal
      (buffer-string)
      "[[id:theorem][contraction argument]]"))))

(ert-deftest yunge-fangcun-retargets-an-id-link-at-normal-state-eol ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'fangcun)

  (with-temp-buffer
    (org-mode)
    (insert "[[id:old][old wording]]")
    (backward-char)
    (evil-normal-state)
    (cl-letf (((symbol-function 'fangcun--read-node)
               (lambda (&optional initial-input)
                 (should (equal initial-input "old wording"))
                 (make-fangcun-node
                  :id "theorem" :title "A theorem")))
              ((symbol-function 'fangcun--ensure-session) #'ignore))
      (fangcun-node-insert))
    (should
     (equal
     (buffer-string)
      "[[id:theorem][old wording]]"))))

(ert-deftest yunge-fangcun-inserts-after-a-non-id-link-at-eol ()
  (yunge-test-enable-evil)
  (require 'yunge-fangcun)
  (require 'fangcun)

  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][Example]]")
    (backward-char)
    (evil-normal-state)
    (cl-letf (((symbol-function 'fangcun--read-node)
               (lambda (&optional _initial-input)
                 (make-fangcun-node
                  :id "theorem" :title "A theorem")))
              ((symbol-function 'fangcun--ensure-session) #'ignore))
      (fangcun-node-insert))
    (should
     (equal
      (buffer-string)
      (concat
       "[[https://example.com][Example]]"
       "[[id:theorem][A theorem]]")))))

;;; yunge-fangcun-test.el ends here
