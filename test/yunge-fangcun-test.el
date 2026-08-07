;;; yunge-fangcun-test.el --- Fangcun integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-fangcun
  (fangcun org which-key))

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
     ("SPC n v" . fangcun-backlinks)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC n"
   '(("b" nil "find backlink")
     ("f" nil "find node")
     ("i" nil "insert node link")
     ("n" nil "new file node")
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
                  :id "theorem" :title "A theorem"))))
      (fangcun-node-insert))
    (should
     (equal (buffer-string)
            "Theorem:[[id:theorem][A theorem]]"))))

;;; yunge-fangcun-test.el ends here
