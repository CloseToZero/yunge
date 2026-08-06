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
     ("SPC n n" . fangcun-file-node-create)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC n"
   '(("b" nil "find backlink")
     ("f" nil "find node")
     ("i" nil "insert node link")
     ("n" nil "new file node"))))

;;; yunge-fangcun-test.el ends here
