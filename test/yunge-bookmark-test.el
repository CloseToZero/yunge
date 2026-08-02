;;; yunge-bookmark-test.el --- Bookmark tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(defvar bookmark-save-flag)

(yunge-test-deftest-lazy-load yunge-bookmark
  (bookmark evil which-key))

(ert-deftest yunge-bookmark-configures-storage-and-keys ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (require 'yunge-bookmark)

  (should (= bookmark-save-flag 1))
  (should
   (equal bookmark-default-file
          (expand-file-name "var/bookmark.eld"
                            user-emacs-directory)))
  (yunge-test-evil-normal-keys
   'fundamental-mode
   '(("SPC j B" . bookmark-bmenu-list)
     ("SPC j m" . bookmark-set)))
  (yunge-test-which-key-prefix-bindings
   'fundamental-mode "SPC j"
   '(("B" nil "manage bookmarks")
     ("m" nil "set bookmark"))))

;;; yunge-bookmark-test.el ends here
