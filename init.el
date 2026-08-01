;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'yunge-state)
(require 'yunge-package)
(require 'yunge-evil)
(require 'yunge-which-key)
(require 'yunge-elpaca)

(autoload 'yunge-test "yunge-test" nil t)

;;; init.el ends here
