;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-state)
(require 'yunge-server)
(require 'yunge-history)
(require 'yunge-edit)
(require 'yunge-treesit)
(when (eq system-type 'darwin)
  (require 'yunge-mac))
(require 'yunge-package)
(require 'yunge-theme)
(require 'yunge-evil)
(require 'yunge-bookmark)
(require 'yunge-window)
(require 'yunge-which-key)
(require 'yunge-vertico)
(require 'yunge-orderless)
(require 'yunge-corfu)
(require 'yunge-marginalia)
(require 'yunge-avy)
(require 'yunge-consult)
(require 'yunge-embark)
(require 'yunge-magit)
(require 'yunge-ghostel)
(require 'yunge-elpaca)
(require 'yunge-dired)
(require 'yunge-help)

;; Load saved Custom state before command-line files are visited.
(load custom-file 'noerror 'nomessage)

(autoload 'yunge-test "yunge-test" nil t)

;;; init.el ends here
