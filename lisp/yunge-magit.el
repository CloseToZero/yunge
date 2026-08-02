;;; yunge-magit.el --- Git interface -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-evil)

(defconst yunge-magit-go-bindings
  '(("g" magit-status "Git status")))

(elpaca magit
  (yunge-key-define yunge-go-map yunge-magit-go-bindings)
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-go-map yunge-magit-go-bindings)))

(provide 'yunge-magit)

;;; yunge-magit.el ends here
