;;; yunge-embark.el --- Contextual actions -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(defconst yunge-embark-global-bindings
  '(("M-a" embark-act "act on target")))

(defun yunge-embark--setup-keys ()
  "Set up the Embark entry point."
  (yunge-key-define global-map yunge-embark-global-bindings))

(defun yunge-embark--describe-keys ()
  "Describe the Embark entry point to Which-Key."
  (yunge-key-which-key-describe-map
   global-map yunge-embark-global-bindings))

(elpaca embark-consult)

(elpaca embark
  (yunge-embark--setup-keys)
  (with-eval-after-load 'which-key
    (yunge-embark--describe-keys)))

(provide 'yunge-embark)

;;; yunge-embark.el ends here
