;;; yunge-key-control.el --- Persistent key control -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defface yunge-key-control-title
  '((t :inherit minibuffer-prompt :weight bold))
  "Face used for the title of a persistent key control."
  :group 'yunge)

(defface yunge-key-control-key
  '((t :inherit help-key-binding))
  "Face used for keys in a persistent key control."
  :group 'yunge)

(defface yunge-key-control-description
  '((t :inherit shadow))
  "Face used for descriptions in a persistent key control."
  :group 'yunge)

(defvar yunge-key-control--exit-function nil)

(defconst yunge-key-control-exit-bindings
  '(("SPC" yunge-key-control-quit "exit")
    ("RET" yunge-key-control-quit "exit")
    ("<escape>" yunge-key-control-quit "exit")))

(defun yunge-key-control--propertize (text face)
  "Apply FACE to TEXT and quote it for `set-transient-map'."
  (propertize (string-replace "%" "%%" text) 'face face))

(defun yunge-key-control-message (title hints)
  "Return a styled control message format headed by TITLE.
Each member of HINTS has the form (KEYS DESCRIPTION)."
  (concat
   (yunge-key-control--propertize title 'yunge-key-control-title)
   ": "
   (mapconcat
    (lambda (hint)
      (concat
       (yunge-key-control--propertize
        (car hint) 'yunge-key-control-key)
       " "
       (yunge-key-control--propertize
        (nth 1 hint) 'yunge-key-control-description)))
    hints "  ")))

(defun yunge-key-control--exited ()
  "Forget the exit function after a persistent key control ends."
  (setq yunge-key-control--exit-function nil))

(defun yunge-key-control-quit ()
  "Leave the active persistent key control."
  (interactive)
  (when yunge-key-control--exit-function
    (funcall yunge-key-control--exit-function)))

(defun yunge-key-control-start (map title hints)
  "Activate MAP as a persistent key control described by TITLE and HINTS."
  (when yunge-key-control--exit-function
    (funcall yunge-key-control--exit-function))
  (setq yunge-key-control--exit-function
        (set-transient-map
         map t #'yunge-key-control--exited
         (yunge-key-control-message title hints))))

(provide 'yunge-key-control)

;;; yunge-key-control.el ends here
