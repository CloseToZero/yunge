;;; yunge-result-edit.el --- Edit source files through search results -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(declare-function occur--targets-start "replace" (targets))

(defvar-local yunge-result-edit--finish-function nil)
(defvar-local yunge-result-edit--source-buffers nil)

(defun yunge-result-edit--remember-source (beginning _end)
  "Remember the source buffer for the result at BEGINNING."
  (when-let* ((targets
               (or (get-text-property beginning 'occur-target)
                   (get-text-property (line-beginning-position)
                                      'occur-target)))
              (marker (occur--targets-start targets))
              (buffer (marker-buffer marker)))
    (cl-pushnew buffer yunge-result-edit--source-buffers)))

(defun yunge-result-edit-setup (finish-function)
  "Arrange to save edited result sources before FINISH-FUNCTION runs."
  (setq-local yunge-result-edit--finish-function finish-function)
  (setq-local yunge-result-edit--source-buffers nil)
  ;; Xref may create `occur-target' in its own before-change hook.  Run
  ;; after it so the first edit of a lazily prepared result is recorded.
  (add-hook 'before-change-functions
            #'yunge-result-edit--remember-source t t))

(defun yunge-result-edit-finish ()
  "Save source buffers changed through the current result editor."
  (interactive)
  (unless yunge-result-edit--finish-function
    (user-error "This is not an editable result buffer"))
  (dolist (buffer yunge-result-edit--source-buffers)
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'buffer-file-name buffer)
               (buffer-modified-p buffer))
      (with-current-buffer buffer
        (save-buffer))))
  (funcall-interactively yunge-result-edit--finish-function))

(defun yunge-result-edit-refuse-abort ()
  "Refuse to discard edits that have already reached source buffers."
  (interactive)
  (user-error "Result edits are live; undo them or finish with ZZ"))

(defun yunge-result-edit-configure-map (map finish-function)
  "Configure MAP for a live result editor using FINISH-FUNCTION."
  (define-key map (vector 'remap finish-function)
              #'yunge-result-edit-finish)
  (define-key map [remap evil-save-and-close]
              #'yunge-result-edit-finish)
  (define-key map [remap evil-save-modified-and-close]
              #'yunge-result-edit-finish)
  (define-key map [remap evil-quit]
              #'yunge-result-edit-refuse-abort))

(provide 'yunge-result-edit)

;;; yunge-result-edit.el ends here
