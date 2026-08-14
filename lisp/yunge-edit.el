;;; yunge-edit.el --- Editing defaults -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(setq-default indent-tabs-mode nil)

(declare-function occur--targets-start "replace" (targets))

(defvar-local yunge-edit--result-finish-function nil)
(defvar-local yunge-edit--result-source-buffers nil)

(defun yunge-edit--remember-result-source-buffer (beginning _end)
  "Remember the source buffer for the result at BEGINNING."
  (when-let* ((targets
               (or (get-text-property beginning 'occur-target)
                   (get-text-property (line-beginning-position)
                                      'occur-target)))
              (marker (occur--targets-start targets))
              (buffer (marker-buffer marker)))
    (cl-pushnew buffer yunge-edit--result-source-buffers)))

(defun yunge-edit-setup-result-session (finish-function)
  "Arrange to save edited result sources before FINISH-FUNCTION runs."
  (setq-local yunge-edit--result-finish-function finish-function)
  (setq-local yunge-edit--result-source-buffers nil)
  ;; Xref may create `occur-target' in its own before-change hook.  Run
  ;; after it so the first edit of a lazily prepared result is recorded.
  (add-hook 'before-change-functions
            #'yunge-edit--remember-result-source-buffer t t))

(defun yunge-edit-finish-result-session ()
  "Save source buffers changed through the current result editor."
  (interactive)
  (unless yunge-edit--result-finish-function
    (user-error "This is not an editable result buffer"))
  (dolist (buffer yunge-edit--result-source-buffers)
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'buffer-file-name buffer)
               (buffer-modified-p buffer))
      (with-current-buffer buffer
        (save-buffer))))
  (funcall-interactively yunge-edit--result-finish-function))

(defun yunge-edit-refuse-result-abort ()
  "Refuse to discard edits that have already reached source buffers."
  (interactive)
  (user-error "Result edits are live; undo them or finish with ZZ"))

(defun yunge-edit-configure-result-map (map finish-function)
  "Configure MAP for a live result editor using FINISH-FUNCTION."
  (define-key map (vector 'remap finish-function)
              #'yunge-edit-finish-result-session)
  (define-key map [remap evil-save-and-close]
              #'yunge-edit-finish-result-session)
  (define-key map [remap evil-save-modified-and-close]
              #'yunge-edit-finish-result-session)
  (define-key map [remap evil-quit]
              #'yunge-edit-refuse-result-abort))

(provide 'yunge-edit)

;;; yunge-edit.el ends here
