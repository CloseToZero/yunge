;;; yunge-input-source.el --- System input source guard -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)

(declare-function mac-input-source "ext:macfns.c" (&optional source format))
(declare-function mac-select-input-source "ext:macfns.c"
                  (source &optional set-keyboard-layout-override-p))
(declare-function w32-get-ime-open-status "ext:w32fns.c" ())
(declare-function w32-set-ime-open-status "ext:w32fns.c" (status))

(defgroup yunge-input-source nil
  "Temporary control of operating-system input sources."
  :group 'environment)

(defcustom yunge-input-source-macos-ascii-source
  "com.apple.keylayout.ABC"
  "macOS input source identifier used for ASCII label keys."
  :type 'string
  :group 'yunge-input-source)

(defcustom yunge-input-source-ibus-ascii-source
  "xkb:us::eng"
  "IBus engine identifier used for ASCII label keys."
  :type 'string
  :group 'yunge-input-source)

(cl-defstruct (yunge-input-source--backend
               (:constructor yunge-input-source--make-backend))
  get
  set
  ascii)

(defun yunge-input-source--program-output (program &rest arguments)
  "Run PROGRAM synchronously with ARGUMENTS and return trimmed output."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil t nil arguments)))
      (unless (equal status 0)
        (error "%s exited with status %s" program status))
      (string-trim (buffer-string)))))

(defun yunge-input-source--set-program-source (program source)
  "Use PROGRAM to select input SOURCE synchronously."
  (let ((status (process-file program nil nil nil source)))
    (unless (equal status 0)
      (error "%s exited with status %s" program status))))

(defun yunge-input-source--external-backend (program ascii)
  "Return a PROGRAM-backed input source manager using ASCII source."
  (yunge-input-source--make-backend
   :get (lambda () (yunge-input-source--program-output program))
   :set (lambda (source)
          (yunge-input-source--set-program-source program source))
   :ascii ascii))

(defun yunge-input-source--fcitx-backend (program)
  "Return a Fcitx input source manager using PROGRAM."
  (yunge-input-source--make-backend
   :get
   (lambda ()
     (let ((state
            (string-to-number
             (yunge-input-source--program-output program))))
       (unless (memq state '(1 2))
         (error "%s returned unexpected state %s" program state))
       state))
   :set
   (lambda (state)
     ;; Fcitx remembers the selected engine while it is inactive, so closing
     ;; and reopening restores the exact engine without knowing its name.
     (let ((status
            (process-file program nil nil nil
                          (if (= state 2) "-o" "-c"))))
       (unless (equal status 0)
         (error "%s exited with status %s" program status))))
   :ascii 1))

(defun yunge-input-source--ibus-backend (program)
  "Return an IBus input source manager using PROGRAM."
  (yunge-input-source--make-backend
   :get
   (lambda ()
     (yunge-input-source--program-output program "engine"))
   :set
   (lambda (source)
     (let ((status (process-file program nil nil nil "engine" source)))
       (unless (equal status 0)
         (error "%s exited with status %s" program status))))
   :ascii yunge-input-source-ibus-ascii-source))

(defun yunge-input-source--detect-backend ()
  "Return the available system input source manager, or nil."
  (cond
   ((and (eq system-type 'windows-nt)
         (fboundp 'w32-get-ime-open-status)
         (fboundp 'w32-set-ime-open-status))
    (yunge-input-source--make-backend
     :get #'w32-get-ime-open-status
     :set #'w32-set-ime-open-status
     :ascii nil))
   ((and (eq system-type 'darwin)
         (fboundp 'mac-input-source)
         (fboundp 'mac-select-input-source))
    (yunge-input-source--make-backend
     :get #'mac-input-source
     :set #'mac-select-input-source
     :ascii yunge-input-source-macos-ascii-source))
   ((and (eq system-type 'darwin)
         (executable-find "macism"))
    (yunge-input-source--external-backend
     (executable-find "macism")
     yunge-input-source-macos-ascii-source))
   ((and (eq system-type 'gnu/linux)
         (executable-find "fcitx5-remote"))
    (yunge-input-source--fcitx-backend
     (executable-find "fcitx5-remote")))
   ((and (eq system-type 'gnu/linux)
         (executable-find "fcitx-remote"))
    (yunge-input-source--fcitx-backend
     (executable-find "fcitx-remote")))
   ((and (eq system-type 'gnu/linux)
         (executable-find "ibus"))
    (yunge-input-source--ibus-backend
     (executable-find "ibus")))))

(defun yunge-input-source-call-with-ascii (function)
  "Call FUNCTION with the system input source temporarily set to ASCII.
Restore the exact previous source even when FUNCTION exits nonlocally.  When
the current platform has no supported input source manager, call FUNCTION
without changing the input source."
  (if-let* ((backend (yunge-input-source--detect-backend)))
      (let ((source
             (funcall (yunge-input-source--backend-get backend))))
        (unwind-protect
            (progn
              (funcall (yunge-input-source--backend-set backend)
                       (yunge-input-source--backend-ascii backend))
              (funcall function))
          (funcall (yunge-input-source--backend-set backend) source)))
    (funcall function)))

(provide 'yunge-input-source)

;;; yunge-input-source.el ends here
