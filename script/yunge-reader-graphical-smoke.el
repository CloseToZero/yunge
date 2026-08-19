;;; yunge-reader-graphical-smoke.el --- Harness -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct
    (yunge-reader-graphical-smoke-context
     (:constructor yunge-reader-graphical-smoke--make-context))
  "Filesystem and diagnostic state for one graphical Reader smoke."
  label
  root
  manifest
  target-directory
  helper
  module
  temporary-root
  log-file)

(defun yunge-reader-graphical-smoke-create
    (script-directory label temporary-prefix log-environment-variable)
  "Return a graphical smoke context rooted above SCRIPT-DIRECTORY.
LABEL names diagnostics.  TEMPORARY-PREFIX names disposable state, and
LOG-ENVIRONMENT-VARIABLE optionally names a diagnostic log file."
  (unless (and (stringp script-directory)
               (file-directory-p script-directory))
    (error "%s script directory is unavailable" label))
  (let* ((root
          (file-name-as-directory
           (expand-file-name ".." script-directory)))
         (target-directory
          (expand-file-name
           "var/yunge-reader/cargo-target" root))
         (helper
          (expand-file-name
           (concat "release/yunge-reader"
                   (if (eq system-type 'windows-nt) ".exe" ""))
           target-directory))
         (module
          (expand-file-name
           (pcase system-type
             ('windows-nt "release/yunge_reader_module.dll")
             ('darwin "release/libyunge_reader_module.dylib")
             (_ "release/libyunge_reader_module.so"))
           target-directory)))
    (yunge-reader-graphical-smoke--make-context
     :label label
     :root root
     :manifest
     (expand-file-name "native/yunge-reader/Cargo.toml" root)
     :target-directory target-directory
     :helper helper
     :module module
     :temporary-root (make-temp-file temporary-prefix t)
     :log-file (getenv log-environment-variable))))

(defun yunge-reader-graphical-smoke-initialize (context)
  "Initialize isolated Emacs paths from smoke CONTEXT."
  (let ((root (yunge-reader-graphical-smoke-context-root context))
        (temporary
         (yunge-reader-graphical-smoke-context-temporary-root context)))
    (setq user-emacs-directory root)
    (startup-redirect-eln-cache (expand-file-name "eln-cache" temporary))
    (add-to-list 'load-path (expand-file-name "lisp" root))))

(defun yunge-reader-graphical-smoke-log
    (context format-string &rest arguments)
  "Write FORMAT-STRING with ARGUMENTS for smoke CONTEXT."
  (let ((text (apply #'format format-string arguments))
        (log-file
         (yunge-reader-graphical-smoke-context-log-file context)))
    (princ text)
    (when log-file
      (write-region text nil log-file 'append 'silent))))

(defun yunge-reader-graphical-smoke-schedule
    (function &rest arguments)
  "Call FUNCTION with ARGUMENTS after the UI settling interval."
  (apply #'run-at-time 0.1 nil function arguments))

(defun yunge-reader-graphical-smoke-run-process
    (context program arguments)
  "Run PROGRAM with ARGUMENTS for smoke CONTEXT or signal with its output."
  (let* ((label
          (yunge-reader-graphical-smoke-context-label context))
         (buffer
          (generate-new-buffer (format " *%s process*" label)))
         process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name (concat label " process")
                 :buffer buffer
                 :command (cons program arguments)
                 :coding 'utf-8-unix
                 :connection-type 'pipe
                 :sentinel #'ignore
                 :noquery t))
          (while (process-live-p process)
            (accept-process-output process 0.1))
          (let ((status (process-exit-status process))
                (output
                 (with-current-buffer buffer
                   (string-trim (buffer-string)))))
            (unless (string-empty-p output)
              (yunge-reader-graphical-smoke-log
               context "%s\n" output))
            (unless (zerop status)
              (error "%s failed with status %d" program status))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun yunge-reader-graphical-smoke-build-helper (context)
  "Build and validate the release native workspace for smoke CONTEXT.
Return the Cargo executable used for the build."
  (let ((cargo
         (or (executable-find "cargo")
             (error "cargo is not available"))))
    (yunge-reader-graphical-smoke-run-process
     context cargo
     (list
      "build" "--release" "--locked" "--workspace"
      "--manifest-path"
      (yunge-reader-graphical-smoke-context-manifest context)
      "--target-dir"
      (yunge-reader-graphical-smoke-context-target-directory context)))
    (unless
        (file-executable-p
         (yunge-reader-graphical-smoke-context-helper context))
      (error
       "Native helper was not built: %s"
       (yunge-reader-graphical-smoke-context-helper context)))
    (unless
        (file-regular-p
         (yunge-reader-graphical-smoke-context-module context))
      (error
       "Native module was not built: %s"
       (yunge-reader-graphical-smoke-context-module context)))
    cargo))

(defun yunge-reader-graphical-smoke-cleanup (context)
  "Remove the system temporary directory owned by smoke CONTEXT."
  (when-let* ((temporary
               (yunge-reader-graphical-smoke-context-temporary-root
                context)))
    (unless (file-in-directory-p temporary temporary-file-directory)
      (error "%s directory escaped system temporary files"
             (yunge-reader-graphical-smoke-context-label context)))
    (when (file-directory-p temporary)
      (delete-directory temporary t))
    (setf
     (yunge-reader-graphical-smoke-context-temporary-root context)
     nil)))

(provide 'yunge-reader-graphical-smoke)

;;; yunge-reader-graphical-smoke.el ends here
