;;; shuying-latex-preamble-test.el --- Shuying LaTeX preamble tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'shuying-latex-preamble)

(defun shuying-latex-preamble-test--spec (source)
  "Return a LaTeX render specification for SOURCE."
  (make-shuying-render-spec
   :source source
   :preamble "\\documentclass{article}\n\\usepackage{color}\n"
   :engine '("latex")
   :backend 'shuying-latex
   :backend-options '(:converter ("dvisvgm"))
   :output-format "svg"
   :foreground "black"
   :background "Transparent"
   :scale 1.0
   :cache-version shuying-cache-format-version))

(ert-deftest shuying-latex-preamble-serializes-warmups-across-preamble-changes ()
  (let* ((root (make-temp-file "shuying-latex-warmup-test-" t))
         (system-type 'windows-nt)
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-precompile-preamble nil)
         (shuying-latex-preamble--warmed-preambles
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmups (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmup-queue nil)
         (shuying-latex-preamble--active-warmup nil)
         (engine
          '("C:/Programs/MiKTeX/miktex/bin/x64/xelatex.exe" "-no-pdf"))
         (first (shuying-latex-preamble-test--spec "$x$"))
         (same-preamble (shuying-latex-preamble-test--spec "$y$"))
         (changed-preamble (shuying-latex-preamble-test--spec "$x$"))
         invocations
         completions)
    (setf (shuying-render-spec-preamble changed-preamble)
          "\\documentclass{article}\n\\usepackage{new-header}\n")
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-process)
              (lambda (&rest arguments)
                (let ((process (make-symbol "warmup-process")))
                  (setq invocations
                        (nconc invocations
                               (list (cons process arguments))))
                  process)))
             ((symbol-function 'process-status)
              (lambda (_process) 'exit))
             ((symbol-function 'process-exit-status)
              (lambda (_process) 0)))
          (shuying-latex-preamble-prepare
           first engine
           (lambda (_key _file error-data)
             (push (cons 'first error-data) completions)))
          (shuying-latex-preamble-prepare
           same-preamble engine
           (lambda (_key _file error-data)
             (push (cons 'same error-data) completions)))
          ;; This represents a changed final preamble after LATEX_HEADER edits.
          (shuying-latex-preamble-prepare
           changed-preamble engine
           (lambda (_key _file error-data)
             (push (cons 'changed error-data) completions)))
          (should (= (length invocations) 1))
          (let* ((first-invocation (car invocations))
                 (arguments (cdr first-invocation))
                 (command (plist-get arguments :command)))
            (should (member "-enable-installer" command))
            (should-not (member "-disable-installer" command))
            (funcall (plist-get arguments :sentinel)
                     (car first-invocation) "finished\n"))
          (should (= (length invocations) 2))
          (should (= (length completions) 2))
          (should (seq-every-p #'null (mapcar #'cdr completions)))
          (let* ((second-invocation (cadr invocations))
                 (arguments (cdr second-invocation)))
            (funcall (plist-get arguments :sentinel)
                     (car second-invocation) "finished\n"))
          (should (= (length completions) 3))
          (should (seq-every-p #'null (mapcar #'cdr completions))))
      (delete-directory root t))))

(ert-deftest shuying-latex-preamble-retries-one-failed-miktex-warmup ()
  (let* ((root (make-temp-file "shuying-latex-warmup-retry-" t))
         (system-type 'windows-nt)
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-precompile-preamble nil)
         (shuying-latex-preamble--warmed-preambles
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmups (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmup-queue nil)
         (shuying-latex-preamble--active-warmup nil)
         (engine
          '("C:/Programs/MiKTeX/miktex/bin/x64/xelatex.exe" "-no-pdf"))
         invocations
         result)
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-process)
              (lambda (&rest arguments)
                (let ((process (make-symbol "warmup-process")))
                  (setq invocations
                        (nconc invocations
                               (list (cons process arguments))))
                  process)))
             ((symbol-function 'process-status)
              (lambda (_process) 'exit))
             ((symbol-function 'process-exit-status)
              (lambda (process)
                (if (eq process (caar invocations)) 1 0))))
          (shuying-latex-preamble-prepare
           (shuying-latex-preamble-test--spec "$x$") engine
           (lambda (_key _file error-data)
             (setq result (or error-data 'success))))
          (let* ((first (car invocations))
                 (sentinel (plist-get (cdr first) :sentinel)))
            (funcall sentinel (car first) "failed\n"))
          (should (= (length invocations) 2))
          (should-not result)
          (let* ((second (cadr invocations))
                 (sentinel (plist-get (cdr second) :sentinel)))
            (funcall sentinel (car second) "finished\n"))
          (should (eq result 'success)))
      (delete-directory root t))))

(ert-deftest shuying-latex-preamble-bounds-failed-miktex-warmup-retries ()
  (let* ((root (make-temp-file "shuying-latex-warmup-failure-" t))
         (system-type 'windows-nt)
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-precompile-preamble nil)
         (shuying-latex-preamble--warmed-preambles
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmups (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmup-queue nil)
         (shuying-latex-preamble--active-warmup nil)
         (engine
          '("C:/Programs/MiKTeX/miktex/bin/x64/xelatex.exe" "-no-pdf"))
         invocations
         result)
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-process)
              (lambda (&rest arguments)
                (let ((process (make-symbol "warmup-process")))
                  (setq invocations
                        (nconc invocations
                               (list (cons process arguments))))
                  process)))
             ((symbol-function 'process-status)
              (lambda (_process) 'exit))
             ((symbol-function 'process-exit-status)
              (lambda (_process) 1)))
          (shuying-latex-preamble-prepare
           (shuying-latex-preamble-test--spec "$x$") engine
           (lambda (_key _file error-data) (setq result error-data)))
          (let* ((first (car invocations))
                 (sentinel (plist-get (cdr first) :sentinel)))
            (funcall sentinel (car first) "failed\n"))
          (let* ((second (cadr invocations))
                 (sentinel (plist-get (cdr second) :sentinel)))
            (funcall sentinel (car second) "failed\n"))
          (should (= (length invocations) 2))
          (should (eq (car result) 'shuying-latex-error)))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p "*Shuying LaTeX warm-up*"
                               (buffer-name buffer))
          (kill-buffer buffer)))
      (delete-directory root t))))

(ert-deftest shuying-latex-preamble-disables-miktex-installer-for-format-builds ()
  (let* ((root (make-temp-file "shuying-latex-format-policy-" t))
         (system-type 'windows-nt)
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory (expand-file-name "formats" root))
         (shuying-latex-preamble--format-builds
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--failed-formats
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmed-preambles
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmups (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmup-queue nil)
         (shuying-latex-preamble--active-warmup nil)
         (engine
          '("C:/Programs/MiKTeX/miktex/bin/x64/latex.exe"))
         (specification (shuying-latex-preamble-test--spec "$x$"))
         invocations)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (push arguments invocations)
                     'format-process))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 0)))
          (shuying-latex-preamble-prepare specification engine #'ignore)
          (funcall (plist-get (car invocations) :sentinel)
                   'format-process "finished\n")
          (let ((command (plist-get (car invocations) :command)))
            (should (member "-disable-installer" command))
            (should-not (member "-enable-installer" command))))
      (dolist (arguments invocations)
        (when-let* ((buffer (plist-get arguments :buffer)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))))
      (delete-directory root t))))

(ert-deftest shuying-latex-preamble-selects-the-pgf-driver-for-its-converter ()
  (let ((specification (shuying-latex-preamble-test--spec "$x$")))
    (setf (shuying-render-spec-preamble specification)
          "\\documentclass{article}\n\\usepackage{tikz-cd}\n")
    (with-temp-buffer
      (shuying-latex-preamble-insert specification)
      (should
       (string-prefix-p
        "\\def\\pgfsysdriver{pgfsys-dvisvgm.def}\n"
        (buffer-string)))
      (should
       (< (string-match-p "pgfsys-dvisvgm" (buffer-string))
          (string-match-p "usepackage{tikz-cd}" (buffer-string)))))
    (setf (shuying-render-spec-backend-options specification)
          '(:converter ("other-converter")))
    (with-temp-buffer
      (shuying-latex-preamble-insert specification)
      (should-not (search-forward "pgfsysdriver" nil t)))))

(ert-deftest shuying-latex-preamble-shares-a-pending-format-build ()
  (let* ((root (make-temp-file "shuying-latex-format-sharing-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory (expand-file-name "formats" root))
         (shuying-latex-preamble--format-builds (make-hash-table :test #'equal))
         (shuying-latex-preamble--failed-formats (make-hash-table :test #'equal))
         (specification (shuying-latex-preamble-test--spec "$x$"))
         invocation
         (starts 0)
         completions)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (setq invocation (cons default-directory arguments))
                     (cl-incf starts)
                     'format-process))
                  ((symbol-function 'process-status) (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_process) 0)))
          (dotimes (_ 2)
            (shuying-latex-preamble-prepare
             specification '("latex")
             (lambda (_key file error-data)
               (push (cons file error-data) completions))))
          (should (= starts 1))
          (should-not completions)
          (let* ((directory (car invocation))
                 (arguments (cdr invocation))
                 (jobname
                  (seq-find (lambda (item) (string-prefix-p "-jobname=" item))
                            (plist-get arguments :command))))
            (with-temp-file
                (expand-file-name (concat (substring jobname 9) ".fmt") directory)
              (insert "format"))
            (funcall (plist-get arguments :sentinel)
                     'format-process "finished\n"))
          (should (= (length completions) 2))
          (should (equal (caar completions) (caadr completions)))
          (should (file-exists-p (caar completions)))
          (should (seq-every-p (lambda (result) (null (cdr result))) completions)))
      (delete-directory root t))))

(ert-deftest shuying-latex-preamble-falls-back-after-a-format-build-fails ()
  (let* ((root (make-temp-file "shuying-latex-format-failure-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory (expand-file-name "formats" root))
         (shuying-latex-preamble--format-builds (make-hash-table :test #'equal))
         (shuying-latex-preamble--failed-formats (make-hash-table :test #'equal))
         (specification (shuying-latex-preamble-test--spec "$x$"))
         invocation
         (starts 0)
         completions)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (setq invocation arguments)
                     (cl-incf starts)
                     'format-process))
                  ((symbol-function 'process-status) (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_process) 1))
                  ((symbol-function 'display-warning) #'ignore))
          (shuying-latex-preamble-prepare
           specification '("latex")
           (lambda (_key file error-data)
             (push (list file error-data) completions)))
          (funcall (plist-get invocation :sentinel)
                   'format-process "failed\n")
          (shuying-latex-preamble-prepare
           specification '("latex")
           (lambda (_key file error-data)
             (push (list file error-data) completions)))
          (should (= starts 1))
          (should (equal completions '((nil nil) (nil nil)))))
      (delete-directory root t))))

(ert-deftest shuying-latex-preamble-falls-back-when-format-process-cannot-start ()
  (let* ((root (make-temp-file "shuying-latex-format-start-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory (expand-file-name "formats" root))
         (shuying-latex-preamble--format-builds (make-hash-table :test #'equal))
         (shuying-latex-preamble--failed-formats (make-hash-table :test #'equal))
         completed)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _arguments)
                     (error "Could not start LaTeX")))
                  ((symbol-function 'display-warning) #'ignore))
          (shuying-latex-preamble-prepare
           (shuying-latex-preamble-test--spec "$x$") '("latex")
           (lambda (_key file error-data)
             (setq completed (list file error-data))))
          (should (equal completed '(nil nil)))
          (should-not (directory-files shuying-work-directory nil
                                       directory-files-no-dot-files-regexp)))
      (delete-directory root t))))

(ert-deftest shuying-latex-preamble-notifies-all-format-waiters-after-a-callback-error ()
  (let* ((root (make-temp-file "shuying-latex-waiters-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory (expand-file-name "formats" root))
         (shuying-latex-preamble--format-builds (make-hash-table :test #'equal))
         (shuying-latex-preamble--failed-formats (make-hash-table :test #'equal))
         (specification (shuying-latex-preamble-test--spec "$x$"))
         invocation
         prepared)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (setq invocation
                           (cons default-directory arguments))
                     'format-process))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 0))
                  ((symbol-function 'display-warning) #'ignore))
          (shuying-latex-preamble-prepare
           specification '("latex")
           (lambda (_key _file _error-data)
             (error "A consumer failed")))
          (shuying-latex-preamble-prepare
           specification '("latex")
           (lambda (_key file error-data)
             (setq prepared (cons file error-data))))
          (let* ((directory (car invocation))
                 (arguments (cdr invocation))
                 (jobname
                  (seq-find (lambda (item) (string-prefix-p "-jobname=" item))
                            (plist-get arguments :command))))
            (with-temp-file
                (expand-file-name (concat (substring jobname 9) ".fmt")
                                  directory)
              (insert "format"))
            (funcall (plist-get arguments :sentinel)
                     'format-process "finished\n"))
          (should (and prepared (car prepared)
                       (file-exists-p (car prepared))))
          (should-not (cdr prepared)))
      (delete-directory root t))))

(provide 'shuying-latex-preamble-test)

;;; shuying-latex-preamble-test.el ends here
