;;; shuying-latex-test.el --- Shuying LaTeX tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'shuying-latex)

(defun shuying-latex-test--spec (source)
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

(ert-deftest shuying-latex-batch-key-excludes-fragment-inputs ()
  (let ((first (shuying-latex-test--spec "$x$"))
        (second (shuying-latex-test--spec "$y$")))
    (setf (shuying-render-spec-equation-number first) 1)
    (setf (shuying-render-spec-equation-number second) 2)
    (should
     (equal (shuying-latex-batch-key first)
            (shuying-latex-batch-key second)))
    (setf (shuying-render-spec-scale second) 2.0)
    (should-not
     (equal (shuying-latex-batch-key first)
            (shuying-latex-batch-key second)))))

(ert-deftest shuying-latex-renders-a-batch-with-two-processes ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (outputs (list (make-temp-file "shuying-latex-output-")
                        (make-temp-file "shuying-latex-output-")))
         (requests
          (cl-mapcar
           (lambda (source output)
             (make-shuying-backend-request
              :specification (shuying-latex-test--spec source)
              :output-file output))
           '("$x$" "$y$") outputs))
         invocations
         process-directories
         results)
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-process)
              (lambda (&rest arguments)
                (let ((process (make-symbol "process")))
                  (push default-directory process-directories)
                  (push (cons process arguments) invocations)
                  process)))
             ((symbol-function 'process-status)
              (lambda (_process) 'exit))
             ((symbol-function 'process-exit-status)
              (lambda (_process) 0)))
          (shuying-latex-render-batch
           requests
           (lambda (request error-data)
             (push (cons request error-data) results)))
          (should (= (length invocations) 1))
          (should (= (length process-directories) 1))
          (should-not results)
          (let* ((latex-invocation (car invocations))
                 (latex-arguments (cdr latex-invocation))
                 (latex-command (plist-get latex-arguments :command))
                 (directory
                  (file-name-directory (car (last latex-command))))
                 (latex-sentinel
                  (plist-get latex-arguments :sentinel)))
            (with-temp-file (expand-file-name "input.dvi" directory))
            (funcall latex-sentinel (car latex-invocation) "finished\n")
            (should (= (length invocations) 2))
            (should (seq-every-p
                     (lambda (process-directory)
                       (equal process-directory directory))
                     process-directories))
            (should-not results)
            (let* ((converter-invocation (car invocations))
                   (converter-arguments (cdr converter-invocation))
                   (converter-sentinel
                    (plist-get converter-arguments :sentinel)))
              (with-temp-file (expand-file-name "page-1.svg" directory)
                (insert "first"))
              (with-temp-file (expand-file-name "page-2.svg" directory)
                (insert "second"))
              (funcall converter-sentinel
                       (car converter-invocation) "finished\n")
              (should (= (length results) 2))
              (should (seq-every-p #'null (mapcar #'cdr results)))
              (should-not (file-directory-p directory))
              (should-not
               (buffer-live-p
                (plist-get latex-arguments :buffer)))))
          (should
           (equal
            (mapcar
             (lambda (file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
             outputs)
            '("first" "second"))))
      (dolist (file outputs)
        (when (file-exists-p file)
          (delete-file file)))
      (delete-directory root t))))

(ert-deftest shuying-latex-renders-svg-pages-across-number-widths ()
  (unless (and (executable-find "latex")
               (executable-find "dvisvgm")
               (executable-find "kpsewhich")
               (= (call-process "kpsewhich" nil nil nil "preview.sty")
                  0))
    (ert-skip "The LaTeX preview toolchain is unavailable"))
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (default-directory
          (if (and (eq system-type 'windows-nt)
                   (file-directory-p "D:/"))
              "D:/"
            default-directory))
         (shuying-cache-directory (expand-file-name "cache" root))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (original-make-process (symbol-function 'make-process))
         (process-count 0)
         results)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-latex-render-batch
           #'shuying-latex-batch-key)
          (cl-letf
              (((symbol-function 'make-process)
                (lambda (&rest arguments)
                  (cl-incf process-count)
                  (apply original-make-process arguments))))
            (shuying-render-batch
             (mapcar
              (lambda (number)
                (cons
                 (shuying-latex-test--spec
                  (format "$x_{%d}$" number))
                 (lambda (artifact error-data)
                   (push (cons artifact error-data) results))))
              (number-sequence 1 12)))
            (should-not results)
            (with-timeout
                (30 (ert-fail "Timed out rendering LaTeX previews"))
              (while (< (length results) 12)
                (accept-process-output nil 0.05))))
          (should (= process-count 2))
          (should (seq-every-p #'null (mapcar #'cdr results)))
          (dolist (result results)
            (should (file-exists-p (car result)))
            (with-temp-buffer
              (insert-file-contents (car result))
              (should (search-forward "<svg" nil t)))))
      (delete-directory root t))))

;;; shuying-latex-test.el ends here
