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

(ert-deftest shuying-latex-keeps-formats-under-var ()
  (should
   (equal shuying-latex-format-directory
          (yunge-var-subdirectory "shuying/formats"))))

(ert-deftest shuying-latex-selects-the-engine-intermediate-format ()
  (should
   (equal (shuying-latex--intermediate-extension
           '("C:/tex/xelatex.exe" "-no-pdf"))
          "xdv"))
  (should
   (equal (shuying-latex--intermediate-extension '("latex")) "dvi")))

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

(ert-deftest shuying-latex-format-key-covers-only-format-inputs ()
  (let* ((first (shuying-latex-test--spec "$x$"))
         (second (shuying-latex-test--spec "$y$"))
         (engine '("latex")))
    (should
     (equal (shuying-latex--format-key first engine)
            (shuying-latex--format-key second engine)))
    (setf (shuying-render-spec-preamble second) "other preamble")
    (should-not
     (equal (shuying-latex--format-key first engine)
            (shuying-latex--format-key second engine)))
    (setf (shuying-render-spec-preamble second)
          (shuying-render-spec-preamble first))
    (setf (shuying-render-spec-backend-options second)
          '(:converter ("other-converter")))
    (should-not
     (equal (shuying-latex--format-key first engine)
            (shuying-latex--format-key second engine)))
    (should-not
     (equal (shuying-latex--format-key first engine)
            (shuying-latex--format-key first '("other-latex"))))))

(ert-deftest shuying-latex-selects-the-pgf-driver-for-its-converter ()
  (let ((specification (shuying-latex-test--spec "$x$")))
    (setf (shuying-render-spec-preamble specification)
          "\\documentclass{article}\n\\usepackage{tikz-cd}\n")
    (with-temp-buffer
      (shuying-latex--write-preamble specification)
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
      (shuying-latex--write-preamble specification)
      (should-not (search-forward "pgfsysdriver" nil t)))))

(ert-deftest shuying-latex-uses-a-cache-hit-without-the-toolchain ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (specification (shuying-latex-test--spec "$x$"))
         (artifact-file (shuying-artifact-file specification))
         (metadata-file
          (shuying--artifact-metadata-file artifact-file))
         result)
    (unwind-protect
        (progn
          (with-temp-file artifact-file
            (insert "cached"))
          (with-temp-file metadata-file
            (insert "(:height 1.0 :depth 0.0)"))
          (shuying-register-backend
           'shuying-latex
           #'shuying-latex-render-batch
           #'shuying-latex-batch-key)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (&rest _arguments)
                       (ert-fail "A cache hit checked the toolchain"))))
            (shuying-render
             specification
             (lambda (artifact error-data)
               (setq result (cons artifact error-data)))))
          (should-not (cdr result))
          (should
           (equal (shuying-artifact-path (car result)) artifact-file)))
      (delete-directory root t))))

(ert-deftest shuying-latex-rejects-a-missing-engine-before-starting-work ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-cache-directory (expand-file-name "cache" root))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying--waiting-batches nil)
         (shuying--active-batch-count 0)
         (shuying--scheduler-running nil)
         process-started
         result)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-latex-render-batch
           #'shuying-latex-batch-key)
          (cl-letf (((symbol-function 'executable-find) #'ignore)
                    ((symbol-function 'make-process)
                     (lambda (&rest _arguments)
                       (setq process-started t)
                       (ert-fail "A process was started without LaTeX"))))
            (shuying-render
             (shuying-latex-test--spec "$x$")
             (lambda (artifact error-data)
               (setq result (list artifact error-data)))))
          (should-not (car result))
          (should
           (eq (car (cadr result)) 'shuying-latex-unavailable))
          (should
           (string-match-p
            "LaTeX engine executable not found: latex"
            (error-message-string (cadr result))))
          (should
           (string-match-p
            (if (eq system-type 'windows-nt)
                "run M-x shuying-setup"
              "install it or add it to exec-path")
            (error-message-string (cadr result))))
          (should-not process-started)
          (should-not (file-exists-p shuying-work-directory))
          (should (zerop (hash-table-count shuying--pending-jobs))))
      (delete-directory root t))))

(ert-deftest shuying-latex-rejects-a-missing-converter-before-compiling ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-cache-directory (expand-file-name "cache" root))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying--waiting-batches nil)
         (shuying--active-batch-count 0)
         (shuying--scheduler-running nil)
         process-started
         result)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-latex-render-batch
           #'shuying-latex-batch-key)
          (cl-letf
              (((symbol-function 'executable-find)
                (lambda (program)
                  (and (equal program "latex") "C:/tex/latex.exe")))
               ((symbol-function 'make-process)
                (lambda (&rest _arguments)
                  (setq process-started t)
                  (ert-fail "LaTeX started without dvisvgm"))))
            (shuying-render
             (shuying-latex-test--spec "$x$")
             (lambda (artifact error-data)
               (setq result (list artifact error-data)))))
          (should-not (car result))
          (should
           (eq (car (cadr result)) 'shuying-latex-unavailable))
          (should
           (string-match-p
            "LaTeX converter executable not found: dvisvgm"
            (error-message-string (cadr result))))
          (should-not process-started)
          (should-not (file-exists-p shuying-work-directory)))
      (delete-directory root t))))

(ert-deftest shuying-latex-extracts-unscaled-geometry-in-em-units ()
  (with-temp-buffer
    (insert
     "  width=11.9pt, height=8.16pt, depth=.85pt\n"
     "  width=19.04pt, height=14.79pt, depth=7.48pt\n")
    (let ((geometries
           (shuying-latex--page-geometries
            (current-buffer) 10.0 1.7)))
      (should (= (length geometries) 2))
      (should (< (abs (- (plist-get (car geometries) :width) 0.7))
                 0.0001))
      (should (< (abs (- (plist-get (car geometries) :height) 0.53))
                 0.0001))
      (should (< (abs (- (plist-get (car geometries) :depth) 0.05))
                 0.0001))
      (should (< (abs (- (plist-get (cadr geometries) :width) 1.12))
                 0.0001))
      (should (< (abs (- (plist-get (cadr geometries) :height) 1.31))
                 0.0001))
      (should (< (abs (- (plist-get (cadr geometries) :depth) 0.44))
                 0.0001)))))

(ert-deftest shuying-latex-extracts-xdv-geometry-from-preview-output ()
  (with-temp-buffer
    (insert
     "! Preview: Snippet 1 started.\n"
     "! Preview: Snippet 1 ended.(524288+131072x983040).\n")
    (let ((geometry
           (car (shuying-latex--page-geometries
                 (current-buffer) 10.0 1.7))))
      (should (< (abs (- (plist-get geometry :width) 1.5)) 0.0001))
      (should (< (abs (- (plist-get geometry :height) 1.0)) 0.0001))
      (should (< (abs (- (plist-get geometry :depth) 0.2)) 0.0001)))))

(ert-deftest shuying-latex-combines-xdv-tight-size-and-baseline ()
  (with-temp-buffer
    (insert
     "! Preview: Snippet 1 started.\n"
     "! Preview: Snippet 1 ended.(524288+131072x983040).\n"
     "  graphic size: 17pt x 8.5pt (16bp x 8bp)\n")
    (let ((geometry
           (car (shuying-latex--page-geometries
                 (current-buffer) 10.0 1.7))))
      (should (< (abs (- (plist-get geometry :width) 1.0)) 0.0001))
      (should (< (abs (- (plist-get geometry :height) 0.5)) 0.0001))
      (should (< (abs (- (plist-get geometry :depth) 0.1)) 0.0001)))))

(ert-deftest shuying-latex-rejects-every-errored-svg-page ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (directory (expand-file-name "work" root))
         (log-buffer (generate-new-buffer " *Shuying LaTeX test*"))
         (first-output (expand-file-name "first.svg" root))
         (second-output (expand-file-name "second.svg" root))
         (first
          (make-shuying-backend-request
           :specification (shuying-latex-test--spec "$x$")
           :output-file first-output))
         (second
          (make-shuying-backend-request
           :specification (shuying-latex-test--spec "$y$")
           :output-file second-output))
         results
         (batch
          (make-shuying-latex--batch
           :requests (list first second)
           :complete
           (lambda (request error-data)
             (push (cons request error-data) results))
           :directory directory
           :log-buffer log-buffer)))
    (make-directory directory)
    (with-temp-file (expand-file-name "page-1.svg" directory)
      (insert "<svg><g id='page1'><path d='partial'/></g></svg>"))
    (with-temp-file (expand-file-name "page-2.svg" directory)
      (insert "<svg><g id='page2'/></svg>"))
    (with-current-buffer log-buffer
      (insert
       "Preview: Fontsize 10pt\n"
       "! Preview: Snippet 1 started.\n"
       "! LaTeX Error: invalid fragment.\n"
       "! Preview: Snippet 1 ended.\n"
       "  width=1pt, height=.5pt, depth=.5pt\n"
       "! Preview: Snippet 2 started.\n"
       "! Preview: Snippet 2 ended.\n"
       "  width=20pt, height=8pt, depth=2pt\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'process-status)
                   (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) 0)))
          (shuying-latex--finish-conversion batch 'dvisvgm-process)
          (let ((first-result (assq first results))
                (second-result (assq second results)))
            (should
             (eq (car (cdr first-result)) 'shuying-latex-error))
            (should
             (string-match-p
              "error on preview page 1"
              (error-message-string (cdr first-result))))
            (should-not (file-exists-p first-output))
            (should-not (cdr second-result))
            (should (file-exists-p second-output))
            (should
             (= (plist-get
                 (shuying-backend-request-metadata second)
                 :width)
                2.0))))
      (when (buffer-live-p log-buffer)
        (kill-buffer log-buffer))
      (delete-directory root t))))

(ert-deftest shuying-latex-shares-a-pending-format-build ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory
          (expand-file-name "formats" root))
         (shuying-latex--format-builds
          (make-hash-table :test #'equal))
         (shuying-latex--failed-formats
          (make-hash-table :test #'equal))
         (specification (shuying-latex-test--spec "$x$"))
         invocation
         formats)
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-process)
              (lambda (&rest arguments)
                (setq invocation arguments)
                'format-process))
             ((symbol-function 'process-status)
              (lambda (_process) 'exit))
             ((symbol-function 'process-exit-status)
              (lambda (_process) 0)))
          (dotimes (_ 2)
            (shuying-latex--ensure-format
             specification '("latex")
             (lambda (_key format-file)
               (push format-file formats))))
          (should invocation)
          (should-not formats)
          (let* ((sentinel (plist-get invocation :sentinel))
                 (key
                  (shuying-latex--format-key
                   specification '("latex")))
                 (build
                  (gethash key shuying-latex--format-builds)))
            (with-temp-file
                (shuying-latex--format-build-built-file build))
            (funcall sentinel 'format-process "finished\n")
            (should (= (length formats) 2))
            (should (equal (car formats) (cadr formats)))
            (should (file-exists-p (car formats)))
            (should-not (gethash key shuying-latex--format-builds))))
      (delete-directory root t))))

(ert-deftest shuying-latex-falls-back-after-a-format-build-fails ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory
          (expand-file-name "formats" root))
         (shuying-latex--format-builds
          (make-hash-table :test #'equal))
         (shuying-latex--failed-formats
          (make-hash-table :test #'equal))
         (specification (shuying-latex-test--spec "$x$"))
         invocation
         formats)
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-process)
              (lambda (&rest arguments)
                (setq invocation arguments)
                'format-process))
             ((symbol-function 'process-status)
              (lambda (_process) 'exit))
             ((symbol-function 'process-exit-status)
              (lambda (_process) 1))
             ((symbol-function 'display-warning) #'ignore))
          (shuying-latex--ensure-format
           specification '("latex")
           (lambda (_key format-file)
             (push format-file formats)))
          (funcall
           (plist-get invocation :sentinel)
           'format-process "failed\n")
          (should (equal formats '(nil)))
          (should (= (hash-table-count
                      shuying-latex--failed-formats)
                     1))
          (setq formats nil invocation nil)
          (shuying-latex--ensure-format
           specification '("latex")
           (lambda (_key format-file)
             (push format-file formats)))
          (should-not invocation)
          (should (equal formats '(nil))))
      (delete-directory root t))))

(ert-deftest shuying-latex-falls-back-when-format-process-cannot-start ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory
          (expand-file-name "formats" root))
         (shuying-latex--format-builds
          (make-hash-table :test #'equal))
         (shuying-latex--failed-formats
          (make-hash-table :test #'equal))
         format)
    (unwind-protect
        (cl-letf
            (((symbol-function 'make-process)
              (lambda (&rest _arguments)
                (error "Could not start LaTeX")))
             ((symbol-function 'display-warning) #'ignore))
          (shuying-latex--ensure-format
           (shuying-latex-test--spec "$x$") '("latex")
           (lambda (_key format-file)
             (setq format format-file)))
          (should-not format)
          (should (= (hash-table-count
                      shuying-latex--failed-formats)
                     1))
          (should (= (hash-table-count
                      shuying-latex--format-builds)
                     0)))
      (delete-directory root t))))

(ert-deftest shuying-latex-invalidates-a-format-after-fallback-succeeds ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (directory (expand-file-name "work" root))
         (tex-file (expand-file-name "input.tex" directory))
         (format-file (expand-file-name "preamble.fmt" root))
         (log-buffer (generate-new-buffer " *Shuying LaTeX test*"))
         (shuying-latex--failed-formats
          (make-hash-table :test #'equal))
         errors
         (request
          (make-shuying-backend-request
           :specification (shuying-latex-test--spec "$x$")))
         (batch
          (make-shuying-latex--batch
           :requests (list request)
           :complete
           (lambda (_request error-data)
             (push error-data errors))
           :directory directory
           :log-buffer log-buffer
           :tex-file tex-file
           :intermediate-file (expand-file-name "input.dvi" directory)
           :engine '("latex")
           :format-key "format-key"
           :format-file format-file))
         (starts 0)
         (conversions 0))
    (make-directory directory t)
    (with-temp-file format-file)
    (unwind-protect
        (cl-letf
            (((symbol-function 'process-status)
              (lambda (_process) 'exit))
             ((symbol-function 'process-exit-status)
              (lambda (_process) 1))
             ((symbol-function 'shuying-latex--start-compiler)
              (lambda (_batch _specification)
                (cl-incf starts)))
             ((symbol-function 'shuying-latex--start-converter)
              (lambda (_batch _specification)
                (cl-incf conversions))))
          (shuying-latex--compilation-sentinel
           batch
           (shuying-backend-request-specification request)
           'latex-process "finished\n")
          (should-not errors)
          (should (= starts 1))
          (should-not (shuying-latex--batch-format-file batch))
          (should
           (equal
            (shuying-latex--batch-suspect-format-file batch)
            format-file))
          (should (file-exists-p format-file))
          (should-not
           (gethash "format-key" shuying-latex--failed-formats))
          (with-temp-buffer
            (insert-file-contents tex-file)
            (should (search-forward "\\documentclass" nil t)))
          (with-temp-file
              (shuying-latex--batch-intermediate-file batch))
          (shuying-latex--compilation-sentinel
           batch
           (shuying-backend-request-specification request)
           'latex-process "finished\n")
          (should (= conversions 1))
          (should-not (file-exists-p format-file))
          (should
           (gethash "format-key" shuying-latex--failed-formats)))
      (when (buffer-live-p log-buffer)
        (kill-buffer log-buffer))
      (delete-directory root t))))

(ert-deftest shuying-latex-sets-the-equation-counter-before-a-fragment ()
  (let ((specification
         (shuying-latex-test--spec
          "\\begin{equation}x = y\\end{equation}")))
    (setf (shuying-render-spec-equation-number specification) 7)
    (with-temp-buffer
      (shuying-latex--insert-fragment specification)
      (should
       (equal
        (buffer-string)
        (concat
         "\n\\begin{preview}\n"
         "\\setcounter{equation}{6}\n"
         "\\begin{equation}x = y\\end{equation}"
         "\n\\end{preview}\n"))))))

(ert-deftest shuying-latex-renders-a-batch-with-two-processes ()
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-precompile-preamble nil)
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
            (((symbol-function 'executable-find)
              (lambda (program)
                (expand-file-name
                 (concat program ".exe") "C:/tex")))
             ((symbol-function 'make-process)
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
                   (converter-command
                    (plist-get converter-arguments :command))
                   (converter-buffer
                    (plist-get converter-arguments :buffer))
                   (converter-sentinel
                    (plist-get converter-arguments :sentinel)))
              (should (member "--bbox=min" converter-command))
              (should
               (member "--currentcolor=#000000" converter-command))
              (should-not (member "--bbox=preview" converter-command))
              (should (equal (car (last converter-command))
                             (expand-file-name "input.dvi" directory)))
              (with-temp-file (expand-file-name "page-1.svg" directory)
                (insert "first"))
              (with-temp-file (expand-file-name "page-2.svg" directory)
                (insert "second"))
              (with-current-buffer converter-buffer
                (insert
                 "Preview: Fontsize 10pt\n"
                 "  width=10pt, height=8pt, depth=2pt\n"
                 "  width=20pt, height=9pt, depth=3pt\n"))
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
         (shuying-latex-precompile-preamble nil)
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
            (should
             (file-exists-p
              (shuying-artifact-path (car result))))
            (should (plist-get
                     (shuying-artifact-metadata (car result))
                     :height))
            (with-temp-buffer
              (insert-file-contents
               (shuying-artifact-path (car result)))
              (should (search-forward "<svg" nil t)))))
      (delete-directory root t))))

(ert-deftest shuying-latex-renders-tikz-paths-from-xdv ()
  (unless (and (executable-find "xelatex")
               (executable-find "dvisvgm")
               (executable-find "kpsewhich")
               (= (call-process "kpsewhich" nil nil nil "preview.sty")
                  0)
               (= (call-process "kpsewhich" nil nil nil "tikz-cd.sty")
                  0)
               (= (call-process
                   "kpsewhich" nil nil nil "pgfsys-dvisvgm.def")
                  0))
    (ert-skip "The XeLaTeX TikZ preview toolchain is unavailable"))
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (specification
          (shuying-latex-test--spec
           (concat
            "\\[\\begin{tikzcd}\n"
            "G \\arrow[r, \"\\varphi\"] "
            "\\arrow[d, \"p\"'] & G' \\\\\n"
            "G/K \\arrow[ur, \"\\widetilde{\\varphi}\"']\n"
            "\\end{tikzcd}\\]")))
         (output (expand-file-name "tikz.svg" root))
         (request
          (make-shuying-backend-request
           :specification specification
           :output-file output))
         done
         error-data)
    (setf (shuying-render-spec-preamble specification)
          "\\documentclass{article}\n\\usepackage{tikz-cd}\n"
          (shuying-render-spec-engine specification)
          '("xelatex" "-no-pdf"))
    (unwind-protect
        (progn
          (shuying-latex-render-batch
           (list request)
           (lambda (_request error)
             (setq done t
                   error-data error)))
          (with-timeout
              (30 (ert-fail "Timed out rendering a TikZ preview"))
            (while (not done)
              (accept-process-output nil 0.05)))
          (should-not error-data)
          (should (file-exists-p output))
          (with-temp-buffer
            (insert-file-contents output)
            (should (search-forward "</defs>" nil t))
            ;; Font outlines live in `defs'.  A path after it is actual TikZ
            ;; drawing output rather than one of the diagram's text glyphs.
            (should (re-search-forward "<path\\(?:[[:space:]]\\|>\\)" nil t))))
      (delete-directory root t))))

(ert-deftest shuying-latex-renders-equations-from-their-document-numbers ()
  (unless (and (executable-find "latex")
               (executable-find "dvisvgm")
               (executable-find "kpsewhich")
               (= (call-process "kpsewhich" nil nil nil "preview.sty")
                  0))
    (ert-skip "The LaTeX preview toolchain is unavailable"))
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-cache-directory (expand-file-name "cache" root))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-precompile-preamble nil)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (first
          (shuying-latex-test--spec
           "\\begin{equation}x = y\\end{equation}"))
         (second
          (shuying-latex-test--spec
           "\\begin{equation}x = y\\end{equation}"))
         results)
    (setf (shuying-render-spec-equation-number first) 2
          (shuying-render-spec-equation-number second) 11)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-latex-render-batch
           #'shuying-latex-batch-key)
          (shuying-render-batch
           (cl-mapcar
            (lambda (number specification)
              (cons specification
                    (lambda (artifact error-data)
                      (push (list number artifact error-data)
                            results))))
            '(2 11) (list first second)))
          (with-timeout
              (30 (ert-fail "Timed out rendering numbered equations"))
            (while (< (length results) 2)
              (accept-process-output nil 0.05)))
          (should (seq-every-p #'null (mapcar #'caddr results)))
          (let ((images
                 (mapcar
                  (lambda (result)
                    (with-temp-buffer
                      (insert-file-contents
                       (shuying-artifact-path (cadr result)))
                      (cons
                       (car result)
                       (replace-regexp-in-string
                        "id='page[0-9]+'" "id='page'"
                        (buffer-string)))))
                  results)))
            ;; These specifications differ only in numbering context, so
            ;; different SVGs confirm that LaTeX used the supplied counter.
            (should-not
             (equal (alist-get 2 images) (alist-get 11 images)))))
      (delete-directory root t))))

(ert-deftest shuying-latex-precompiles-and-reuses-a-preamble ()
  (unless (and (executable-find "latex")
               (executable-find "dvisvgm")
               (executable-find "kpsewhich")
               (= (call-process
                   "kpsewhich" nil nil nil "mylatexformat.ltx")
                  0))
    (ert-skip "The LaTeX precompile toolchain is unavailable"))
  (let* ((root (make-temp-file "shuying-latex-test-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory
          (expand-file-name "formats" root))
         (shuying-latex-precompile-preamble t)
         (shuying-latex--format-builds
          (make-hash-table :test #'equal))
         (shuying-latex--failed-formats
          (make-hash-table :test #'equal))
         (original-make-process (symbol-function 'make-process))
         (process-count 0)
         results)
    (unwind-protect
        (cl-labels
            ((render
              (source name)
              (let* ((output (expand-file-name name root))
                     (request
                      (make-shuying-backend-request
                       :specification
                       (shuying-latex-test--spec source)
                       :output-file output)))
                (shuying-latex-render-batch
                 (list request)
                 (lambda (_request error-data)
                   (push (cons output error-data) results))))))
          (cl-letf
              (((symbol-function 'make-process)
                (lambda (&rest arguments)
                  (cl-incf process-count)
                  (apply original-make-process arguments))))
            (render "$x$" "first.svg")
            (with-timeout
                (30 (ert-fail "Timed out precompiling a LaTeX preamble"))
              (while (< (length results) 1)
                (accept-process-output nil 0.05)))
            (should (= process-count 3))
            (should (= (length
                        (directory-files
                         shuying-latex-format-directory nil "\\.fmt\\'"))
                       1))
            (render "$y$" "second.svg")
            (with-timeout
                (30 (ert-fail "Timed out reusing a LaTeX preamble"))
              (while (< (length results) 2)
                (accept-process-output nil 0.05)))
            (should (= process-count 5))
            (should (seq-every-p #'null (mapcar #'cdr results)))
            (dolist (result results)
              (should (file-exists-p (car result)))
              (with-temp-buffer
                (insert-file-contents (car result))
                (should (search-forward "<svg" nil t))))))
      (delete-directory root t))))

;;; shuying-latex-test.el ends here
