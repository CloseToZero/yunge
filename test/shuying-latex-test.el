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

(ert-deftest shuying-latex-disables-miktex-installer-for-rendering ()
  (let* ((root (make-temp-file "shuying-latex-installer-test-" t))
         (system-type 'windows-nt)
         (directory (expand-file-name "work" root))
         (log-buffer (generate-new-buffer " *Shuying installer test*"))
         (engine
          '("C:/Programs/MiKTeX/miktex/bin/x64/xelatex.exe" "-no-pdf"))
         (specification (shuying-latex-test--spec "$x$"))
         (batch
          (make-shuying-latex--batch
           :directory directory
           :log-buffer log-buffer
           :tex-file (expand-file-name "input.tex" directory)
           :engine engine))
         invocation)
    (make-directory directory t)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (setq invocation arguments)
                     'latex-process)))
          (shuying-latex--start-compiler batch specification)
          (let ((command (plist-get invocation :command)))
            (should (member "-disable-installer" command))
            (should-not (member "-enable-installer" command))))
      (when (buffer-live-p log-buffer)
        (kill-buffer log-buffer))
      (delete-directory root t))))

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

(ert-deftest shuying-latex-cleans-an-unprepared-format-build ()
  (let* ((root (make-temp-file "shuying-latex-prepare-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory (expand-file-name "formats" root))
         (shuying-latex-preamble--format-builds (make-hash-table :test #'equal))
         (shuying-latex-preamble--failed-formats (make-hash-table :test #'equal))
         (request
          (make-shuying-backend-request
           :specification (shuying-latex-test--spec "$x$")
           :output-file (expand-file-name "out.svg" root)))
         (write-region (symbol-function 'write-region))
         results)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (program) program))
                  ((symbol-function 'write-region)
                   (lambda (start end filename &rest arguments)
                     (if (and (equal (file-name-nondirectory filename)
                                     "preamble.tex")
                              (string-match-p "format-" filename))
                         (error "Cannot prepare a format source")
                       (apply write-region start end filename arguments)))))
          (shuying-latex-render-batch
           (list request)
           (lambda (_request error-data) (push error-data results)))
          (should (= (length results) 1))
          (should (car results))
          (should-not (directory-files shuying-work-directory nil
                                       directory-files-no-dot-files-regexp)))
      (delete-directory root t))))

(ert-deftest shuying-latex-invalidates-a-format-after-fallback-succeeds ()
  (let* ((root (make-temp-file "shuying-latex-fallback-" t))
         (shuying-work-directory (expand-file-name "work" root))
         (shuying-latex-format-directory (expand-file-name "formats" root))
         (shuying-latex-preamble--format-builds (make-hash-table :test #'equal))
         (shuying-latex-preamble--failed-formats (make-hash-table :test #'equal))
         (output (expand-file-name "result.svg" root))
         (request
          (make-shuying-backend-request
           :specification (shuying-latex-test--spec "$x$")
           :output-file output))
         invocations
         completions)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (program) (expand-file-name program "C:/tex")))
                  ((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (let ((process (make-symbol "latex-process")))
                       (setq invocations
                             (nconc invocations
                                    (list (cons process
                                                (cons default-directory arguments)))))
                       process)))
                  ((symbol-function 'process-status) (lambda (_process) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_process) 0)))
          (shuying-latex-render-batch
           (list request)
           (lambda (_request error-data) (push error-data completions)))
          (let* ((format-invocation (car invocations))
                 (directory (cadr format-invocation))
                 (arguments (cddr format-invocation))
                 (jobname
                  (seq-find (lambda (item) (string-prefix-p "-jobname=" item))
                            (plist-get arguments :command))))
            (with-temp-file
                (expand-file-name (concat (substring jobname 9) ".fmt") directory)
              (insert "format"))
            (funcall (plist-get arguments :sentinel)
                     (car format-invocation) "finished\n"))
          (let* ((first-compiler (nth 1 invocations))
                 (arguments (cddr first-compiler))
                 (format-argument
                  (seq-find (lambda (item) (string-prefix-p "-fmt=" item))
                            (plist-get arguments :command)))
                 (format-file
                  (expand-file-name
                   (concat (substring format-argument 5) ".fmt")
                   shuying-latex-format-directory)))
            (should (file-exists-p format-file))
            ;; A successful process with no DVI means the cached format failed.
            (funcall (plist-get arguments :sentinel)
                     (car first-compiler) "finished\n")
            (let* ((retry (nth 2 invocations))
                   (retry-arguments (cddr retry))
                   (retry-command (plist-get retry-arguments :command))
                   (tex-file (car (last retry-command)))
                   (directory (file-name-directory tex-file)))
              (should-not (seq-some
                           (lambda (item) (string-prefix-p "-fmt=" item))
                           retry-command))
              (with-temp-buffer
                (insert-file-contents tex-file)
                (should (search-forward "\\documentclass" nil t)))
              (with-temp-file (expand-file-name "input.dvi" directory))
              (funcall (plist-get retry-arguments :sentinel)
                       (car retry) "finished\n")
              (let* ((converter (nth 3 invocations))
                     (converter-arguments (cddr converter)))
                (with-temp-file (expand-file-name "page-1.svg" directory)
                  (insert "rendered"))
                (with-current-buffer (plist-get converter-arguments :buffer)
                  (insert "Preview: Fontsize 10pt\n"
                          "  width=10pt, height=8pt, depth=2pt\n"))
                (funcall (plist-get converter-arguments :sentinel)
                         (car converter) "finished\n")))
            (should (equal completions '(nil)))
            (should-not (file-exists-p format-file))
            (with-temp-buffer
              (insert-file-contents output)
              (should (equal (buffer-string) "rendered")))))
      (delete-directory root t))))

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
         (shuying-latex-preamble--warmed-preambles
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmups (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmup-queue nil)
         (shuying-latex-preamble--active-warmup nil)
         results)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-latex-render-batch
           #'shuying-latex-batch-key)
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
              (accept-process-output nil 0.05)))
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
         (shuying-latex-preamble--format-builds
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--failed-formats
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmed-preambles
          (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmups (make-hash-table :test #'equal))
         (shuying-latex-preamble--warmup-queue nil)
         (shuying-latex-preamble--active-warmup nil)
         (original-make-process (symbol-function 'make-process))
         (format-build-count 0)
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
                  (when (member "-ini" (plist-get arguments :command))
                    (cl-incf format-build-count))
                  (apply original-make-process arguments))))
            (render "$x$" "first.svg")
            (with-timeout
                (30 (ert-fail "Timed out precompiling a LaTeX preamble"))
              (while (< (length results) 1)
                (accept-process-output nil 0.05)))
            (should (= format-build-count 1))
            (should (= (length
                        (directory-files
                         shuying-latex-format-directory nil "\\.fmt\\'"))
                       1))
            (render "$y$" "second.svg")
            (with-timeout
                (30 (ert-fail "Timed out reusing a LaTeX preamble"))
              (while (< (length results) 2)
                (accept-process-output nil 0.05)))
            (should (= format-build-count 1))
            (should (seq-every-p #'null (mapcar #'cdr results)))
            (dolist (result results)
              (should (file-exists-p (car result)))
              (with-temp-buffer
                (insert-file-contents (car result))
                (should (search-forward "<svg" nil t))))))
      (delete-directory root t))))

;;; shuying-latex-test.el ends here
