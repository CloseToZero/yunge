;;; shuying-org-test.el --- Shuying Org tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'shuying-org)

(defvar org-latex-compiler)
(defvar shuying-org-test--render-count 0)
(defvar shuying-org-test--backend-call-count 0)
(defvar view-mode-hook)

(defun shuying-org-test--render-now
    (requests complete)
  "Write fake images for REQUESTS and call COMPLETE."
  (cl-incf shuying-org-test--backend-call-count)
  (dolist (request requests)
    (cl-incf shuying-org-test--render-count)
    (with-temp-file (shuying-backend-request-output-file request)
      (insert "image"))
    (setf (shuying-backend-request-metadata request)
          '(:width 1.0 :height 1.2 :depth 0.2))
    (funcall complete request nil)))

(defun shuying-org-test--overlay ()
  "Return the first Shuying overlay in the current buffer."
  (seq-find
   (lambda (overlay)
     (overlay-get overlay 'shuying-org))
   (overlays-in (point-min) (point-max))))

(ert-deftest shuying-org-finds-only-displayed-previews-at-position ()
  (with-temp-buffer
    (insert "before $x$ after")
    (let ((overlay (make-overlay 8 11)))
      (overlay-put overlay 'shuying-org t)
      (overlay-put overlay 'display 'image)
      (should (eq (shuying-org-preview-overlay-at 8) overlay))
      (should (eq (shuying-org-preview-overlay-at 9) overlay))
      (should-not (shuying-org-preview-overlay-at 7))
      (should-not (shuying-org-preview-overlay-at 11))
      (overlay-put overlay 'display nil)
      (should-not (shuying-org-preview-overlay-at 9)))))

(ert-deftest shuying-org-keeps-the-preview-at-point-in-view-mode ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert "$x$")
              (goto-char (point-min))
              (shuying-org-mode 1)
              (should
               (memq #'shuying-org--view-mode-changed view-mode-hook))
              (view-mode 1)
              (should buffer-read-only)
              (should (= shuying-org-test--render-count 1))
              (should
               (overlay-get (shuying-org-test--overlay) 'display))
              (shuying-org--post-command)
              (should
               (overlay-get (shuying-org-test--overlay) 'display))
              (view-mode -1)
              (should-not buffer-read-only)
              (should-not
               (overlay-get (shuying-org-test--overlay) 'display))
              (shuying-org-mode -1)
              (should-not
               (memq #'shuying-org--view-mode-changed
                     view-mode-hook)))))
      (delete-directory root t))))

(ert-deftest shuying-org-keeps-environment-line-ending-outside-preview ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{align*}\n"
     "x &= y\n"
     "\\end{align*}\n\n"
     "After.\n")
    (let* ((fragment (car (shuying-org-source-formulas)))
           (overlay (shuying-org--ensure-overlay fragment)))
      (overlay-put overlay 'display 'image)
      (goto-char (point-min))
      (search-forward "\\end{align*}")
      (should (shuying-org-preview-overlay-at (1- (point))))
      (should-not
       (shuying-org-preview-overlay-at (line-end-position))))))

(ert-deftest shuying-org-previews-non-standalone-block-math-at-source ()
  (with-temp-buffer
    (org-mode)
    (insert
     "before \\[a\\]\n"
     "\\[b\\] after\n"
     "before $$c$$\n"
     "  \\[\n"
     "d\n"
     "  \\]  \n"
     "\\begin{equation}\n"
     "e = f\n"
     "\\end{equation}\n")
    (let ((fragments (shuying-org-source-formulas))
          (image '(image :type svg :data "formula")))
      (should
       (equal
        (mapcar #'shuying-org-formula-standalone-p fragments)
        '(nil nil nil t t)))
      (dolist (fragment fragments)
        (let ((overlay (shuying-org--ensure-overlay fragment)))
          (overlay-put overlay 'shuying-org-image image)
          (shuying-org--show-overlay overlay)
          (should (equal (overlay-get overlay 'display) image))
          (if (shuying-org-formula-standalone-p fragment)
              (should (overlay-get overlay 'before-string))
            (should-not (overlay-get overlay 'before-string))))))))

(ert-deftest shuying-org-refreshes-cached-block-math-layout ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         scheduled)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (&rest _) 'image))
                    ((symbol-function 'shuying-org--preamble)
                     (lambda (&optional _info) "test-preamble"))
                    ((symbol-function
                      'shuying-org--schedule-visible-preview)
                     (lambda (&optional _immediate)
                       (setq scheduled t))))
            (with-temp-buffer
              (org-mode)
              (insert "1. prefix \\[x = y\\]\n")
              (let ((fragments (shuying-org-source-formulas)))
                (shuying-org--preview-formulas fragments t)
                (let ((overlay (shuying-org-test--overlay)))
                  (should overlay)
                  (should-not (overlay-get overlay 'before-string))
                  (should (= shuying-org-test--render-count 1))

                  (add-hook 'after-change-functions
                            #'shuying-org--layout-context-changed nil t)
                  (delete-region (point-min) (overlay-start overlay))
                  (should scheduled)
                  (shuying-org--preview-formulas
                   (shuying-org-source-formulas) t)
                  (should (eq overlay (shuying-org-test--overlay)))
                  (should (overlay-get overlay 'before-string))
                  (should (= shuying-org-test--render-count 1))

                  (setq scheduled nil)
                  (goto-char (point-min))
                  (insert "prefix ")
                  (should scheduled)
                  (shuying-org--preview-formulas
                   (shuying-org-source-formulas) t)
                  (setq overlay (shuying-org-test--overlay))
                  (should overlay)
                  (should-not (overlay-get overlay 'before-string))
                  (should (= shuying-org-test--render-count 1)))))))
      (delete-directory root t))))

(ert-deftest shuying-org-aligns-block-math-in-the-window-text-area ()
  (with-temp-buffer
    (insert "inline block")
    (let* ((image '(image :type svg :data "formula"))
           (inline (make-overlay 1 7))
           (block (make-overlay 8 13))
           (shuying-org-block-math-alignment 'center))
      (dolist (overlay (list inline block))
        (overlay-put overlay 'shuying-org-image image))
      (overlay-put block 'shuying-org-block-math t)
      (overlay-put block 'shuying-org-standalone t)
      (shuying-org--show-overlay inline)
      (shuying-org--show-overlay block)
      (should-not (overlay-get inline 'before-string))
      (let ((prefix (overlay-get block 'before-string)))
        (should (stringp prefix))
        (should
         (equal
          (get-text-property 0 'display prefix)
          `(space :align-to (- center (0.5 . ,image))))))
      (let ((shuying-org-block-math-alignment 'source))
        (shuying-org--show-overlay block)
        (should-not (overlay-get block 'before-string)))
      (let ((shuying-org-block-math-alignment 'center))
        (shuying-org--show-overlay block)
        (should (overlay-get block 'before-string))
        (shuying-org--hide-overlay block)
        (should-not (overlay-get block 'display))
        (should-not (overlay-get block 'before-string))))))

(ert-deftest shuying-org-aligns-images-from-rendered-geometry ()
  (let ((artifact
         (make-shuying-artifact
          :path "formula.svg"
          :metadata '(:width 1.0 :height 1.2 :depth 0.2)))
        arguments)
    (cl-letf (((symbol-function 'create-image)
               (lambda (&rest values)
                 (setq arguments values)
                 'image)))
      (should
       (eq (shuying-org--image artifact) 'image))
      (should
       (equal arguments
               '("formula.svg" nil nil
                 :height (1.2 . em) :ascent 83))))))

(ert-deftest shuying-org-rejects-invalid-artifact-geometry ()
  (dolist (metadata '(nil
                      (:height 0 :depth 0)
                      (:height 1.0 :depth 2.0)))
    (should-error
     (shuying-org--image
      (make-shuying-artifact
       :path "formula.svg"
       :metadata metadata)))))

(ert-deftest shuying-org-keeps-zero-geometry-artifacts-as-source ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           (lambda (requests complete)
             (dolist (request requests)
               (cl-incf render-count)
               (with-temp-file
                   (shuying-backend-request-output-file request)
                 (insert "empty image"))
               (setf (shuying-backend-request-metadata request)
                     '(:width 0.0 :height 0.0 :depth 0.0))
               (funcall complete request nil))))
          (cl-letf (((symbol-function 'shuying-org--preamble)
                     (lambda (&optional _info) "test-preamble")))
            (with-temp-buffer
              (org-mode)
              (insert "\\(\\phantom{x}\\)")
              (let ((fragments (shuying-org-source-formulas)))
                (shuying-org--preview-formulas fragments t)
                (let ((overlay (shuying-org-test--overlay)))
                  (should overlay)
                  (should (= render-count 1))
                  (should
                   (overlay-get overlay 'shuying-org-empty-artifact))
                  (should-not (overlay-get overlay 'shuying-org-image))
                  (should-not (overlay-get overlay 'display))
                  (should-not (overlay-get overlay 'shuying-org-error))
                  (shuying-org--preview-formulas fragments t)
                  (should (= render-count 1)))))))
      (delete-directory root t))))

(ert-deftest shuying-org-removes-a-preview-that-becomes-blank ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (&rest _) 'image))
                    ((symbol-function 'shuying-org--preamble)
                     (lambda (&optional _info) "test-preamble")))
            (with-temp-buffer
              (org-mode)
              (insert "Before \\(x\\) after")
              (goto-char (point-min))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (let ((overlay (shuying-org-test--overlay)))
                (should overlay)
                (should (= shuying-org-test--render-count 1))
                (search-forward "x")
                (shuying-org--post-command)
                (delete-char -1)
                (shuying-org--post-command)
                (goto-char (point-max))
                (shuying-org--post-command)
                (should-not (shuying-org-source-formulas))
                (should-not (overlay-buffer overlay))
                (should-not (shuying-org-test--overlay))
                (should (= shuying-org-test--render-count 1))))))
      (delete-directory root t))))

(ert-deftest shuying-org-records-image-construction-errors ()
  (with-temp-buffer
    (let* ((overlay (make-overlay (point-min) (point-min)))
           (artifact
            (make-shuying-artifact
             :path "formula.svg"
             :metadata '(:width 1.0 :height 1.2 :depth 0.2)))
           reported)
      (overlay-put overlay 'shuying-org-generation 1)
      (overlay-put overlay 'shuying-org-dirty nil)
      (overlay-put overlay 'shuying-org-specification-hash "current")
      (cl-letf (((symbol-function 'create-image)
                 (lambda (&rest _arguments)
                   (error "SVG is unavailable"))))
        (shuying-org--finish-render
         (current-buffer) overlay 1 artifact nil
         (lambda (error-data)
           (setq reported error-data))))
      (should (equal reported '(error "SVG is unavailable")))
      (should-not (overlay-get overlay 'shuying-org-dirty))
      (should (equal (overlay-get overlay 'shuying-org-error)
                     reported))
      (should
       (equal (overlay-get overlay 'shuying-org-specification-hash)
              "current"))
      (should-not (overlay-get overlay 'display)))))

(ert-deftest shuying-org-builds-direct-latex-render-specifications ()
  (let ((shuying-latex-engine-command '("test-latex"))
        (shuying-latex-converter-command '("test-dvisvgm")))
    (with-temp-buffer
      (org-mode)
      (insert "$x$")
      (goto-char (point-min))
      (let ((specification
             (shuying-org--render-spec
              (shuying-org-source-at-position (point))
              "test-preamble")))
        (should (eq (shuying-render-spec-backend specification)
                    'shuying-latex))
        (should (equal (shuying-render-spec-preamble specification)
                       "test-preamble"))
        (should (equal (shuying-render-spec-engine specification)
                       '("test-latex")))
        (should-not
         (shuying-render-spec-equation-number specification))
        (should
         (equal (shuying-render-spec-foreground specification)
                "Black"))
        (should
         (equal (shuying-render-spec-background specification)
                "Transparent"))
        (should
         (equal
          (plist-get
           (shuying-render-spec-backend-options specification)
           :converter)
          '("test-dvisvgm")))
        (should (= (shuying-render-spec-scale specification) 1.7))))))

(ert-deftest shuying-org-follows-the-org-latex-compiler ()
  (dolist (case '(("pdflatex" . ("pdflatex" "-output-format=dvi"))
                  ("xelatex" . ("xelatex" "-no-pdf"))
                  ("lualatex" . ("lualatex" "--output-format=dvi"))))
    (should
     (equal
      (shuying-org--latex-engine-command
      (list :latex-compiler (car case)))
      (cdr case)))))

(ert-deftest shuying-org-honors-a-buffer-latex-compiler ()
  (let ((org-latex-compiler "xelatex")
        (shuying-latex-engine-command nil))
    (with-temp-buffer
      (org-mode)
      (insert "#+LATEX_COMPILER: pdflatex\n\n$x$\n")
      (should
       (equal (shuying-org--latex-engine-command
               (shuying-org--latex-info))
              '("pdflatex" "-output-format=dvi"))))))

(ert-deftest shuying-org-adds-equation-number-to-render-specification ()
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\nx = y\n\\end{equation}\n")
    (let ((specification
           (shuying-org--render-spec
            (car (shuying-org-source-formulas)) "test-preamble")))
      (should
       (= (shuying-render-spec-equation-number specification) 1)))))

(ert-deftest shuying-org-updates-visible-equation-numbers-after-an-edit ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         rendered)
    (unwind-protect
        (save-window-excursion
          (shuying-register-backend
           'shuying-latex
           (lambda (requests complete)
             (dolist (request requests)
               (let ((spec (shuying-backend-request-specification request)))
                 (push (cons (shuying-render-spec-source spec)
                             (shuying-render-spec-equation-number spec))
                       rendered))
               (with-temp-file (shuying-backend-request-output-file request)
                 (insert "image"))
               (setf (shuying-backend-request-metadata request)
                     '(:width 1.0 :height 1.2 :depth 0.2))
               (funcall complete request nil))))
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties) (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert
               "\\begin{equation}\nx = y\n\\end{equation}\n\n"
               "\\begin{equation}\ny = z\n\\end{equation}\n")
              (set-window-buffer (selected-window) (current-buffer))
              (goto-char (point-min))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (shuying-org--preview-visible-windows)
              (should (member '("\\begin{equation}\ny = z\n\\end{equation}\n" . 2)
                              rendered))
              (search-forward "x = y")
              (insert " \\tag{manual}")
              ;; Another consumer may refresh the source catalog first.
              (shuying-org-source-formulas)
              (goto-char (point-max))
              (shuying-org--post-command)
              (shuying-org--preview-visible-windows)
              (should (member '("\\begin{equation}\ny = z\n\\end{equation}\n" . 1)
                              rendered))
              (shuying-org-mode -1))))
      (delete-directory root t))))

(ert-deftest shuying-org-previews-after-leaving-edited-source ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert "Before $x$ after.\n")
              (goto-char (point-min))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (let ((overlay (shuying-org-test--overlay)))
                (should overlay)
                (should (overlay-get overlay 'display))
                (should (= shuying-org-test--render-count 1))
                (search-forward "x")
                (shuying-org--post-command)
                (should-not (overlay-get overlay 'display))
                (insert "2")
                (shuying-org--post-command)
                (should (overlay-get overlay 'shuying-org-dirty))
                (goto-char (point-max))
                (shuying-org--post-command)
                (should (= shuying-org-test--render-count 2))
                (should-not
                 (overlay-get overlay 'shuying-org-dirty))
                (should (overlay-get overlay 'display))))))
      (delete-directory root t))))

(ert-deftest shuying-org-keeps-joined-space-outside-preview ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert "有一个根\n\\( a \\)，继续。\n")
              (goto-char (point-min))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              ;; This is the edit made by Evil's `J' operator.
              (join-line 1)
              (shuying-org--post-command)
              (let ((formula-start
                     (save-excursion
                       (goto-char (point-min))
                       (search-forward "\\( a \\)")
                       (- (point) (length "\\( a \\)")))))
                (should-not
                 (shuying-org-preview-overlay-at (1- formula-start)))
                (should
                 (shuying-org-preview-overlay-at formula-start))))))
      (delete-directory root t))))

(ert-deftest shuying-org-restores-a-preview-after-undo-outside-it ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert "$n$\n")
              (goto-char (point-max))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (should (= shuying-org-test--render-count 1))
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (goto-char (1+ (point-min)))
              (shuying-org--post-command)
              (atomic-change-group
                (delete-char 1)
                (insert "x"))
              (undo-boundary)
              (goto-char (point-max))
              (shuying-org--post-command)
              (should (= shuying-org-test--render-count 2))
              (undo-only 1)
              (goto-char (point-max))
              (shuying-org--post-command)
              (should (equal (buffer-string) "$n$\n"))
              ;; The original artifact is restored from Shuying's cache.
              (should (= shuying-org-test--render-count 2))
              (should
               (overlay-get
                (shuying-org-test--overlay) 'display))
              (should-not
               (overlay-get
                (shuying-org-test--overlay) 'shuying-org-dirty)))))
      (delete-directory root t))))

(ert-deftest shuying-org-repreviews-a-formula-recreated-by-undo ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         scheduled)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file)))
                    ((symbol-function 'shuying-org--window-state)
                     (lambda () 'visible))
                    ((symbol-function 'shuying-org--visible-ranges)
                     (lambda () (list (cons (point-min) (point-max)))))
                    ((symbol-function
                      'shuying-org--schedule-visible-preview)
                     (lambda (&optional immediate)
                       (setq scheduled immediate))))
            (with-temp-buffer
              (org-mode)
              (insert "$n$\n")
              (goto-char (point-max))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (should (= shuying-org-test--render-count 1))
              (buffer-enable-undo)
              (setq buffer-undo-list nil
                    scheduled nil)
              (delete-region (point-min) (1- (point-max)))
              (undo-boundary)
              (shuying-org--post-command)
              (should-not (shuying-org-test--overlay))
              ;; Model the visible pass that observed the formula-less text.
              (setq shuying-org--visible-window-state 'visible
                    scheduled nil)
              (goto-char (point-max))
              ;; Model an undo front end that preserves a cursor outside the
              ;; restored source, without issuing another cursor command.
              (save-excursion
                (undo-only 1))
              (should (equal (buffer-string) "$n$\n"))
              (should (= (point) (point-max)))
              ;; Undo must schedule the restored source without waiting for
              ;; another cursor command to run `post-command-hook'.
              (should scheduled)
              (should-not shuying-org--visible-window-state)
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 1))
              (should
               (overlay-get
                (shuying-org-test--overlay) 'display)))))
      (delete-directory root t))))

(ert-deftest shuying-org-restores-a-cache-hit-after-catalog-change ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert "Above.\n|\\( A \\)|\n")
              (goto-char (point-min))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (let ((overlay (shuying-org-test--overlay)))
                (should (overlay-get overlay 'display))
                (search-forward "\\( A")
                (shuying-org--post-command)
                (should-not (overlay-get overlay 'display))
                (end-of-line)
                (insert "\\( T \\)|")
                (shuying-org--post-command)
                (should (overlay-get overlay 'display))
                ;; Reusing the unchanged artifact must not render it again.
                (should (= shuying-org-test--render-count 1))))))
      (delete-directory root t))))

(ert-deftest shuying-org-previews-visible-fragments-after-display ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (buffer (generate-new-buffer " *shuying-org-visible*"))
         visible-end
         window-state
         ranges)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file)))
                    ((symbol-function 'shuying-org--visible-ranges)
                     (lambda ()
                       ranges))
                    ((symbol-function 'shuying-org--window-state)
                     (lambda () window-state)))
            (with-current-buffer buffer
              (org-mode)
              (insert "Visible $x$.\n")
              (setq visible-end (point))
              (dotimes (_ 100)
                (insert "Filler.\n"))
              (insert "Hidden $y$.\n")
              (goto-char (point-min))
              (shuying-org-mode 1)
              (should (= shuying-org-test--render-count 0))
              (setq window-state 'initial
                    ranges (list (cons (point-min) visible-end)))
              (should
               (memq #'shuying-org--schedule-visible-preview
                     post-command-hook)))
            (with-current-buffer buffer
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 1))
              (should (= (length
                          (shuying-org--formula-overlays
                           (point-min) (point-max)))
                         1))
              (should
               (overlay-get
                (shuying-org-test--overlay) 'display))
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 1))
              (setq window-state 'scrolled
                    ranges (list (cons (point-min) (point-max))))
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 2))
              (should (= (length
                          (shuying-org--formula-overlays
                           (point-min) (point-max)))
                         2))
              (goto-char (point-max))
              (insert "Added $z$.\n")
              (setq window-state 'edited
                    ranges (list (cons (point-min) (point-max))))
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 3)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest shuying-org-leaves-svg-colors-face-relative ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (buffer (generate-new-buffer " *shuying-org-theme*"))
         window-state
         ranges
         image-arguments)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (&rest arguments)
                       (push arguments image-arguments)
                       (cons 'image arguments)))
                    ((symbol-function 'shuying-org--visible-ranges)
                     (lambda () ranges))
                    ((symbol-function 'shuying-org--window-state)
                     (lambda () window-state)))
            (with-current-buffer buffer
              (org-mode)
              (insert "Visible $x$.\n")
              (setq ranges (list (cons (point-min) (point-max)))
                    window-state 'visible)
              (shuying-org-mode 1)
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 1))
              (should (= (length image-arguments) 1))
              (let ((arguments (car image-arguments)))
                (should-not (plist-member (nthcdr 3 arguments)
                                          :foreground))
                (should-not (plist-member (nthcdr 3 arguments)
                                          :background)))
              (setq shuying-org--visible-window-state nil)
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 1))
              (should (= (length image-arguments) 1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest shuying-org-refreshes-previews-after-preamble-changes ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (preamble "first"))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file)))
                    ((symbol-function 'shuying-org--preamble)
                     (lambda (&optional _info) preamble)))
            (with-temp-buffer
              (org-mode)
              (insert "$x$")
              (let ((fragments (shuying-org-source-formulas)))
                (shuying-org--preview-formulas fragments t)
                (should (= shuying-org-test--render-count 1))
                (shuying-org--preview-formulas fragments t)
                (should (= shuying-org-test--render-count 1))
                (setq preamble "second")
                (shuying-org--preview-formulas fragments t)
                (should (= shuying-org-test--render-count 2))))))
      (delete-directory root t))))

(ert-deftest shuying-org-rechecks-preview-context-after-save ()
  (with-temp-buffer
    (org-mode)
    (let (scheduled)
      (cl-letf (((symbol-function 'shuying-org--window-state)
                 (lambda () 'visible))
                ((symbol-function 'shuying-org--schedule-visible-preview)
                 (lambda (&optional immediate)
                   (setq scheduled immediate))))
        (shuying-org-mode 1)
        (should
         (memq #'shuying-org--layout-context-changed
               after-change-functions))
        (setq scheduled nil
              shuying-org--visible-window-state 'visible)
        (run-hooks 'after-save-hook)
        (should-not shuying-org--visible-window-state)
        (should scheduled)
        (shuying-org-mode -1)
        (should-not
         (memq #'shuying-org--layout-context-changed
               after-change-functions))
        (should-not
         (memq #'shuying-org--buffer-saved after-save-hook))))))

(ert-deftest shuying-org-previews-reverted-disk-formulas ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (file (expand-file-name "notes.org" root))
         (shuying-cache-directory (expand-file-name "cache" root))
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (org-mode-hook (cons (lambda () (shuying-org-mode 1))
                              org-mode-hook))
         buffer)
    (unwind-protect
        (save-window-excursion
          (with-temp-file file (insert "$old$\n"))
          (shuying-register-backend 'shuying-latex
                                    #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (path &rest _properties) (list 'image path))))
            (setq buffer (find-file-noselect file))
            (set-window-buffer (selected-window) buffer)
            (with-current-buffer buffer
              (org-mode)
              (goto-char (point-max))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (should (overlay-get (shuying-org-test--overlay) 'display))
              (should (= shuying-org-test--render-count 1))
              (with-temp-file file (insert "$new$\n"))
              (revert-buffer t t)
              (should (equal (buffer-string) "$new$\n"))
              (shuying-org--preview-visible-windows)
              (should (overlay-get (shuying-org-test--overlay) 'display))
              (should (= shuying-org-test--render-count 2))
              (shuying-org-mode -1))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest shuying-org-collects-visible-ranges-from-every-window ()
  (let ((buffer (generate-new-buffer " *shuying-org-window-ranges*"))
        second-start)
    (unwind-protect
        (save-window-excursion
          (let* ((left (selected-window))
                 (right (split-window-right)))
            (with-current-buffer buffer
              (org-mode)
              (insert "First $x$.\n")
              (dotimes (_ 100)
                (insert "Filler.\n"))
              (setq second-start (point))
              (insert "Second $y$.\n"))
            (set-window-buffer left buffer)
            (set-window-start left (with-current-buffer buffer (point-min)))
            (set-window-point left (with-current-buffer buffer (point-min)))
            (set-window-buffer right buffer)
            (set-window-start right second-start)
            (set-window-point right second-start)
            (with-current-buffer buffer
              (let ((starts (mapcar #'car
                                    (shuying-org--visible-ranges))))
                (should (= (length starts) 2))
                (should (memq (point-min) starts))
                (should (memq second-start starts))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest shuying-org-previews-when-a-window-first-shows-the-buffer ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (buffer (generate-new-buffer " *shuying-org-first-display*"))
         (idle-timer (symbol-function 'run-with-idle-timer))
         visible-start
         scheduled-delay
         scheduled)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file)))
                    ((symbol-function 'run-with-idle-timer)
                     (lambda (seconds repeat function &rest arguments)
                       (if (eq function
                               #'shuying-org--run-visible-preview)
                           (progn
                             (setq scheduled-delay seconds)
                             (setq scheduled
                                   (cons function arguments)))
                         (apply idle-timer seconds repeat
                                function arguments)))))
            (with-current-buffer buffer
              (org-mode)
              (insert "Hidden $x$.\n")
              (dotimes (_ 100)
                (insert "Filler.\n"))
              (setq visible-start (point))
              (insert "Restored $y$.\n")
              (goto-char (point-min))
              (shuying-org-mode 1)
              (should (= shuying-org-test--render-count 0)))
            (save-window-excursion
              (set-window-buffer (selected-window) buffer)
              (with-current-buffer buffer
                (set-window-start
                 (selected-window)
                 visible-start)
                (shuying-org--window-buffer-changed
                 (selected-window))
                (should scheduled)
                (should (= scheduled-delay 0))
                (should (= shuying-org-test--render-count 0))
                (apply (car scheduled) (cdr scheduled))
                (should (= shuying-org-test--render-count 1))
                (should
                 (= (overlay-start (shuying-org-test--overlay))
                    (+ visible-start 9)))
                (should
                 (overlay-get
                  (shuying-org-test--overlay) 'display))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest shuying-org-previews-after-leaving-a-newly-closed-fragment ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert "Text $x")
              (shuying-org-mode 1)
              (insert "$")
              (shuying-org--post-command)
              (should (= shuying-org-test--render-count 0))
              (should-not (shuying-org-test--overlay))
              ;; A state change at the same editing boundary must not make
              ;; the source disappear.
              (shuying-org--post-command)
              (should (= shuying-org-test--render-count 0))
              (goto-char (point-min))
              (shuying-org--post-command)
              (should (= shuying-org-test--render-count 1))
              (should
               (overlay-get
                (shuying-org-test--overlay) 'display)))))
      (delete-directory root t))))

(ert-deftest shuying-org-reuses-preview-after-list-meta-return ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert "1. First.\n2. Second.\n3. \\(x\\) [law]")
              (goto-char (point-max))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (let* ((fragment (car (last (shuying-org-source-formulas))))
                     (overlay (shuying-org--formula-overlay fragment)))
                (should (= shuying-org-test--render-count 1))
                (should (overlay-get overlay 'display))
                (call-interactively #'org-meta-return)
                (shuying-org--post-command)
                (setq fragment (car (last (shuying-org-source-formulas))))
                (should
                 (eq overlay
                     (shuying-org--formula-overlay fragment)))
                (should (= shuying-org-test--render-count 1))
                (should (overlay-get overlay 'display))
                (should-not
                 (overlay-get overlay 'shuying-org-dirty))))))
      (delete-directory root t))))

(ert-deftest shuying-org-submits-a-region-as-one-backend-batch ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (shuying-org-test--backend-call-count 0)
         (preamble-count 0))
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file)))
                    ((symbol-function 'shuying-org--preamble)
                     (lambda (&optional _info)
                       (cl-incf preamble-count)
                       "test-preamble")))
            (with-temp-buffer
              (org-mode)
              (insert "$x$ and $y$\n")
              (goto-char (point-max))
              (shuying-org-preview-buffer)
              (should (= preamble-count 1))
              (should (= shuying-org-test--render-count 2))
              (should (= shuying-org-test--backend-call-count 1))
              (should
               (= (length
                   (shuying-org--formula-overlays
                    (point-min) (point-max)))
                  2)))))
      (delete-directory root t))))

(ert-deftest shuying-org-reports-a-batch-error-once ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         warnings)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           (lambda (requests complete)
             (cl-loop
              for request in requests
              for page from 1
              do (funcall complete request
                          (list 'error
                                (format "Missing page %d" page))))))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest warning)
                       (push warning warnings))))
            (with-temp-buffer
              (org-mode)
              (insert "$x$ and $y$")
              (shuying-org-preview-buffer)
              (should (= (length warnings) 1))
              (should
               (seq-every-p
                (lambda (overlay)
                  (overlay-get overlay 'shuying-org-error))
                (shuying-org--formula-overlays
                 (point-min) (point-max)))))))
      (delete-directory root t))))

(ert-deftest shuying-org-silences-unavailable-automatic-previews ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         warnings)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           (lambda (requests complete)
             (dolist (request requests)
               (funcall complete request
                        '(shuying-latex-unavailable
                          "LaTeX engine executable not found: latex")))))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest warning)
                       (push warning warnings))))
            (with-temp-buffer
              (org-mode)
              (insert "$x$")
              (let ((fragments (shuying-org-source-formulas)))
                (shuying-org--preview-formulas fragments nil t)
                (should-not warnings)
                (should
                 (eq
                  (car
                   (overlay-get
                    (car (shuying-org--formula-overlays
                          (point-min) (point-max)))
                    'shuying-org-error))
                  'shuying-latex-unavailable))
                (shuying-org--preview-formulas fragments)
                (should (= (length warnings) 1))))))
      (delete-directory root t))))

(ert-deftest shuying-org-rejects-an-older-render-result ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         requests)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           (lambda (backend-requests complete)
             (dolist (request backend-requests)
               (setq requests
                     (append
                      requests
                      (list (cons request complete)))))))
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file))))
            (with-temp-buffer
              (org-mode)
              (insert "$x$\n")
              (goto-char (point-max))
              (shuying-org-mode 1)
              (shuying-org-preview-buffer)
              (goto-char (point-min))
              (search-forward "x")
              (insert "2")
              (shuying-org--preview-formula
               (shuying-org-source-at-position (point)))
              (should (= (length requests) 2))
              (let* ((overlay (shuying-org-test--overlay))
                     (older (car requests))
                     (newer (cadr requests)))
                (with-temp-file
                    (shuying-backend-request-output-file (car newer))
                  (insert "newer"))
                (setf (shuying-backend-request-metadata (car newer))
                      '(:width 1.0 :height 1.2 :depth 0.2))
                (funcall (cdr newer) (car newer) nil)
                (let ((newer-artifact
                       (overlay-get overlay
                                    'shuying-org-artifact)))
                  (should newer-artifact)
                  (with-temp-file
                      (shuying-backend-request-output-file (car older))
                    (insert "older"))
                  (setf (shuying-backend-request-metadata (car older))
                        '(:width 1.0 :height 1.2 :depth 0.2))
                  (funcall (cdr older) (car older) nil)
                  (should
                   (equal
                    newer-artifact
                    (overlay-get overlay
                                 'shuying-org-artifact))))))))
      (delete-directory root t))))

(ert-deftest shuying-org-renders-chinese-svg-with-xelatex-and-dvisvgm ()
  (unless (and (executable-find "xelatex")
               (executable-find "dvisvgm")
               (executable-find "kpsewhich")
               (= (call-process "kpsewhich" nil nil nil
                                 "preview.sty")
                  0)
               (= (call-process "kpsewhich" nil nil nil "ctex.sty") 0))
    (ert-skip "The XeLaTeX Chinese preview toolchain is unavailable"))
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (org-latex-compiler "xelatex")
         (org-latex-packages-alist
          '(("" "amssymb" t ("xelatex"))
            ("UTF8" "ctex" t ("xelatex")))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert
             "\\[\\mathbb{N}: (x+y)^n = \\sum_{k=0}^{n} "
             "\\binom{n}{k} x^{n-k} y^k, "
             "\\quad \\text{是归纳集}.\\]\n")
            (goto-char (point-max))
            (shuying-org-preview-buffer)
            (let ((overlay (shuying-org-test--overlay)))
              (with-timeout
                  (30 (ert-fail "Timed out rendering an Org preview"))
                (while (not (overlay-get overlay
                                         'shuying-org-artifact))
                  (accept-process-output nil 0.05)))
              (let ((artifact
                     (overlay-get overlay 'shuying-org-artifact)))
                (should (file-exists-p artifact))
                (let* ((cached
                        (shuying--read-artifact
                         artifact
                         (shuying--artifact-metadata-file artifact)))
                       (metadata (shuying-artifact-metadata cached)))
                  (should (< (plist-get metadata :width) 20.0))
                  (should (< (plist-get metadata :height) 4.0)))
                (with-temp-buffer
                  (insert-file-contents artifact)
                  (should (search-forward "<svg" nil t))
                  (goto-char (point-min))
                  (should
                   (re-search-forward
                    "width=['\"]\\([0-9.]+\\)pt['\"]" nil t))
                  ;; A preview-package page box is about 900pt at the
                  ;; configured scale.  The rendered formula is much tighter.
                  (should (< (string-to-number (match-string 1))
                             500.0))
                  (goto-char (point-min))
                  (should (search-forward "currentColor" nil t)))))))
      (delete-directory root t))))

;;; shuying-org-test.el ends here
