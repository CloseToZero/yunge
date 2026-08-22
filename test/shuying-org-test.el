;;; shuying-org-test.el --- Shuying Org tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'shuying-org)

(defvar shuying-org-test--render-count 0)
(defvar shuying-org-test--backend-call-count 0)

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

(ert-deftest shuying-org-removes-a-stale-nested-preview-on-entry ()
  (with-temp-buffer
    (org-mode)
    (insert "\\( \\m\\)")
    (goto-char (point-min))
    (let ((fragment (shuying-org--fragment-at-point))
          (outer (make-overlay 1 8))
          (inner (make-overlay 4 6)))
      (dolist (overlay (list outer inner))
        (overlay-put overlay 'shuying-org t)
        (overlay-put overlay 'display 'image))
      (shuying-org--enter-fragment fragment)
      (should-not (overlay-get outer 'display))
      (should-not (overlay-buffer inner)))))

(ert-deftest shuying-org-rechecks-viewport-after-displaying-an-image ()
  (with-temp-buffer
    (insert "formula")
    (let ((overlay (make-overlay (point-min) (point-max)))
          (shuying-org-mode t)
          (shuying-org--visible-window-state 'rendered)
          schedule-called
          scheduled-immediately)
      (overlay-put overlay 'shuying-org-image 'image)
      (cl-letf (((symbol-function 'shuying-org--schedule-visible-preview)
                 (lambda (&optional immediate)
                   (setq schedule-called t
                         scheduled-immediately immediate))))
        (shuying-org--show-overlay overlay)
        (should (eq (overlay-get overlay 'display) 'image))
        (should-not shuying-org--visible-window-state)
        (should schedule-called)
        (should-not scheduled-immediately)
        (setq schedule-called nil
              shuying-org--visible-window-state 'settled)
        (shuying-org--show-overlay overlay)
        (should (eq shuying-org--visible-window-state 'settled))
        (should-not schedule-called)))))

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
      (should (eq (shuying-org--image artifact) 'image))
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
              (shuying-org--fragment-at-point)
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
         (equal
          (plist-get
           (shuying-render-spec-backend-options specification)
           :converter)
          '("test-dvisvgm")))
        (should (= (shuying-render-spec-scale specification) 1.7))))))

(ert-deftest shuying-org-previews-only-explicit-latex-math ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\mathrm{A}\n"
     "$x$\n"
     "\\(y\\)\n"
     "\\[z\\]\n"
     "\\begin{equation}\nw = 1\n\\end{equation}\n")
    (goto-char (point-min))
    (should-not (shuying-org--fragment-at-point))
    (should
     (equal
      (mapcar #'shuying-org-fragment-value
              (shuying-org--fragments))
      '("$x$" "\\(y\\)" "\\[z\\]"
        "\\begin{equation}\nw = 1\n\\end{equation}\n")))))

(ert-deftest shuying-org-tracks-equation-numbering-context ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{equation}\na = b\n\\end{equation}\n\n"
     "$x$\n\n"
     "\\begin{align}\n"
     "a &= b \\\\\n"
     "c &= \\begin{aligned}\n"
     "  x & = y \\\\\n"
     "  z & = w\n"
     "\\end{aligned} \\nonumber \\\\\n"
     "d &= e \\tag{manual} \\\\\n"
     "% A commented \\\\ does not start another row.\n"
     "f &= g\n"
     "\\end{align}\n\n"
     "\\begin{equation}\nj = k \\tag{manual}\n\\end{equation}\n\n"
     "\\begin{multline}\np + q \\\\\n"
     "+ r = s\n\\end{multline}\n\n"
     "\\begin{equation}\nh = i\n\\end{equation}\n")
    (should
     (equal
      (mapcar #'shuying-org-fragment-equation-number
              (shuying-org--fragments))
      '(1 nil 2 4 4 5)))))

(ert-deftest shuying-org-does-not-number-starred-environments ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{align*}\na &= b\n\\end{align*}\n\n"
     "\\begin{displaymath}\nx = y\n\\end{displaymath}\n\n"
     "\\begin{equation}\nc = d\n\\end{equation}\n")
    (should
     (equal
      (mapcar #'shuying-org-fragment-equation-number
              (shuying-org--fragments))
      '(nil nil 1)))))

(ert-deftest shuying-org-adds-equation-number-to-render-specification ()
  (with-temp-buffer
    (org-mode)
    (insert "\\begin{equation}\nx = y\n\\end{equation}\n")
    (let ((specification
           (shuying-org--render-spec
            (car (shuying-org--fragments)) "test-preamble")))
      (should
       (= (shuying-render-spec-equation-number specification) 1)))))

(ert-deftest shuying-org-renumbers-after-an-earlier-environment-changes ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{equation}\nx = y\n\\end{equation}\n\n"
     "\\begin{equation}\ny = z\n\\end{equation}\n")
    (should
     (equal
      (mapcar #'shuying-org-fragment-equation-number
              (shuying-org--fragments))
      '(1 2)))
    (goto-char (point-min))
    (search-forward "x = y")
    (insert " \\tag{manual}")
    (should
     (equal
      (mapcar #'shuying-org-fragment-equation-number
              (shuying-org--fragments))
      '(1 1)))))

(ert-deftest shuying-org-reschedules-visible-numbering-after-an-edit ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{equation}\nx = y\n\\end{equation}\n\n"
     "\\begin{equation}\ny = z\n\\end{equation}\n")
    (let ((fragment (car (shuying-org--fragments)))
          scheduled)
      (cl-letf (((symbol-function 'shuying-org--preview-fragments)
                 #'ignore)
                ((symbol-function 'shuying-org--window-state)
                 (lambda () 'visible))
                ((symbol-function 'shuying-org--schedule-visible-preview)
                 (lambda (&optional immediate)
                   (setq scheduled immediate))))
        (setq shuying-org-mode t
              shuying-org--visible-window-state 'visible)
        (goto-char (shuying-org-fragment-beginning fragment))
        (search-forward "x = y")
        (insert " \\tag{manual}")
        (shuying-org--preview-fragment
         (shuying-org--fragment-at-position (1- (point))))
        (should scheduled)
        (should-not shuying-org--visible-window-state)))))

(ert-deftest shuying-org-selects-fragments-from-disjoint-ranges ()
  (with-temp-buffer
    (org-mode)
    (insert "$a$ gap $b$ gap $c$")
    (let* ((fragments (shuying-org--fragments))
           (first (nth 0 fragments))
           (second (nth 1 fragments))
           (third (nth 2 fragments)))
      (should
       (equal
        (shuying-org--fragments-in-ranges
         (list
          (cons (shuying-org-fragment-beginning third)
                (shuying-org-fragment-end third))
          (cons (shuying-org-fragment-beginning first)
                (shuying-org-fragment-end first))))
        (list first third)))
      (should
       (equal
        (shuying-org--fragments-in-region
         (shuying-org-fragment-end first)
         (shuying-org-fragment-beginning third))
        (list second))))))

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
         ranges
         (parse-count 0)
         (parse-buffer (symbol-function 'org-element-parse-buffer)))
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
                     (lambda () window-state))
                    ((symbol-function 'org-element-parse-buffer)
                     (lambda (&rest arguments)
                       (cl-incf parse-count)
                       (apply parse-buffer arguments))))
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
              (should (= parse-count 1))
              (should (= shuying-org-test--render-count 1))
              (should (= (length
                          (shuying-org--fragment-overlays
                           (point-min) (point-max)))
                         1))
              (should
               (overlay-get
                (shuying-org-test--overlay) 'display))
              (shuying-org--preview-visible-windows)
              (should (= parse-count 1))
              (should (= shuying-org-test--render-count 1))
              (setq window-state 'scrolled
                    ranges (list (cons (point-min) (point-max))))
              (shuying-org--preview-visible-windows)
              (should (= parse-count 1))
              (should (= shuying-org-test--render-count 2))
              (should (= (length
                          (shuying-org--fragment-overlays
                           (point-min) (point-max)))
                         2))
              (goto-char (point-max))
              (insert "Added $z$.\n")
              (setq window-state 'edited
                    ranges (list (cons (point-min) (point-max))))
              (shuying-org--preview-visible-windows)
              (should (= parse-count 2))
              (should (= shuying-org-test--render-count 3)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest shuying-org-refreshes-visible-previews-after-theme-changes ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (buffer (generate-new-buffer " *shuying-org-theme*"))
         (foreground "light")
         window-state
         ranges
         scheduled)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file)))
                    ((symbol-function 'shuying-org--face-color)
                     (lambda (_option attribute)
                       (if (eq attribute :foreground)
                           foreground
                         "Transparent")))
                    ((symbol-function 'shuying-org--visible-ranges)
                     (lambda () ranges))
                    ((symbol-function 'shuying-org--window-state)
                     (lambda () window-state))
                    ((symbol-function 'shuying-org--schedule-visible-preview)
                     (lambda (&optional immediate)
                       (push (cons (current-buffer) immediate)
                             scheduled))))
            (with-current-buffer buffer
              (org-mode)
              (insert "Visible $x$.\n")
              (setq ranges (list (cons (point-min) (point-max)))
                    window-state 'visible)
              (shuying-org-mode 1)
              (setq scheduled nil)
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 1))
              (setq shuying-org--visible-window-state nil)
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 1))
              (setq scheduled nil)
              (setq foreground "dark")
              (shuying-org--theme-changed 'dark-theme))
            (should (equal scheduled (list (cons buffer t))))
            (with-current-buffer buffer
              (should-not shuying-org--visible-window-state)
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 2)))))
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
                     (lambda () preamble)))
            (with-temp-buffer
              (org-mode)
              (insert "$x$")
              (let ((fragments (shuying-org--fragments)))
                (shuying-org--preview-fragments fragments t)
                (should (= shuying-org-test--render-count 1))
                (shuying-org--preview-fragments fragments t)
                (should (= shuying-org-test--render-count 1))
                (setq preamble "second")
                (shuying-org--preview-fragments fragments t)
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
        (setq scheduled nil
              shuying-org--visible-window-state 'visible)
        (run-hooks 'after-save-hook)
        (should-not shuying-org--visible-window-state)
        (should scheduled)
        (shuying-org-mode -1)
        (should-not
         (memq #'shuying-org--buffer-saved after-save-hook))))))

(ert-deftest shuying-org-rebuilds-visible-previews-after-revert ()
  (with-temp-buffer
    (org-mode)
    (insert "$x$")
    (let (scheduled)
      (cl-letf (((symbol-function 'shuying-org--window-state)
                 (lambda () 'visible))
                ((symbol-function 'shuying-org--schedule-visible-preview)
                 (lambda (&optional immediate)
                   (setq scheduled immediate))))
        (shuying-org-mode 1)
        (let* ((fragment (car (shuying-org--fragments)))
               (overlay (shuying-org--ensure-overlay fragment)))
          (shuying-org--set-active-fragment fragment)
          (setq scheduled nil
                shuying-org--changed-overlays (list overlay)
                shuying-org--visible-window-state 'visible)
          (run-hooks 'after-revert-hook)
          (should-not (overlay-buffer overlay))
          (should-not shuying-org--fragment-catalog)
          (should-not shuying-org--catalog-tick)
          (should-not shuying-org--active-start)
          (should-not shuying-org--changed-overlays)
          (should (= shuying-org--previous-point (point)))
          (should
           (= shuying-org--previous-tick
              (buffer-chars-modified-tick)))
          (should-not shuying-org--visible-window-state)
          (should scheduled))
        (shuying-org-mode -1)
        (should-not
         (memq #'shuying-org--buffer-reverted after-revert-hook))))))

(ert-deftest shuying-org-tracks-widths-of-windows-showing-one-buffer ()
  (let ((buffer (generate-new-buffer " *shuying-org-window-width*")))
    (unwind-protect
        (save-window-excursion
          (let* ((left (selected-window))
                 (right (split-window-right)))
            (set-window-buffer left buffer)
            (set-window-buffer right buffer)
            (with-current-buffer buffer
              (org-mode)
              (insert "First $x$.\nSecond $y$.\n")
              (let ((before (shuying-org--window-state)))
                (select-window left)
                (enlarge-window-horizontally 5)
                (should-not
                 (equal before (shuying-org--window-state)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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

(ert-deftest shuying-org-rechecks-visible-previews-after-window-resize ()
  (let ((buffer (generate-new-buffer " *shuying-org-window-resize*"))
        scheduled)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (org-mode)
            (cl-letf (((symbol-function
                        'shuying-org--schedule-visible-preview)
                       (lambda (&optional immediate)
                         (setq scheduled immediate))))
              (shuying-org-mode 1)
              (should
               (memq #'shuying-org--window-size-changed
                     window-size-change-functions))
              (setq scheduled nil
                    shuying-org--visible-window-state 'settled)
              (shuying-org--window-size-changed (selected-window))
              (should scheduled)
              (should-not shuying-org--visible-window-state)
              (shuying-org-mode -1)
              (should-not
               (memq #'shuying-org--window-size-changed
                     window-size-change-functions)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest shuying-org-coalesces-viewport-preview-updates ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (buffer (generate-new-buffer " *shuying-org-coalesce*"))
         (idle-timer (symbol-function 'run-with-idle-timer))
         (cancel (symbol-function 'cancel-timer))
         window-state
         ranges
         scheduled
         cancelled)
    (unwind-protect
        (progn
          (shuying-register-backend
           'shuying-latex
           #'shuying-org-test--render-now)
          (cl-letf (((symbol-function 'create-image)
                     (lambda (file &rest _properties)
                       (list 'image file)))
                    ((symbol-function 'shuying-org--visible-ranges)
                     (lambda () ranges))
                    ((symbol-function 'shuying-org--window-state)
                     (lambda () window-state))
                    ((symbol-function 'run-with-idle-timer)
                     (lambda (seconds repeat function &rest arguments)
                       (if (eq function
                               #'shuying-org--run-visible-preview)
                           (let ((timer (timer-create)))
                             (push (list timer seconds function arguments)
                                   scheduled)
                             timer)
                         (apply idle-timer seconds repeat
                                function arguments))))
                    ((symbol-function 'cancel-timer)
                     (lambda (timer)
                       (if (seq-find
                            (lambda (entry) (eq (car entry) timer))
                            scheduled)
                           (push timer cancelled)
                         (funcall cancel timer)))))
            (with-current-buffer buffer
              (org-mode)
              (insert "First $x$.\n")
              (let ((first-end (point)))
                (dotimes (_ 100)
                  (insert "Filler.\n"))
                (let ((second-start (point)))
                  (insert "Second $y$.\n")
                  (shuying-org-mode 1)
                  (setq window-state 'first
                        ranges (list (cons (point-min) first-end)))
                  (shuying-org--schedule-visible-preview)
                  (let* ((first shuying-org--visible-preview-timer)
                         (first-entry
                          (seq-find
                           (lambda (entry) (eq (car entry) first))
                           scheduled)))
                    (should (= (cadr first-entry)
                               shuying-org-visible-preview-delay))
                    (shuying-org--schedule-visible-preview)
                    (should
                     (eq first shuying-org--visible-preview-timer))
                    (should-not (memq first cancelled))
                    (setq window-state 'second
                          ranges
                          (list (cons second-start (point-max))))
                    (shuying-org--schedule-visible-preview)
                    (let* ((latest shuying-org--visible-preview-timer)
                           (latest-entry
                            (seq-find
                             (lambda (entry) (eq (car entry) latest))
                             scheduled)))
                      (should-not (eq latest first))
                      (should (memq first cancelled))
                      (apply (nth 2 latest-entry)
                             (nth 3 latest-entry))))
                  (should (= shuying-org-test--render-count 1))
                  (should
                   (= (overlay-start (shuying-org-test--overlay))
                      (+ second-start 7))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

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

(ert-deftest shuying-org-previews-after-meta-return-from-list-formula ()
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
              (insert "1. First.\n2. Second.\n3. \\(x")
              (shuying-org-mode 1)
              (insert "\\)")
              (shuying-org--post-command)
              (should (markerp shuying-org--active-start))
              (call-interactively #'org-meta-return)
              (shuying-org--post-command)
              (should
               (equal
                (buffer-string)
                "1. First.\n2. Second.\n3. \\(x\\)\n4. "))
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
              (let* ((fragment (car (last (shuying-org--fragments))))
                     (overlay (shuying-org--fragment-overlay fragment)))
                (should (= shuying-org-test--render-count 1))
                (should (overlay-get overlay 'display))
                (call-interactively #'org-meta-return)
                (shuying-org--post-command)
                (setq fragment (car (last (shuying-org--fragments))))
                (should
                 (eq overlay
                     (shuying-org--fragment-overlay fragment)))
                (should (= shuying-org-test--render-count 1))
                (should (overlay-get overlay 'display))
                (should-not
                 (overlay-get overlay 'shuying-org-dirty))))))
      (delete-directory root t))))

(ert-deftest shuying-org-keeps-an-incomplete-inline-formula-visible ()
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
              (insert "Before.\n")
              (shuying-org-mode 1)
              (dolist (character '(?\\ ?\( ?\s ?\\ ?m))
                (insert character)
                (shuying-org--post-command))
              (should (equal (buffer-string) "Before.\n\\( \\m"))
              (should (= shuying-org-test--render-count 0))
              (should-not (shuying-org-test--overlay))
              (dolist (character '(?\\ ?\)))
                (insert character)
                (shuying-org--post-command))
              (should (equal (buffer-string) "Before.\n\\( \\m\\)"))
              (should (= shuying-org-test--render-count 0))
              (shuying-org--post-command)
              (should (= shuying-org-test--render-count 0))
              (goto-char (point-min))
              (shuying-org--post-command)
              (should (= shuying-org-test--render-count 1))
              (should
               (overlay-get
                (shuying-org-test--overlay) 'display)))))
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
                     (lambda ()
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
                   (shuying-org--fragment-overlays
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
                (shuying-org--fragment-overlays
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
              (let ((fragments (shuying-org--fragments)))
                (shuying-org--preview-fragments fragments nil t)
                (should-not warnings)
                (should
                 (eq
                  (car
                   (overlay-get
                    (car (shuying-org--fragment-overlays
                          (point-min) (point-max)))
                    'shuying-org-error))
                  'shuying-latex-unavailable))
                (shuying-org--preview-fragments fragments)
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
              (shuying-org--preview-fragment
               (shuying-org--fragment-at-point))
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

(ert-deftest shuying-org-renders-svg-with-latex-and-dvisvgm ()
  (unless (and (executable-find "latex")
               (executable-find "dvisvgm")
               (executable-find "kpsewhich")
               (= (call-process "kpsewhich" nil nil nil
                                "preview.sty")
                  0))
    (ert-skip "The LaTeX preview toolchain is unavailable"))
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying--pending-jobs (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (org-mode)
            (insert
             "\\[(x+y)^n = \\sum_{k=0}^{n} "
             "\\binom{n}{k} x^{n-k} y^k.\\]\n")
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
                (with-temp-buffer
                  (insert-file-contents artifact)
                  (should (search-forward "<svg" nil t)))))))
      (delete-directory root t))))

;;; shuying-org-test.el ends here
