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
    (funcall complete request nil)))

(defun shuying-org-test--overlay ()
  "Return the first Shuying overlay in the current buffer."
  (seq-find
   (lambda (overlay)
     (overlay-get overlay 'shuying-org))
   (overlays-in (point-min) (point-max))))

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
        (should
         (equal
          (plist-get
           (shuying-render-spec-backend-options specification)
           :converter)
          '("test-dvisvgm")))
        (should (= (shuying-render-spec-scale specification) 1.7))))))

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

(ert-deftest shuying-org-previews-visible-fragments-after-display ()
  (let* ((root (make-temp-file "shuying-org-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying-org-test--render-count 0)
         (buffer (generate-new-buffer " *shuying-org-visible*"))
         visible-end
         (window-state 'initial)
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
              (setq ranges (list (cons (point-min) visible-end)))
              (shuying-org-startup)
              (should
               (memq #'shuying-org--preview-visible-windows
                     post-command-hook)))
            (with-current-buffer buffer
              (shuying-org--preview-visible-windows)
              (should (= shuying-org-test--render-count 1))
              (should (= (length
                          (shuying-org--fragment-overlays
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
                          (shuying-org--fragment-overlays
                           (point-min) (point-max)))
                         2)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest shuying-org-previews-a-newly-closed-fragment ()
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
                (funcall (cdr newer) (car newer) nil)
                (let ((newer-artifact
                       (overlay-get overlay
                                    'shuying-org-artifact)))
                  (should newer-artifact)
                  (with-temp-file
                      (shuying-backend-request-output-file (car older))
                    (insert "older"))
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
