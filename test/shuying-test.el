;;; shuying-test.el --- Shuying rendering tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'shuying)

(defun shuying-test--spec (&optional source)
  "Return a render specification for SOURCE."
  (make-shuying-render-spec
   :source (or source "$x$")
   :preamble "preamble"
   :engine '("latex input.tex")
   :backend 'test
   :backend-options '(:converter "dvisvgm input.dvi")
   :output-format "svg"
   :foreground "black"
   :background "white"
   :scale 1.0
   :cache-version shuying-cache-format-version))

(ert-deftest shuying-keeps-artifacts-under-var ()
  (should
   (equal shuying-cache-directory
          (yunge-var-subdirectory "shuying/cache"))))

(ert-deftest shuying-keeps-render-work-under-var ()
  (should
   (equal shuying-work-directory
          (yunge-var-subdirectory "shuying/work"))))

(ert-deftest shuying-render-spec-hash-covers-rendering-inputs ()
  (let* ((first (shuying-test--spec))
         (second (copy-shuying-render-spec first)))
    (should (equal (shuying-render-spec-hash first)
                   (shuying-render-spec-hash second)))
    (setf (shuying-render-spec-scale second) 2.0)
    (should-not (equal (shuying-render-spec-hash first)
                       (shuying-render-spec-hash second)))))

(ert-deftest shuying-reuses-a-cached-artifact ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (calls 0)
         artifacts)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (requests complete)
             (cl-incf calls)
             (dolist (request requests)
               (with-temp-file
                   (shuying-backend-request-output-file request)
                 (insert "artifact"))
               (setf (shuying-backend-request-metadata request)
                     '(:height 1.2 :depth 0.2))
               (funcall complete request nil))))
          (let ((specification (shuying-test--spec)))
            (shuying-render
             specification
             (lambda (artifact error-data)
               (should-not error-data)
               (push artifact artifacts)))
            (shuying-render
             specification
             (lambda (artifact error-data)
               (should-not error-data)
               (push artifact artifacts))))
          (should (= calls 1))
          (should (= (length artifacts) 2))
          (should (equal (car artifacts) (cadr artifacts)))
          (should
           (file-exists-p (shuying-artifact-path (car artifacts))))
          (should
           (equal (shuying-artifact-metadata (car artifacts))
                  '(:height 1.2 :depth 0.2))))
      (delete-directory root t))))

(ert-deftest shuying-coalesces-pending-render-requests ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (calls 0)
         completion
         backend-request
         output-file
         callbacks)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (requests complete)
             (cl-incf calls)
             (setq backend-request (car requests)
                   output-file
                   (shuying-backend-request-output-file backend-request)
                   completion complete)))
          (let ((specification (shuying-test--spec)))
            (shuying-render
             specification
             (lambda (artifact error-data)
               (push (cons artifact error-data) callbacks)))
            (shuying-render
             specification
             (lambda (artifact error-data)
               (push (cons artifact error-data) callbacks))))
          (should (= calls 1))
          (should-not callbacks)
          (with-temp-file output-file
            (insert "artifact"))
          (setf (shuying-backend-request-metadata backend-request)
                '(:height 1.2 :depth 0.2))
          (funcall completion backend-request nil)
          (should (= (length callbacks) 2))
          (should (seq-every-p
                   (lambda (result)
                     (and (file-exists-p
                           (shuying-artifact-path (car result)))
                          (null (cdr result))))
                   callbacks)))
      (delete-directory root t))))

(ert-deftest shuying-groups-compatible-render-requests ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (first (shuying-test--spec "$x$"))
         (second (shuying-test--spec "$y$"))
         (third (shuying-test--spec "$z$"))
         batches
         artifacts)
    (setf (shuying-render-spec-preamble third) "other preamble")
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (requests complete)
             (push
              (mapcar
               (lambda (request)
                 (shuying-render-spec-source
                  (shuying-backend-request-specification request)))
               requests)
              batches)
             (dolist (request requests)
               (with-temp-file
                   (shuying-backend-request-output-file request)
                 (insert "artifact"))
               (setf (shuying-backend-request-metadata request)
                     '(:height 1.2 :depth 0.2))
               (funcall complete request nil)))
           #'shuying-render-spec-preamble)
          (shuying-render-batch
           (mapcar
            (lambda (specification)
              (cons
               specification
               (lambda (artifact error-data)
                 (should-not error-data)
                 (push artifact artifacts))))
            (list first second third)))
          (should
           (equal (nreverse batches)
                  '(("$x$" "$y$") ("$z$"))))
          (should (= (length artifacts) 3))
          (should
           (seq-every-p
            (lambda (artifact)
              (file-exists-p (shuying-artifact-path artifact)))
            artifacts)))
      (delete-directory root t))))

(ert-deftest shuying-limits-render-batches-and-preserves-fifo-order ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying--waiting-batches nil)
         (shuying--active-batch-count 0)
         (shuying--scheduler-running nil)
         (shuying-max-concurrent-batches 2)
         controls
         started
         results)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (requests complete)
             (let ((source
                    (shuying-render-spec-source
                     (shuying-backend-request-specification
                      (car requests)))))
               (push source started)
               (push (list source (car requests) complete)
                     controls)))
           #'shuying-render-spec-preamble)
          (cl-labels
              ((complete
                (source)
                (pcase-let* ((`(,_ ,request ,callback)
                              (assoc source controls))
                             (output
                              (shuying-backend-request-output-file
                               request)))
                  (with-temp-file output
                    (insert source))
                  (funcall callback request nil))))
            (shuying-render-batch
             (mapcar
              (lambda (source)
                (let ((specification (shuying-test--spec source)))
                  (setf (shuying-render-spec-preamble specification)
                        source)
                  (cons
                   specification
                   (lambda (artifact error-data)
                     (should-not error-data)
                     (push artifact results)))))
              '("$a$" "$b$" "$c$" "$d$")))
            (should (equal (reverse started) '("$a$" "$b$")))
            (should (= shuying--active-batch-count 2))
            (should (= (length shuying--waiting-batches) 2))
            (complete "$a$")
            (should (equal (reverse started)
                           '("$a$" "$b$" "$c$")))
            (complete "$b$")
            (should (equal (reverse started)
                           '("$a$" "$b$" "$c$" "$d$")))
            (complete "$c$")
            (complete "$d$")
            (should (= (length results) 4))
            (should (zerop shuying--active-batch-count))
            (should-not shuying--waiting-batches)))
      (delete-directory root t))))

(ert-deftest shuying-holds-a-batch-slot-until-every-request-finishes ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying--waiting-batches nil)
         (shuying--active-batch-count 0)
         (shuying--scheduler-running nil)
         (shuying-max-concurrent-batches 1)
         calls
         results)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (requests complete)
             (push (cons requests complete) calls))
           #'shuying-render-spec-preamble)
          (let ((first (shuying-test--spec "$x$"))
                (second (shuying-test--spec "$y$"))
                (third (shuying-test--spec "$z$")))
            (setf (shuying-render-spec-preamble third) "other")
            (shuying-render-batch
             (mapcar
              (lambda (specification)
                (cons
                 specification
                 (lambda (artifact error-data)
                   (should-not error-data)
                   (push artifact results))))
              (list first second third))))
          (should (= (length calls) 1))
          (should (= (length shuying--waiting-batches) 1))
          (pcase-let* ((`(,requests . ,complete) (car calls))
                       (first (car requests))
                       (second (cadr requests)))
            (dolist (request (list first second))
              (with-temp-file
                  (shuying-backend-request-output-file request)
                (insert "artifact")))
            (funcall complete first nil)
            (should (= (length calls) 1))
            (should (= shuying--active-batch-count 1))
            (funcall complete second nil))
          (should (= (length calls) 2))
          (should-not shuying--waiting-batches)
          (pcase-let* ((`(,requests . ,complete) (car calls))
                       (request (car requests)))
            (with-temp-file
                (shuying-backend-request-output-file request)
              (insert "artifact"))
            (funcall complete request nil))
          (should (= (length results) 3))
          (should (zerop shuying--active-batch-count)))
      (delete-directory root t))))

(ert-deftest shuying-dispatches-synchronous-batches-without-reentry ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying--waiting-batches nil)
         (shuying--active-batch-count 0)
         (shuying--scheduler-running nil)
         (shuying-max-concurrent-batches 1)
         (depth 0)
         (maximum-depth 0)
         started)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (requests complete)
             (cl-incf depth)
             (setq maximum-depth (max depth maximum-depth))
             (let* ((request (car requests))
                    (source
                     (shuying-render-spec-source
                      (shuying-backend-request-specification
                       request))))
               (push source started)
               (with-temp-file
                   (shuying-backend-request-output-file request)
                 (insert source))
               (funcall complete request nil))
             (cl-decf depth))
           #'shuying-render-spec-preamble)
          (shuying-render-batch
           (mapcar
            (lambda (source)
              (let ((specification (shuying-test--spec source)))
                (setf (shuying-render-spec-preamble specification)
                      source)
                (cons specification #'ignore)))
            '("$a$" "$b$" "$c$")))
          (should (equal (reverse started) '("$a$" "$b$" "$c$")))
          (should (= maximum-depth 1))
          (should (zerop shuying--active-batch-count))
          (should-not shuying--waiting-batches))
      (delete-directory root t))))

(ert-deftest shuying-dispatches-the-next-batch-after-an-error ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (shuying--waiting-batches nil)
         (shuying--active-batch-count 0)
         (shuying--scheduler-running nil)
         (shuying-max-concurrent-batches 1)
         started
         results)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (requests complete)
             (let* ((request (car requests))
                    (source
                     (shuying-render-spec-source
                      (shuying-backend-request-specification
                       request))))
               (push source started)
               (if (equal source "$bad$")
                   (error "Backend failed")
                 (with-temp-file
                     (shuying-backend-request-output-file request)
                   (insert source))
                 (funcall complete request nil))))
           #'shuying-render-spec-preamble)
          (shuying-render-batch
           (mapcar
            (lambda (source)
              (let ((specification (shuying-test--spec source)))
                (setf (shuying-render-spec-preamble specification)
                      source)
                (cons
                 specification
                 (lambda (artifact error-data)
                   (push (cons artifact error-data) results)))))
            '("$bad$" "$good$")))
          (should (equal (reverse started) '("$bad$" "$good$")))
          (should (= (length results) 2))
          (should (= (length (seq-filter #'car results)) 1))
          (should (= (length (seq-filter #'cdr results)) 1))
          (should (zerop shuying--active-batch-count))
          (should-not shuying--waiting-batches))
      (delete-directory root t))))

(ert-deftest shuying-finishes-a-partially-failed-backend-batch ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backends nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         results)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (requests complete)
             (let ((first (car requests)))
               (with-temp-file
                   (shuying-backend-request-output-file first)
                 (insert "artifact"))
               (setf (shuying-backend-request-metadata first)
                     '(:height 1.2 :depth 0.2))
               (funcall complete first nil))
             (error "Batch failed")))
          (shuying-render-batch
           (mapcar
            (lambda (specification)
              (cons
               specification
               (lambda (artifact error-data)
                 (push
                  (list
                   (shuying-render-spec-source specification)
                   artifact
                   error-data)
                  results))))
            (list (shuying-test--spec "$x$")
                  (shuying-test--spec "$y$"))))
          (should (= (length results) 2))
          (let ((first (assoc "$x$" results))
                (second (assoc "$y$" results)))
            (should
             (file-exists-p
              (shuying-artifact-path (cadr first))))
            (should-not (caddr first))
            (should-not (cadr second))
            (should (equal (caddr second) '(error "Batch failed"))))
          (should (= (hash-table-count shuying--pending-jobs) 0)))
      (delete-directory root t))))

;;; shuying-test.el ends here
