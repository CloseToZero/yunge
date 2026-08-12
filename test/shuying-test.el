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
         (shuying-backend-functions nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (calls 0)
         artifacts)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (_specification output-file done)
             (cl-incf calls)
             (with-temp-file output-file
               (insert "artifact"))
             (funcall done nil)))
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
          (should (file-exists-p (car artifacts))))
      (delete-directory root t))))

(ert-deftest shuying-coalesces-pending-render-requests ()
  (let* ((root (make-temp-file "shuying-cache-" t))
         (shuying-cache-directory root)
         (shuying-backend-functions nil)
         (shuying--pending-jobs (make-hash-table :test #'equal))
         (calls 0)
         completion
         output-file
         callbacks)
    (unwind-protect
        (progn
          (shuying-register-backend
           'test
           (lambda (_specification output done)
             (cl-incf calls)
             (setq output-file output
                   completion done)))
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
          (funcall completion nil)
          (should (= (length callbacks) 2))
          (should (seq-every-p
                   (lambda (result)
                     (and (file-exists-p (car result))
                          (null (cdr result))))
                   callbacks)))
      (delete-directory root t))))

;;; shuying-test.el ends here
