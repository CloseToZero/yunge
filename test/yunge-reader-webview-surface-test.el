;;; yunge-reader-webview-surface-test.el --- Tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-webview-surface)

(ert-deftest yunge-reader-webview-surface-loads-without-presentation ()
  (yunge-test-run-emacs
   "-l" "yunge-reader-webview-surface"
   "--eval"
   (prin1-to-string
    '(when (featurep 'yunge-reader-webview)
       (error "Surface lifecycle loaded presentation integration")))))

(ert-deftest yunge-reader-webview-parses-decimal-and-hex-frame-handles ()
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame _parameter) "12345")))
    (should (= (yunge-reader-webview--frame-handle 'frame) 12345)))
  (cl-letf (((symbol-function 'frame-parameter)
             (lambda (_frame _parameter) "0x2a")))
    (should (= (yunge-reader-webview--frame-handle 'frame) 42))))

(ert-deftest yunge-reader-webview-coalesces-window-resizes ()
  (let* ((view
          (yunge-reader-webview--make-view
           :id 7
           :surface-state 'native-ready
           :bounds '((x . 0) (y . 0) (width . 100) (height . 100))
           :requested-bounds
           '((x . 0) (y . 0) (width . 200) (height . 100))))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests)
    (puthash 7 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--request)
          (lambda (_operation parameters complete)
            (push (cons parameters complete) requests))))
      (yunge-reader-webview--send-latest-bounds view)
      (setf (yunge-reader-webview--view-requested-bounds view)
            '((x . 0) (y . 0) (width . 300) (height . 100)))
      (yunge-reader-webview--send-latest-bounds view)
      (should (= (length requests) 1))
      (funcall (cdar requests) nil nil)
      (should (= (length requests) 2))
      (should
       (= (alist-get
           'width
           (alist-get 'bounds (caar requests)))
          300)))))

(ert-deftest yunge-reader-webview-ignores-obsolete-surface-bounds ()
  (let* ((view
          (yunge-reader-webview--make-view
           :id 7
           :surface-state 'native-ready
           :bounds '((x . 0) (y . 0) (width . 100) (height . 100))
           :requested-bounds
           '((x . 0) (y . 0) (width . 200) (height . 100))))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests)
    (puthash 7 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--request)
          (lambda (_operation parameters complete)
            (push (cons parameters complete) requests))))
      (yunge-reader-webview--send-latest-bounds view)
      (remhash 7 yunge-reader-webview--views)
      (setf (yunge-reader-webview--view-id view) 8
            (yunge-reader-webview--view-bounds view) nil
            (yunge-reader-webview--view-bounds-pending view) t)
      (puthash 8 view yunge-reader-webview--views)
      (funcall (cdar requests) nil nil)
      (should (yunge-reader-webview--view-bounds-pending view))
      (should-not (yunge-reader-webview--view-bounds view))
      (should (= (length requests) 1)))))

(provide 'yunge-reader-webview-surface-test)

;;; yunge-reader-webview-surface-test.el ends here
