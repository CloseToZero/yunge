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

(ert-deftest yunge-reader-webview-parses-decimal-and-hex-native-identifiers ()
  ;; Any non-macOS value exercises the native frame-identifier path without
  ;; making native-comp install Windows trampolines on a macOS test host.
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame _parameter) "12345")))
      (should (= (yunge-reader-webview--frame-handle 'frame) 12345)))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame _parameter) "0x2a")))
      (should (= (yunge-reader-webview--frame-handle 'frame) 42)))))

(ert-deftest yunge-reader-webview-uses-frame-id-as-macos-parent ()
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (should (eq parameter 'window-id))
                 "4321")))
      (should (= (yunge-reader-webview--frame-handle 'frame) 4321)))))

(ert-deftest yunge-reader-webview-uses-relative-macos-window-bounds ()
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'window-body-pixel-edges)
               (lambda (_window) '(10 20 210 320))))
      (should
       (equal (yunge-reader-webview--window-bounds 'window)
              '((x . 10) (y . 20) (width . 200) (height . 300)))))))

(ert-deftest yunge-reader-webview-describes-the-parent-frame ()
  (cl-letf (((symbol-function 'frame-position)
             (lambda (_frame) '(30 40)))
            ((symbol-function 'frame-pixel-width)
             (lambda (_frame) 900))
            ((symbol-function 'frame-pixel-height)
             (lambda (_frame) 700)))
    (should
     (equal (yunge-reader-webview--frame-bounds 'frame)
            '((x . 30) (y . 40) (width . 900) (height . 700))))))

(ert-deftest yunge-reader-webview-omits-an-unplaced-parent-frame ()
  (cl-letf (((symbol-function 'frame-position)
             (lambda (_frame) '(nil nil))))
    (should-not (yunge-reader-webview--frame-bounds 'frame))))

(ert-deftest yunge-reader-webview-creates-macos-surfaces-visible-in-frame ()
  (let* ((system-type 'darwin)
         (surface
          (yunge-reader-webview--make-surface
           :id 7
           :window 'window
           :requested-bounds
           '((x . 10) (y . 20) (width . 300) (height . 400))))
         (view
          (yunge-reader-webview--make-view
           :surface surface
           :renderer-url
           "http://127.0.0.1:32123/0123456789abcdef0123456789abcdef/app/index.html"))
         request)
    (cl-letf (((symbol-function 'window-frame)
               (lambda (_window) 'frame))
              ((symbol-function 'yunge-reader-webview--frame-handle)
               (lambda (_frame) 1234))
              ((symbol-function 'yunge-reader-webview--frame-bounds)
               (lambda (_frame)
                 '((x . 30) (y . 40) (width . 900) (height . 700))))
              ((symbol-function 'yunge-reader-webview--request)
               (lambda (operation parameters complete)
                 (setq request (list operation parameters complete)))))
      (yunge-reader-webview--request-create view))
    (should (equal (car request) "view-create"))
    (should
     (equal (alist-get 'frame (cadr request))
            '((x . 30) (y . 40) (width . 900) (height . 700))))
    (should (eq (alist-get 'visible (cadr request)) t))))

(ert-deftest yunge-reader-webview-bounds-renderer-open-times-out ()
  (let* ((surface
          (yunge-reader-webview--make-surface
           :id 7 :state 'opening))
         (view
          (yunge-reader-webview--make-view
           :surface surface :buffer (current-buffer)))
         warning)
    (cl-letf (((symbol-function
                'yunge-reader-webview--set-buffer-message)
               (lambda (_view message) (setq warning message)))
              ((symbol-function 'display-warning) #'ignore))
      (yunge-reader-webview--open-watchdog view 7))
    (should (eq (yunge-reader-webview--surface-state surface) 'failed))
    (should (equal warning "Timed out while opening the EPUB renderer"))))

(ert-deftest yunge-reader-webview-coalesces-window-resizes ()
  (let* ((view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview--make-surface
            :id 7
            :state 'native-ready
            :bounds '((x . 0) (y . 0) (width . 100) (height . 100))
            :requested-bounds
            '((x . 0) (y . 0) (width . 200) (height . 100)))))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests)
    (puthash 7 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--request)
          (lambda (_operation parameters complete)
            (push (cons parameters complete) requests))))
      (yunge-reader-webview--send-latest-bounds view)
      (setf (yunge-reader-webview--surface-requested-bounds
             (yunge-reader-webview--view-surface view))
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
           :surface
           (yunge-reader-webview--make-surface
            :id 7
            :state 'native-ready
            :bounds '((x . 0) (y . 0) (width . 100) (height . 100))
            :requested-bounds
            '((x . 0) (y . 0) (width . 200) (height . 100)))))
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
      (setf (yunge-reader-webview--view-surface view)
            (yunge-reader-webview--make-surface
             :id 8 :state 'native-ready :bounds-pending t))
      (puthash 8 view yunge-reader-webview--views)
      (funcall (cdar requests) nil nil)
      (should
       (yunge-reader-webview--surface-bounds-pending
        (yunge-reader-webview--view-surface view)))
      (should-not
       (yunge-reader-webview--surface-bounds
        (yunge-reader-webview--view-surface view)))
      (should (= (length requests) 1)))))

(provide 'yunge-reader-webview-surface-test)

;;; yunge-reader-webview-surface-test.el ends here
