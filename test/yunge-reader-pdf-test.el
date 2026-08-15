;;; yunge-reader-pdf-test.el --- PDF reader tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-pdf)

(ert-deftest yunge-reader-pdf-registers-only-when-requested ()
  (let ((yunge-reader-drivers nil)
        (modes auto-mode-alist))
    (should-not (yunge-reader-driver-for-file "book.pdf"))
    (yunge-reader-pdf-register)
    (should
     (eq (yunge-reader-driver-name
          (yunge-reader-driver-for-file "book.PDF"))
         'pdf))
    (should (equal auto-mode-alist modes))))

(ert-deftest yunge-reader-pdf-open-and-close-balance-native-lease ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let (opened
          properties
          error-data
          request
          (leases 0))
      (cl-letf (((symbol-function 'yunge-reader-native-acquire)
                 (lambda () (cl-incf leases)))
                ((symbol-function 'yunge-reader-native-release)
                 (lambda () (cl-decf leases)))
                ((symbol-function 'yunge-reader-native-live-p)
                 (lambda () t))
                ((symbol-function 'yunge-reader-native-request)
                 (lambda (operation parameters complete)
                   (setq request (list operation parameters))
                   (pcase operation
                     ("open"
                      (funcall complete
                               '((document . 7)
                                 (page-count . 3)
                                 (layout . "fixed"))
                               nil))
                     ("close"
                      (funcall complete '((closed . t)) nil))))))
        (yunge-reader-pdf--open
         "C:/books/test.pdf"
         (lambda (handle value error)
           (setq opened handle
                 properties value
                 error-data error)))
        (should (= leases 1))
        (should (= opened 7))
        (should-not error-data)
        (should (eq (plist-get properties :layout) 'fixed))
        (should (= (plist-get (plist-get properties :metadata)
                              :page-count)
                   3))
        (should (equal (car request) "open"))
        (yunge-reader-pdf--close
         (make-yunge-reader-document :handle opened))
        (should (zerop leases))
        (should (equal (car request) "close"))))))

(ert-deftest yunge-reader-pdf-open-failure-releases-native-lease ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((leases 0)
          completion-error)
      (cl-letf (((symbol-function 'yunge-reader-native-acquire)
                 (lambda () (cl-incf leases)))
                ((symbol-function 'yunge-reader-native-release)
                 (lambda () (cl-decf leases)))
                ((symbol-function 'yunge-reader-native-request)
                 (lambda (_operation _parameters complete)
                   (funcall complete nil '(error "cannot open")))))
        (yunge-reader-pdf--open
         "C:/books/broken.pdf"
         (lambda (_handle _properties error-data)
           (setq completion-error error-data))))
      (should (zerop leases))
      (should (equal completion-error '(error "cannot open"))))))

(ert-deftest yunge-reader-pdf-close-does-not-restart-a-stopped-helper ()
  (let ((leases 1)
        requested)
    (cl-letf (((symbol-function 'yunge-reader-native-live-p)
               (lambda () nil))
              ((symbol-function 'yunge-reader-native-release)
               (lambda () (cl-decf leases)))
              ((symbol-function 'yunge-reader-native-request)
               (lambda (&rest _arguments) (setq requested t))))
      (yunge-reader-pdf--close
       (make-yunge-reader-document :handle 7)))
    (should (zerop leases))
    (should-not requested)))

(ert-deftest yunge-reader-pdf-maps-reader-requests-to-native-operations ()
  (let ((document (make-yunge-reader-document :handle 11))
        calls)
    (cl-letf (((symbol-function 'yunge-reader-native-request)
               (lambda (operation parameters _complete)
                 (push (cons operation parameters) calls))))
      (yunge-reader-pdf--request
       document 'page-info '(:page 2) #'ignore)
      (yunge-reader-pdf--request
       document 'page-text '(:page 2) #'ignore)
      (yunge-reader-pdf--request
       document 'render-page
       '(:page 2 :width 900 :cache-key "key") #'ignore))
    (setq calls (nreverse calls))
    (should (equal (caar calls) "page-info"))
    (should (equal (cdr (assq 'document (cdar calls))) 11))
    (should (equal (cdr (assq 'page (cdar calls))) 2))
    (should (equal (caadr calls) "page-text"))
    (should (equal (caaddr calls) "render-page"))
    (should (equal (cdr (assq 'width (cdaddr calls))) 900))
    (should (equal (cdr (assq 'cache-key (cdaddr calls))) "key"))))

(ert-deftest yunge-reader-pdf-resolves-selection-text-natively ()
  (let* ((document (make-yunge-reader-document :handle 11))
         (start (make-yunge-reader-position :unit 2 :offset 9))
         (end (make-yunge-reader-position :unit 2 :offset 4))
         request
         result)
    (cl-letf (((symbol-function 'yunge-reader-native-request)
               (lambda (operation parameters complete)
                 (setq request (cons operation parameters))
                 (funcall complete '((text . "exact text")) nil))))
      (yunge-reader-pdf--request
       document 'selection-text
       (list :start start :end end)
       (lambda (value error-data)
         (should-not error-data)
         (setq result value))))
    (should (equal (car request) "selection-text"))
    (should (equal (cdr (assq 'page (cdr request))) 2))
    (should (equal (cdr (assq 'start (cdr request))) 9))
    (should (equal (cdr (assq 'end (cdr request))) 4))
    (should (equal result "exact text"))))

(ert-deftest yunge-reader-pdf-resolves-fit-and-manual-widths ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((yunge-reader-pdf-page-margin 24)
          (page-info '((width . 612.0) (height . 792.0))))
      (cl-letf (((symbol-function 'window-body-width)
                 (lambda (_window _pixelwise) 1000))
                ((symbol-function 'window-body-height)
                 (lambda (_window _pixelwise) 800)))
        (setq yunge-reader-zoom-mode 'fit-width)
        (should (= (yunge-reader-pdf--target-width page-info 'window)
                   952))
        (setq yunge-reader-zoom-mode 'fit-page)
        (should (= (yunge-reader-pdf--target-width page-info 'window)
                   581))
        (setq yunge-reader-zoom-mode 'manual
              yunge-reader-scale 2.0)
        (should (= (yunge-reader-pdf--target-width page-info 'window)
                   1632))
        (should (= yunge-reader-effective-scale 2.0))))))

(ert-deftest yunge-reader-pdf-rejects-stale-render-completions ()
  (with-temp-buffer
    (yunge-reader-mode)
    (setq yunge-reader-pdf--generation 2)
    (let (created)
      (cl-letf (((symbol-function 'create-image)
                 (lambda (&rest _arguments) (setq created t))))
        (yunge-reader-pdf--display-image
         1 0 '((path . "stale.png")) nil))
      (should-not created))))

(ert-deftest yunge-reader-pdf-converts-display-to-page-coordinates ()
  (with-temp-buffer
    (setq yunge-reader-pdf--page-info
          '((width . 100.0) (height . 200.0)))
    (let ((point
           (yunge-reader-pdf--pixel-to-page-point
            500 250 1000 1000)))
      (should (= (car point) 50.0))
      (should (= (cdr point) 150.0)))))

(ert-deftest yunge-reader-pdf-hit-testing-creates-logical-selection ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf-page 0
          yunge-reader-pdf--page-info
          '((width . 100.0) (height . 200.0)))
    (puthash
     0
     '((characters
        . (((index . 4)
            (text . "A")
            (bounds . ((left . 10.0) (bottom . 10.0)
                       (right . 20.0) (top . 20.0))))
           ((index . 5)
            (text . "B")
            (bounds . ((left . 22.0) (bottom . 10.0)
                       (right . 32.0) (top . 20.0)))))))
     yunge-reader-pdf--text-cache)
    (cl-letf (((symbol-function 'yunge-reader-pdf--paint-image)
               #'ignore))
      (yunge-reader-pdf--select-points
       '(15.0 . 15.0) '(27.0 . 15.0)))
    (should (= (yunge-reader-position-offset
                (yunge-reader-selection-start
                 yunge-reader-selection))
               4))
    (should (= (yunge-reader-position-offset
                (yunge-reader-selection-end
                 yunge-reader-selection))
               5))))

(ert-deftest yunge-reader-pdf-caches-page-text-once ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (let ((requests 0)
          (result '((page . 0) (characters . ()))))
      (cl-letf (((symbol-function 'yunge-reader-request)
                 (lambda (operation arguments complete)
                   (cl-incf requests)
                   (should (eq operation 'page-text))
                   (should (= (plist-get arguments :page) 0))
                   (funcall complete result nil))))
        (yunge-reader-pdf--request-text 0)
        (yunge-reader-pdf--request-text 0))
      (should (= requests 1))
      (should (eq (gethash 0 yunge-reader-pdf--text-cache)
                  result)))))

(ert-deftest yunge-reader-pdf-prioritizes-image-before-text-layer ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--generation 1)
    (let (operations)
      (cl-letf (((symbol-function 'yunge-reader-pdf--target-width)
                 (lambda (_page-info &optional _window) 900))
                ((symbol-function 'yunge-reader-pdf--cache-key)
                 (lambda (_page _width) "key"))
                ((symbol-function 'yunge-reader-request)
                 (lambda (operation _arguments _complete)
                   (push operation operations))))
        (yunge-reader-pdf--render-with-info
         1 0 '((width . 100.0) (height . 200.0)) nil))
      (should (equal (nreverse operations)
                     '(render-page page-text))))))

(ert-deftest yunge-reader-pdf-paints-selection-in-svg-coordinates ()
  (with-temp-buffer
    (setq yunge-reader-pdf-page 0
          yunge-reader-pdf--page-info
          '((width . 100.0) (height . 200.0))
          yunge-reader-selection
          (make-yunge-reader-selection
           :start (make-yunge-reader-position
                   :unit 0 :offset 5)
           :end (make-yunge-reader-position
                 :unit 0 :offset 4)))
    (let ((svg (svg-create 1000 1000)))
      (yunge-reader-pdf--paint-selection
       svg
       '((characters
          . (((index . 4)
              (bounds . ((left . 10.0) (bottom . 10.0)
                         (right . 20.0) (top . 20.0))))
             ((index . 5)
              (bounds . ((left . 22.0) (bottom . 10.0)
                         (right . 32.0) (top . 20.0)))))))
       1000 1000)
      (let ((rectangles (dom-by-tag svg 'rect)))
        (should (= (length rectangles) 2))
        (should (= (dom-attr (car rectangles) 'x) 100.0))
        (should (= (dom-attr (car rectangles) 'y) 900.0))
        (should (= (dom-attr (car rectangles) 'width) 100.0))
        (should (= (dom-attr (car rectangles) 'height) 50.0))))))

;;; yunge-reader-pdf-test.el ends here
