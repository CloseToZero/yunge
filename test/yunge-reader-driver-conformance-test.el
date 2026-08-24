;;; yunge-reader-driver-conformance-test.el --- Driver contracts -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-epub)
(require 'yunge-reader-pdf)

(defun yunge-reader-driver-conformance-test--invoke
    (operation arguments)
  "Invoke generic OPERATION with ARGUMENTS and return its observed result."
  (let (value error-data (calls 0))
    (yunge-reader-request
     operation arguments
     (lambda (result error)
       (cl-incf calls)
       (setq value result
             error-data error)))
    (list :value value :error error-data :calls calls)))

(defun yunge-reader-driver-conformance-test--successes (cases)
  "Assert successful generic requests described by CASES."
  (dolist (case cases)
    (pcase-let* ((`(,operation ,arguments ,predicate) case)
                 (result
                  (yunge-reader-driver-conformance-test--invoke
                   operation arguments)))
      (should (= (plist-get result :calls) 1))
      (should-not (plist-get result :error))
      (should (funcall predicate (plist-get result :value))))))

(ert-deftest yunge-reader-pdf-production-driver-conforms-to-generic-api ()
  (let* ((yunge-reader-drivers nil)
         (start (make-yunge-reader-position :unit 0 :offset 2))
         (end (make-yunge-reader-position :unit 0 :offset 8))
         (search-arguments
          (list :query "Needle" :case-sensitive t :direction 'forward
                :origin nil :cursor nil :match-limit 9 :page-limit 3))
         (selection-arguments
          (list :start start :end end :cursor nil
                :unit-limit 2 :character-limit 32)))
    (yunge-reader-pdf-register)
    (let* ((driver (yunge-reader-driver-for-file "contract.PDF"))
           (document
            (make-yunge-reader-document
             :file "contract.PDF"
             :driver driver
             :handle (make-yunge-reader-pdf-handle
                      :session 17 :id 11 :identity 'contract)
             :layout 'fixed
             :metadata '(:page-count 1 :pages
                          (((page . 0)
                            (width . 100.0) (height . 200.0)))))))
      (should (eq (yunge-reader-driver-name driver) 'pdf))
      (with-temp-buffer
        (yunge-reader-mode)
        (setq yunge-reader-document document)
        (cl-letf
            (((symbol-function 'yunge-reader-native-session-live-p)
              (lambda (session) (= session 17)))
             ((symbol-function 'yunge-reader-native-request-in-session)
              (lambda (session operation parameters complete
                       &rest _options)
                (should (= session 17))
                (should (= (alist-get 'document parameters) 11))
                (pcase operation
                  ("outline" nil)
                  ("search"
                   (should (equal (alist-get 'query parameters) "Needle"))
                   (should (= (alist-get 'match-limit parameters) 9))
                   (should (= (alist-get 'page-limit parameters) 3)))
                  ("selection-text"
                   (should (equal (alist-get 'start parameters)
                                  '((page . 0) (offset . 2))))
                   (should (= (alist-get 'character-limit parameters)
                              32)))
                  (_ (ert-fail (format "Unexpected PDF request: %s"
                                       operation))))
                (let ((value
                       (pcase operation
                         ("outline" '((items) (truncated)))
                         ("search" '((matches) (cursor) (done . t)))
                         ("selection-text"
                          '((text . "PDF text")
                            (cursor) (done . t))))))
                  (funcall complete value nil)))))
          (yunge-reader-driver-conformance-test--successes
           (list
            (list 'outline nil #'yunge-reader-outline-data-p)
            (list 'search search-arguments
                  #'yunge-reader-search-batch-p)
            (list 'selection-text selection-arguments
                  #'yunge-reader-selection-batch-p))))))))

(ert-deftest yunge-reader-epub-production-driver-conforms-to-generic-api ()
  (let* ((yunge-reader-drivers nil)
         (selection
          '((href . "OPS/chapter.xhtml")
            (start . "epubcfi(/6/4!/4/2/1:0)")
            (end . "epubcfi(/6/4!/4/2/1:8)")))
         (start
          (make-yunge-reader-position
           :unit (alist-get 'href selection)
           :offset (alist-get 'start selection)))
         (end
          (make-yunge-reader-position
           :unit (alist-get 'href selection)
           :offset (alist-get 'end selection)))
         (search-arguments
          (list :query "Needle" :case-sensitive t :direction 'forward
                :origin nil :cursor nil :match-limit 9 :page-limit 3))
         (selection-arguments
          (list :start start :end end :cursor nil
                :unit-limit 2 :character-limit 32)))
    (yunge-reader-epub-register)
    (let* ((driver (yunge-reader-driver-for-file "contract.EPUB"))
           (handle
            (make-yunge-reader-epub-handle
             :session 1
             :publication 7
             :renderer-url "http://127.0.0.1/app/index.html"
             :resource-root "http://127.0.0.1/book/"
             :pending-detaches 0))
           (document
            (make-yunge-reader-document
             :file "contract.EPUB" :driver driver :handle handle
             :layout 'reflow :metadata '(:title "Contract")))
           (view
            (yunge-reader-webview--make-view
             :publication 7 :selection selection)))
      (should (eq (yunge-reader-driver-name driver) 'epub))
      (with-temp-buffer
        (yunge-reader-mode)
        (setq yunge-reader-document document
              yunge-reader-webview--buffer-view view)
        (cl-letf
            (((symbol-function
               'yunge-reader-webview--request-view-outline)
              (lambda (actual-view complete)
                (should (eq actual-view view))
                (funcall
                 complete
                 '((items
                    . (((title . "Chapter") (depth . 0)
                        (href . "OPS/chapter.xhtml"))))
                   (truncated))
                 nil)))
             ((symbol-function 'yunge-reader-webview--request-search)
              (lambda (actual-view query case-sensitive direction
                                   origin cursor match-limit unit-limit
                                   complete &optional _revision)
                (should (eq actual-view view))
                (should (equal query "Needle"))
                (should case-sensitive)
                (should (eq direction 'forward))
                (should-not origin)
                (should-not cursor)
                (should (= match-limit 9))
                (should (= unit-limit 3))
                (funcall
                 complete '((matches) (cursor) (done . t)) nil)))
             ((symbol-function
               'yunge-reader-webview--request-selection-text)
              (lambda (actual-view actual-selection offset
                                   character-limit complete
                                   &optional _revision)
                (should (eq actual-view view))
                (should (equal actual-selection selection))
                (should (zerop offset))
                (should (= character-limit 32))
                (funcall
                 complete
                 '((text . "EPUB text")
                   (next-offset) (done . t))
                 nil))))
          (yunge-reader-driver-conformance-test--successes
           (list
            (list 'outline nil #'yunge-reader-outline-data-p)
            (list 'search search-arguments
                  #'yunge-reader-search-batch-p)
            (list 'selection-text selection-arguments
                  #'yunge-reader-selection-batch-p))))))))

;;; yunge-reader-driver-conformance-test.el ends here
