;;; yunge-reader-webview-test.el --- WebView tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-webview)

(defmacro yunge-reader-webview-test--with-fake-process (&rest body)
  "Run BODY with an isolated fake WebView process implementation."
  (declare (indent 0) (debug t))
  `(let ((system-type 'windows-nt)
         (yunge-reader-webview--process nil)
         (yunge-reader-webview--callbacks
          (make-hash-table :test #'eql))
         (yunge-reader-webview--outbound-queue nil)
         (yunge-reader-webview--next-request-id 0)
         (yunge-reader-webview--next-view-id 0)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         (yunge-reader-webview--force-stop-timer nil)
         (properties (make-hash-table :test #'equal))
         (live nil)
         sent)
     (cl-letf
         (((symbol-function
            'yunge-reader-webview--program-available-p)
           (lambda () t))
          ((symbol-function 'yunge-reader-native--build-id)
           (lambda () "test-build"))
          ((symbol-function 'make-process)
           (lambda (&rest _arguments)
             (setq live t)
             'fake-webview-process))
          ((symbol-function 'process-live-p)
           (lambda (process)
             (and live (eq process 'fake-webview-process))))
          ((symbol-function 'process-status)
           (lambda (process)
             (if (and live (eq process 'fake-webview-process))
                 'run
               'exit)))
          ((symbol-function 'process-put)
           (lambda (process property value)
             (puthash (cons process property) value properties)))
          ((symbol-function 'process-get)
           (lambda (process property)
             (gethash (cons process property) properties)))
          ((symbol-function 'process-send-string)
           (lambda (_process string)
             (push string sent)))
          ((symbol-function 'delete-process)
           (lambda (_process)
             (setq live nil))))
       ,@body)))

(defun yunge-reader-webview-test--ready-message ()
  "Return one valid test ready message."
  (copy-tree
   '((kind . "webview-ready")
     (protocol . 1)
     (build-id . "test-build")
     (platform . "windows")
     (engine . "webview2")
     (available . t)
     (version . "test-version")
     (capabilities
      . ("publication-close" "publication-info" "publication-open"
         "publication-resources" "view-bounds"
         "view-clear-selection" "view-create"
         "view-destroy" "view-events" "view-focus"
         "view-focus-parent" "view-info"
         "view-navigate" "view-open-publication"
         "view-status" "view-visible")))))

(defun yunge-reader-webview-test--location (&optional fraction)
  "Return one valid EPUB test locator with optional FRACTION."
  (append
   '((cfi . "epubcfi(/6/4!/4/2)"))
   '((href . "OPS/chapter.xhtml"))
   (when fraction `((fraction . ,fraction)))))

(ert-deftest yunge-reader-webview-queues-until-ready ()
  (yunge-reader-webview-test--with-fake-process
    (let (result)
      (yunge-reader-webview--request
       "view-info" nil
       (lambda (value error-data)
         (should-not error-data)
         (setq result value)))
      (should-not sent)
      (yunge-reader-webview--handle-message
       'fake-webview-process
       (yunge-reader-webview-test--ready-message))
      (should (= (length sent) 1))
      (let ((request
             (json-parse-string (car sent) :object-type 'alist)))
        (should (equal (alist-get 'op request) "view-info")))
      (yunge-reader-webview--handle-message
       'fake-webview-process
       '((id . 1)
         (ok . t)
         (result . ((available . t)))))
      (should (alist-get 'available result)))))

(ert-deftest yunge-reader-webview-rejects-incomplete-handshakes ()
  (yunge-reader-webview-test--with-fake-process
    (let ((message (yunge-reader-webview-test--ready-message)))
      (setf (alist-get 'capabilities message)
            '("view-create" "view-destroy"))
      (should-error
       (yunge-reader-webview--validate-ready message)
       :type 'error))))

(ert-deftest yunge-reader-webview-preserves-native-error-codes ()
  (should
   (equal
    (yunge-reader-webview--response-error
     '((error
        . ((code . "epub-limit-exceeded")
           (message . "EPUB is too large")))))
    '(yunge-reader-webview-native-error
      "epub-limit-exceeded" "EPUB is too large"))))

(ert-deftest yunge-reader-webview-validates-bounded-epub-locations ()
  (let ((location (yunge-reader-webview-test--location 0.25)))
    (should (yunge-reader-webview--valid-location-p location))
    (should (eq (yunge-reader-webview--check-location location)
                location)))
  (dolist
      (location
       '(nil
         ((cfi . "bad") (href . "OPS/chapter.xhtml"))
         ((cfi . "epubcfi(/6/4)"))
         ((cfi . "epubcfi(/6/4)") (href . "../chapter.xhtml"))
         ((cfi . "epubcfi(/6/4)") (href . "https:chapter.xhtml"))
         ((cfi . "epubcfi(/6/4)")
          (href . "OPS/chapter.xhtml") (fraction . 2))))
    (should-not (yunge-reader-webview--valid-location-p location))))

(ert-deftest yunge-reader-webview-serializes-location-navigation ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (let ((view (yunge-reader-webview--make-view :id 4))
          (location (yunge-reader-webview-test--location 0.25)))
      (yunge-reader-webview--open-view-publication
       view 7 #'ignore location)
      (yunge-reader-webview--navigate-view
       view "next-screen" #'ignore)
      (yunge-reader-webview--navigate-view
       view "go-to" #'ignore location)
      (let* ((requests
              (mapcar
               (lambda (line)
                 (json-parse-string line :object-type 'alist))
               (nreverse sent)))
             (open (nth 0 requests))
             (next (nth 1 requests))
             (go-to (nth 2 requests)))
        (should
         (equal
          (mapcar (lambda (request) (alist-get 'op request)) requests)
          '("view-open-publication" "view-navigate" "view-navigate")))
        (should
         (equal (alist-get 'location (alist-get 'params open))
                location))
        (should
         (equal (alist-get 'command (alist-get 'params next))
                "next-screen"))
        (should
         (equal (alist-get 'location (alist-get 'params go-to))
                location))))))

(ert-deftest yunge-reader-webview-wraps-publication-operations ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (let ((path (expand-file-name "book.epub" temporary-file-directory)))
      (yunge-reader-webview--open-publication
       path (lambda (_value _error-data)))
      (yunge-reader-webview--publication-info
       7 (lambda (_value _error-data)))
      (yunge-reader-webview--open-view-publication
       (yunge-reader-webview--make-view :id 4)
       7 (lambda (_value _error-data)))
      (yunge-reader-webview--close-publication
       7 (lambda (_value _error-data)))
      (let ((requests
             (mapcar
              (lambda (line)
                (json-parse-string line :object-type 'alist))
              (nreverse sent))))
        (should
         (equal
          (mapcar (lambda (request) (alist-get 'op request)) requests)
          '("publication-open" "publication-info"
            "view-open-publication" "publication-close")))
        (should
         (equal
          (alist-get 'path (alist-get 'params (car requests)))
          path))))))

(ert-deftest yunge-reader-webview-routes-escape-to-its-owning-window ()
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (view
          (yunge-reader-webview--make-view
           :id 8 :window window :buffer buffer))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests
         selected
         focused)
    (puthash 8 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--request)
          (lambda (operation parameters _complete)
            (push (list operation parameters) requests)))
         ((symbol-function 'select-window)
          (lambda (value &optional _norecord)
            (setq selected value)))
         ((symbol-function 'select-frame-set-input-focus)
          (lambda (frame &optional _norecord)
            (setq focused frame))))
      (yunge-reader-webview--handle-event
       'fake-webview-process
       '((kind . "event") (event . "escape") (view . 8))))
    (should (eq selected window))
    (should (eq focused (window-frame window)))
    (should
     (equal (nreverse requests)
            '(("view-clear-selection" ((view . 8)))
              ("view-focus-parent" ((view . 8))))))))

(ert-deftest yunge-reader-webview-events-do-not-consume-callbacks ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (process-put 'fake-webview-process 'yunge-reader-webview-ready t)
    (let (handled)
      (cl-letf (((symbol-function 'yunge-reader-webview--handle-event)
                 (lambda (process message)
                   (setq handled (list process message)))))
        (yunge-reader-webview--handle-message
         'fake-webview-process
         '((kind . "event") (event . "escape") (view . 3))))
      (should (equal handled
                     '(fake-webview-process
                       ((kind . "event")
                        (event . "escape")
                        (view . 3)))))
      (should (zerop
               (hash-table-count
                yunge-reader-webview--callbacks))))))

(ert-deftest yunge-reader-webview-records-renderer-publication-events ()
  (let* ((buffer (generate-new-buffer " *webview EPUB event*"))
         (view
          (yunge-reader-webview--make-view :id 6 :buffer buffer))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         warning)
    (unwind-protect
        (progn
          (puthash 6 view yunge-reader-webview--views)
          (yunge-reader-webview--handle-event
           'fake-webview-process
           '((kind . "event")
             (event . "publication-ready")
             (view . 6)
             (location
              . ((cfi . "epubcfi(/6/4!/4/2)")
                 (href . "OPS/chapter.xhtml")
                 (fraction . 0.25)))))
          (should
           (yunge-reader-webview--view-publication-ready view))
          (should
           (equal (yunge-reader-webview--view-location view)
                  (yunge-reader-webview-test--location 0.25)))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest value) (setq warning value))))
            (yunge-reader-webview--handle-event
             'fake-webview-process
             '((kind . "event")
               (event . "publication-error")
               (view . 6)
               (message . "bad chapter"))))
          (should-not
           (yunge-reader-webview--view-publication-ready view))
          (should (equal (cadr warning) "bad chapter"))
          (with-current-buffer buffer
            (should (equal (string-trim (buffer-string))
                           "bad chapter"))))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-keeps-locations-per-view ()
  (let* ((first (yunge-reader-webview--make-view :id 11))
         (second (yunge-reader-webview--make-view :id 12))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql)))
    (puthash 11 first yunge-reader-webview--views)
    (puthash 12 second yunge-reader-webview--views)
    (yunge-reader-webview--handle-event
     'fake-webview-process
     '((kind . "event")
       (event . "location")
       (view . 11)
       (location
        . ((cfi . "epubcfi(/6/4)")
           (href . "OPS/first.xhtml")
           (fraction . 0.2)))))
    (yunge-reader-webview--handle-event
     'fake-webview-process
     '((kind . "event")
       (event . "location")
       (view . 12)
       (location
        . ((cfi . "epubcfi(/6/8)")
           (href . "OPS/second.xhtml")
           (fraction . 0.8)))))
    (should (= (alist-get
                'fraction
                (yunge-reader-webview--view-location first))
               0.2))
    (should (= (alist-get
                'fraction
                (yunge-reader-webview--view-location second))
               0.8))))

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
           :created t
           :bounds '((x . 0) (y . 0) (width . 100) (height . 100))
           :requested-bounds
           '((x . 0) (y . 0) (width . 200) (height . 100))))
         requests)
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

(ert-deftest yunge-reader-webview-destroys-a-replaced-window-view ()
  (let* ((buffer (generate-new-buffer " *webview owner*"))
         (other (generate-new-buffer " *webview replacement*"))
         (window (selected-window))
         (view
          (yunge-reader-webview--make-view
           :id 9
           :window window
           :buffer buffer
           :created t))
         requests)
    (unwind-protect
        (let ((yunge-reader-webview--views
               (make-hash-table :test #'eql)))
          (puthash 9 view yunge-reader-webview--views)
          (cl-letf
              (((symbol-function 'window-buffer)
                (lambda (_window) other))
               ((symbol-function 'process-live-p)
                (lambda (_process) t))
               ((symbol-function 'yunge-reader-webview--request)
                (lambda (operation parameters _complete)
                  (push (list operation parameters) requests))))
            (yunge-reader-webview--sync-view view))
          (should (yunge-reader-webview--view-destroyed view))
          (should-not (gethash 9 yunge-reader-webview--views))
          (should (equal (caar requests) "view-destroy")))
      (kill-buffer buffer)
      (kill-buffer other))))

(ert-deftest yunge-reader-webview-closes-publication-after-destroy ()
  (let* ((location (yunge-reader-webview-test--location 0.6))
         (view
          (yunge-reader-webview--make-view
           :id 10 :created t :publication 3 :location location))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests)
    (puthash 10 view yunge-reader-webview--views)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'yunge-reader-webview--request)
               (lambda (operation parameters complete)
                 (push (list operation parameters complete) requests))))
      (yunge-reader-webview--destroy-view view)
      (should (equal (yunge-reader-webview--view-location view)
                     location))
      (should (equal (caar requests) "view-destroy"))
      (funcall (nth 2 (car requests)) nil nil)
      (should
       (equal (mapcar #'car (nreverse requests))
              '("view-destroy" "publication-close"))))))

;;; yunge-reader-webview-test.el ends here
