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
         (yunge-reader-webview--logical-views
          (make-hash-table :test #'eq))
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
         "view-status" "view-style" "view-visible")))))

(defun yunge-reader-webview-test--location (&optional fraction)
  "Return one valid EPUB test locator with optional FRACTION."
  (append
   '((cfi . "epubcfi(/6/4!/4/2)"))
   '((href . "OPS/chapter.xhtml"))
   (when fraction `((fraction . ,fraction)))))

(defun yunge-reader-webview-test--outline ()
  "Return one valid bounded EPUB test outline."
  (copy-tree
   '((items
      . (((title . "Part One") (depth . 0))
         ((title . "Chapter")
          (depth . 1)
          (href . "OPS/chapter.xhtml#start"))))
     (truncated))))

(defun yunge-reader-webview-test--selection ()
  "Return one valid bounded EPUB test selection."
  (copy-tree
   '((href . "OPS/chapter.xhtml")
     (start . "epubcfi(/6/4!/4/2/1:0)")
     (end . "epubcfi(/6/4!/4/2/1:7)"))))

(defun yunge-reader-webview-test--style ()
  "Return one valid semantic EPUB reading style."
  (copy-tree
   '((font-scale . 1.25)
     (line-height . 1.6)
     (content-width . 760)
     (side-padding . 8.0))))

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

(ert-deftest yunge-reader-webview-validates-bounded-epub-targets ()
  (should
   (yunge-reader-webview--valid-target-p
    '((href . "OPS/chapter.xhtml#section"))))
  (should
   (yunge-reader-webview--valid-outline-p
    (yunge-reader-webview-test--outline)))
  (dolist (target
           '(((href . "../chapter.xhtml"))
             ((href . "https:chapter.xhtml"))
             ((href . "OPS/chapter.xhtml?query"))
             ((href . "OPS/chapter.xhtml#one#two"))))
    (should-not (yunge-reader-webview--valid-target-p target))))

(ert-deftest yunge-reader-webview-validates-bounded-epub-selections ()
  (should
   (yunge-reader-webview--valid-selection-p
    (yunge-reader-webview-test--selection)))
  (dolist
      (selection
       '(nil
         ((href . "OPS/chapter.xhtml")
          (start . "bad")
          (end . "epubcfi(/6/4!/4/2/1:7)"))
         ((href . "../chapter.xhtml")
          (start . "epubcfi(/6/4!/4/2/1:0)")
          (end . "epubcfi(/6/4!/4/2/1:7)"))
         ((href . "OPS/chapter.xhtml")
          (start . "epubcfi(/6/4!/4/2/1:0)")
          (end . "epubcfi(/6/4!/4/2/1:0)"))
         ((href . "OPS/chapter.xhtml")
          (start . "epubcfi(/6/4!/4/2/1:0)")
          (end . "epubcfi(/6/4!/4/2/1:7)")
          (text . "chapter"))))
    (should-not
     (yunge-reader-webview--valid-selection-p selection)))
  (should-not
   (yunge-reader-webview--valid-selection-p
    `((href . "OPS/chapter.xhtml")
      (start . "epubcfi(/6/4!/4/2/1:0)")
      (end . ,(format "epubcfi(%s)" (make-string 3072 ?x)))))))

(ert-deftest yunge-reader-webview-validates-bounded-epub-styles ()
  (let ((style (yunge-reader-webview-test--style)))
    (should (yunge-reader-webview--valid-style-p style))
    (should
     (yunge-reader-webview--valid-style-p (reverse style)))
    (should (eq (yunge-reader-webview--check-style style) style)))
  (dolist
      (style
       '(((font-scale . 0.49)
          (line-height . 1.6)
          (content-width . 720)
          (side-padding . 7.0))
         ((font-scale . 1.0)
          (line-height . 3.1)
          (content-width . 720)
          (side-padding . 7.0))
         ((font-scale . 1.0)
          (line-height . 1.6)
          (content-width . 319)
          (side-padding . 7.0))
         ((font-scale . 1.0)
          (line-height . 1.6)
          (content-width . 720)
          (side-padding . 20.1))
         ((font-scale . 1.0)
          (line-height . 1.6)
          (content-width . 720)
          (color . "red"))))
    (should-not (yunge-reader-webview--valid-style-p style))))

(ert-deftest yunge-reader-webview-defers-hidden-reading-style ()
  (let* ((style (yunge-reader-webview-test--style))
         (view (yunge-reader-webview--make-view :publication 8))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--set-native-view-style)
          (lambda (value reading-style complete)
            (push (list value reading-style complete) requests))))
      (yunge-reader-webview--set-view-style view style)
      (should-not requests)
      (should (equal (yunge-reader-webview--view-style view) style))
      (should-not (eq (yunge-reader-webview--view-style view) style))
      (setcdr (assq 'font-scale style) 2.0)
      (should
       (= (alist-get
           'font-scale (yunge-reader-webview--view-style view))
          1.25))
      (setf (yunge-reader-webview--view-id view) 23
            (yunge-reader-webview--view-created view) t
            (yunge-reader-webview--view-publication-ready view) t)
      (puthash 23 view yunge-reader-webview--views)
      (yunge-reader-webview--sync-view-style view)
      (should (= (length requests) 1))
      (should
       (equal (cadar requests)
              (yunge-reader-webview--view-style view)))
      (should
       (equal (yunge-reader-webview--view-surface-style view)
              (yunge-reader-webview--view-style view))))))

(ert-deftest yunge-reader-webview-reconciles-style-after-opening ()
  (let* ((style (yunge-reader-webview-test--style))
         (old-style (copy-tree style))
         (view
          (yunge-reader-webview--make-view
           :id 24 :created t :publication 8
           :style style :surface-style old-style))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requested)
    (setcdr (assq 'font-scale style) 1.5)
    (puthash 24 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--set-native-view-style)
          (lambda (_view reading-style _complete)
            (setq requested reading-style))))
      (yunge-reader-webview--handle-event
       'fake-webview-process
       `((kind . "event")
         (event . "publication-ready")
         (view . 24)
         (location . ,(yunge-reader-webview-test--location 0.25))
         (outline . ,(yunge-reader-webview-test--outline)))))
    (should (equal requested style))
    (should
     (equal (yunge-reader-webview--view-surface-style view) style))))

(ert-deftest yunge-reader-webview-style-failure-is-retryable ()
  (let* ((style (yunge-reader-webview-test--style))
         (view
          (yunge-reader-webview--make-view
           :id 25 :created t :publication 8
           :publication-ready t :style style))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         warning
         complete)
    (puthash 25 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--set-native-view-style)
          (lambda (_view _style callback) (setq complete callback)))
         ((symbol-function 'display-warning)
          (lambda (&rest value) (setq warning value))))
      (yunge-reader-webview--sync-view-style view)
      (should complete)
      (funcall complete nil '(error "style failed")))
    (should-not (yunge-reader-webview--view-surface-style view))
    (should (equal (cadr warning) "style failed"))))

(ert-deftest yunge-reader-webview-serializes-location-navigation ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (let ((view (yunge-reader-webview--make-view :id 4))
          (location (yunge-reader-webview-test--location 0.25))
          (style (yunge-reader-webview-test--style))
          (target '((href . "OPS/chapter.xhtml#section"))))
      (yunge-reader-webview--open-view-publication
       view 7 #'ignore location style)
      (yunge-reader-webview--navigate-view
       view "next-screen" #'ignore)
      (yunge-reader-webview--navigate-view
       view "go-to" #'ignore target)
      (yunge-reader-webview--set-native-view-style
       view style #'ignore)
      (let* ((requests
              (mapcar
               (lambda (line)
                 (json-parse-string line :object-type 'alist))
               (nreverse sent)))
             (open (nth 0 requests))
             (next (nth 1 requests))
             (go-to (nth 2 requests))
             (styled (nth 3 requests)))
        (should
         (equal
          (mapcar (lambda (request) (alist-get 'op request)) requests)
          '("view-open-publication" "view-navigate" "view-navigate"
            "view-style")))
        (should
         (equal (alist-get 'location (alist-get 'params open))
                location))
        (should
         (equal (alist-get 'style (alist-get 'params open))
                style))
        (should
         (equal (alist-get 'command (alist-get 'params next))
                "next-screen"))
        (should
         (equal (alist-get 'location (alist-get 'params go-to))
                target))
        (should
         (equal (alist-get 'style (alist-get 'params styled))
                style))))))

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

(ert-deftest yunge-reader-webview-routes-keys-to-the-owning-buffer ()
  (let* (routed
         (buffer (generate-new-buffer " *webview key owner*"))
         (view
          (yunge-reader-webview--make-view
           :id 9
           :buffer buffer
           :accelerator-function
           (lambda (value key)
             (setq routed (list value key (current-buffer))))))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql)))
    (unwind-protect
        (progn
          (puthash 9 view yunge-reader-webview--views)
          (yunge-reader-webview--handle-event
           'fake-webview-process
           '((kind . "event")
             (event . "accelerator")
             (view . 9)
             (key . "+")))
          (should (equal routed (list view "+" buffer))))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-rejects-unknown-forwarded-keys ()
  (let ((yunge-reader-webview--process 'fake-webview-process)
        (yunge-reader-webview--views
         (make-hash-table :test #'eql)))
    (should-error
     (yunge-reader-webview--handle-event
      'fake-webview-process
      '((kind . "event")
        (event . "accelerator")
        (view . 9)
        (key . "j")))
     :type 'error)))

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
         warning
         (location-notifications 0)
         outline-result
         outline-error)
    (unwind-protect
        (progn
          (puthash 6 view yunge-reader-webview--views)
          (setf
           (yunge-reader-webview--view-location-changed-function view)
           (lambda (_value) (cl-incf location-notifications)))
          (yunge-reader-webview--request-view-outline
           view
           (lambda (value error-data)
             (setq outline-result value
                   outline-error error-data)))
          (yunge-reader-webview--handle-event
           'fake-webview-process
           '((kind . "event")
             (event . "publication-ready")
             (view . 6)
             (location
              . ((cfi . "epubcfi(/6/4!/4/2)")
                 (href . "OPS/chapter.xhtml")
                 (fraction . 0.25)))
             (outline
              . ((items
                  . (((title . "Part One") (depth . 0))
                     ((title . "Chapter")
                      (depth . 1)
                      (href . "OPS/chapter.xhtml#start"))))
                 (truncated)))))
          (should
           (yunge-reader-webview--view-publication-ready view))
          (should
           (equal (yunge-reader-webview--view-location view)
                  (yunge-reader-webview-test--location 0.25)))
          (should (zerop location-notifications))
          (should
           (yunge-reader-webview--view-outline-ready view))
          (should-not outline-error)
          (should
           (equal outline-result
                  (yunge-reader-webview-test--outline)))
          (yunge-reader-webview--handle-event
           'fake-webview-process
           '((kind . "event")
             (event . "location")
             (view . 6)
             (location
              . ((cfi . "epubcfi(/6/6!/4/2)")
                 (href . "OPS/next.xhtml")
                 (fraction . 0.3)))))
          (should (= location-notifications 1))
          (should
           (equal (yunge-reader-webview--view-location view)
                  '((cfi . "epubcfi(/6/6!/4/2)")
                    (href . "OPS/next.xhtml")
                    (fraction . 0.3))))
          (setf (yunge-reader-webview--view-surface-style view)
                (yunge-reader-webview-test--style))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest value) (setq warning value))))
            (yunge-reader-webview--handle-event
             'fake-webview-process
             '((kind . "event")
               (event . "style-error")
               (view . 6)
               (message . "bad style"))))
          (should-not
           (yunge-reader-webview--view-surface-style view))
          (should (equal (cadr warning) "bad style"))
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

(ert-deftest yunge-reader-webview-finishes-outline-waiters-on-destroy ()
  (let ((view (yunge-reader-webview--make-view))
        result
        request-error)
    (yunge-reader-webview--request-view-outline
     view
     (lambda (value error-data)
       (setq result value
             request-error error-data)))
    (yunge-reader-webview--finish-view-destroy view)
    (should-not result)
    (should request-error)
    (should-not (yunge-reader-webview--view-outline-waiters view))))

(ert-deftest yunge-reader-webview-keeps-publication-error-for-outline ()
  (let ((view (yunge-reader-webview--make-view :id 41))
        (yunge-reader-webview--process 'fake-webview-process)
        (yunge-reader-webview--views
         (make-hash-table :test #'eql))
        result
        request-error)
    (puthash 41 view yunge-reader-webview--views)
    (setf (yunge-reader-webview--view-pending-target view)
          '((href . "OPS/chapter.xhtml#section")))
    (cl-letf (((symbol-function 'display-warning) #'ignore)
              ((symbol-function
                'yunge-reader-webview--set-buffer-message)
               #'ignore))
      (yunge-reader-webview--handle-event
       'fake-webview-process
       '((kind . "event")
         (event . "publication-error")
         (view . 41)
         (message . "Could not parse EPUB"))))
    (yunge-reader-webview--request-view-outline
     view
     (lambda (value error-data)
       (setq result value
             request-error error-data)))
    (should-not result)
    (should (equal request-error
                   '(error "Could not parse EPUB")))
    (should-not (yunge-reader-webview--view-pending-target view))
    (should-not (yunge-reader-webview--view-outline-waiters view))))

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

(ert-deftest yunge-reader-webview-keeps-selections-per-view ()
  (let* ((first (yunge-reader-webview--make-view :id 31))
         (second (yunge-reader-webview--make-view :id 32))
         (selection (yunge-reader-webview-test--selection))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql)))
    (puthash 31 first yunge-reader-webview--views)
    (puthash 32 second yunge-reader-webview--views)
    (yunge-reader-webview--handle-event
     'fake-webview-process
     `((kind . "event")
       (event . "selection")
       (view . 31)
       (selection . ,selection)))
    (should
     (equal (yunge-reader-webview--view-selection first)
            selection))
    (should-not (yunge-reader-webview--view-selection second))
    (yunge-reader-webview--handle-event
     'fake-webview-process
     '((kind . "event")
       (event . "selection")
       (view . 31)
       (selection)))
    (should-not (yunge-reader-webview--view-selection first))
    (should-error
     (yunge-reader-webview--handle-event
      'fake-webview-process
      '((kind . "event")
        (event . "selection")
        (view . 32)))
     :type 'error)))

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
           :created t
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

(ert-deftest yunge-reader-webview-hides-persistent-native-surfaces ()
  (let* ((buffer (generate-new-buffer " *persistent EPUB owner*"))
         (other (generate-new-buffer " *persistent EPUB replacement*"))
         (window (selected-window))
         (location (yunge-reader-webview-test--location 0.4))
         (style (yunge-reader-webview-test--style))
         (view
          (yunge-reader-webview--make-view
           :id 15
           :window window
           :buffer buffer
           :created t
           :persistent t
           :publication 6
           :style style
           :surface-style (copy-tree style)
           :selection (yunge-reader-webview-test--selection)
           :location location))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         (yunge-reader-webview--logical-views
          (make-hash-table :test #'eq))
         requests)
    (unwind-protect
        (progn
          (puthash 15 view yunge-reader-webview--views)
          (puthash view t yunge-reader-webview--logical-views)
          (cl-letf
              (((symbol-function 'window-buffer)
                (lambda (_window) other))
               ((symbol-function 'get-buffer-window)
                (lambda (&rest _arguments) nil))
               ((symbol-function 'process-live-p)
                (lambda (_process) t))
               ((symbol-function 'yunge-reader-webview--request)
                (lambda (operation parameters _complete)
                  (push (list operation parameters) requests))))
            (yunge-reader-webview--sync-view view))
          (should-not (yunge-reader-webview--view-destroyed view))
          (should-not (yunge-reader-webview--view-id view))
          (should (gethash view yunge-reader-webview--logical-views))
          (should (equal (yunge-reader-webview--view-location view)
                         location))
          (should-not
           (yunge-reader-webview--view-surface-style view))
          (should-not
           (yunge-reader-webview--view-selection view))
          (should (equal (caar requests) "view-destroy"))
          (should-not
           (cl-find-if
            (lambda (request)
              (equal (car request) "publication-close"))
            requests)))
      (kill-buffer buffer)
      (kill-buffer other))))

(ert-deftest yunge-reader-webview-recreates-visible-persistent-surfaces ()
  (let* ((buffer (generate-new-buffer " *persistent EPUB visible*"))
         (window (selected-window))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer
           :persistent t
           :publication 8
           :outline-error '(error "old renderer error")))
         (yunge-reader-webview--next-view-id 20)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requested)
    (unwind-protect
        (cl-letf
            (((symbol-function 'get-buffer-window)
              (lambda (&rest _arguments) window))
             ((symbol-function 'window-buffer)
              (lambda (_window) buffer))
             ((symbol-function 'window-live-p)
              (lambda (_window) t))
             ((symbol-function 'yunge-reader-webview--window-bounds)
              (lambda (_window)
                '((x . 0) (y . 0) (width . 800) (height . 600))))
             ((symbol-function 'yunge-reader-webview--request-create)
              (lambda (value) (setq requested value))))
          (yunge-reader-webview--sync-view view)
          (should (eq requested view))
          (should (= (yunge-reader-webview--view-id view) 21))
          (should (eq (yunge-reader-webview--view-window view) window))
          (should-not
           (yunge-reader-webview--view-outline-error view))
          (should (eq (gethash 21 yunge-reader-webview--views) view)))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-reopens-with-view-local-style ()
  (let* ((location (yunge-reader-webview-test--location 0.4))
         (style (yunge-reader-webview-test--style))
         (view
          (yunge-reader-webview--make-view
           :id 22 :created t :publication 8
           :location location :style style))
         opened)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--open-view-publication)
          (lambda (value publication _complete
                         &optional target reading-style)
            (setq opened
                  (list value publication target reading-style)))))
      (yunge-reader-webview--try-open-publication view))
    (should (equal opened (list view 8 location style)))
    (should
     (equal (yunge-reader-webview--view-surface-style view) style))
    (should-not
     (eq (yunge-reader-webview--view-surface-style view) style))))

(ert-deftest yunge-reader-webview-copies-attached-reading-style ()
  (let ((style (yunge-reader-webview-test--style))
        (yunge-reader-webview--logical-views
         (make-hash-table :test #'eq)))
    (with-temp-buffer
      (let ((view
             (yunge-reader-webview--attach-shared-publication
              8 nil nil nil style)))
        (should
         (equal (yunge-reader-webview--view-style view) style))
        (should-not (eq (yunge-reader-webview--view-style view) style))
        (setcdr (assq 'font-scale style) 2.0)
        (should
         (= (alist-get 'font-scale
                       (yunge-reader-webview--view-style view))
            1.25))))))

(ert-deftest yunge-reader-webview-waits-for-every-obsolete-surface ()
  (let* ((view
          (yunge-reader-webview--make-view
           :id 31 :created t :persistent t :publication 8))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         (yunge-reader-webview--logical-views
          (make-hash-table :test #'eq))
         requests
         finished)
    (puthash 31 view yunge-reader-webview--views)
    (puthash view t yunge-reader-webview--logical-views)
    (cl-letf
        (((symbol-function 'process-live-p) (lambda (_process) t))
         ((symbol-function 'yunge-reader-webview--request)
          (lambda (_operation parameters complete)
            (push (cons (alist-get 'view parameters) complete)
                  requests))))
      (yunge-reader-webview--release-surface view)
      (setf (yunge-reader-webview--view-id view) 32
            (yunge-reader-webview--view-created view) t)
      (puthash 32 view yunge-reader-webview--views)
      (yunge-reader-webview--destroy-view
       view (lambda () (setq finished t)))
      (should (= (length requests) 2))
      (funcall (cdr (assq 32 requests)) nil nil)
      (should-not finished)
      (funcall (cdr (assq 31 requests)) nil nil)
      (should finished)
      (should
       (yunge-reader-webview--view-destroy-finished view)))))

(ert-deftest yunge-reader-webview-closes-publication-after-destroy ()
  (let* ((location (yunge-reader-webview-test--location 0.6))
         (view
          (yunge-reader-webview--make-view
           :id 10 :created t :publication 3 :location location
           :owns-publication t))
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
