;;; yunge-reader-webview-test.el --- WebView tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-webview)

(defconst yunge-reader-webview-test--renderer-url
  "http://127.0.0.1:32123/0123456789abcdef0123456789abcdef/app/index.html"
  "Valid broker renderer URL used by WebView tests.")

(defconst yunge-reader-webview-test--resource-root
  "http://127.0.0.1:32123/0123456789abcdef0123456789abcdef/book/abcdef0123456789abcdef0123456789/"
  "Valid broker publication root used by WebView tests.")

(defmacro yunge-reader-webview-test--with-fake-process (&rest body)
  "Run BODY with an isolated fake WebView process implementation."
  (declare (indent 0) (debug t))
  `(let ((yunge-reader-webview--process nil)
         (yunge-reader-webview--transport nil)
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
          ((symbol-function 'yunge-reader-webview--ensure-module)
           #'ignore)
          ((symbol-function 'make-pipe-process)
           (lambda (&rest _arguments)
             (setq live t)
             'fake-webview-process))
          ((symbol-function 'yunge-reader-module-start)
           (lambda (_process) t))
          ((symbol-function 'yunge-reader-module-request)
           (lambda (line) (push line sent)))
          ((symbol-function 'yunge-reader-module-pump)
           (lambda () t))
          ((symbol-function 'yunge-reader-module-running-p)
           (lambda () live))
          ((symbol-function 'yunge-reader-module-stop)
           (lambda () (setq live nil)))
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
          ((symbol-function 'delete-process)
           (lambda (_process)
             (setq live nil))))
       ,@body)))

(defun yunge-reader-webview-test--ready-message ()
  "Return one valid test ready message."
  (let ((message
         (copy-tree
          '((kind . "webview-ready")
     (protocol . 2)
     (build-id . "test-build")
     (available . t)
     (version . "test-version")
     (accelerators
      . ("'" "+" "-" "=" "<escape>" "<next>" "<prior>"
         "C-d" "C-g" "C-u" "G" "J" "K" "M-m" "SPC"
         "g" "j" "k" "m" "y"))
     (capabilities
      . ("view-appearance" "view-bounds"
         "view-clear-selection" "view-create"
         "view-destroy" "view-events" "view-focus"
         "view-focus-parent" "view-info"
         "view-navigate" "view-open-publication"
         "view-search"
         "view-search-result"
         "view-current-selection"
         "view-selection-text"
         "view-set-selection"
         "view-scroll-bars" "view-status" "view-style"
         "view-visible" "view-zoom"))))))
    (setf (alist-get 'platform message)
          (pcase system-type
            ('windows-nt "windows")
            ('darwin "macos"))
          (alist-get 'engine message)
          (pcase system-type
            ('windows-nt "webview2")
            ('darwin "wkwebview")))
    message))

(defun yunge-reader-webview-test--location (&optional fraction x y)
  "Return one valid EPUB test locator with optional FRACTION, X, and Y."
  (append
   '((cfi . "epubcfi(/6/4!/4/2)"))
   '((href . "OPS/chapter.xhtml"))
   (when fraction `((fraction . ,fraction)))
   (when (and x y) `((x . ,x) (y . ,y)))))

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

(defun yunge-reader-webview-test--original-appearance (&optional _window)
  "Return one original EPUB appearance payload."
  '((mode . original)))

(defun yunge-reader-webview-test--follow-appearance (&optional _window)
  "Return one follow-Emacs EPUB appearance payload."
  (copy-tree
   '((mode . follow-emacs)
     (foreground . "#112233")
     (background . "#f4f5f6")
     (link . "#2244aa")
     (selection-foreground . "#ffffff")
     (selection-background . "#335577")
     (search-background . "#aa5500"))))

(defun yunge-reader-webview-test--surface
    (id state &rest properties)
  "Return a native test surface with ID, STATE, and PROPERTIES."
  (apply #'yunge-reader-webview--make-surface
         :id id :state state properties))

(ert-deftest yunge-reader-webview-validates-explicit-surface-states ()
  (let ((surface
         (yunge-reader-webview-test--surface 7 'creating)))
    (should-error
     (yunge-reader-webview--set-surface-state surface 'unknown))
    (should
     (eq (yunge-reader-webview--surface-state surface) 'creating))
    (yunge-reader-webview--set-surface-state surface 'ready)
    (should (yunge-reader-webview--surface-created-p surface))
    (should (yunge-reader-webview--surface-ready-p surface))))

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

(ert-deftest yunge-reader-webview-owns-view-requests ()
  (yunge-reader-webview-test--with-fake-process
    (let ((view (yunge-reader-webview--make-view))
          error-data)
      (yunge-reader-webview-start)
      (puthash 41 view yunge-reader-webview--views)
      (let ((task
             (yunge-reader-webview--request
              "view-info" '((view . 41))
              (lambda (_result error) (setq error-data error))
              :revision 5)))
        (should (eq (yunge-reader-task-owner task) view))
        (should (= (yunge-reader-task-revision task) 5))
        (should
         (= (yunge-reader-webview--cancel-view-requests
             view "view destroyed")
            1))
        (should (eq (yunge-reader-task-state task) 'cancelled))
        (should (eq (car error-data) 'yunge-reader-task-cancelled))))))

(ert-deftest yunge-reader-webview-rebuilds-surfaces-after-service-exit ()
  (yunge-reader-webview-test--with-fake-process
    (let* ((buffer (generate-new-buffer " *stopped EPUB view*"))
           (location '((cfi . "epubcfi(/6/4)")
                       (href . "OPS/chapter.xhtml")))
           (view
            (yunge-reader-webview--make-view
             :buffer buffer
             :location location
             :surface
             (yunge-reader-webview-test--surface 7 'ready)))
           recreated)
      (unwind-protect
          (progn
            (yunge-reader-webview-start)
            (puthash 7 view yunge-reader-webview--views)
            (puthash view t yunge-reader-webview--logical-views)
            (setq live nil)
            (cl-letf (((symbol-function 'display-warning) #'ignore))
              (yunge-reader-webview--sentinel
               'fake-webview-process "finished"))
            (should-not yunge-reader-webview--process)
            (should-not (yunge-reader-webview--view-destroyed view))
            (should-not (yunge-reader-webview--view-surface view))
            (should (equal (yunge-reader-webview--view-location view)
                           location))
            (should
             (zerop
              (hash-table-count yunge-reader-webview--views)))
            (should
             (gethash view yunge-reader-webview--logical-views))
            (cl-letf
                (((symbol-function 'yunge-reader-webview--visible-windows)
                  (lambda (_view) '(visible-window)))
                 ((symbol-function 'yunge-reader-webview--visible-window)
                  (lambda (_view) 'visible-window))
                 ((symbol-function 'yunge-reader-webview--start-surface)
                  (lambda (candidate window)
                    (setq recreated (list candidate window)))))
              (yunge-reader-webview--sync-view view))
            (should (equal recreated (list view 'visible-window))))
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-webview-finishes-destroy-after-service-exit ()
  (let* ((yunge-reader-webview--views (make-hash-table :test #'eql))
         (yunge-reader-webview--logical-views
          (make-hash-table :test #'eq))
         (surface (yunge-reader-webview-test--surface 8 'ready))
         (completed nil)
         (view
          (yunge-reader-webview--make-view
           :surface surface
           :destroyed t
           :pending-destroys 1
           :destroy-waiters (list (lambda () (setq completed t))))))
    (puthash 8 view yunge-reader-webview--views)
    (yunge-reader-webview--forget-all-surfaces)
    (should completed)
    (should (yunge-reader-webview--view-destroy-finished view))
    (should-not (yunge-reader-webview--view-surface view))
    (should (zerop (hash-table-count yunge-reader-webview--views)))))

(ert-deftest yunge-reader-webview-rejects-incomplete-handshakes ()
  (yunge-reader-webview-test--with-fake-process
    (let ((message (yunge-reader-webview-test--ready-message)))
      (setf (alist-get 'capabilities message)
            '("view-create" "view-destroy"))
      (should-error
       (yunge-reader-webview--validate-ready message)
       :type 'error))))

(ert-deftest yunge-reader-webview-accepts-macos-wkwebview-handshake ()
  (let ((system-type 'darwin)
        (message (yunge-reader-webview-test--ready-message)))
    (setf (alist-get 'platform message) "macos"
          (alist-get 'engine message) "wkwebview")
    (cl-letf (((symbol-function 'yunge-reader-native--build-id)
               (lambda () "test-build")))
      (should-not (yunge-reader-webview--validate-ready message)))))

(ert-deftest yunge-reader-webview-rejects-accelerator-contract-drift ()
  (dolist (accelerators
           '(nil
             ("'" "+" "-" "=" "<escape>" "<next>" "<prior>"
              "C-d" "C-g" "C-u" "G" "J" "K" "M-m" "SPC"
              "g" "j" "k" "m")))
    (let ((message (yunge-reader-webview-test--ready-message)))
      (setf (alist-get 'accelerators message) accelerators)
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
  (let ((location
         (yunge-reader-webview-test--location 0.25 12.5 30.0)))
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
          (href . "OPS/chapter.xhtml") (fraction . 2))
         ((cfi . "epubcfi(/6/4)")
          (href . "OPS/chapter.xhtml") (x . 1.0))
         ((cfi . "epubcfi(/6/4)")
          (href . "OPS/chapter.xhtml")
          (x . -1.0) (y . 0.0))
         ((cfi . "epubcfi(/6/4)")
          (href . "OPS/chapter.xhtml")
          (x . 1000001.0) (y . 0.0))))
    (should-not (yunge-reader-webview--valid-location-p location))))

(ert-deftest yunge-reader-webview-requires-location-user-origin ()
  (let ((location (yunge-reader-webview-test--location 0.25)))
    (should
     (yunge-reader-webview--event-location-user
      `((user . t) (location . ,location))))
    (should-not
     (yunge-reader-webview--event-location-user
      `((user) (location . ,location))))
    (should-error
     (yunge-reader-webview--event-location-user
      `((location . ,location))))
    (should-error
     (yunge-reader-webview--event-location-user
      `((user . "yes") (location . ,location))))))

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

(ert-deftest yunge-reader-webview-validates-epub-selection-text-batches ()
  (should
   (yunge-reader-webview--valid-selection-text-result-p
    '((text . "A😀") (total . 4) (next-offset . 2) (done))
    0 2))
  (should
   (yunge-reader-webview--valid-selection-text-result-p
    '((text . "bc") (total . 4) (done . t))
    2 8))
  (dolist
      (result
       '(((text . "abc") (total . 4) (next-offset . 2) (done))
         ((text . "") (total . 4) (next-offset . 0) (done))
         ((text . "a") (total . 1) (next-offset . 1) (done . t))
         ((text . "a") (total . 1) (done . t) (extra . t))))
    (should-not
     (yunge-reader-webview--valid-selection-text-result-p
     result 0 2))))

(ert-deftest yunge-reader-webview-requests-current-epub-selection ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (let ((view
           (yunge-reader-webview--make-view
            :surface
            (yunge-reader-webview-test--surface 9 'ready))))
      (yunge-reader-webview--request-current-selection view #'ignore)
      (let* ((request
              (json-parse-string (car sent) :object-type 'alist))
             (params (alist-get 'params request)))
        (should (equal (alist-get 'op request)
                       "view-current-selection"))
        (should (= (alist-get 'view params) 9))))))

(ert-deftest yunge-reader-webview-validates-current-epub-selection ()
  (let (result error-data)
    (yunge-reader-webview--current-selection-complete
     (lambda (value error)
       (setq result value
             error-data error))
     (yunge-reader-webview-test--selection) nil)
    (should (equal result (yunge-reader-webview-test--selection)))
    (should-not error-data)
    (setq result 'unset
          error-data 'unset)
    (yunge-reader-webview--current-selection-complete
     (lambda (value error)
       (setq result value
             error-data error))
     nil nil)
    (should-not result)
    (should-not error-data)
    (yunge-reader-webview--current-selection-complete
     (lambda (value error)
       (setq result value
             error-data error))
     '((href . "OPS/chapter.xhtml")) nil)
    (should-not result)
    (should (eq (car error-data) 'error))))

(ert-deftest yunge-reader-webview-requests-bounded-selection-text ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (let ((view
           (yunge-reader-webview--make-view
            :surface
            (yunge-reader-webview-test--surface 9 'ready))))
      (yunge-reader-webview--request-selection-text
       view (yunge-reader-webview-test--selection) 3 16
       #'ignore)
      (let* ((request
              (json-parse-string (car sent) :object-type 'alist))
             (params (alist-get 'params request)))
        (should (equal (alist-get 'op request)
                       "view-selection-text"))
        (should (= (alist-get 'view params) 9))
        (should (= (alist-get 'offset params) 3))
        (should (= (alist-get 'character-limit params) 16))
        (should
         (equal (alist-get 'selection params)
                (yunge-reader-webview-test--selection)))))))

(ert-deftest yunge-reader-webview-rejects-malformed-selection-text-results ()
  (let (result error-data)
    (yunge-reader-webview--selection-text-complete
     0 2
     (lambda (value error)
       (setq result value
             error-data error))
     '((text . "abc") (total . 3) (done . t))
     nil)
    (should-not result)
    (should (eq (car error-data) 'error))))

(ert-deftest yunge-reader-webview-validates-epub-search-batches ()
  (let ((match
         '((href . "OPS/chapter.xhtml")
           (start . "epubcfi(/6/4!/4/2/1:0)")
           (end . "epubcfi(/6/4!/4/2/1:7)")
           (text . "Chapter")
           (before . "A ")
           (after . " title"))))
    (should
     (yunge-reader-webview--valid-search-result-p
      `((matches . (,match))
        (cursor . ((href . "OPS/chapter.xhtml") (offset . 1)))
        (done))
      2))
    (should
     (yunge-reader-webview--valid-search-result-p
      `((matches . (,match)) (done . t))
      2))
    (dolist
        (result
         `(((matches) (done))
           ((matches . (,match)) (cursor) (done))
           ((matches . (,match))
            (cursor . ((href . "../chapter.xhtml") (offset . 1)))
            (done))
           ((matches . (,match)) (cursor . nil) (done . t))))
      (should-not
       (yunge-reader-webview--valid-search-result-p result 2)))))

(ert-deftest yunge-reader-webview-requests-bounded-epub-search ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (let ((view
           (yunge-reader-webview--make-view
            :surface
            (yunge-reader-webview-test--surface 9 'ready))))
      (yunge-reader-webview--request-search
       view "Chapter" t 'forward nil
       '((href . "OPS/chapter.xhtml") (offset . 3))
       32 8 #'ignore)
      (let* ((request
              (json-parse-string (car sent) :object-type 'alist))
             (params (alist-get 'params request)))
        (should (equal (alist-get 'op request) "view-search"))
        (should (= (alist-get 'view params) 9))
        (should (equal (alist-get 'query params) "Chapter"))
        (should (eq (alist-get 'case-sensitive params) t))
        (should (equal (alist-get 'direction params) "forward"))
        (should (= (alist-get 'match-limit params) 32))
        (should (= (alist-get 'section-limit params) 8))
        (should (= (alist-get 'offset
                              (alist-get 'cursor params))
                   3)))
      (yunge-reader-webview--request-search
       view "chapter" nil 'backward
       '((cfi . "epubcfi(/6/4!/4/2/1:7)")
         (href . "OPS/chapter.xhtml"))
       nil 16 4 #'ignore)
      (let* ((request
              (json-parse-string (car sent) :object-type 'alist))
             (params (alist-get 'params request)))
        (should (equal (alist-get 'query params) "chapter"))
        (should (eq (alist-get 'case-sensitive params) :false))
        (should (equal (alist-get 'direction params) "backward"))
        (should
         (equal
          (alist-get 'cfi (alist-get 'origin params))
          "epubcfi(/6/4!/4/2/1:7)"))))))

(ert-deftest yunge-reader-webview-rejects-malformed-search-results ()
  (let (result error-data)
    (yunge-reader-webview--search-complete
     2
     (lambda (value error)
       (setq result value
             error-data error))
     '((matches) (cursor) (done))
     nil)
    (should-not result)
    (should (eq (car error-data) 'error))))

(ert-deftest yunge-reader-webview-persists-current-epub-search-result ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (let ((view (yunge-reader-webview--make-view :publication 7))
          (selection (yunge-reader-webview-test--selection)))
      (yunge-reader-webview--set-view-search-result view selection)
      (should-not sent)
      (setf (yunge-reader-webview--view-surface view)
            (yunge-reader-webview-test--surface 9 'ready))
      (should (yunge-reader-webview--sync-view-search-result view))
      (let* ((request
              (json-parse-string (car sent) :object-type 'alist))
             (params (alist-get 'params request)))
        (should (equal (alist-get 'op request)
                       "view-search-result"))
        (should (= (alist-get 'view params) 9))
        (should (equal (alist-get 'selection params) selection))
        (should (eq (alist-get 'reveal params) :false)))
      (yunge-reader-webview--set-view-search-result view nil)
      (let* ((request
              (json-parse-string (car sent) :object-type 'alist))
             (params (alist-get 'params request)))
        (should (equal (alist-get 'op request)
                       "view-search-result"))
        (should (eq (alist-get 'selection params) :null))
        (should (eq (alist-get 'reveal params) t))))))

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

(ert-deftest yunge-reader-webview-validates-epub-appearances ()
  (dolist (appearance
           (list (yunge-reader-webview-test--original-appearance)
                 (yunge-reader-webview-test--follow-appearance)))
    (should (yunge-reader-webview--valid-appearance-p appearance))
    (should (eq (yunge-reader-webview--check-appearance appearance)
                appearance)))
  (should
   (yunge-reader-webview--valid-appearance-p
    (reverse (yunge-reader-webview-test--follow-appearance))))
  (dolist
      (appearance
       (list nil 'original '((mode . themed))
             '((mode . original) (foreground . "#112233"))
             (butlast (yunge-reader-webview-test--follow-appearance))
             (append
              (yunge-reader-webview-test--follow-appearance)
              '((extra . "#112233")))
             '((mode . follow-emacs)
               (foreground . "#AABBCC")
               (background . "#ffffff")
               (link . "#112233")
               (selection-foreground . "#112233")
               (selection-background . "#112233")
               (search-background . "#112233"))))
    (should-error
     (yunge-reader-webview--check-appearance appearance))))

(ert-deftest yunge-reader-webview-defers-hidden-appearance ()
  (let* ((appearance (yunge-reader-webview-test--follow-appearance))
         (view (yunge-reader-webview--make-view :publication 8))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--set-native-view-appearance)
          (lambda (value appearance complete)
            (push (list value appearance complete) requests))))
      (yunge-reader-webview--set-view-appearance view appearance)
      (should-not requests)
      (should
       (equal (yunge-reader-webview--view-appearance view)
              appearance))
      (setf (yunge-reader-webview--view-surface view)
            (yunge-reader-webview-test--surface 22 'ready))
      (puthash 22 view yunge-reader-webview--views)
      (yunge-reader-webview--sync-view-appearance view)
      (should (= (length requests) 1))
      (should (equal (cadar requests) appearance))
      (should
       (equal (yunge-reader-webview--surface-appearance
               (yunge-reader-webview--view-surface view))
              appearance)))))

(ert-deftest yunge-reader-webview-surface-value-failure-is-retryable ()
  (let* ((requested '((font-scale . 1.25)))
         (newer '((font-scale . 1.5)))
         (surface (yunge-reader-webview-test--surface 23 'ready))
         (view
          (yunge-reader-webview--make-view
           :surface surface :publication 8))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         warnings
         completions)
    (puthash 23 view yunge-reader-webview--views)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest value) (push value warnings))))
      (dolist (desired (list requested newer))
        (yunge-reader-webview--sync-surface-value
         view desired
         #'yunge-reader-webview--surface-style
         (lambda (value applied)
           (setf (yunge-reader-webview--surface-style value)
                 applied))
         (lambda (_value _style complete)
           (push complete completions))))
      (should (equal (yunge-reader-webview--surface-style surface)
                     newer))
      (funcall (cadr completions) nil '(error "older failed"))
      (should (equal (yunge-reader-webview--surface-style surface)
                     newer))
      (funcall (car completions) nil '(error "latest failed")))
    (should-not (yunge-reader-webview--surface-style surface))
    (should (equal (mapcar #'cadr warnings)
                   '("latest failed" "older failed")))))

(ert-deftest yunge-reader-webview-refreshes-appearance-for-current-window ()
  (let* ((buffer (generate-new-buffer " *EPUB appearance owner*"))
         (original (yunge-reader-webview-test--original-appearance))
         (follow (yunge-reader-webview-test--follow-appearance))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            24 'ready :window 'surface-window
            :appearance (copy-tree original))
           :buffer buffer :publication 8
           :appearance original
           :appearance-function
           (lambda (window)
             (should (eq window 'surface-window))
             follow)))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requested)
    (unwind-protect
        (progn
          (puthash 24 view yunge-reader-webview--views)
          (cl-letf
              (((symbol-function 'window-live-p)
                (lambda (window) (eq window 'surface-window)))
               ((symbol-function 'window-buffer)
                (lambda (_window) buffer))
               ((symbol-function
                 'yunge-reader-webview--set-native-view-appearance)
                (lambda (_view appearance _complete)
                  (setq requested appearance))))
            (yunge-reader-webview--refresh-view-appearance view))
          (should (equal requested follow))
          (should-not (eq requested follow))
          (should
           (equal (yunge-reader-webview--view-appearance view) follow))
          (should-not
           (eq (yunge-reader-webview--view-appearance view) follow)))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-validates-fixed-layout-zoom ()
  (dolist (zoom '(fit-page fit-width 0.25 1.0 8.0))
    (should (yunge-reader-webview--valid-fixed-zoom-p zoom))
    (should (eq (yunge-reader-webview--check-fixed-zoom zoom) zoom)))
  (dolist (zoom '(fit-height 0.24 8.01 "fit-page"))
    (should-not (yunge-reader-webview--valid-fixed-zoom-p zoom))
    (should-error (yunge-reader-webview--check-fixed-zoom zoom))))

(ert-deftest yunge-reader-webview-validates-resolved-scroll-bar-modes ()
  (should
   (eq (yunge-reader-webview--check-scroll-bar-mode 'hidden)
       'hidden))
  (should
   (eq (yunge-reader-webview--check-scroll-bar-mode 'visible)
       'visible))
  (should-error
   (yunge-reader-webview--check-scroll-bar-mode 'follow-emacs)))

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
      (setf (yunge-reader-webview--view-surface view)
            (yunge-reader-webview-test--surface 23 'ready))
      (puthash 23 view yunge-reader-webview--views)
      (yunge-reader-webview--sync-view-style view)
      (should (= (length requests) 1))
      (should
       (equal (cadar requests)
              (yunge-reader-webview--view-style view)))
      (should
       (equal (yunge-reader-webview--surface-style
               (yunge-reader-webview--view-surface view))
              (yunge-reader-webview--view-style view))))))

(ert-deftest yunge-reader-webview-reconciles-style-after-opening ()
  (let* ((style (yunge-reader-webview-test--style))
         (old-style (copy-tree style))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            24 'native-ready :style old-style)
           :publication 8 :style style))
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
            (setq requested reading-style)))
         ((symbol-function 'yunge-reader-webview--sync-view)
          #'ignore))
      (yunge-reader-webview--handle-event
       'fake-webview-process
       `((kind . "event")
         (event . "publication-ready")
         (view . 24)
         (location . ,(yunge-reader-webview-test--location 0.25))
         (outline . ,(yunge-reader-webview-test--outline)))))
    (should (equal requested style))
    (should
     (equal (yunge-reader-webview--surface-style
             (yunge-reader-webview--view-surface view))
            style))))

(ert-deftest yunge-reader-webview-defers-hidden-fixed-zoom ()
  (let* ((view
          (yunge-reader-webview--make-view
           :layout 'fixed :publication 8))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--set-native-view-zoom)
          (lambda (value zoom complete)
            (push (list value zoom complete) requests))))
      (yunge-reader-webview--set-view-zoom view 'fit-width)
      (should-not requests)
      (should (eq (yunge-reader-webview--view-zoom view) 'fit-width))
      (setf (yunge-reader-webview--view-surface view)
            (yunge-reader-webview-test--surface 27 'ready))
      (puthash 27 view yunge-reader-webview--views)
      (yunge-reader-webview--sync-view-zoom view)
      (should (= (length requests) 1))
      (should (eq (cadar requests) 'fit-width))
      (should
       (eq (yunge-reader-webview--surface-zoom
            (yunge-reader-webview--view-surface view))
           'fit-width)))))

(ert-deftest yunge-reader-webview-reconciles-scroll-bars ()
  (let* ((view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            26 'ready :scroll-bar-mode 'visible)
           :publication 8
           :scroll-bar-mode 'hidden))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requested)
    (puthash 26 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--set-native-scroll-bar-mode)
          (lambda (_view mode _callback) (setq requested mode))))
      (yunge-reader-webview--sync-view-scroll-bars view))
    (should (eq requested 'hidden))
    (should
     (eq (yunge-reader-webview--surface-scroll-bar-mode
          (yunge-reader-webview--view-surface view))
         'hidden))))

(ert-deftest yunge-reader-webview-serializes-location-navigation ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (let ((view
           (yunge-reader-webview--make-view
            :surface
            (yunge-reader-webview-test--surface 4 'ready)
            :layout 'reflow
            :resource-root
            yunge-reader-webview-test--resource-root))
          (location
           (yunge-reader-webview-test--location 0.25 12.5 30.0))
          (style (yunge-reader-webview-test--style))
          (original
           (yunge-reader-webview-test--original-appearance))
          (follow
           (yunge-reader-webview-test--follow-appearance))
          (target '((href . "OPS/chapter.xhtml#section"))))
      (yunge-reader-webview--open-view-publication
       view 7 #'ignore location original style nil 'hidden)
      (yunge-reader-webview--navigate-view
       view "next-screen" #'ignore)
      (yunge-reader-webview--navigate-view
       view "next-page" #'ignore)
      (yunge-reader-webview--navigate-view
       view "previous-page" #'ignore)
      (yunge-reader-webview--navigate-view
       view "next-line" #'ignore)
      (yunge-reader-webview--navigate-view
       view "go-to" #'ignore target)
      (yunge-reader-webview--set-native-view-appearance
       view follow #'ignore)
      (yunge-reader-webview--set-native-view-style
       view style #'ignore)
      (yunge-reader-webview--set-native-view-zoom
       view 'fit-width #'ignore)
      (yunge-reader-webview--set-native-scroll-bar-mode
       view 'visible #'ignore)
      (let* ((requests
              (mapcar
               (lambda (line)
                 (json-parse-string line :object-type 'alist))
               (nreverse sent)))
              (open (nth 0 requests))
              (next (nth 1 requests))
              (next-page (nth 2 requests))
              (previous-page (nth 3 requests))
              (line (nth 4 requests))
              (go-to (nth 5 requests))
              (appearance (nth 6 requests))
              (styled (nth 7 requests))
              (zoomed (nth 8 requests))
              (scroll-bars (nth 9 requests)))
        (should
         (equal
          (mapcar (lambda (request) (alist-get 'op request)) requests)
           '("view-open-publication" "view-navigate" "view-navigate"
             "view-navigate" "view-navigate" "view-navigate"
             "view-appearance" "view-style" "view-zoom"
             "view-scroll-bars")))
        (should
         (equal (alist-get 'location (alist-get 'params open))
                location))
        (should
         (equal (alist-get 'style (alist-get 'params open))
                style))
        (should
         (equal (alist-get 'appearance (alist-get 'params open))
                '((mode . "original"))))
        (should
         (equal (alist-get 'layout (alist-get 'params open))
                "reflowable"))
        (should
         (equal (alist-get 'resource-root (alist-get 'params open))
                yunge-reader-webview-test--resource-root))
        (should
         (eq (alist-get 'scroll-bars (alist-get 'params open))
             :false))
        (should
         (equal (alist-get 'command (alist-get 'params next))
                "next-screen"))
        (should
         (equal (alist-get 'command (alist-get 'params next-page))
                "next-page"))
        (should
         (equal
          (alist-get 'command (alist-get 'params previous-page))
          "previous-page"))
        (should
         (equal (alist-get 'command (alist-get 'params line))
                "next-line"))
        (should
         (equal (alist-get 'zoom (alist-get 'params zoomed))
                "fit-width"))
        (should
         (equal (alist-get 'location (alist-get 'params go-to))
                target))
        (should
         (equal (alist-get 'style (alist-get 'params styled))
                style))
        (should
         (equal
          (alist-get 'appearance (alist-get 'params appearance))
          (let ((value (copy-tree follow)))
            (setf (alist-get 'mode value) "follow-emacs")
            value)))
        (should
         (alist-get 'visible (alist-get 'params scroll-bars)))))))

(ert-deftest yunge-reader-webview-serializes-initial-fixed-zoom ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-webview--handle-message
     'fake-webview-process
     (yunge-reader-webview-test--ready-message))
    (yunge-reader-webview--open-view-publication
     (yunge-reader-webview--make-view
      :surface
      (yunge-reader-webview-test--surface 4 'ready)
      :layout 'fixed
      :resource-root yunge-reader-webview-test--resource-root)
     7 #'ignore nil
     (yunge-reader-webview-test--original-appearance)
     nil 'fit-page 'hidden)
    (let* ((request
            (json-parse-string (car sent) :object-type 'alist))
           (parameters (alist-get 'params request)))
      (should (equal (alist-get 'zoom parameters) "fit-page"))
      (should (equal (alist-get 'layout parameters) "pre-paginated"))
      (should
       (equal (alist-get 'resource-root parameters)
              yunge-reader-webview-test--resource-root))
      (should-not (assq 'style parameters)))))

(ert-deftest yunge-reader-webview-serializes-boundary-navigation ()
  (let ((view
         (yunge-reader-webview--make-view
          :surface
          (yunge-reader-webview-test--surface 4 'ready)))
        requests)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--request)
          (lambda (operation parameters _callback)
            (push (list operation parameters) requests))))
      (yunge-reader-webview--navigate-view view "first" #'ignore)
      (yunge-reader-webview--navigate-view view "last" #'ignore)
      (should-error
       (yunge-reader-webview--navigate-view
        view "first" #'ignore
        '((href . "OPS/chapter.xhtml")))))
    (should
     (equal
      (nreverse requests)
      '(("view-navigate" ((view . 4) (command . "first")))
        ("view-navigate" ((view . 4) (command . "last"))))))))

(ert-deftest yunge-reader-webview-wraps-publication-operations ()
  (let ((path (expand-file-name "book.epub" temporary-file-directory))
        requests
        released
        opened
        informed
        closed)
    (cl-letf
        (((symbol-function 'yunge-reader-native-acquire)
          (lambda () 3))
         ((symbol-function 'yunge-reader-native-release)
          (lambda () (setq released t)))
         ((symbol-function 'yunge-reader-native-request-in-session)
          (lambda (session operation parameters complete)
            (push (list session operation parameters complete) requests))))
      (yunge-reader-webview--open-publication
       path (lambda (value error-data)
              (setq opened (list value error-data))))
      (let ((request (pop requests)))
        (should (= (nth 0 request) 3))
        (should (equal (nth 1 request) "epub-open"))
        (should (equal (alist-get 'path (nth 2 request)) path))
        (funcall
         (nth 3 request)
         `((publication . 7)
           (renderer-url . ,yunge-reader-webview-test--renderer-url)
           (resource-root . ,yunge-reader-webview-test--resource-root))
         nil))
      (should (= (alist-get 'session (car opened)) 3))
      (should-not (cadr opened))
      (yunge-reader-webview--publication-info
       3 7 (lambda (value error-data)
             (setq informed (list value error-data))))
      (let ((request (pop requests)))
        (should (equal (butlast request) '(3 "epub-info" ((publication . 7)))))
        (funcall (nth 3 request) '((publication . 7)) nil))
      (should (equal informed '(((publication . 7)) nil)))
      (yunge-reader-webview--close-publication
       3 7 (lambda (value error-data)
             (setq closed (list value error-data))))
      (let ((request (pop requests)))
        (should (equal (butlast request) '(3 "epub-close" ((publication . 7)))))
        (funcall (nth 3 request) '((closed . t)) nil))
      (should released)
      (should (equal closed '(((closed . t)) nil)))
      (should-not requests))))

(ert-deftest yunge-reader-webview-routes-dismiss-keys-to-owning-window ()
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         routed
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            8 'native-ready :window window)
           :buffer buffer
           :accelerator-function
           (lambda (_view key) (push key routed))))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests
         selected
         focused)
    (puthash 8 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function 'process-live-p)
          (lambda (_process) t))
         ((symbol-function 'yunge-reader-webview--request)
          (lambda (operation parameters complete)
            (push (list operation parameters) requests)
            (funcall complete nil nil)))
         ((symbol-function 'select-window)
          (lambda (value &optional _norecord)
            (setq selected value)))
         ((symbol-function 'select-frame-set-input-focus)
          (lambda (frame &optional _norecord)
            (setq focused frame))))
      (dolist (key '("<escape>" "C-g"))
        (yunge-reader-webview--handle-event
         'fake-webview-process
         `((kind . "event")
           (event . "accelerator")
           (view . 8)
           (repeat . nil)
           (key . ,key)))))
    (should (equal (nreverse routed) '("<escape>" "C-g")))
    (should (eq selected window))
    (should (eq focused (window-frame window)))
    (should
     (equal (nreverse requests)
            '(("view-focus-parent" ((view . 8)))
              ("view-focus-parent" ((view . 8))))))))

(ert-deftest yunge-reader-webview-selects-the-native-focus-owner ()
  (let* ((reader-window 'reader-window)
         (selected 'other-window)
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            9 'ready :window reader-window)))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql)))
    (puthash 9 view yunge-reader-webview--views)
    (cl-letf (((symbol-function 'window-live-p)
               (lambda (window) (eq window reader-window)))
              ((symbol-function 'selected-window)
               (lambda () selected))
              ((symbol-function 'select-window)
               (lambda (window &optional _norecord)
                 (setq selected window))))
      (yunge-reader-webview--handle-event
       'fake-webview-process
       '((kind . "event")
         (event . "focus-gained")
         (view . 9)))
      (should (eq selected reader-window))
      (should
       (yunge-reader-webview--surface-native-focused
        (yunge-reader-webview--view-surface view)))
      (yunge-reader-webview--handle-event
       'fake-webview-process
       '((kind . "event")
         (event . "focus-lost")
         (view . 9)))
      (should-not
       (yunge-reader-webview--surface-native-focused
        (yunge-reader-webview--view-surface view))))))

(ert-deftest yunge-reader-webview-releases-an-unselected-native-focus ()
  (let* ((view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            10 'native-ready :window 'reader-window
            :native-focused t)))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         (yunge-reader-webview--logical-views
          (make-hash-table :test #'eq))
         request)
    (puthash 10 view yunge-reader-webview--views)
    (puthash view t yunge-reader-webview--logical-views)
    (cl-letf (((symbol-function 'selected-window)
               (lambda () 'other-window))
              ((symbol-function 'process-live-p)
               (lambda (_process) t))
              ((symbol-function 'yunge-reader-webview--request)
               (lambda (operation parameters complete)
                 (setq request
                       (list operation parameters complete)))))
      (yunge-reader-webview--sync-native-focus)
      (should
       (equal (seq-take request 2)
              '("view-focus-parent" ((view . 10)))))
      (should
       (yunge-reader-webview--surface-focus-release-pending
        (yunge-reader-webview--view-surface view)))
      (funcall (nth 2 request) nil nil)
      (should-not
       (yunge-reader-webview--surface-native-focused
        (yunge-reader-webview--view-surface view)))
      (should-not
       (yunge-reader-webview--surface-focus-release-pending
        (yunge-reader-webview--view-surface view))))))

(ert-deftest yunge-reader-webview-routes-keys-to-the-owning-buffer ()
  (let* (routed
         (buffer (generate-new-buffer " *webview key owner*"))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 9 'ready)
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
          (dolist (key '("y" "j" "k"))
            (yunge-reader-webview--handle-event
             'fake-webview-process
             `((kind . "event")
               (event . "accelerator")
               (view . 9)
               (repeat . nil)
               (key . ,key)))
            (should (equal routed (list view key buffer)))))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-drops-repeated-owning-prefix-keys ()
  (let* ((buffer (generate-new-buffer " *webview repeated prefix*"))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 11 'ready)
           :buffer buffer))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         (unread-command-events nil)
         focused)
    (unwind-protect
        (progn
          (puthash 11 view yunge-reader-webview--views)
          (cl-letf
              (((symbol-function
                 'yunge-reader-webview--focus-owning-window)
                (lambda (value) (setq focused value))))
            (dolist (key '("'" "SPC" "M-m" "g" "m"))
              (yunge-reader-webview--handle-event
               'fake-webview-process
               `((kind . "event")
                 (event . "accelerator")
                 (view . 11)
                 (repeat . t)
                 (key . ,key))))
            (should-not focused)
            (should-not unread-command-events)))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-keeps-repeated-direct-keys ()
  (let* (routed
         (buffer (generate-new-buffer " *webview repeated direct*"))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 9 'ready)
           :buffer buffer
           :accelerator-function
           (lambda (_view key) (setq routed key))))
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
             (repeat . t)
             (key . "j")))
          (should (equal routed "j")))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-routes-external-links-to-owning-buffer ()
  (let* (routed
         (buffer (generate-new-buffer " *webview link owner*"))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 12 'ready)
           :buffer buffer
           :external-link-function
           (lambda (value uri)
             (setq routed (list value uri (current-buffer))))))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql)))
    (unwind-protect
        (progn
          (puthash 12 view yunge-reader-webview--views)
          (yunge-reader-webview--handle-event
           'fake-webview-process
           '((kind . "event")
             (event . "external-link")
             (view . 12)
             (uri . "https://example.com/reference")))
          (should
           (equal routed
                  (list view "https://example.com/reference" buffer)))
          (dolist (uri '(nil "relative/path" "https://bad uri"))
            (should-error
             (yunge-reader-webview--handle-event
              'fake-webview-process
              `((kind . "event")
                (event . "external-link")
                (view . 12)
                (uri . ,uri))))))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-relays-prefixes-after-returning-focus ()
  (let* ((buffer (generate-new-buffer " *webview leader owner*"))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 11 'ready)
           :buffer buffer))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         (unread-command-events nil)
         focused)
    (unwind-protect
        (progn
          (puthash 11 view yunge-reader-webview--views)
          (cl-letf
              (((symbol-function
                 'yunge-reader-webview--focus-owning-window)
                (lambda (value) (setq focused value))))
            (dolist (key '("'" "SPC" "M-m" "g" "m"))
              (setq unread-command-events nil
                    focused nil)
              (yunge-reader-webview--handle-event
               'fake-webview-process
               `((kind . "event")
                 (event . "accelerator")
                 (view . 11)
                 (repeat . nil)
                 (key . ,key)))
              (should (eq focused view))
              (should
               (equal unread-command-events
                      (listify-key-sequence (kbd key)))))))
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
        (repeat . nil)
        (key . "z")))
     :type 'error)))

(ert-deftest yunge-reader-webview-rejects-malformed-key-repeat-state ()
  (let ((yunge-reader-webview--process 'fake-webview-process)
        (yunge-reader-webview--views
         (make-hash-table :test #'eql)))
    (dolist
        (message
         '(((kind . "event")
            (event . "accelerator")
            (view . 9)
            (key . "j"))
           ((kind . "event")
            (event . "accelerator")
            (view . 9)
            (repeat . 1)
            (key . "j"))))
      (should-error
       (yunge-reader-webview--handle-event
        'fake-webview-process message)
       :type 'error))))

(ert-deftest yunge-reader-webview-events-do-not-consume-callbacks ()
  (yunge-reader-webview-test--with-fake-process
    (yunge-reader-webview-start)
    (yunge-reader-transport--mark-ready
     yunge-reader-webview--transport 'fake-webview-process)
    (let (handled)
      (cl-letf (((symbol-function 'yunge-reader-webview--handle-event)
                 (lambda (process message)
                   (setq handled (list process message)))))
        (yunge-reader-webview--handle-message
         'fake-webview-process
         '((kind . "event")
           (event . "accelerator")
           (view . 3)
           (repeat . nil)
           (key . "C-g"))))
      (should (equal handled
                     '(fake-webview-process
                       ((kind . "event")
                        (event . "accelerator")
                        (view . 3)
                        (repeat . nil)
                        (key . "C-g")))))
      (should (zerop
               (hash-table-count
                (yunge-reader-transport--session-callbacks
                 yunge-reader-webview--transport)))))))

(ert-deftest yunge-reader-webview-records-renderer-publication-events ()
  (let* ((buffer (generate-new-buffer " *webview EPUB event*"))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 6 'opening)
           :buffer buffer))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         warning
         (location-notifications 0)
         zoom-notification
         location-user
         outline-result
         outline-error)
    (unwind-protect
        (progn
          (puthash 6 view yunge-reader-webview--views)
          (setf
           (yunge-reader-webview--view-location-changed-function view)
           (lambda (_value user)
             (setq location-user user)
             (cl-incf location-notifications))
           (yunge-reader-webview--view-zoom-changed-function view)
           (lambda (value scale)
             (setq zoom-notification (list value scale))))
          (yunge-reader-webview--request-view-outline
           view
           (lambda (value error-data)
             (setq outline-result value
                   outline-error error-data)))
          (cl-letf (((symbol-function 'yunge-reader-webview--sync-view)
                     #'ignore))
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
                   (truncated))))))
          (should
           (yunge-reader-webview--surface-ready-p
            (yunge-reader-webview--view-surface view)))
          (should
           (equal
            (with-current-buffer buffer (buffer-string))
            (concat yunge-reader-webview--passive-buffer-message "\n")))
          (should
           (equal (yunge-reader-webview--view-location view)
                  (yunge-reader-webview-test--location 0.25)))
          (should (= location-notifications 1))
          (should-not location-user)
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
             (user . t)
             (location
              . ((cfi . "epubcfi(/6/6!/4/2)")
                 (href . "OPS/next.xhtml")
                 (fraction . 0.3)))))
          (should (= location-notifications 2))
          (should location-user)
          (should
           (equal (yunge-reader-webview--view-location view)
                   '((cfi . "epubcfi(/6/6!/4/2)")
                     (href . "OPS/next.xhtml")
                     (fraction . 0.3))))
          (setf (yunge-reader-webview--surface-appearance
                 (yunge-reader-webview--view-surface view))
                (yunge-reader-webview-test--follow-appearance))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest value) (setq warning value))))
            (yunge-reader-webview--handle-event
             'fake-webview-process
             '((kind . "event")
               (event . "appearance-error")
               (view . 6)
               (message . "bad appearance"))))
          (should-not
           (yunge-reader-webview--surface-appearance
            (yunge-reader-webview--view-surface view)))
          (should (equal (cadr warning) "bad appearance"))
          (setf (yunge-reader-webview--surface-style
                 (yunge-reader-webview--view-surface view))
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
           (yunge-reader-webview--surface-style
            (yunge-reader-webview--view-surface view)))
          (should (equal (cadr warning) "bad style"))
          (yunge-reader-webview--handle-event
           'fake-webview-process
           '((kind . "event")
             (event . "zoom-changed")
             (view . 6)
             (scale . 1.25)))
          (should (equal zoom-notification (list view 1.25)))
          (should-error
           (yunge-reader-webview--handle-event
            'fake-webview-process
            '((kind . "event")
              (event . "zoom-changed")
              (view . 6)
              (scale . 0))))
          (setf (yunge-reader-webview--surface-zoom
                 (yunge-reader-webview--view-surface view))
                'fit-width)
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest value) (setq warning value))))
            (yunge-reader-webview--handle-event
             'fake-webview-process
             '((kind . "event")
               (event . "zoom-error")
               (view . 6)
               (message . "bad zoom"))))
          (should-not
           (yunge-reader-webview--surface-zoom
            (yunge-reader-webview--view-surface view)))
          (should (equal (cadr warning) "bad zoom"))
          (setf
           (yunge-reader-webview--surface-scroll-bar-mode
            (yunge-reader-webview--view-surface view))
           'visible)
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest value) (setq warning value))))
            (yunge-reader-webview--handle-event
             'fake-webview-process
             '((kind . "event")
               (event . "scroll-bars-error")
               (view . 6)
               (message . "bad scroll bars"))))
          (should-not
           (yunge-reader-webview--surface-scroll-bar-mode
            (yunge-reader-webview--view-surface view)))
          (should (equal (cadr warning) "bad scroll bars"))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest value) (setq warning value))))
            (yunge-reader-webview--handle-event
             'fake-webview-process
             '((kind . "event")
               (event . "publication-error")
               (view . 6)
               (message . "bad chapter"))))
          (should
           (eq (yunge-reader-webview--surface-state
                (yunge-reader-webview--view-surface view))
               'failed))
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
  (let ((view
         (yunge-reader-webview--make-view
          :surface
          (yunge-reader-webview-test--surface 41 'ready)))
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
  (let* ((first
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 11 'ready)))
         (second
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 12 'ready)))
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
       (user)
       (location
        . ((cfi . "epubcfi(/6/4)")
           (href . "OPS/first.xhtml")
           (fraction . 0.2)))))
    (yunge-reader-webview--handle-event
     'fake-webview-process
     '((kind . "event")
       (event . "location")
       (view . 12)
       (user . t)
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
  (let* (changes
         (first
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 31 'ready)
           :selection-changed-function
           (lambda (view)
             (push
              (copy-tree
               (yunge-reader-webview--view-selection view))
              changes))))
         (second
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 32 'ready)))
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
    (should (equal changes (list selection)))
    (should-not (yunge-reader-webview--view-selection second))
    (yunge-reader-webview--handle-event
     'fake-webview-process
     '((kind . "event")
       (event . "selection")
       (view . 31)
       (selection)))
    (should-not (yunge-reader-webview--view-selection first))
    (should (equal changes (list nil selection)))
    (should-error
     (yunge-reader-webview--handle-event
      'fake-webview-process
      '((kind . "event")
        (event . "selection")
        (view . 32)))
     :type 'error)))

(ert-deftest yunge-reader-webview-clears-only-live-native-selections ()
  (let ((view
         (yunge-reader-webview--make-view
          :surface
          (yunge-reader-webview-test--surface 31 'ready)))
        (yunge-reader-webview--process 'fake-webview-process)
        requests)
    (cl-letf
        (((symbol-function 'process-live-p) (lambda (_process) t))
         ((symbol-function 'yunge-reader-webview--request)
          (lambda (operation parameters _complete)
            (push (list operation parameters) requests))))
      (should (yunge-reader-webview--clear-view-selection view))
      (setf (yunge-reader-webview--view-destroyed view) t)
      (should-not (yunge-reader-webview--clear-view-selection view)))
    (should
     (equal requests
            '(("view-clear-selection" ((view . 31))))))))

(ert-deftest yunge-reader-webview-selects-only-live-native-ranges ()
  (let ((view
         (yunge-reader-webview--make-view
          :surface
          (yunge-reader-webview-test--surface 31 'ready)))
        (selection (yunge-reader-webview-test--selection))
        (yunge-reader-webview--process 'fake-webview-process)
        requests)
    (cl-letf
        (((symbol-function 'process-live-p) (lambda (_process) t))
         ((symbol-function 'yunge-reader-webview--request)
          (lambda (operation parameters _complete)
            (push (list operation parameters) requests))))
      (should
       (yunge-reader-webview--select-view-range view selection))
      (should-not (yunge-reader-webview--view-selection view))
      (setf (yunge-reader-webview--view-destroyed view) t)
      (should-not
       (yunge-reader-webview--select-view-range view selection)))
    (should
     (equal
      requests
      `(("view-set-selection"
         ((view . 31) (selection . ,selection))))))))

(ert-deftest yunge-reader-webview-reconciles-resize-during-creation ()
  (let* ((created-bounds
          '((x . 0) (y . 20) (width . 800) (height . 700)))
         (latest-bounds
          '((x . 0) (y . 40) (width . 800) (height . 680)))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            8 'creating :window 'window
            :requested-bounds latest-bounds)))
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         request)
    (puthash 8 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--visible-windows)
          (lambda (_view) '(window)))
         ((symbol-function 'yunge-reader-webview--visible-window)
          (lambda (_view) 'window))
         ((symbol-function 'yunge-reader-webview--resolved-appearance)
          (lambda (&rest _arguments)
            (yunge-reader-webview-test--original-appearance)))
         ((symbol-function 'yunge-reader-webview--update-scroll-bar-mode)
          #'ignore)
         ((symbol-function 'yunge-reader-webview--window-bounds)
          (lambda (_window) latest-bounds))
         ((symbol-function 'yunge-reader-webview--request)
          (lambda (operation parameters complete)
            (setq request (list operation parameters complete)))))
      (yunge-reader-webview--create-complete
       view 8 created-bounds nil nil)
      (should
       (eq (yunge-reader-webview--surface-state
            (yunge-reader-webview--view-surface view))
           'native-ready))
      (should (equal (yunge-reader-webview--surface-bounds
                      (yunge-reader-webview--view-surface view))
                     created-bounds))
      (should (equal (car request) "view-bounds"))
      (should (equal (alist-get 'bounds (cadr request))
                     latest-bounds))
      (funcall (caddr request) nil nil)
      (should (equal (yunge-reader-webview--surface-bounds
                      (yunge-reader-webview--view-surface view))
                     latest-bounds)))))

(ert-deftest yunge-reader-webview-targets-the-active-presentation ()
  (let ((buffer (generate-new-buffer " *webview presentations*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((second (split-window-right))
                (view
                 (yunge-reader-webview--make-view :buffer buffer)))
            (set-window-buffer second buffer)
            (select-window second)
            (should
             (eq (yunge-reader-webview--visible-window view) second))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-webview-keeps-one-surface-per-visible-window ()
  (let ((buffer (generate-new-buffer " *webview same frame*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let* ((first (selected-window))
                 (second (split-window-right))
                 (first-surface
                  (yunge-reader-webview-test--surface
                   9 'ready :window first))
                 (view
                  (yunge-reader-webview--make-view
                   :surface first-surface :buffer buffer))
                 synchronized
                 started)
            (set-window-buffer second buffer)
            (select-window second)
            (cl-letf
                (((symbol-function 'yunge-reader-webview--sync-surface)
                  (lambda (_view surface window)
                    (push (cons surface window) synchronized)))
                 ((symbol-function 'yunge-reader-webview--release-surface)
                  (lambda (&rest _arguments)
                    (ert-fail "Visible presentation was released")))
                 ((symbol-function 'yunge-reader-webview--start-surface)
                  (lambda (value window)
                    (setq started window)
                    (yunge-reader-webview--register-surface
                     value
                     (yunge-reader-webview-test--surface
                      10 'creating :window window)))))
              (yunge-reader-webview--sync-view view))
            (should (eq started second))
            (should (assq first-surface synchronized))
            (should (eq (yunge-reader-webview--view-surface-for-window
                         view first)
                        first-surface))
            (let ((second-surface
                   (yunge-reader-webview--view-surface-for-window
                    view second)))
              (should second-surface)
              (should-not (eq first-surface second-surface))
              (should (= (yunge-reader-webview--surface-id second-surface)
                         10))
              (should (eq (yunge-reader-webview--view-surface view)
                          second-surface)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-webview-releases-only-removed-presentations ()
  (let* ((first
          (yunge-reader-webview-test--surface
           9 'ready :window 'first))
         (second
          (yunge-reader-webview-test--surface
           10 'ready :window 'second))
         (view (yunge-reader-webview--make-view :surface first))
         released
         synchronized)
    (yunge-reader-webview--register-surface view first)
    (yunge-reader-webview--register-surface view second)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--visible-windows)
          (lambda (_view) '(second)))
         ((symbol-function 'yunge-reader-webview--visible-window)
          (lambda (_view) 'second))
         ((symbol-function 'yunge-reader-webview--sync-surface)
          (lambda (_view surface window)
            (setq synchronized (cons surface window))))
         ((symbol-function 'yunge-reader-webview--release-surface)
          (lambda (value &optional _complete surface)
            (push surface released)
            (yunge-reader-webview--unregister-surface value surface)))
         ((symbol-function 'yunge-reader-webview--start-surface)
          (lambda (&rest _arguments)
            (ert-fail "Existing presentation was recreated"))))
      (yunge-reader-webview--sync-view view)
      (should (equal released (list first)))
      (should (equal synchronized (cons second 'second)))
      (should (eq (yunge-reader-webview--view-surface view) second)))))

(ert-deftest yunge-reader-webview-selects-the-active-window-surface ()
  (let* ((first
          (yunge-reader-webview-test--surface
           40 'ready :window 'first))
         (second
          (yunge-reader-webview-test--surface
           41 'ready :window 'second))
         (view (yunge-reader-webview--make-view :surface first)))
    (yunge-reader-webview--register-surface view first)
    (yunge-reader-webview--register-surface view second)
    (cl-letf (((symbol-function 'yunge-reader-webview--visible-windows)
               (lambda (_view) '(first second)))
              ((symbol-function 'yunge-reader-webview--visible-window)
               (lambda (_view) 'second))
              ((symbol-function 'yunge-reader-webview--sync-surface)
               #'ignore)
              ((symbol-function 'yunge-reader-webview--release-surface)
               (lambda (&rest _arguments)
                 (ert-fail "Visible presentation was released")))
              ((symbol-function 'yunge-reader-webview--start-surface)
               (lambda (&rest _arguments)
                 (ert-fail "Visible presentation was recreated"))))
      (yunge-reader-webview--sync-view view))
    (should (eq (yunge-reader-webview--view-surface view) second))))

(ert-deftest yunge-reader-webview-keeps-presentation-location-and-selection ()
  (let* ((first-location (yunge-reader-webview-test--location 0.1))
         (second-location (yunge-reader-webview-test--location 0.6))
         (first-selection (yunge-reader-webview-test--selection))
         (second-selection
          '((href . "OPS/next.xhtml")
            (start . "epubcfi(/6/6!/4/2:1)")
            (end . "epubcfi(/6/6!/4/2:4)")))
         (first
          (yunge-reader-webview-test--surface
           50 'ready :window 'first :location first-location
           :selection first-selection))
         (second
          (yunge-reader-webview-test--surface
           51 'ready :window 'second))
         (view
          (yunge-reader-webview--make-view
           :surface first :location first-location
           :selection first-selection))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views (make-hash-table :test #'eql))
         (location-notifications 0)
         (selection-notifications 0))
    (yunge-reader-webview--register-surface view first)
    (yunge-reader-webview--register-surface view second)
    (puthash 50 view yunge-reader-webview--views)
    (puthash 51 view yunge-reader-webview--views)
    (setf (yunge-reader-webview--view-location-changed-function view)
          (lambda (&rest _arguments)
            (cl-incf location-notifications))
          (yunge-reader-webview--view-selection-changed-function view)
          (lambda (&rest _arguments)
            (cl-incf selection-notifications)))
    (cl-letf (((symbol-function 'yunge-reader-webview--visible-window)
               (lambda (_view) 'first)))
      (yunge-reader-webview--handle-event
       'fake-webview-process
       `((kind . "event") (event . "location") (view . 51)
         (user . t) (location . ,second-location)))
      (yunge-reader-webview--handle-event
       'fake-webview-process
       `((kind . "event") (event . "selection") (view . 51)
         (selection . ,second-selection))))
    (should (equal (yunge-reader-webview--surface-location second)
                   second-location))
    (should (equal (yunge-reader-webview--surface-selection second)
                   second-selection))
    (should (equal (yunge-reader-webview--view-location view)
                   first-location))
    (should (equal (yunge-reader-webview--view-selection view)
                   first-selection))
    (should (zerop location-notifications))
    (should (zerop selection-notifications))
    (cl-letf (((symbol-function 'yunge-reader-webview--visible-windows)
               (lambda (_view) '(first second)))
              ((symbol-function 'yunge-reader-webview--visible-window)
               (lambda (_view) 'second))
              ((symbol-function 'yunge-reader-webview--sync-surface)
               #'ignore))
      (yunge-reader-webview--sync-view view))
    (should (eq (yunge-reader-webview--view-surface view) second))
    (should (equal (yunge-reader-webview--view-location view)
                   second-location))
    (should (equal (yunge-reader-webview--view-selection view)
                   second-selection))
    (should (= location-notifications 1))
    (should (= selection-notifications 1))))

(ert-deftest yunge-reader-webview-moves-before-synchronizing-focus ()
  (let (calls)
    (cl-letf (((symbol-function 'yunge-reader-webview--sync-views)
               (lambda (&rest _arguments) (push 'views calls)))
              ((symbol-function 'yunge-reader-webview--sync-native-focus)
               (lambda (&rest _arguments) (push 'focus calls))))
      (yunge-reader-webview--window-selection-changed 'frame)
      (should (equal (nreverse calls) '(views focus))))))

(ert-deftest yunge-reader-webview-destroys-a-replaced-window-view ()
  (let* ((buffer (generate-new-buffer " *webview owner*"))
         (other (generate-new-buffer " *webview replacement*"))
         (window (selected-window))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            9 'native-ready :window window)
           :buffer buffer))
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
  (let* ((system-type 'darwin)
         (buffer (generate-new-buffer " *persistent EPUB owner*"))
         (other (generate-new-buffer " *persistent EPUB replacement*"))
         (window (selected-window))
         (location (yunge-reader-webview-test--location 0.4))
         (style (yunge-reader-webview-test--style))
         (appearance
          (yunge-reader-webview-test--follow-appearance))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface
            15 'ready :window window
            :appearance (copy-tree appearance)
            :style (copy-tree style)
            :zoom 'fit-width)
           :buffer buffer
           :persistent t
           :publication 6
           :appearance appearance
           :style style
           :zoom 'fit-width
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
          (should
           (= (yunge-reader-webview--surface-id
               (yunge-reader-webview--view-surface view))
              15))
          (should (eq (gethash 15 yunge-reader-webview--views) view))
          (should
           (eq (yunge-reader-webview--surface-window
                (yunge-reader-webview--view-surface view))
               window))
          (should (gethash view yunge-reader-webview--logical-views))
          (should (equal (yunge-reader-webview--view-location view)
                         location))
          (should
           (equal (yunge-reader-webview--view-appearance view)
                  appearance))
          (should-not
           (yunge-reader-webview--view-selection view))
          (should
           (equal (mapcar #'car (reverse requests))
                  '("view-clear-selection" "view-visible")))
          (should
           (eq (alist-get 'visible (cadr (car requests))) :false))
          (should-not
           (cl-find-if
            (lambda (request)
              (equal (car request) "view-destroy"))
            requests))
          (should-not
           (cl-find-if
            (lambda (request)
              (equal (car request) "publication-close"))
            requests)))
      (kill-buffer buffer)
      (kill-buffer other))))

(ert-deftest yunge-reader-webview-releases-focus-before-hiding-persistent-surface ()
  (let* ((surface
          (yunge-reader-webview-test--surface
           16 'ready :window 'window :native-focused t))
         (view
          (yunge-reader-webview--make-view
           :surface surface
           :buffer (current-buffer)
           :persistent t))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests)
    (puthash 16 view yunge-reader-webview--views)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--visible-windows)
          (lambda (_view) nil))
         ((symbol-function 'yunge-reader-webview--visible-window)
          (lambda (_view) nil))
         ((symbol-function 'process-live-p)
          (lambda (_process) t))
         ((symbol-function 'yunge-reader-webview--request)
          (lambda (operation parameters complete)
            (push (list operation parameters complete) requests))))
      (yunge-reader-webview--sync-view view))
    (should
     (equal (mapcar #'car (reverse requests))
            '("view-focus-parent" "view-clear-selection" "view-visible")))
    (should
     (yunge-reader-webview--surface-native-focused surface))
    (should
     (yunge-reader-webview--surface-focus-release-pending surface))
    (let ((request
           (seq-find
            (lambda (value)
              (equal (car value) "view-focus-parent"))
            requests)))
      (funcall (nth 2 request) nil nil))
    (should-not
     (yunge-reader-webview--surface-native-focused surface))
    (should-not
     (yunge-reader-webview--surface-focus-release-pending surface))))

(ert-deftest yunge-reader-webview-releases-hidden-failed-surfaces ()
  (let* ((system-type 'darwin)
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 16 'failed :window 'window)
           :buffer (current-buffer)
           :persistent t))
         released)
    (cl-letf (((symbol-function 'yunge-reader-webview--visible-window)
               (lambda (_view) nil))
              ((symbol-function 'yunge-reader-webview--visible-windows)
               (lambda (_view) nil))
              ((symbol-function 'yunge-reader-webview--release-surface)
               (lambda (value &optional _complete _surface)
                 (setq released value)))
              ((symbol-function 'yunge-reader-webview--set-view-visible)
               (lambda (&rest _arguments)
                 (ert-fail "Failed surface was retained"))))
      (yunge-reader-webview--sync-view view))
    (should (eq released view))))

(ert-deftest yunge-reader-webview-recreates-visible-persistent-surfaces ()
  (let* ((buffer (generate-new-buffer " *persistent EPUB visible*"))
         (window (selected-window))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer
           :persistent t
           :publication 8
           :appearance-function
           #'yunge-reader-webview-test--original-appearance
           :outline-error '(error "old renderer error")))
         (yunge-reader-webview--next-view-id 20)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requested)
    (unwind-protect
        (cl-letf
            (((symbol-function 'yunge-reader-webview--visible-windows)
              (lambda (_view) (list window)))
             ((symbol-function 'yunge-reader-webview--visible-window)
              (lambda (_view) window))
             ((symbol-function 'window-buffer)
              (lambda (_window) buffer))
             ((symbol-function 'window-live-p)
              (lambda (_window) t))
             ((symbol-function 'yunge-reader-webview--window-bounds)
              (lambda (_window)
                '((x . 0) (y . 0) (width . 800) (height . 600))))
             ((symbol-function 'yunge-reader-webview--request-create)
              (lambda (value _surface) (setq requested value))))
          (yunge-reader-webview--sync-view view)
          (should (eq requested view))
          (let ((surface
                 (yunge-reader-webview--view-surface view)))
            (should (= (yunge-reader-webview--surface-id surface) 21))
            (should
             (eq (yunge-reader-webview--surface-window surface)
                 window))
            (should
             (eq (yunge-reader-webview--surface-state surface)
                 'creating))
            (should-not
             (yunge-reader-webview--surface-zoom surface)))
          (should-not
           (yunge-reader-webview--view-outline-error view))
          (should (eq (gethash 21 yunge-reader-webview--views) view)))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-webview-reopens-with-view-local-style ()
  (let* ((location (yunge-reader-webview-test--location 0.4))
         (style (yunge-reader-webview-test--style))
         (appearance
          (yunge-reader-webview-test--follow-appearance))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 22 'native-ready)
           :publication 8
           :location location :appearance appearance
           :style style :scroll-bar-mode 'hidden))
         opened)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--open-view-publication)
          (lambda (value publication _complete
                         target appearance reading-style zoom bar-mode)
            (setq opened
                  (list value publication target appearance
                        reading-style zoom bar-mode)))))
      (yunge-reader-webview--try-open-publication view))
    (should
     (equal opened
            (list view 8 location appearance style nil 'hidden)))
    (should
     (eq (yunge-reader-webview--surface-state
          (yunge-reader-webview--view-surface view))
         'opening))
    (should
     (equal (yunge-reader-webview--surface-appearance
             (yunge-reader-webview--view-surface view))
            appearance))
    (should
     (equal (yunge-reader-webview--surface-style
             (yunge-reader-webview--view-surface view))
            style))
    (should-not
     (eq (yunge-reader-webview--surface-style
          (yunge-reader-webview--view-surface view))
         style))))

(ert-deftest yunge-reader-webview-reopens-with-view-local-fixed-zoom ()
  (let* ((location (yunge-reader-webview-test--location 0.4))
         (appearance
          (yunge-reader-webview-test--original-appearance))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 23 'native-ready)
           :layout 'fixed
           :publication 8 :location location :zoom 'fit-width
           :appearance appearance :scroll-bar-mode 'hidden))
         opened)
    (cl-letf
        (((symbol-function
           'yunge-reader-webview--open-view-publication)
          (lambda (value publication _complete
                         target appearance style zoom bar-mode)
            (setq opened
                  (list value publication target appearance
                        style zoom bar-mode)))))
      (yunge-reader-webview--try-open-publication view))
    (should
     (equal opened
            (list view 8 location appearance nil 'fit-width 'hidden)))
    (should
     (eq (yunge-reader-webview--surface-zoom
          (yunge-reader-webview--view-surface view))
         'fit-width))))

(ert-deftest yunge-reader-webview-copies-attached-reading-style ()
  (let ((style (yunge-reader-webview-test--style))
        (link-function (lambda (_view _uri)))
        (yunge-reader-webview--logical-views
         (make-hash-table :test #'eq)))
    (with-temp-buffer
      (let ((view
             (yunge-reader-webview--attach-shared-publication
               8 'reflow
               yunge-reader-webview-test--resource-root
               yunge-reader-webview-test--renderer-url 3
               :appearance-function
               #'yunge-reader-webview-test--original-appearance
               :style style
              :external-link-function link-function)))
        (should (eq (yunge-reader-webview--view-layout view) 'reflow))
        (should
         (equal (yunge-reader-webview--view-style view) style))
        (should-not (eq (yunge-reader-webview--view-style view) style))
        (setcdr (assq 'font-scale style) 2.0)
        (should
         (= (alist-get 'font-scale
                       (yunge-reader-webview--view-style view))
            1.25))
        (should
         (eq (yunge-reader-webview--view-external-link-function view)
             link-function))))))

(ert-deftest yunge-reader-webview-rejects-style-for-fixed-layout ()
  (let ((view
         (yunge-reader-webview--make-view :layout 'fixed)))
    (should-error
     (yunge-reader-webview--set-view-style
     view (yunge-reader-webview-test--style)))))

(ert-deftest yunge-reader-webview-attaches-independent-fixed-zoom ()
  (let ((function (lambda (_view _scale)))
        (yunge-reader-webview--logical-views
         (make-hash-table :test #'eq)))
    (with-temp-buffer
      (let ((view
             (yunge-reader-webview--attach-shared-publication
               8 'fixed
               yunge-reader-webview-test--resource-root
               yunge-reader-webview-test--renderer-url 3
              :appearance-function
              #'yunge-reader-webview-test--original-appearance
              :zoom 1.5
              :zoom-changed-function function)))
        (should (eq (yunge-reader-webview--view-layout view) 'fixed))
        (should (= (yunge-reader-webview--view-zoom view) 1.5))
        (should
         (eq (yunge-reader-webview--view-zoom-changed-function view)
             function))))
    (with-temp-buffer
      (let ((view
             (yunge-reader-webview--attach-shared-publication
               8 'fixed
               yunge-reader-webview-test--resource-root
               yunge-reader-webview-test--renderer-url 3
              :appearance-function
              #'yunge-reader-webview-test--original-appearance)))
        (should
         (eq (yunge-reader-webview--view-zoom view) 'fit-page))))))

(ert-deftest yunge-reader-webview-rejects-layout-presentation-mismatches ()
  (let ((yunge-reader-webview--logical-views
         (make-hash-table :test #'eq)))
    (with-temp-buffer
      (should-error
        (yunge-reader-webview--attach-shared-publication
         8 'fixed
         yunge-reader-webview-test--resource-root
         yunge-reader-webview-test--renderer-url 3
         :appearance-function
         #'yunge-reader-webview-test--original-appearance
         :style (yunge-reader-webview-test--style))))
    (with-temp-buffer
      (should-error
        (yunge-reader-webview--attach-shared-publication
         8 'reflow
         yunge-reader-webview-test--resource-root
         yunge-reader-webview-test--renderer-url 3
         :appearance-function
         #'yunge-reader-webview-test--original-appearance
         :zoom 'fit-page)))
    (with-temp-buffer
      (should-error
        (yunge-reader-webview--attach-shared-publication
         8 'reflow
         yunge-reader-webview-test--resource-root
         yunge-reader-webview-test--renderer-url 3
         :appearance-function
         #'yunge-reader-webview-test--original-appearance
         :zoom-changed-function #'ignore)))))

(ert-deftest yunge-reader-webview-waits-for-every-obsolete-surface ()
  (let* ((view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-webview-test--surface 31 'native-ready)
           :persistent t :publication 8))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         (yunge-reader-webview--logical-views
          (make-hash-table :test #'eq))
         requests
         cancelled
         finished)
    (puthash 31 view yunge-reader-webview--views)
    (puthash view t yunge-reader-webview--logical-views)
    (cl-letf
        (((symbol-function 'process-live-p) (lambda (_process) t))
         ((symbol-function 'yunge-reader-webview--request)
          (lambda (_operation parameters complete)
            (push (cons (alist-get 'view parameters) complete)
                  requests)))
         ((symbol-function 'yunge-reader-webview--cancel-view-requests)
          (lambda (candidate _reason)
            (setq cancelled candidate))))
      (yunge-reader-webview--release-surface view)
      (setf (yunge-reader-webview--view-surface view)
            (yunge-reader-webview-test--surface 32 'native-ready))
      (puthash 32 view yunge-reader-webview--views)
      (yunge-reader-webview--destroy-view
       view (lambda () (setq finished t)))
      (should (eq cancelled view))
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
           :surface
           (yunge-reader-webview-test--surface 10 'native-ready)
           :publication 3 :location location
           :broker-session 9 :owns-publication t))
         (yunge-reader-webview--process 'fake-webview-process)
         (yunge-reader-webview--views
          (make-hash-table :test #'eql))
         requests
         closed)
    (puthash 10 view yunge-reader-webview--views)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'yunge-reader-webview--request)
               (lambda (operation parameters complete)
                 (push (list operation parameters complete) requests)))
              ((symbol-function 'yunge-reader-webview--close-publication)
               (lambda (session publication complete)
                 (setq closed (list session publication))
                 (funcall complete '((closed . t)) nil))))
      (yunge-reader-webview--destroy-view view)
      (should (equal (yunge-reader-webview--view-location view)
                     location))
      (should (equal (caar requests) "view-destroy"))
      (funcall (nth 2 (car requests)) nil nil)
      (should (equal closed '(9 3)))
      (should
       (equal (mapcar #'car (nreverse requests))
              '("view-destroy"))))))

;;; yunge-reader-webview-test.el ends here
