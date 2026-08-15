;;; yunge-reader-test.el --- Document reader tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader)

(ert-deftest yunge-reader-registers-replaces-and-resolves-drivers ()
  (let ((yunge-reader-drivers nil))
    (yunge-reader-register-driver
     'fallback
     :match (lambda (_file) t)
     :open #'ignore :close #'ignore :request #'ignore)
    (yunge-reader-register-driver
     'pdf
     :match (lambda (file) (string-suffix-p ".pdf" file t))
     :open #'ignore :close #'ignore :request #'ignore)
    (should
     (eq (yunge-reader-driver-name
          (yunge-reader-driver-for-file "book.pdf"))
         'pdf))
    (let ((replacement
           (yunge-reader-register-driver
            'pdf
            :match (lambda (file) (string-suffix-p ".xps" file t))
            :open #'ignore :close #'ignore :request #'ignore)))
      (should (= (length yunge-reader-drivers) 2))
      (should (eq (car yunge-reader-drivers) replacement))
      (should
       (eq (yunge-reader-driver-name
            (yunge-reader-driver-for-file "book.pdf"))
           'fallback))
      (should
       (eq (yunge-reader-driver-name
            (yunge-reader-driver-for-file "book.xps"))
           'pdf)))))

(ert-deftest yunge-reader-opens-reuses-and-closes-one-document ()
  (let ((yunge-reader-drivers nil)
        opened
        closed
        buffer)
    (unwind-protect
        (save-window-excursion
          (yunge-reader-register-driver
           'test
           :match (lambda (_file) t)
           :open
           (lambda (file complete)
             (setq opened file)
             (funcall complete 'handle
                      '(:layout fixed :metadata (:pages 3)) nil))
           :close (lambda (document) (push document closed))
           :request #'ignore)
          (setq buffer (yunge-reader-open "book.pdf"))
          (with-current-buffer buffer
            (should (eq major-mode 'yunge-reader-mode))
            (should (equal opened
                           (expand-file-name "book.pdf")))
            (should
             (eq (yunge-reader-document-handle yunge-reader-document)
                 'handle))
            (should
             (eq (yunge-reader-document-layout yunge-reader-document)
                 'fixed)))
          (should (eq (yunge-reader-open "book.pdf") buffer))
          (kill-buffer buffer)
          (setq buffer nil)
          (should (= (length closed) 1))
          (should
           (eq (yunge-reader-document-handle (car closed)) 'handle)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-closes-a-handle-returned-after-buffer-death ()
  (let ((yunge-reader-drivers nil)
        complete
        closed
        buffer)
    (unwind-protect
        (save-window-excursion
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open (lambda (_file callback)
                          (setq complete callback))
                  :close (lambda (document) (push document closed))
                  :request #'ignore)))
            (setq buffer (generate-new-buffer " *reader-late-test*"))
            (with-current-buffer buffer
              (yunge-reader-mode))
            (yunge-reader--begin-open buffer driver "late.pdf")
            (kill-buffer buffer)
            (setq buffer nil)
            (funcall complete 'late-handle '(:layout fixed) nil)
            (should (= (length closed) 1))
            (should
             (eq (yunge-reader-document-handle (car closed))
                 'late-handle))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-rejects-invalid-driver-layouts ()
  (let ((yunge-reader-drivers nil)
        closed
        buffer)
    (unwind-protect
        (save-window-excursion
          (yunge-reader-register-driver
           'test
           :match (lambda (_file) t)
           :open (lambda (_file complete)
                   (funcall complete 'handle '(:layout pages) nil))
           :close (lambda (document) (push document closed))
           :request #'ignore)
          (setq buffer (yunge-reader-open "invalid.pdf"))
          (with-current-buffer buffer
            (should-not yunge-reader-document)
            (should (string-match-p "invalid layout" (buffer-string))))
          (should (= (length closed) 1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-zoom-state-is-bounded-and-refreshes ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((yunge-reader-minimum-scale 0.5)
          (yunge-reader-maximum-scale 2.0)
          (yunge-reader-zoom-factor 2.0)
          (refreshes 0))
      (add-hook 'yunge-reader-refresh-hook
                (lambda () (cl-incf refreshes)) nil t)
      (yunge-reader-set-effective-scale 1.5)
      (should (= (yunge-reader-zoom-in) 2.0))
      (should (eq yunge-reader-zoom-mode 'manual))
      (should (= (yunge-reader-zoom-out 3) 0.5))
      (should (eq (yunge-reader-fit-width) 'fit-width))
      (should-not yunge-reader-effective-scale)
      (should (eq (yunge-reader-fit-page) 'fit-page))
      (should (= refreshes 4)))))

(ert-deftest yunge-reader-copies-cached-and-driver-resolved-selections ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (kill-ring nil)
           (start (make-yunge-reader-position :unit 1 :offset 2))
           (end (make-yunge-reader-position :unit 1 :offset 5))
           (requests 0)
           completion
           (driver
            (yunge-reader-register-driver
             'selection-test
             :match #'ignore
             :open #'ignore
             :close #'ignore
             :request
             (lambda (_document operation arguments complete)
               (cl-incf requests)
               (should (eq operation 'selection-text))
               (should (eq (plist-get arguments :start) start))
               (should (eq (plist-get arguments :end) end))
               (setq completion complete)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "selection.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-set-selection start end "cached")
      (let ((inhibit-message t))
        (should (equal (yunge-reader-copy-selection) "cached")))
      (should (equal (car kill-ring) "cached"))
      (yunge-reader-set-selection start end)
      (let ((inhibit-message t))
        (yunge-reader-copy-selection))
      (should (= requests 1))
      (with-temp-buffer
        (let ((inhibit-message t))
          (funcall completion '(:text "resolved") nil)))
      (should (equal (car kill-ring) "resolved"))
      (should
       (equal (yunge-reader-selection-text yunge-reader-selection)
              "resolved")))))

(ert-deftest yunge-reader-request-completes-only-once ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (calls 0)
           (driver
            (yunge-reader-register-driver
             'request-test
             :match #'ignore :open #'ignore :close #'ignore
             :request
             (lambda (_document _operation _arguments complete)
               (funcall complete 'first nil)
               (funcall complete 'second nil)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "request.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-request
       'test nil
       (lambda (value error-data)
         (should-not error-data)
         (should (eq value 'first))
         (cl-incf calls)))
      (should (= calls 1)))))

(ert-deftest yunge-reader-search-loads-bounded-batches-on-demand ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completions
           (hook-calls 0)
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 8 :offset 3)
             :end (make-yunge-reader-position :unit 8 :offset 8)
             :text "needle" :before "a " :after " b"))
           (second
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 9 :offset 4)
             :end (make-yunge-reader-position :unit 9 :offset 9)
             :text "needle"))
           (driver
            (yunge-reader-register-driver
             'search-test
             :match #'ignore :open #'ignore :close #'ignore
             :request
             (lambda (_document operation arguments complete)
               (should (eq operation 'search))
               (setq requests (append requests (list arguments))
                     completions (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (add-hook 'yunge-reader-search-result-hook
                (lambda () (cl-incf hook-calls)) nil t)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest arguments)
                   (apply function arguments))))
        (let ((inhibit-message t))
          (yunge-reader-search "needle"))
        (should-not (plist-get (car requests) :case-sensitive))
        (should (= (plist-get (car requests) :page-limit)
                   yunge-reader-search-page-limit))
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results nil
          :cursor (make-yunge-reader-position :unit 8 :offset 0))
         nil)
        (should (= (length requests) 2))
        (should
         (equal (plist-get (cadr requests) :cursor)
                (make-yunge-reader-position :unit 8 :offset 0)))
        (let ((inhibit-message t))
          (funcall
           (pop completions)
           (make-yunge-reader-search-batch
            :results (list first second)
            :cursor (make-yunge-reader-position :unit 10 :offset 0))
           nil))
        (should (eq yunge-reader-search-result first))
        (should (= yunge-reader--search-index 0))
        (let ((inhibit-message t))
          (yunge-reader-search-next))
        (should (eq yunge-reader-search-result second))
        (let ((inhibit-message t))
          (yunge-reader-search-next))
        (should (= (length requests) 3))
        (let ((inhibit-message t))
          (funcall
           (pop completions)
           (make-yunge-reader-search-batch :results nil :done t)
           nil))
        (should (eq yunge-reader-search-result first))
        (should (= hook-calls 4))))))

(ert-deftest yunge-reader-search-uses-smart-case-and-rejects-late-results ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completions
           (late
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 0 :offset 0)
             :end (make-yunge-reader-position :unit 0 :offset 2)
             :text "old"))
           (current
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 1 :offset 1)
             :end (make-yunge-reader-position :unit 1 :offset 3)
             :text "New"))
           (driver
            (yunge-reader-register-driver
             'stale-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :request
             (lambda (_document _operation arguments complete)
               (push arguments requests)
               (push complete completions)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((inhibit-message t))
        (yunge-reader-search "old")
        (yunge-reader-search "New"))
      (should (plist-get (car requests) :case-sensitive))
      (should-not (plist-get (cadr requests) :case-sensitive))
      (let ((new-completion (car completions))
            (old-completion (cadr completions))
            (inhibit-message t))
        (funcall
         old-completion
         (make-yunge-reader-search-batch
          :results (list late) :done t)
         nil)
        (should-not yunge-reader-search-result)
        (funcall
         new-completion
         (make-yunge-reader-search-batch
          :results (list current) :done t)
         nil))
      (should (eq yunge-reader-search-result current)))))

(ert-deftest yunge-reader-search-previous-finishes-before-wrapping ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           completion
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 0 :offset 0)
             :end (make-yunge-reader-position :unit 0 :offset 0)))
           (last
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 2 :offset 0)
             :end (make-yunge-reader-position :unit 2 :offset 0)))
           (driver
            (yunge-reader-register-driver
             'backward-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :request
             (lambda (_document _operation _arguments complete)
               (setq completion complete)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((inhibit-message t))
        (yunge-reader-search "x")
        (funcall
         completion
         (make-yunge-reader-search-batch
          :results (list first)
          :cursor (make-yunge-reader-position :unit 1 :offset 0))
         nil)
        (yunge-reader-search-previous)
        (funcall
         completion
         (make-yunge-reader-search-batch
          :results (list last) :done t)
         nil))
      (should (eq yunge-reader-search-result last)))))

;;; yunge-reader-test.el ends here
