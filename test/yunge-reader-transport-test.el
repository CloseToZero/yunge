;;; yunge-reader-transport-test.el --- Transport -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-transport)

(defmacro yunge-reader-transport-test--with-process (&rest body)
  "Run BODY with one fake process and isolated process properties."
  (declare (indent 0) (debug t))
  `(let ((properties (make-hash-table :test #'equal))
         sent)
     (cl-letf
         (((symbol-function 'process-put)
           (lambda (process property value)
             (puthash (cons process property) value properties)))
          ((symbol-function 'process-get)
           (lambda (process property)
             (gethash (cons process property) properties)))
          ((symbol-function 'process-send-string)
           (lambda (_process string) (push string sent))))
       ,@body)))

(defun yunge-reader-transport-test--parse (line)
  "Parse one serialized transport LINE."
  (json-parse-string line :object-type 'alist))

(ert-deftest yunge-reader-transport-queues-and-routes-requests ()
  (yunge-reader-transport-test--with-process
    (let* ((validated nil)
           (ready nil)
           first
           second
           (session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready
             (lambda (message)
               (should (equal (alist-get 'kind message) "ready"))
               (setq validated t))
             :ready-function
             (lambda (_process _message) (setq ready t))
             :response-error-function
             (lambda (message)
               (list 'test-error
                     (alist-get 'message (alist-get 'error message)))))))
      (yunge-reader-transport--bind session 'fake-process)
      (should
       (= (yunge-reader-transport--request
           session 'fake-process "first" nil
           (lambda (result error-data)
             (setq first (list result error-data))))
          1))
      (should
       (= (yunge-reader-transport--request
           session 'fake-process "second" '((value . 2))
           (lambda (result error-data)
             (setq second (list result error-data))))
          2))
      (should-not sent)
      (yunge-reader-transport--handle-message
       session 'fake-process '((kind . "ready")))
      (should validated)
      (should ready)
      (should (yunge-reader-transport--ready-p session 'fake-process))
      (let ((requests
             (mapcar #'yunge-reader-transport-test--parse
                     (nreverse sent))))
        (should (equal (mapcar (lambda (value)
                                (alist-get 'id value))
                              requests)
                       '(1 2)))
        (should (equal (alist-get 'op (car requests)) "first"))
        (should
         (= (alist-get 'value
                       (alist-get 'params (cadr requests)))
            2)))
      (yunge-reader-transport--handle-message
       session 'fake-process
       '((id . 1) (ok . t) (result . ((value . 1)))))
      (yunge-reader-transport--handle-message
       session 'fake-process
       '((id . 2) (ok) (error . ((message . "failed")))))
      (should (equal first '(((value . 1)) nil)))
      (should (equal second '(nil (test-error "failed"))))
      (should
       (zerop
        (hash-table-count
         (yunge-reader-transport--session-callbacks session)))))))

(ert-deftest yunge-reader-transport-frames-events-and-responses ()
  (yunge-reader-transport-test--with-process
    (let* (events result
           (session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :event-function
             (lambda (_process message) (push message events))
             :response-error-function #'ignore)))
      (yunge-reader-transport--bind session 'fake-process)
      (yunge-reader-transport--filter
       'fake-process
       "{\"kind\":\"ready\"}\n{\"kind\":\"event\",\"value\":")
      (should-not events)
      (yunge-reader-transport--filter 'fake-process "3}\r\n")
      (should (= (alist-get 'value (car events)) 3))
      (yunge-reader-transport--request
       session 'fake-process "read" nil
       (lambda (value error-data)
         (should-not error-data)
         (setq result value)))
      (yunge-reader-transport--filter
       'fake-process "{\"id\":1,\"ok\":true,\"result\":")
      (should-not result)
      (yunge-reader-transport--filter 'fake-process "{\"value\":4}}\n")
      (should (= (alist-get 'value result) 4)))))

(ert-deftest yunge-reader-transport-reports-invalid-output ()
  (yunge-reader-transport-test--with-process
    (let* (invalid warning
           (session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :response-error-function #'ignore
             :invalid-output-function
             (lambda (process error-data)
               (setq invalid (list process error-data))))))
      (yunge-reader-transport--bind session 'fake-process)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest value) (setq warning value))))
        (yunge-reader-transport--filter 'fake-process "not-json\n"))
      (should (eq (car invalid) 'fake-process))
      (should (eq (car (cadr invalid)) 'json-parse-error))
      (should
       (string-prefix-p "Invalid test helper output: "
                        (cadr warning))))))

(ert-deftest yunge-reader-transport-fails-one-session ()
  (yunge-reader-transport-test--with-process
    (let* (errors
           (session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :response-error-function #'ignore)))
      (yunge-reader-transport--bind session 'fake-process)
      (dotimes (_ 2)
        (yunge-reader-transport--request
         session 'fake-process "pending" nil
         (lambda (_result error-data) (push error-data errors))))
      (yunge-reader-transport--fail session '(test-session-lost))
      (should (equal errors
                     '((test-session-lost) (test-session-lost))))
      (should-not (yunge-reader-transport--session-outbound session))
      (should
       (zerop
        (hash-table-count
         (yunge-reader-transport--session-callbacks session)))))))

(provide 'yunge-reader-transport-test)

;;; yunge-reader-transport-test.el ends here
