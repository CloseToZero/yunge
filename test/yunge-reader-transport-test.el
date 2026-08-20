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
       (= (yunge-reader-task-id
           (yunge-reader-transport--request
            session 'fake-process "first" nil
            (lambda (result error-data)
              (setq first (list result error-data)))))
          1))
      (should
       (= (yunge-reader-task-id
           (yunge-reader-transport--request
            session 'fake-process "second" '((value . 2))
            (lambda (result error-data)
              (setq second (list result error-data)))
            :revision 4))
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
        (should (= (alist-get 'revision (cadr requests)) 4))
        (should
         (= (alist-get 'value
                       (alist-get 'params (cadr requests)))
            2)))
      (yunge-reader-transport--handle-message
       session 'fake-process
       '((id . 1) (ok . t) (result . ((value . 1)))))
      (yunge-reader-transport--handle-message
       session 'fake-process
       '((id . 2) (revision . 4)
         (ok) (error . ((message . "failed")))))
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

(ert-deftest yunge-reader-transport-cancels-queued-and-sent-tasks ()
  (yunge-reader-transport-test--with-process
    (let* (queued-error sent-error
           (session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :response-error-function #'ignore)))
      (yunge-reader-transport--bind session 'fake-process)
      (let ((queued
             (yunge-reader-transport--request
              session 'fake-process "queued" nil
              (lambda (_value error-data) (setq queued-error error-data))
              :owner 'document :revision 4)))
        (should (eq (yunge-reader-task-state queued) 'queued))
        (should (= (yunge-reader-task-revision queued) 4))
        (should (yunge-reader-task-cancel queued "closed"))
        (should (eq (yunge-reader-task-state queued) 'cancelled))
        (should (eq (car queued-error) 'yunge-reader-task-cancelled))
        (should-not (yunge-reader-task-cancel queued)))
      (yunge-reader-transport--handle-message
       session 'fake-process '((kind . "ready")))
      (should-not sent)
      (let ((task
             (yunge-reader-transport--request
              session 'fake-process "sent" nil
              (lambda (_value error-data) (setq sent-error error-data))
              :owner 'document)))
        (should (eq (yunge-reader-task-state task) 'sent))
        (should (yunge-reader-task-cancel task))
        (should (eq (yunge-reader-task-state task) 'cancelled))
        (should (eq (car sent-error) 'yunge-reader-task-cancelled))
        ;; A cancelled in-flight response is a valid late response, not
        ;; protocol corruption.
        (yunge-reader-transport--handle-message
         session 'fake-process
         `((id . ,(yunge-reader-task-id task)) (ok . t) (result))))
      (should
       (zerop
        (hash-table-count
        (yunge-reader-transport--session-retired session)))))))

(ert-deftest yunge-reader-transport-rejects-mismatched-active-revisions ()
  (yunge-reader-transport-test--with-process
    (let* ((session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :response-error-function #'ignore))
           completed)
      (yunge-reader-transport--bind session 'fake-process)
      (yunge-reader-transport--handle-message
       session 'fake-process '((kind . "ready")))
      (let ((task
             (yunge-reader-transport--request
              session 'fake-process "read" nil
              (lambda (&rest _arguments) (setq completed t))
              :revision 7)))
        (should-error
         (yunge-reader-transport--handle-message
          session 'fake-process
          `((id . ,(yunge-reader-task-id task))
            (revision . 6) (ok . t) (result)))
         :type 'error)
        (should-not completed)
        (should (eq (yunge-reader-task-state task) 'sent))
        (should
         (eq task
             (gethash
              (yunge-reader-task-id task)
              (yunge-reader-transport--session-callbacks session))))))))

(ert-deftest yunge-reader-transport-rejects-mismatched-retired-revisions ()
  (yunge-reader-transport-test--with-process
    (let* ((session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :response-error-function #'ignore)))
      (yunge-reader-transport--bind session 'fake-process)
      (yunge-reader-transport--handle-message
       session 'fake-process '((kind . "ready")))
      (let ((task
             (yunge-reader-transport--request
              session 'fake-process "read" nil #'ignore :revision 7)))
        (should (yunge-reader-task-cancel task))
        (should-error
         (yunge-reader-transport--handle-message
          session 'fake-process
          `((id . ,(yunge-reader-task-id task))
            (revision . 6) (ok . t) (result)))
         :type 'error)
        (should
         (gethash
          (yunge-reader-task-id task)
          (yunge-reader-transport--session-retired session)))
        (yunge-reader-transport--handle-message
         session 'fake-process
         `((id . ,(yunge-reader-task-id task))
           (revision . 7) (ok . t) (result)))
        (should-not
         (gethash
          (yunge-reader-task-id task)
          (yunge-reader-transport--session-retired session)))))))

(ert-deftest yunge-reader-transport-cancels-tasks-by-owner ()
  (yunge-reader-transport-test--with-process
    (let* ((session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :response-error-function #'ignore))
           cancelled
           retained)
      (yunge-reader-transport--bind session 'fake-process)
      (dotimes (_ 2)
        (yunge-reader-transport--request
         session 'fake-process "owned" nil
         (lambda (_value error-data) (push error-data cancelled))
         :owner 'document))
      (setq retained
            (yunge-reader-transport--request
             session 'fake-process "other" nil #'ignore :owner 'other))
      (should
       (= (yunge-reader-transport-cancel-owner
           session 'document "document closed")
          2))
      (should (= (length cancelled) 2))
      (should (eq (yunge-reader-task-state retained) 'queued))
      (should
       (= (hash-table-count
           (yunge-reader-transport--session-callbacks session))
          1)))))

(ert-deftest yunge-reader-transport-times-out-owned-tasks ()
  (yunge-reader-transport-test--with-process
    (let* ((session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :response-error-function #'ignore))
           error-data)
      (yunge-reader-transport--bind session 'fake-process)
      (let ((task
             (yunge-reader-transport--request
              session 'fake-process "slow" nil
              (lambda (_value error) (setq error-data error))
              :timeout 60)))
        (should (numberp (yunge-reader-task-deadline task)))
        (yunge-reader-transport--timeout-task task)
        (should (eq (yunge-reader-task-state task) 'timed-out))
        (should (eq (car error-data) 'yunge-reader-task-timed-out))))))

(ert-deftest yunge-reader-transport-isolates-callback-errors ()
  (yunge-reader-transport-test--with-process
    (let* ((session
            (yunge-reader-transport--make-session
             :label "test helper"
             :validate-ready #'ignore
             :response-error-function #'ignore))
           warning)
      (yunge-reader-transport--bind session 'fake-process)
      (yunge-reader-transport--handle-message
       session 'fake-process '((kind . "ready")))
      (let ((task
             (yunge-reader-transport--request
              session 'fake-process "callback" nil
              (lambda (&rest _arguments) (error "callback broke")))))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest value) (setq warning value))))
          (yunge-reader-transport--handle-message
           session 'fake-process
           `((id . ,(yunge-reader-task-id task)) (ok . t) (result))))
        (should (eq (yunge-reader-task-state task) 'completed))
        (should (string-match-p "callback broke" (cadr warning)))))))

(provide 'yunge-reader-transport-test)

;;; yunge-reader-transport-test.el ends here
