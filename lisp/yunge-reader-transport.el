;;; yunge-reader-transport.el --- NDJSON transport -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst yunge-reader-transport--session-property
  'yunge-reader-transport-session
  "Process property containing its bound Reader transport session.")

(defconst yunge-reader-transport--output-property
  'yunge-reader-transport-output
  "Process property containing an incomplete NDJSON fragment.")

(defconst yunge-reader-transport--ready-property
  'yunge-reader-transport-ready
  "Process property recording a completed transport handshake.")

(defconst yunge-reader-transport--send-function-property
  'yunge-reader-transport-send-function
  "Process property containing an alternate line sender.")

(cl-defstruct (yunge-reader-transport--session
               (:constructor yunge-reader-transport--make-session))
  "Request state and protocol adapters for one native service."
  label
  validate-ready
  ready-function
  event-function
  response-error-function
  invalid-output-function
  (callbacks (make-hash-table :test #'eql))
  outbound
  (next-id 0))

(defun yunge-reader-transport--bind
    (session process &optional send-function)
  "Bind fresh transport SESSION to PROCESS.
SEND-FUNCTION, when non-nil, receives PROCESS and one complete line instead
of writing that line to the process input pipe."
  (process-put process yunge-reader-transport--session-property session)
  (process-put process yunge-reader-transport--output-property "")
  (process-put process yunge-reader-transport--ready-property nil)
  (process-put process yunge-reader-transport--send-function-property
               send-function)
  process)

(defun yunge-reader-transport--bound-p (session process)
  "Return whether transport SESSION is bound to PROCESS."
  (eq (process-get process yunge-reader-transport--session-property)
      session))

(defun yunge-reader-transport--ready-p (session process)
  "Return whether PROCESS completed the handshake for SESSION."
  (and (yunge-reader-transport--bound-p session process)
       (process-get process yunge-reader-transport--ready-property)))

(defun yunge-reader-transport--mark-ready (session process)
  "Mark bound PROCESS ready for transport SESSION."
  (unless (yunge-reader-transport--bound-p session process)
    (error "Reader transport session is not bound to process: %S"
           process))
  (process-put process yunge-reader-transport--ready-property t))

(defun yunge-reader-transport--send-line (process line)
  "Send one protocol LINE through transport PROCESS."
  (if-let* ((function
             (process-get
              process yunge-reader-transport--send-function-property)))
      (funcall function process line)
    (process-send-string process (concat line "\n"))))

(defun yunge-reader-transport--flush (session process)
  "Send SESSION's queued requests to ready PROCESS in FIFO order."
  (dolist
      (entry
       (nreverse (yunge-reader-transport--session-outbound session)))
    (yunge-reader-transport--send-line process (cdr entry)))
  (setf (yunge-reader-transport--session-outbound session) nil))

(defun yunge-reader-transport--handle-ready
    (session process message)
  "Validate and record SESSION's ready MESSAGE from PROCESS."
  (funcall
   (yunge-reader-transport--session-validate-ready session)
   message)
  (yunge-reader-transport--mark-ready session process)
  (when-let* ((function
               (yunge-reader-transport--session-ready-function
                session)))
    (funcall function process message))
  (yunge-reader-transport--flush session process))

(defun yunge-reader-transport--handle-response (session message)
  "Route one response MESSAGE through transport SESSION."
  (let* ((id (alist-get 'id message))
         (callbacks
          (yunge-reader-transport--session-callbacks session))
         (callback (and (integerp id) (gethash id callbacks))))
    (unless callback
      (error "Unexpected %s response: %S"
             (yunge-reader-transport--session-label session)
             message))
    (remhash id callbacks)
    (if (alist-get 'ok message)
        (funcall callback (alist-get 'result message) nil)
      (funcall
       callback nil
       (funcall
        (yunge-reader-transport--session-response-error-function
         session)
        message)))))

(defun yunge-reader-transport--handle-message
    (session process message)
  "Handle one parsed MESSAGE from PROCESS for transport SESSION."
  (unless (yunge-reader-transport--bound-p session process)
    (error "Reader transport received output from an unbound process"))
  (cond
   ((not (yunge-reader-transport--ready-p session process))
    (yunge-reader-transport--handle-ready session process message))
   ((and (equal (alist-get 'kind message) "event")
         (yunge-reader-transport--session-event-function session))
    (funcall
     (yunge-reader-transport--session-event-function session)
     process message))
   (t
    (yunge-reader-transport--handle-response session message))))

(defun yunge-reader-transport--request
    (session process operation parameters complete)
  "Send one request through SESSION to PROCESS.
OPERATION is a string.  PARAMETERS is an alist or nil.  COMPLETE receives a
result and nil, or nil and an Emacs error value."
  (unless (stringp operation)
    (error "Reader transport operation must be a string: %S" operation))
  (unless (functionp complete)
    (error "Reader transport completion must be a function: %S" complete))
  (unless (yunge-reader-transport--bound-p session process)
    (error "Reader transport session is not bound to process: %S"
           process))
  (let* ((id
          (cl-incf (yunge-reader-transport--session-next-id session)))
         (request
          (append
           (list (cons 'id id) (cons 'op operation))
           (when parameters
             (list (cons 'params parameters)))))
         (line
          (json-serialize request :null-object nil :false-object :false)))
    (puthash
     id complete
     (yunge-reader-transport--session-callbacks session))
    (if (yunge-reader-transport--ready-p session process)
        (yunge-reader-transport--send-line process line)
      (push
       (cons id line)
       (yunge-reader-transport--session-outbound session)))
    id))

(defun yunge-reader-transport--fail (session error-data)
  "Complete SESSION's pending callbacks with ERROR-DATA."
  (let (callbacks)
    (maphash
     (lambda (_id callback)
       (push callback callbacks))
     (yunge-reader-transport--session-callbacks session))
    (clrhash (yunge-reader-transport--session-callbacks session))
    (setf (yunge-reader-transport--session-outbound session) nil)
    (dolist (callback callbacks)
      (funcall callback nil error-data))))

(defun yunge-reader-transport--filter (process output)
  "Collect and handle complete NDJSON messages in PROCESS OUTPUT."
  (let* ((session
          (process-get process yunge-reader-transport--session-property))
         (pending
          (concat
           (or
            (process-get process yunge-reader-transport--output-property)
            "")
           output))
         newline)
    (unless session
      (error "Reader transport process has no bound session"))
    (while (setq newline (string-match "\n" pending))
      (let ((line
             (string-trim-right (substring pending 0 newline) "\r")))
        (setq pending (substring pending (1+ newline)))
        (unless (string-empty-p line)
          (condition-case error-data
              (yunge-reader-transport--handle-message
               session process
               (json-parse-string
                line
                :object-type 'alist
                :array-type 'list
                :null-object nil
                :false-object nil))
            (error
             (display-warning
              'yunge-reader
              (format
               "Invalid %s output: %s"
               (yunge-reader-transport--session-label session)
               (error-message-string error-data))
              :warning)
             (when-let*
                 ((function
                   (yunge-reader-transport--session-invalid-output-function
                    session)))
               (funcall function process error-data)))))))
    (process-put process yunge-reader-transport--output-property pending)))

(provide 'yunge-reader-transport)

;;; yunge-reader-transport.el ends here
