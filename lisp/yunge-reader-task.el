;;; yunge-reader-task.el --- Owned asynchronous Reader tasks -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(define-error 'yunge-reader-task-cancelled
  "Yunge Reader task was cancelled")

(define-error 'yunge-reader-task-timed-out
  "Yunge Reader task timed out")

(cl-defstruct (yunge-reader-task
               (:constructor yunge-reader-task--make))
  "One owned asynchronous Reader operation.
ID and SESSION identify transport work when present.  OPERATION, OWNER, and
REVISION describe logical work independently of its execution backend.  STATE
is `queued', `sent', or `running' until the task reaches a terminal state.
CHILD is the currently executing task owned by a composite operation."
  id
  operation
  owner
  revision
  state
  created-at
  deadline
  timer
  complete
  session
  cancel-function
  child)

(defun yunge-reader-task-active-p (task)
  "Return non-nil when TASK has not reached a terminal state."
  (and (yunge-reader-task-p task)
       (memq (yunge-reader-task-state task)
             '(queued sent running))))

(defun yunge-reader-task-cancel (task &optional reason)
  "Cancel live Reader TASK and return non-nil when it was pending.
The completion callback runs before this function returns.  Cancellation
prevents a later child result from completing TASK again; stopping backend
work is best effort."
  (unless (yunge-reader-task-p task)
    (error "Invalid Yunge Reader task: %S" task))
  (if-let* ((cancel (yunge-reader-task-cancel-function task)))
      (funcall cancel task reason)
    (error "Yunge Reader task has no cancellation function: %S" task)))

(defun yunge-reader-task--invoke-complete
    (task complete value error-data)
  "Safely invoke TASK's COMPLETE with VALUE and ERROR-DATA."
  (condition-case callback-error
      (funcall complete value error-data)
    (error
     (display-warning
      'yunge-reader
      (format "Reader callback for %s failed: %s"
              (yunge-reader-task-operation task)
              (error-message-string callback-error))
      :warning))))

(defun yunge-reader-task-finish (task state value error-data)
  "Finish active Reader TASK once with STATE, VALUE, and ERROR-DATA.
STATE is one of `completed', `failed', `cancelled', or `timed-out'.
The completion callback runs synchronously after TASK becomes terminal, so
reentrant completion attempts have no effect."
  (when (yunge-reader-task-active-p task)
    (when-let* ((timer (yunge-reader-task-timer task)))
      (when (timerp timer)
        (cancel-timer timer)))
    (let ((complete (yunge-reader-task-complete task)))
      (setf (yunge-reader-task-state task) state
            (yunge-reader-task-timer task) nil
            (yunge-reader-task-complete task) nil
            (yunge-reader-task-child task) nil)
      (when complete
        (yunge-reader-task--invoke-complete
         task complete value error-data)))
    t))

(defun yunge-reader-task--abort-composite
    (task state reason error-data)
  "Abort composite TASK in STATE, cancelling its child for REASON."
  (when (yunge-reader-task-active-p task)
    (let ((complete (yunge-reader-task-complete task))
          (child (yunge-reader-task-child task)))
      (when-let* ((timer (yunge-reader-task-timer task)))
        (when (timerp timer)
          (cancel-timer timer)))
      ;; Child cancellation may synchronously invoke the parent completion.
      (setf (yunge-reader-task-state task) state
            (yunge-reader-task-timer task) nil
            (yunge-reader-task-complete task) nil
            (yunge-reader-task-child task) nil)
      (when (yunge-reader-task-active-p child)
        (condition-case child-error
            (yunge-reader-task-cancel child reason)
          (error
           (display-warning
            'yunge-reader
            (format "Reader child cancellation for %s failed: %s"
                    (yunge-reader-task-operation task)
                    (error-message-string child-error))
            :warning))))
      (when complete
        (yunge-reader-task--invoke-complete
         task complete nil error-data))
      t)))

(defun yunge-reader-task--cancel-composite (task reason)
  "Cancel composite Reader TASK for REASON."
  (yunge-reader-task--abort-composite
   task 'cancelled reason
   (list 'yunge-reader-task-cancelled
         (or reason "The Reader operation was cancelled"))))

(defun yunge-reader-task--timeout-composite (task)
  "Expire composite Reader TASK at its deadline."
  (yunge-reader-task--abort-composite
   task 'timed-out "The parent operation timed out"
   (list 'yunge-reader-task-timed-out
         (format "Reader %s operation timed out"
                 (yunge-reader-task-operation task)))))

(cl-defun yunge-reader-task-create
    (operation complete
     &key owner timeout revision id session
     (state 'running)
     (cancel-function #'yunge-reader-task--cancel-composite)
     (timeout-function #'yunge-reader-task--timeout-composite))
  "Create an active task for OPERATION and COMPLETE.
OWNER groups related work.  TIMEOUT is an optional positive number of seconds.
REVISION is opaque state identifying the user intent served by the task.  ID,
SESSION, STATE, CANCEL-FUNCTION, and TIMEOUT-FUNCTION let an execution backend
describe its pending work while this library retains task construction and
terminal completion.  Custom cancellation functions must make TASK terminal
and invoke COMPLETE synchronously if it is still active."
  (unless (functionp complete)
    (error "Reader completion must be a function: %S" complete))
  (unless (memq state '(queued sent running))
    (error "Reader task initial state must be active: %S" state))
  (unless (functionp cancel-function)
    (error "Reader task cancellation must be a function: %S"
           cancel-function))
  (unless (functionp timeout-function)
    (error "Reader task timeout must be a function: %S"
           timeout-function))
  (when (and timeout
             (not (and (numberp timeout) (> timeout 0))))
    (error "Reader task timeout must be positive: %S" timeout))
  (let* ((created-at (float-time))
         (task
          (yunge-reader-task--make
           :id id
           :operation operation
           :owner owner
           :revision revision
           :state state
           :created-at created-at
           :deadline (and timeout (+ created-at timeout))
           :complete complete
           :session session
           :cancel-function cancel-function)))
    (when timeout
      (setf (yunge-reader-task-timer task)
            (run-at-time timeout nil timeout-function task)))
    task))

(defun yunge-reader-task-adopt-child (task child)
  "Make composite TASK own cancellable CHILD and return CHILD.
An active CHILD adopted after TASK finished is cancelled immediately."
  (when (and (yunge-reader-task-p task)
             (yunge-reader-task-p child)
             (not (eq task child)))
    (if (yunge-reader-task-active-p task)
        (setf (yunge-reader-task-child task) child)
      (when (yunge-reader-task-active-p child)
        (yunge-reader-task-cancel
         child "The parent Reader operation already finished"))))
  child)

(provide 'yunge-reader-task)
;;; yunge-reader-task.el ends here
