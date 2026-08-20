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
  "Cancel live Reader TASK and return non-nil when it was pending."
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
  "Finish composite Reader TASK once with STATE, VALUE, and ERROR-DATA."
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

(defun yunge-reader-task--cancel-composite (task reason)
  "Cancel composite Reader TASK for REASON."
  (when (yunge-reader-task-active-p task)
    (let ((complete (yunge-reader-task-complete task))
          (child (yunge-reader-task-child task))
          (error-data
           (list 'yunge-reader-task-cancelled
                 (or reason "The Reader operation was cancelled"))))
      (when-let* ((timer (yunge-reader-task-timer task)))
        (when (timerp timer)
          (cancel-timer timer)))
      ;; A child cancellation can complete synchronously through its backend.
      ;; Make the parent terminal first so that callback cannot finish it twice.
      (setf (yunge-reader-task-state task) 'cancelled
            (yunge-reader-task-timer task) nil
            (yunge-reader-task-complete task) nil
            (yunge-reader-task-child task) nil)
      (when (yunge-reader-task-active-p child)
        (yunge-reader-task-cancel child reason))
      (when complete
        (yunge-reader-task--invoke-complete
         task complete nil error-data))
      t)))

(defun yunge-reader-task--timeout-composite (task)
  "Expire composite Reader TASK at its deadline."
  (when (yunge-reader-task-active-p task)
    (let ((complete (yunge-reader-task-complete task))
          (child (yunge-reader-task-child task))
          (error-data
           (list 'yunge-reader-task-timed-out
                 (format "Reader %s operation timed out"
                         (yunge-reader-task-operation task)))))
      (setf (yunge-reader-task-state task) 'timed-out
            (yunge-reader-task-timer task) nil
            (yunge-reader-task-complete task) nil
            (yunge-reader-task-child task) nil)
      (when (yunge-reader-task-active-p child)
        (yunge-reader-task-cancel child "The parent operation timed out"))
      (when complete
        (yunge-reader-task--invoke-complete
         task complete nil error-data))
      t)))

(cl-defun yunge-reader-task-create
    (operation complete &key owner timeout revision)
  "Create a running composite task for OPERATION and COMPLETE.
OWNER groups related work.  TIMEOUT is an optional positive number of seconds.
REVISION is opaque state identifying the user intent served by the task."
  (unless (functionp complete)
    (error "Reader completion must be a function: %S" complete))
  (when (and timeout
             (not (and (numberp timeout) (> timeout 0))))
    (error "Reader task timeout must be positive: %S" timeout))
  (let* ((created-at (float-time))
         (task
          (yunge-reader-task--make
           :operation operation
           :owner owner
           :revision revision
           :state 'running
           :created-at created-at
           :deadline (and timeout (+ created-at timeout))
           :complete complete
           :cancel-function #'yunge-reader-task--cancel-composite)))
    (when timeout
      (setf (yunge-reader-task-timer task)
            (run-at-time timeout nil
                         #'yunge-reader-task--timeout-composite task)))
    task))

(defun yunge-reader-task-adopt-child (task child)
  "Make composite TASK own cancellable CHILD and return CHILD."
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
