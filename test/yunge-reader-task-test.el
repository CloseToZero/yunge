;;; yunge-reader-task-test.el --- Reader task completion -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-task)

(ert-deftest yunge-reader-task-cancellation-completes-once-with-reentrant-child ()
  (let (parent completions cancelled)
    (setq parent
          (yunge-reader-task-create
           'search
           (lambda (_value error-data)
             (push error-data completions))))
    (yunge-reader-task-adopt-child
     parent
     (yunge-reader-task-create
      'request #'ignore
      :cancel-function
      (lambda (task reason)
        (push reason cancelled)
        (yunge-reader-task-finish parent 'completed 'late nil)
        (yunge-reader-task-finish task 'cancelled nil nil))))
    (should (yunge-reader-task-cancel parent "search replaced"))
    (should-not (yunge-reader-task-cancel parent))
    (should (equal cancelled '("search replaced")))
    (should (equal completions
                   '((yunge-reader-task-cancelled "search replaced"))))
    (yunge-reader-task-adopt-child
     parent
     (yunge-reader-task-create
      'late #'ignore
      :cancel-function
      (lambda (task _reason)
        (push 'late cancelled)
        (yunge-reader-task-finish task 'cancelled nil nil))))
    (should (equal (car cancelled) 'late))
    (should (= (length completions) 1))))

(ert-deftest yunge-reader-task-child-cancel-error-does-not-lose-completion ()
  (let (completion warning)
    (let ((parent
           (yunge-reader-task-create
            'search
            (lambda (_value error-data)
              (setq completion error-data)))))
      (yunge-reader-task-adopt-child
       parent
       (yunge-reader-task-create
        'request #'ignore
        :cancel-function (lambda (_task _reason)
                           (error "backend cancellation failed"))))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest arguments) (setq warning arguments))))
        (should (yunge-reader-task-cancel parent "view closed")))
      (should (equal completion
                     '(yunge-reader-task-cancelled "view closed")))
      (should (string-match-p "backend cancellation failed"
                              (cadr warning))))))

(ert-deftest yunge-reader-task-timeout-cancels-child-and-completes-once ()
  (let (timeout-callback timeout-task completions child-reason)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function task)
                 (setq timeout-callback function
                       timeout-task task)
                 nil)))
      (let ((parent
             (yunge-reader-task-create
              'copy
              (lambda (_value error-data)
                (push error-data completions))
              :timeout 10)))
        (yunge-reader-task-adopt-child
         parent
         (yunge-reader-task-create
          'request #'ignore
          :cancel-function
          (lambda (task reason)
            (setq child-reason reason)
            (yunge-reader-task-finish task 'cancelled nil nil))))
        (funcall timeout-callback timeout-task)
        (funcall timeout-callback timeout-task)
        (should (eq (caar completions) 'yunge-reader-task-timed-out))
        (should (= (length completions) 1))
        (should child-reason)
        (should-not (yunge-reader-task-cancel parent))))))

(provide 'yunge-reader-task-test)
;;; yunge-reader-task-test.el ends here
