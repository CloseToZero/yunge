;;; yunge-reader-native-test.el --- Native tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-native)
(require 'yunge-reader-setup)

(defmacro yunge-reader-native-test--with-fake-process (&rest body)
  "Run BODY with an isolated fake native process implementation."
  (declare (indent 0) (debug t))
  `(let ((yunge-reader-native--process nil)
         (yunge-reader-native--build-process nil)
         (yunge-reader-native--callbacks (make-hash-table :test #'eql))
         (yunge-reader-native--outbound-queue nil)
         (yunge-reader-native--next-id 0)
         (yunge-reader-native--client-count 0)
         (yunge-reader-native--idle-timer nil)
         (yunge-reader-native--cache-pruning nil)
         (yunge-reader-native--cache-prune-stop-after nil)
         (yunge-reader-native--force-stop-timer nil)
         (yunge-reader-native--restart-after-stop nil)
         (yunge-reader-native--build-after-stop nil)
         (yunge-reader-native--restart-count 0)
         (properties (make-hash-table :test #'equal))
         (live nil)
         sent)
     (cl-letf (((symbol-function 'yunge-reader-native--available-p)
                (lambda () t))
               ((symbol-function 'yunge-reader-native--build-id)
                (lambda () "test-build"))
               ((symbol-function 'make-process)
                (lambda (&rest _arguments)
                  (setq live t)
                  'fake-reader-process))
               ((symbol-function 'process-live-p)
                (lambda (process)
                  (and live (eq process 'fake-reader-process))))
               ((symbol-function 'process-put)
                (lambda (process property value)
                  (puthash (cons process property) value properties)))
               ((symbol-function 'process-get)
                (lambda (process property)
                  (gethash (cons process property) properties)))
               ((symbol-function 'process-send-string)
                (lambda (_process string) (push string sent)))
               ((symbol-function 'delete-process)
                (lambda (_process) (setq live nil))))
       ,@body)))

(ert-deftest yunge-reader-native-queues-until-validated-ready ()
  (yunge-reader-native-test--with-fake-process
    (let (result)
      (yunge-reader-native-request
       "ping" nil
       (lambda (value error-data)
         (should-not error-data)
         (setq result value)))
      (should-not sent)
      (should (= (length yunge-reader-native--outbound-queue) 1))
      (yunge-reader-native--handle-message
       'fake-reader-process
       '((kind . "ready")
         (protocol . 1)
         (build-id . "test-build")
         (pdfium-api . "7881")
         (capabilities
           . ("cache-maintenance" "lifecycle" "pdf-links"
              "pdf-outline" "pdf-render" "pdf-search" "pdf-text"))))
      (should (= (length sent) 1))
      (let ((request
             (json-parse-string
              (car sent) :object-type 'alist)))
        (should (= (alist-get 'id request) 1))
        (should (equal (alist-get 'op request) "ping")))
      (yunge-reader-native--handle-message
       'fake-reader-process
       '((id . 1) (ok . t) (result . ((backend . "none")))))
      (should (equal (alist-get 'backend result) "none"))
      (should (zerop (hash-table-count
                      yunge-reader-native--callbacks))))))

(ert-deftest yunge-reader-native-rejects-an-outdated-helper ()
  (yunge-reader-native-test--with-fake-process
    (should-error
     (yunge-reader-native--validate-ready
      '((kind . "ready")
        (protocol . 1)
        (build-id . "old-build")
        (pdfium-api . "7881")
        (capabilities
          . ("cache-maintenance" "lifecycle" "pdf-links"
             "pdf-outline" "pdf-render" "pdf-search" "pdf-text"))))
     :type 'error)
    (should-error
     (yunge-reader-native--validate-ready
      '((kind . "ready")
        (protocol . 2)
        (build-id . "test-build")
        (pdfium-api . "7881")
        (capabilities
          . ("cache-maintenance" "lifecycle" "pdf-links"
             "pdf-outline" "pdf-render" "pdf-search" "pdf-text"))))
     :type 'error)
    (should-error
     (yunge-reader-native--validate-ready
      '((kind . "ready")
        (protocol . 1)
        (build-id . "test-build")
        (pdfium-api . "7881")
        (capabilities
          . ("cache-maintenance" "lifecycle" "pdf-outline"
             "pdf-render" "pdf-search" "pdf-text"))))
     :type 'error)))

(ert-deftest yunge-reader-native-stop-requests-shutdown-then-arms-timeout ()
  (yunge-reader-native-test--with-fake-process
    (let (timer-arguments)
      (yunge-reader-native-start)
      (process-put 'fake-reader-process 'yunge-reader-ready t)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest arguments)
                   (setq timer-arguments arguments)
                   'fake-timer)))
        (yunge-reader-native-stop))
      (should (process-get 'fake-reader-process
                           'yunge-reader-intentional-stop))
      (should live)
      (should (= (length sent) 1))
      (let ((request
             (json-parse-string (car sent) :object-type 'alist)))
        (should (equal (alist-get 'op request) "shutdown")))
      (should (= (car timer-arguments)
                 yunge-reader-native-stop-timeout)))))

(ert-deftest yunge-reader-native-force-stop-terminates-immediately ()
  (yunge-reader-native-test--with-fake-process
    (yunge-reader-native-start)
    (yunge-reader-native-stop t)
    (should-not live)
    (should-not sent)))

(ert-deftest yunge-reader-native-reference-count-schedules-idle-stop ()
  (yunge-reader-native-test--with-fake-process
    (let ((yunge-reader-native-idle-seconds 42)
          timer-arguments)
      (yunge-reader-native-acquire)
      (should (= yunge-reader-native--client-count 1))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest arguments)
                   (setq timer-arguments arguments)
                   'fake-timer)))
        (should (zerop (yunge-reader-native-release))))
      (should (= (car timer-arguments) 42))
      (should (eq (nth 2 timer-arguments)
                  #'yunge-reader-native--idle-stop)))))

(ert-deftest yunge-reader-native-idle-stop-prunes-before-shutdown ()
  (yunge-reader-native-test--with-fake-process
    (let ((yunge-reader-cache-max-bytes 100)
          (yunge-reader-cache-target-bytes 60))
      (yunge-reader-native-start)
      (process-put 'fake-reader-process 'yunge-reader-ready t)
      (yunge-reader-native--idle-stop)
      (should yunge-reader-native--cache-pruning)
      (should (= (length sent) 1))
      (let* ((request
              (json-parse-string (car sent) :object-type 'alist))
             (parameters (alist-get 'params request)))
        (should (equal (alist-get 'op request) "cache-prune"))
        (should (= (alist-get 'max-bytes parameters) 100))
        (should (= (alist-get 'target-bytes parameters) 60)))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _arguments) 'fake-timer)))
        (yunge-reader-native--handle-message
         'fake-reader-process
         '((id . 1)
           (ok . t)
           (result
            . ((scanned . 3)
               (before-bytes . 120)
               (after-bytes . 60)
               (removed-files . 2)
               (removed-bytes . 60)
               (failed-files . 0)
               (over-budget . nil))))))
      (should-not yunge-reader-native--cache-pruning)
      (should (= (length sent) 2))
      (let ((request
             (json-parse-string (car sent) :object-type 'alist)))
        (should (equal (alist-get 'op request) "shutdown"))))))

(ert-deftest yunge-reader-native-acquire-during-prune-keeps-helper ()
  (yunge-reader-native-test--with-fake-process
    (yunge-reader-native-start)
    (process-put 'fake-reader-process 'yunge-reader-ready t)
    (yunge-reader-native--idle-stop)
    (yunge-reader-native-acquire)
    (yunge-reader-native--handle-message
     'fake-reader-process
     '((id . 1)
       (ok . t)
       (result
        . ((after-bytes . 0)
           (removed-files . 0)
           (removed-bytes . 0)
           (failed-files . 0)
           (over-budget . nil)))))
    (should-not yunge-reader-native--cache-pruning)
    (should (= yunge-reader-native--client-count 1))
    (should (= (length sent) 1))
    (should live)))

(ert-deftest yunge-reader-cache-prune-refuses-active-readers ()
  (yunge-reader-native-test--with-fake-process
    (setq yunge-reader-native--client-count 1)
    (should-error (yunge-reader-cache-prune) :type 'user-error)
    (should-not sent)))

(ert-deftest yunge-reader-cache-prune-validates-limits ()
  (yunge-reader-native-test--with-fake-process
    (let ((yunge-reader-cache-max-bytes 10)
          (yunge-reader-cache-target-bytes 11))
      (should-error (yunge-reader-cache-prune) :type 'user-error)
      (should-not sent))))

(ert-deftest yunge-reader-cache-prune-stops-a-temporary-helper ()
  (yunge-reader-native-test--with-fake-process
    (yunge-reader-cache-prune)
    (should yunge-reader-native--cache-pruning)
    (should-not sent)
    (yunge-reader-native--handle-message
     'fake-reader-process
     '((kind . "ready")
       (protocol . 1)
       (build-id . "test-build")
       (pdfium-api . "7881")
       (capabilities
         . ("cache-maintenance" "lifecycle" "pdf-links"
            "pdf-outline" "pdf-render" "pdf-search" "pdf-text"))))
    (should (= (length sent) 1))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _arguments) 'fake-timer)))
      (yunge-reader-native--handle-message
       'fake-reader-process
       '((id . 1)
         (ok . t)
         (result
          . ((after-bytes . 0)
             (removed-files . 0)
             (removed-bytes . 0)
             (failed-files . 0)
             (over-budget . nil))))))
    (should (= (length sent) 2))
    (let ((request
           (json-parse-string (car sent) :object-type 'alist)))
      (should (equal (alist-get 'op request) "shutdown")))))

(ert-deftest yunge-reader-native-skips-a-stale-crash-restart ()
  (yunge-reader-native-test--with-fake-process
    (let (started)
      (cl-letf (((symbol-function 'yunge-reader-native-start)
                 (lambda () (setq started t))))
        (yunge-reader-native--start-after-crash))
      (should-not started))))

(ert-deftest yunge-reader-native-status-distinguishes-starting-and-ready ()
  (yunge-reader-native-test--with-fake-process
    (should (eq (plist-get (yunge-reader-native-status) :state)
                'stopped))
    (yunge-reader-native-start)
    (should (eq (plist-get (yunge-reader-native-status) :state)
                'starting))
    (process-put 'fake-reader-process 'yunge-reader-ready t)
    (should (eq (plist-get (yunge-reader-native-status) :state)
                'ready))))

(ert-deftest yunge-reader-native-setup-delegates-to-pdfium-setup ()
  (yunge-reader-native-test--with-fake-process
    (let (setup)
      (cl-letf (((symbol-function 'yunge-reader-setup)
                 (lambda () (setq setup t))))
        (yunge-reader-native-setup))
      (should setup))))

;;; yunge-reader-native-test.el ends here
