;;; yunge-reader-native-test.el --- Native tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-native)
(require 'yunge-reader-setup)

(defvar yunge-test-external-checks-running-separately)

(defmacro yunge-reader-native-test--with-fake-process (&rest body)
  "Run BODY with an isolated fake native process implementation."
  (declare (indent 0) (debug t))
  `(let ((yunge-reader-native--process nil)
         (yunge-reader-native--build-process nil)
         (yunge-reader-native--transport nil)
         (yunge-reader-native--session-counter 0)
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
               ((symbol-function 'process-status)
                (lambda (process)
                  (if (and live (eq process 'fake-reader-process))
                      'run
                    'exit)))
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

(defun yunge-reader-native-test--mark-ready ()
  "Mark the fake native helper transport ready."
  (yunge-reader-transport--mark-ready
   yunge-reader-native--transport 'fake-reader-process))

(defun yunge-reader-native-test--pending-count ()
  "Return the fake native transport's pending callback count."
  (hash-table-count
   (yunge-reader-transport--session-callbacks
    yunge-reader-native--transport)))

(ert-deftest yunge-reader-native-renderer-tests-pass ()
  (skip-when yunge-test-external-checks-running-separately)
  (let ((node (executable-find "node")))
    (skip-unless node)
    (with-temp-buffer
      (let ((syntax-status
             (call-process
              node nil t nil "--check"
              (expand-file-name
               "native/yunge-reader/renderer/yunge-reader.js"
               yunge-test-root))))
        (unless (zerop syntax-status)
          (ert-fail
           (format "Renderer syntax check exited with %S:\n%s"
                   syntax-status (buffer-string)))))
      (erase-buffer)
      (let ((test-status
             (call-process
              node nil t nil "--test"
              (expand-file-name
               (concat
                "native/yunge-reader/renderer-test/"
                "yunge-reader-core.test.mjs")
               yunge-test-root))))
        (unless (zerop test-status)
          (ert-fail
           (format "Renderer tests exited with %S:\n%s"
                   test-status (buffer-string))))))))

(ert-deftest yunge-reader-native-resolves-platform-pdfium-layouts ()
  (cl-letf (((symbol-function 'yunge-reader-native-pdfium-directory)
             (lambda () "/pdfium/7881")))
    (dolist (case '((windows-nt . "bin/pdfium.dll")
                    (darwin . "lib/libpdfium.dylib")
                    (gnu/linux . "lib/libpdfium.so")))
      (let ((system-type (car case)))
        (should
         (equal (yunge-reader-native-pdfium-library)
                (expand-file-name (cdr case) "/pdfium/7881")))))))

(ert-deftest yunge-reader-native-resolves-platform-module-layouts ()
  (cl-letf (((symbol-function
              'yunge-reader-native--cargo-target-directory)
             (lambda () "/reader-target")))
    (dolist (case '((windows-nt . "release/yunge_reader_module.dll")
                    (darwin . "release/libyunge_reader_module.dylib")
                    (gnu/linux . "release/libyunge_reader_module.so")))
      (let ((system-type (car case)))
        (should
         (equal (yunge-reader-native-module-file)
                (expand-file-name (cdr case) "/reader-target")))))))

(ert-deftest yunge-reader-native-publishes-artifact-build-identity ()
  (let* ((directory (make-temp-file "yunge-reader-build-id-" t))
         (build-id (make-string 64 ?a)))
    (unwind-protect
        (cl-letf (((symbol-function
                    'yunge-reader-native--cargo-target-directory)
                   (lambda () directory))
                  ((symbol-function 'yunge-reader-native--source-build-id)
                   (lambda () build-id))
                  ((symbol-function 'yunge-reader-native--embedded-build-id)
                   (lambda () build-id)))
          (should-not (yunge-reader-native--build-id))
          (should (equal (yunge-reader-native--publish-build-id) build-id))
          (should (equal (yunge-reader-native--build-id) build-id))
          (should
           (equal
            (directory-files
             (expand-file-name "release" directory) nil
             "\\`[^.]" t)
            '("yunge-reader.build-id"))))
      (delete-directory directory t))))

(ert-deftest yunge-reader-native-rejects-invalid-artifact-build-identity ()
  (let ((directory (make-temp-file "yunge-reader-build-id-" t)))
    (unwind-protect
        (cl-letf (((symbol-function
                    'yunge-reader-native--cargo-target-directory)
                   (lambda () directory)))
          (let ((file (yunge-reader-native-build-id-file)))
            (make-directory (file-name-directory file) t)
            (write-region "not-a-build-id\n" nil file nil 'silent)
            (should-not (yunge-reader-native--build-id))))
      (delete-directory directory t))))

(ert-deftest yunge-reader-native-rejects-stale-artifacts ()
  (cl-letf (((symbol-function 'yunge-reader-native--source-build-id)
             (lambda () (make-string 64 ?a)))
            ((symbol-function 'yunge-reader-native--artifact-build-id)
             (lambda () (make-string 64 ?b))))
    (should-not (yunge-reader-native--build-id))))

(ert-deftest yunge-reader-native-refuses-to-publish-a-stale-build ()
  (cl-letf (((symbol-function 'yunge-reader-native--source-build-id)
             (lambda () (make-string 64 ?a)))
            ((symbol-function 'yunge-reader-native--embedded-build-id)
             (lambda () (make-string 64 ?b))))
    (should-error (yunge-reader-native--publish-build-id) :type 'error)))

(ert-deftest yunge-reader-native-queues-until-validated-ready ()
  (yunge-reader-native-test--with-fake-process
    (let (result)
      (yunge-reader-native-request
       "ping" nil
       (lambda (value error-data)
         (should-not error-data)
         (setq result value)))
      (should-not sent)
      (should
       (= (length
           (yunge-reader-transport--session-outbound
            yunge-reader-native--transport))
          1))
      (yunge-reader-native--handle-message
       'fake-reader-process
       '((kind . "ready")
         (protocol . 2)
         (build-id . "test-build")
         (pdfium-api . "7881")
         (capabilities
           . ("cache-maintenance" "epub-publications" "epub-renderer"
              "epub-resources" "lifecycle" "pdf-links"
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
      (should (zerop (yunge-reader-native-test--pending-count))))))

(ert-deftest yunge-reader-native-infers-and-cancels-document-owners ()
  (yunge-reader-native-test--with-fake-process
    (let* (error-data
           (task
            (yunge-reader-native-request
             "render" '((document . 17) (page . 0))
             (lambda (_result error) (setq error-data error))
             :revision 3))
           (session (yunge-reader-native-current-session)))
      (should
       (equal (yunge-reader-task-owner task)
              (list 'pdf-document session 17)))
      (should (= (yunge-reader-task-revision task) 3))
      (should
       (= (yunge-reader-native-cancel-document-requests
           session 17 "document closed")
          1))
      (should (eq (yunge-reader-task-state task) 'cancelled))
      (should (eq (car error-data) 'yunge-reader-task-cancelled))
      (let ((close
             (yunge-reader-native-request
              "close" '((document . 17)) #'ignore)))
        (should-not (yunge-reader-task-owner close))))))

(ert-deftest yunge-reader-native-infers-publication-owners ()
  (yunge-reader-native-test--with-fake-process
    (let* ((task
            (yunge-reader-native-request
             "epub-info" '((publication . 23)) #'ignore))
           (session (yunge-reader-native-current-session)))
      (should
       (equal (yunge-reader-task-owner task)
              (list 'epub-publication session 23)))
      (let ((close
             (yunge-reader-native-request
              "epub-close" '((publication . 23)) #'ignore)))
        (should-not (yunge-reader-task-owner close))))))

(ert-deftest yunge-reader-native-rejects-an-outdated-helper ()
  (yunge-reader-native-test--with-fake-process
    (should-error
     (yunge-reader-native--validate-ready
      '((kind . "ready")
        (protocol . 2)
        (build-id . "old-build")
        (pdfium-api . "7881")
        (capabilities
          . ("cache-maintenance" "epub-publications" "epub-renderer"
             "epub-resources" "lifecycle" "pdf-links"
             "pdf-outline" "pdf-render" "pdf-search" "pdf-text"))))
     :type 'error)
    (should-error
     (yunge-reader-native--validate-ready
      '((kind . "ready")
        (protocol . 1)
        (build-id . "test-build")
        (pdfium-api . "7881")
        (capabilities
          . ("cache-maintenance" "epub-publications" "epub-renderer"
             "epub-resources" "lifecycle" "pdf-links"
             "pdf-outline" "pdf-render" "pdf-search" "pdf-text"))))
     :type 'error)
    (should-error
     (yunge-reader-native--validate-ready
      '((kind . "ready")
        (protocol . 2)
        (build-id . "test-build")
        (pdfium-api . "7881")
        (capabilities
          . ("cache-maintenance" "epub-publications" "epub-renderer"
             "epub-resources" "lifecycle" "pdf-outline"
             "pdf-render" "pdf-search" "pdf-text"))))
     :type 'error)))

(ert-deftest yunge-reader-native-preserves-pdf-password-errors ()
  (should
   (equal
    (yunge-reader-native--response-error
     '((error
        . ((code . "pdf-password-error")
           (message . "wrong password")))))
    '(yunge-reader-native-pdf-password-error)))
  (should
   (equal
    (yunge-reader-native--response-error
     '((error
        . ((code . "pdf-open-failed")
           (message . "broken PDF")))))
    '(error "broken PDF"))))

(ert-deftest yunge-reader-native-stop-requests-shutdown-then-arms-timeout ()
  (yunge-reader-native-test--with-fake-process
    (let (timer-arguments)
      (yunge-reader-native-start)
      (yunge-reader-native-test--mark-ready)
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

(ert-deftest yunge-reader-native-intentional-stop-fails-without-restart ()
  (yunge-reader-native-test--with-fake-process
    (let (request-error
          restart-function)
      (should (= (yunge-reader-native-acquire) 1))
      (yunge-reader-native-test--mark-ready)
      (yunge-reader-native-request
       "ping" nil
       (lambda (_result error-data)
         (setq request-error error-data)))
      (yunge-reader-native-stop t)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest _arguments)
                   (setq restart-function function)
                   'fake-timer)))
        (yunge-reader-native--sentinel
         'fake-reader-process "killed"))
      (should
       (eq (car request-error)
           'yunge-reader-native-session-stopped))
      (should-not restart-function))))

(ert-deftest yunge-reader-native-reference-count-schedules-idle-stop ()
  (yunge-reader-native-test--with-fake-process
    (let ((yunge-reader-native-idle-seconds 42)
          timer-arguments)
      (should (= (yunge-reader-native-acquire) 1))
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
      (yunge-reader-native-test--mark-ready)
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
    (yunge-reader-native-test--mark-ready)
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
       (protocol . 2)
       (build-id . "test-build")
       (pdfium-api . "7881")
       (capabilities
         . ("cache-maintenance" "epub-publications" "epub-renderer"
            "epub-resources" "lifecycle" "pdf-links"
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

(ert-deftest yunge-reader-native-rejects-requests-from-an-old-session ()
  (yunge-reader-native-test--with-fake-process
    (yunge-reader-native-start)
    (let ((old-session (yunge-reader-native-current-session))
          request-error)
      (should (= old-session 1))
      (yunge-reader-native-stop t)
      (yunge-reader-native--sentinel 'fake-reader-process "killed")
      (yunge-reader-native-start)
      (should (= (yunge-reader-native-current-session) 2))
      (yunge-reader-native-test--mark-ready)
      (yunge-reader-native-request-in-session
       old-session "page-info" '((document . 1) (page . 0))
       (lambda (_result error-data)
         (setq request-error error-data)))
      (should
       (eq (car request-error)
           'yunge-reader-native-session-lost))
      (should-not sent)
      (should (zerop (yunge-reader-native-test--pending-count))))))

(ert-deftest yunge-reader-native-crash-fails-callbacks-for-its-session ()
  (yunge-reader-native-test--with-fake-process
    (let ((calls 0)
          request-error
          restart-function)
      (should (= (yunge-reader-native-acquire) 1))
      (yunge-reader-native-request
       "ping" nil
       (lambda (_result error-data)
         (cl-incf calls)
         (setq request-error error-data)))
      (setq live nil)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest _arguments)
                   (setq restart-function function)
                   'fake-timer)))
        (yunge-reader-native--sentinel
         'fake-reader-process "exited"))
      (should
       (eq (car request-error)
           'yunge-reader-native-session-lost))
      (should (= calls 1))
      (should (zerop (yunge-reader-native-test--pending-count)))
      (should (= yunge-reader-native--restart-count 1))
      (should (eq restart-function
                  #'yunge-reader-native--start-after-crash))
      (yunge-reader-native--sentinel
       'fake-reader-process "exited again")
      (should (= calls 1))
      (funcall restart-function)
      (should (= (yunge-reader-native-current-session) 2)))))

(ert-deftest yunge-reader-native-status-distinguishes-starting-and-ready ()
  (yunge-reader-native-test--with-fake-process
    (should (eq (plist-get (yunge-reader-native-status) :state)
                'stopped))
    (yunge-reader-native-start)
    (should (eq (plist-get (yunge-reader-native-status) :state)
                'starting))
    (should (= (plist-get (yunge-reader-native-status) :session) 1))
    (yunge-reader-native-test--mark-ready)
    (should (eq (plist-get (yunge-reader-native-status) :state)
                'ready))))

;;; yunge-reader-native-test.el ends here
