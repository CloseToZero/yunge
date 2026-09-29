;;; fangcun-native-test.el --- Fangcun native helper tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'fangcun)
(require 'fangcun-test-helper)

(ert-deftest fangcun-native-scan-decodes-file-states ()
  (fangcun-test-with-notes
    (let* ((yiyus (fangcun--configured-yiyus))
           (native-file
            (expand-file-name "native.org" personal-root))
           command)
      (cl-letf (((symbol-function 'process-file)
                 (lambda (program _input _destination _display
                                  &rest arguments)
                   (setq command (cons program arguments))
                   (insert
                    (json-serialize
                     '((kind . "ready")
                       (build-id . "test-build")))
                    "\n"
                    (json-serialize
                     `((kind . "state")
                       (yiyu . "personal")
                       (file . ,native-file)
                       (mtime . 42.5)
                       (size . 17)))
                    "\n")
                   0))
                ((symbol-function
                  'fangcun--native-helper-build-id)
                 (lambda () "test-build")))
        (let ((states
               (fangcun--native-scan-file-states yiyus)))
          (should (= (length states) 1))
          (let ((state (car states)))
            (should
             (equal
              (fangcun-yiyu-id (fangcun-file-state-yiyu state))
              "personal"))
            (should
             (equal (fangcun-file-state-relative-file state)
                    "native.org"))
            (should (= (fangcun-file-state-mtime state) 42.5))
            (should (= (fangcun-file-state-size state) 17))))
        (should
         (equal
          (cdr command)
          (list "scan"
                "personal" personal-root
                "work" work-root)))))))

(ert-deftest fangcun-native-ready-validates-the-source-build-id ()
  (cl-letf (((symbol-function 'fangcun--native-helper-build-id)
             (lambda () "expected")))
    (should-not
     (fangcun--validate-native-ready-message
      '((kind . "ready") (build-id . "expected"))))
    (should-error
     (fangcun--validate-native-ready-message
      '((kind . "ready") (build-id . "old")))
     :type 'fangcun-native-helper-outdated)
    (should-error
     (fangcun--validate-native-ready-message
      '((kind . "state")))
     :type 'fangcun-native-helper-outdated)))

(ert-deftest fangcun-native-mismatch-keeps-indexing-without-cargo ()
  (fangcun-test-with-notes
    (let ((fangcun-native-helper-enabled t)
          (emacs-program (expand-file-name invocation-name invocation-directory))
          (real-executable-find (symbol-function 'executable-find))
          (warnings-start
           (with-current-buffer (get-buffer-create "*Warnings*")
             (point-max))))
      (cl-letf (((symbol-function 'fangcun--native-helper-program)
                 (lambda () emacs-program))
                ((symbol-function 'process-file)
                 (lambda (_program _input _destination _display &rest _arguments)
                   (insert (json-serialize
                            '((kind . "ready") (build-id . "obsolete")))
                           "\n")
                   0))
                ((symbol-function 'make-process)
                 (lambda (&rest options)
                   (make-pipe-process
                    :name (plist-get options :name)
                    :noquery t
                    :filter (plist-get options :filter)
                    :sentinel (plist-get options :sentinel))))
                ((symbol-function 'executable-find)
                 (lambda (name)
                   (unless (equal name "cargo")
                     (funcall real-executable-find name)))))
        (fangcun-db-sync)
        (should (equal (fangcun-node-title
                        (fangcun-node-from-id "personal-file"))
                       "Personal Notes"))
        (with-current-buffer "*Warnings*"
          (goto-char warnings-start)
          (should (search-forward "Cargo is unavailable" nil t)))))))

(ert-deftest fangcun-disabling-native-helper-keeps-manual-index-updates ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (setq fangcun-native-helper-enabled t)
    (let ((monitor (make-pipe-process :name "fangcun-test-monitor" :noquery t))
          (build (make-pipe-process :name "fangcun-test-build" :noquery t)))
      (setq fangcun--native-watch-process monitor
            fangcun--native-build-process build)
      (fangcun--queue-native-full-sync)
      (setopt fangcun-native-helper-enabled nil)
      (should-not (process-live-p monitor))
      (should-not (process-live-p build))
      (fangcun--native-watch-filter monitor "{\"kind\":\"rescan\"}\n")
      (fangcun-test--write-file
       personal-file
       (concat ":PROPERTIES:\n:ID: personal-file\n:END:\n"
               "#+title: Manual update only\n"))
      (fangcun--process-native-events)
      (should (equal (fangcun-node-title
                      (fangcun-node-from-id "personal-file"))
                     "Personal Notes"))
      (fangcun-db-update-file personal-file t)
      (should (equal (fangcun-node-title
                      (fangcun-node-from-id "personal-file"))
                     "Manual update only")))))

(ert-deftest fangcun-enabling-native-helper-starts-monitoring-active-session ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let ((emacs-program (expand-file-name invocation-name invocation-directory)))
      (cl-letf (((symbol-function 'fangcun--native-helper-program)
                 (lambda () emacs-program))
                ((symbol-function 'make-process)
                 (lambda (&rest options)
                   (make-pipe-process
                    :name (plist-get options :name)
                    :noquery t
                    :filter (plist-get options :filter)
                    :sentinel (plist-get options :sentinel)))))
        (setopt fangcun-native-helper-enabled t)
        (let ((monitor fangcun--native-watch-process))
          (should (process-live-p monitor))
          (should (fangcun-node-from-id "personal-file")))))))

(ert-deftest fangcun-manual-native-build-succeeds-while-monitoring-is-disabled ()
  (fangcun-test-with-notes
    (fangcun-db-sync)
    (let* ((emacs-program (expand-file-name invocation-name invocation-directory))
           (fangcun-state-directory (expand-file-name "state/" root))
           (real-make-process (symbol-function 'make-process))
           messages)
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name)
                   (when (equal name "cargo") emacs-program)))
                ((symbol-function 'fangcun--native-helper-program)
                 (lambda () emacs-program))
                ((symbol-function 'make-process)
                 (lambda (&rest options)
                   (apply real-make-process
                          (plist-put options :command
                                     (list emacs-program "--batch" "-Q"
                                           "--eval" "(kill-emacs 0)")))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (fangcun-native-build)
        (let ((process fangcun--native-build-process)
              (deadline (+ (float-time) 10)))
          (while (and (process-live-p process)
                      (< (float-time) deadline))
            (accept-process-output process 0.1))
          (should-not (process-live-p process))
          (should (member "Built Fangcun native helper" messages))
          (should-not (process-live-p fangcun--native-watch-process)))))))

(provide 'fangcun-native-test)

;;; fangcun-native-test.el ends here
