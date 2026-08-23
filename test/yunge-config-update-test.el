;;; yunge-config-update-test.el --- Config update tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-config-update)

(ert-deftest yunge-config-update-registers-startup-and-mode-line ()
  (should (memq #'yunge-config-update-start emacs-startup-hook))
  (should (memq #'yunge-config-update-stop kill-emacs-hook))
  (should (member yunge-config-update-mode-line-format
                  global-mode-string))
  (should-not (memq 'yunge-config-update-mode-line-format
                    global-mode-string))
  (should-not
   (string-match-p "\\*invalid\\*"
                   (format-mode-line global-mode-string)))
  (should
   (eq (lookup-key yunge-config-update-mode-line-map
                   [mode-line mouse-1])
       #'yunge-config-update-open)))

(ert-deftest yunge-config-update-builds-explicit-remote-branch-commands ()
  (let ((yunge-config-directory "C:/config/")
        (yunge-config-update-remote "origin"))
    (should
     (equal
      (yunge-config-update--fetch-command "git.exe" "topic/name")
      '("git.exe" "-C" "C:/config/" "fetch" "--quiet" "--no-tags"
        "origin"
        "+refs/heads/topic/name:refs/remotes/origin/topic/name")))
    (should
     (equal
      (yunge-config-update--compare-command "git.exe" "topic/name")
      '("git.exe" "-C" "C:/config/" "rev-list" "--left-right"
        "--count" "HEAD...refs/remotes/origin/topic/name")))))

(ert-deftest yunge-config-update-shows-state-changes-without-repeating ()
  (let ((yunge-config-update--state nil)
        (yunge-config-update--last-error "old error")
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (push (apply #'format format-string arguments) messages)))
              ((symbol-function 'force-mode-line-update) #'ignore))
      (yunge-config-update--set-state "main" 0 2)
      (should (equal (car messages)
                     "Config update available: origin/main is ahead by 2 commits"))
      (should-not yunge-config-update--last-error)
      (yunge-config-update--set-state "main" 0 2)
      (should (= (length messages) 1))
      (yunge-config-update--set-state "main" 0 0)
      (should (equal (car messages)
                     "Config is now in sync with origin/main"))
      (should-not yunge-config-update--state))))

(ert-deftest yunge-config-update-mode-line-distinguishes-divergence ()
  (cl-letf (((symbol-function 'mode-line-window-selected-p)
             (lambda () t)))
    (dolist (case '(((:branch "main" :ahead 3 :behind 0) . " Config↑3")
                    ((:branch "main" :ahead 0 :behind 4) . " Config↓4")
                    ((:branch "main" :ahead 2 :behind 5) . " Config↕2/5")))
      (let* ((yunge-config-update--state (car case))
             (indicator (yunge-config-update--mode-line)))
        (should (equal indicator (cdr case)))
        (should (eq (get-text-property 1 'face indicator)
                    'yunge-config-update-mode-line))
        (should (eq (get-text-property 1 'local-map indicator)
                    yunge-config-update-mode-line-map))))))

(ert-deftest yunge-config-update-starts-an-asynchronous-fetch ()
  (let ((yunge-config-update--process nil)
        (yunge-config-update-timeout 60)
        arguments
        properties)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "C:/Git/git.exe"))
              ((symbol-function 'yunge-config-update--current-branch)
               (lambda (_git) "main"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq arguments plist)
                 'config-update-process))
              ((symbol-function 'process-put)
               (lambda (process property value)
                 (push (list process property value) properties)))
              ((symbol-function 'run-at-time)
               (lambda (&rest _arguments) 'timeout-timer))
              ((symbol-function 'process-live-p) #'ignore))
      (yunge-config-update-check)
      (should (eq yunge-config-update--process 'config-update-process))
      (should
       (equal
        (plist-get arguments :command)
        (list "C:/Git/git.exe" "-C" yunge-config-directory
              "fetch" "--quiet" "--no-tags" "origin"
              "+refs/heads/main:refs/remotes/origin/main")))
      (should (eq (plist-get arguments :sentinel)
                  #'yunge-config-update--fetch-sentinel))
      (should-not (plist-member arguments :stderr))
      (should (member '(config-update-process
                        yunge-config-update-branch "main")
                      properties))
      (should (member '(config-update-process
                        yunge-config-update-timeout timeout-timer)
                      properties)))))

(ert-deftest yunge-config-update-periodic-check-waits-for-short-idle ()
  (let ((yunge-config-update--idle-timer nil)
        (yunge-config-update--process nil)
        (yunge-config-update-idle-delay 5)
        arguments)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest values)
                 (setq arguments values)
                 'idle-timer))
              ((symbol-function 'process-live-p) #'ignore))
      (yunge-config-update--schedule-idle-check)
      (should (equal arguments
                     '(5 nil yunge-config-update--run-idle-check)))
      (should (eq yunge-config-update--idle-timer 'idle-timer)))))

;;; yunge-config-update-test.el ends here
