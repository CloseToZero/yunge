;;; yunge-test-runner.el --- Test runner -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-test)

(setq yunge-test-external-checks-running-separately t)

(dolist (file (directory-files
               (file-name-directory load-file-name)
               t (concat "\\`\\(?:fangcun\\|shuying\\(?:-.+\\)?"
                         "\\|yunge-.+\\)-test\\.el\\'")))
  (load file nil nil t))

(when noninteractive
  (let* ((statistics (ert-run-tests-batch t))
         (ert-failures (ert-stats-completed-unexpected statistics))
         (external-failures (yunge-test--run-external-checks))
         (failures (+ ert-failures external-failures)))
    (princ
     (format
      "\nRepository checks: %d ERT failure(s), %d external failure(s)\n"
      ert-failures external-failures))
    (kill-emacs (if (zerop failures) 0 1))))

;;; yunge-test-runner.el ends here
