;;; yunge-test-runner.el --- Test runner -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(dolist (file (directory-files
               (file-name-directory load-file-name)
               t (concat "\\`\\(?:fangcun\\|shuying\\(?:-.+\\)?"
                         "\\|yunge-.+\\)-test\\.el\\'")))
  (load file nil nil t))

(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; yunge-test-runner.el ends here
