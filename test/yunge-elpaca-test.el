;;; yunge-elpaca-test.el --- Elpaca tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-elpaca
  (evil which-key elpaca-ui elpaca-log elpaca-info))

(ert-deftest yunge-elpaca-log-moves-from-the-header-to-an-entry ()
  (require 'yunge-elpaca)
  (yunge-test-enable-evil)
  (require 'elpaca-log)

  (with-temp-buffer
    (elpaca-log-mode)
    (setq tabulated-list-entries
          (list
           (list 'elpaca
                 (vector "elpaca" "finished" "Built" "Now"))))
    (tabulated-list-print)
    (goto-char (point-min))
    (call-interactively (key-binding (kbd "j")))
    (should (eq (tabulated-list-get-id) 'elpaca))))

;;; yunge-elpaca-test.el ends here
