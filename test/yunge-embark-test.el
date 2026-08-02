;;; yunge-embark-test.el --- Embark tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(yunge-test-deftest-lazy-load yunge-embark
  (embark embark-consult))

(ert-deftest yunge-embark-configures-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-embark 'embark
   :before-ready
   '(progn
      (when (featurep 'embark)
        (error "Embark was loaded before its Elpaca body ran"))
      (when (eq (key-binding (kbd "M-a")) 'embark-act)
        (error "Embark was bound before its Elpaca body ran")))
   :after-ready
   '(progn
      (unless (eq (key-binding (kbd "M-a")) 'embark-act)
        (error "Embark was not bound after package readiness"))
      (when (featurep 'embark)
        (error "Embark was loaded by its configuration")))))

(ert-deftest yunge-embark-loads-consult-integration-on-demand ()
  (yunge-test-load-package-config 'yunge-embark)
  (require 'consult)
  (require 'embark)
  (should (featurep 'embark-consult)))

(ert-deftest yunge-embark-preserves-package-defaults ()
  (yunge-test-run-emacs
   "--eval"
   (prin1-to-string
    '(progn
       (require 'embark)
       (let ((targets (copy-tree embark-target-finders))
             (actions (copy-tree embark-keymap-alist))
             (indicators (copy-tree embark-indicators))
             (help-key embark-help-key))
         (defmacro elpaca (&rest _body) nil)
         (require 'yunge-embark)
         (unless (and (equal embark-target-finders targets)
                      (equal embark-keymap-alist actions)
                      (equal embark-indicators indicators)
                      (equal embark-help-key help-key))
           (error "Embark package defaults were changed")))))))

;;; yunge-embark-test.el ends here
