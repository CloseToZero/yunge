;;; yunge-vertico-test.el --- Vertico tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function evil-local-mode "evil-core")
(declare-function evil-normal-state "evil-states")
(declare-function vertico--advice "vertico")
(declare-function vertico--exhibit "vertico")
(declare-function vertico--prepare "vertico")
(declare-function vertico--update "vertico")
(declare-function vertico-first "vertico")
(declare-function vertico-insert "vertico")
(declare-function vertico-last "vertico")

(defvar evil-local-mode)
(defvar evil-echo-state)
(defvar evil-state)
(defvar last-command-event)
(defvar vertico-count)
(defvar vertico--candidates-ov)
(defvar vertico--count-ov)

(yunge-test-deftest-lazy-load yunge-vertico
  (vertico))

(ert-deftest yunge-vertico-enables-after-package-ready ()
  (yunge-test-run-package-config
   'yunge-vertico 'vertico
   :before-ready
   '(when (or (featurep 'vertico)
              (bound-and-true-p vertico-mode))
      (error "Vertico was enabled before its Elpaca body ran"))
   :after-ready
   '(unless (and (featurep 'vertico) vertico-mode)
      (error "Vertico was not enabled after package readiness"))))

(ert-deftest yunge-vertico-moves-by-half-pages ()
  (require 'vertico-autoloads)
  (yunge-test-enable-evil)
  (yunge-test-load-package-config 'yunge-vertico)
  (yunge-test-with-evil-minibuffer
    (let ((original-input (minibuffer-contents-no-properties))
          (minibuffer-completion-table
           (cl-loop for n below 14 collect (format "item-%02d" n)))
          (minibuffer-completion-predicate nil)
          (minibuffer--require-match t)
          (vertico-count 10))
      (unwind-protect
          (progn
            (use-local-map minibuffer-local-completion-map)
            (vertico--advice (lambda () (run-hooks 'minibuffer-setup-hook)))
            (cl-labels
                ((selected (start command count &optional narrowed-input)
                   (delete-minibuffer-contents)
                   (vertico--update)
                   (if (eq start 'first) (vertico-first) (vertico-last))
                   (when narrowed-input (insert narrowed-input))
                   (let ((this-command command))
                     (run-hooks 'pre-command-hook))
                   (funcall command count)
                   (vertico-insert)
                   (minibuffer-contents-no-properties)))
              ;; The typed query has changed while Vertico's display is still stale.
              (should (equal (selected 'first #'yunge-vertico-next-half-page
                                       1 "item-1")
                             "item-13"))
              (should (equal (selected 'first #'yunge-vertico-next-half-page 2)
                             "item-10"))
              (should (equal (selected 'last #'yunge-vertico-previous-half-page 1)
                             "item-08"))
              (should (equal (selected 'last #'yunge-vertico-previous-half-page 5)
                             "item-00"))))
        (delete-minibuffer-contents)
        (insert original-input)
        (when (overlayp vertico--candidates-ov)
          (delete-overlay vertico--candidates-ov))
        (when (overlayp vertico--count-ov)
          (delete-overlay vertico--count-ov))
        (remove-hook 'pre-command-hook #'vertico--prepare t)
        (remove-hook 'post-command-hook #'vertico--exhibit t)))))

(ert-deftest yunge-vertico-return-accepts-selected-candidate ()
  (require 'yunge-minibuffer)
  (yunge-test-enable-evil)
  (require 'vertico-autoloads)
  (yunge-test-load-package-config 'yunge-vertico)
  (let (setup-hook)
    ;; Batch Emacs reads stdin instead of opening a live minibuffer.  Capture
    ;; the hook assembled by Vertico's real completion advice and run it in
    ;; the minibuffer buffer.
    (vertico--advice
     (lambda ()
       (setq setup-hook (copy-sequence minibuffer-setup-hook))))

    (yunge-test-with-evil-minibuffer
      (let ((original-input (minibuffer-contents-no-properties))
            (original-echo-state evil-echo-state)
            (echo-local (local-variable-p 'evil-echo-state))
            (minibuffer-completion-table '("alpha" "alpine"))
            (minibuffer-completion-predicate nil))
        (unwind-protect
            (progn
              (use-local-map minibuffer-local-completion-map)
              (delete-minibuffer-contents)
              (let ((minibuffer-setup-hook setup-hook))
                (run-hooks 'minibuffer-setup-hook))
              (yunge-test-evil-keys
               'insert
               '(("M-p" . previous-history-element)
                 ("M-n" . next-history-element)
                 ("C-j" . vertico-next)
                 ("C-k" . vertico-previous)
                 ("TAB" . vertico-insert)
                 ("<tab>" . vertico-insert)))
              (vertico--update)
              (vertico-last)
              (evil-normal-state)
              (yunge-test-evil-keys
               'normal
               '(("j" . vertico-next)
                 ("k" . vertico-previous)
                 ("<down>" . vertico-next)
                 ("<up>" . vertico-previous)
                 ("gg" . vertico-first)
                 ("G" . vertico-last)
                 ("C-d" . yunge-vertico-next-half-page)
                 ("C-u" . yunge-vertico-previous-half-page)
                 ("C-f" . vertico-scroll-up)
                 ("C-b" . vertico-scroll-down)
                 ("TAB" . vertico-insert)
                 ("<tab>" . vertico-insert)
                 ("C-g" . abort-minibuffers)
                 ("d" . evil-delete)))
              ;; A batch minibuffer has no recursive edit; its exit guards
              ;; need the active-loop context while Vertico stays real.
              (cl-letf (((symbol-function 'minibuffer-innermost-command-loop-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'innermost-minibuffer-p)
                         (lambda (&rest _) t)))
                (let ((last-command-event ?\r))
                  (should-not
                   (catch 'exit
                     (call-interactively (key-binding (kbd "RET")))
                     'not-exited))))
              (should (equal (minibuffer-contents-no-properties) "alpine")))
          (delete-minibuffer-contents)
          (insert original-input)
          (if echo-local
              (setq-local evil-echo-state original-echo-state)
            (kill-local-variable 'evil-echo-state))
          (when (overlayp vertico--candidates-ov)
            (delete-overlay vertico--candidates-ov))
          (when (overlayp vertico--count-ov)
            (delete-overlay vertico--count-ov))
          (remove-hook 'pre-command-hook #'vertico--prepare t)
          (remove-hook 'post-command-hook #'vertico--exhibit t))))))

;;; yunge-vertico-test.el ends here
