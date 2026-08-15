;;; yunge-avy.el --- IME-friendly visible text jumps -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-evil)
(require 'yunge-input-source)
(require 'yunge-key)
(require 'yunge-pinyin)

(declare-function avy-jump "avy" (regex &rest arguments))
(declare-function avy-action-goto "avy" (position))
(declare-function evil-add-command-properties "evil-common")

(defvar avy-action)
(defvar avy-action-oneshot)
(defvar avy-command)
(defvar avy-dispatch-alist)

(cl-defstruct (yunge-avy-projection
               (:constructor yunge-avy-make-projection))
  "A visible Avy candidate projected from a logical source match."
  identity
  beginning
  end
  target)

(defvar yunge-avy-candidate-project-functions nil
  "Functions that project logical matches onto visible Avy candidates.
Each function receives match BEGINNING, END, and WINDOW.  It should return a
`yunge-avy-projection' or nil.  A non-nil projection identity deduplicates
matches with the same identity in one window.")

(defconst yunge-avy-bindings
  '(("j" yunge-avy-jump-to-text "jump to text")))

(defun yunge-avy--query-regexp (text)
  "Return a literal or Pinyin-aware regexp for TEXT."
  (or (yunge-pinyin-query-regexp text)
      (regexp-quote text)))

(defun yunge-avy--project-match ()
  "Return the candidate projection for the current regexp match."
  (let ((beginning (match-beginning 0))
        (end (match-end 0)))
    (or (run-hook-with-args-until-success
         'yunge-avy-candidate-project-functions
         beginning end (selected-window))
        (yunge-avy-make-projection
         :beginning beginning :end end :target beginning))))

(defun yunge-avy--target-position (targets position)
  "Resolve Avy POSITION through the projected TARGETS table."
  (or (gethash (cons (selected-window) position) targets)
      position))

(defun yunge-avy--mapped-action (action targets)
  "Return an Avy action applying ACTION to its source position in TARGETS."
  (lambda (position)
    (funcall action (yunge-avy--target-position targets position))))

(defun yunge-avy--jump (regexp)
  "Jump to REGEXP after projecting logical matches onto visible candidates."
  (let ((current-projection nil)
        (seen-identities (make-hash-table :test #'equal))
        (targets (make-hash-table :test #'equal)))
    (cl-labels
        ((candidate-p ()
           (setq current-projection (yunge-avy--project-match))
           (let* ((identity
                   (yunge-avy-projection-identity current-projection))
                  (identity-key
                   (and identity (cons (selected-window) identity))))
             (unless (and identity-key
                          (gethash identity-key seen-identities))
               (when identity-key
                 (puthash identity-key t seen-identities))
               (puthash
                (cons
                 (selected-window)
                 (yunge-avy-projection-beginning current-projection))
                (yunge-avy-projection-target current-projection)
                targets)
               t)))
         (candidate-bounds ()
           (prog1
               (cons (yunge-avy-projection-beginning current-projection)
                     (yunge-avy-projection-end current-projection))
             (setq current-projection nil))))
      (let* ((default-action
              (or avy-action avy-action-oneshot #'avy-action-goto))
             (avy-action-oneshot nil)
             (avy-action
              (yunge-avy--mapped-action default-action targets))
             (avy-dispatch-alist
              (mapcar
               (lambda (entry)
                 (cons (car entry)
                       (yunge-avy--mapped-action (cdr entry) targets)))
               avy-dispatch-alist)))
        (avy-jump regexp
                  :pred #'candidate-p
                  :group #'candidate-bounds)))))

(defun yunge-avy-jump-to-text (text)
  "Jump to visible TEXT or projected source matches.
Also accept structured Pinyin, or permissive Pinyin after `:py:'."
  (interactive (list (read-from-minibuffer "Jump to text: ")))
  (when (string-empty-p text)
    (user-error "Jump text cannot be empty"))
  (require 'avy)
  (let ((regexp (yunge-avy--query-regexp text))
        (avy-action nil)
        (avy-command 'yunge-avy-jump-to-text))
    (yunge-input-source-call-with-ascii
     (lambda ()
       (yunge-avy--jump regexp)))))

(defun yunge-avy--setup ()
  "Expose the IME-friendly Avy command after Avy is ready."
  (yunge-key-define yunge-jump-map yunge-avy-bindings)
  (with-eval-after-load 'evil
    (evil-add-command-properties 'yunge-avy-jump-to-text
                                 :jump nil :repeat nil)
    (yunge-jump-history-track-command 'yunge-avy-jump-to-text))
  (with-eval-after-load 'which-key
    (yunge-key-add-which-key-descriptions
     yunge-jump-map yunge-avy-bindings)))

(elpaca avy
  (yunge-avy--setup))

(provide 'yunge-avy)

;;; yunge-avy.el ends here
