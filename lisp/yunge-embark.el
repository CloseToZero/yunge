;;; yunge-embark.el --- Contextual actions -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function embark-target-file-at-point "embark")
(declare-function ffap-string-at-point "ffap")
(declare-function ffap-url-p "ffap")

(defvar embark-buffer-map)
(defvar embark-function-map)
(defvar embark-general-map)
(defvar embark-tab-map)
(defvar embark-target-finders)
(defvar ffap-string-at-point-region)

(defconst yunge-embark-global-bindings
  '(("M-a" embark-act "act on target")))

;; Embark renders action maps and their command docstrings itself.
(defconst yunge-embark-debug-bindings
  '(("b" debug-on-entry "break on entry")
    ("c" cancel-debug-on-entry "clear break on entry")
    ("m" elp-instrument-function "measure")
    ("r" elp-restore-function "restore measurement")
    ("t" trace-function "trace")
    ("u" untrace-function "untrace")))

(defvar-keymap yunge-embark-debug-map
  :doc "Debug or instrument functions.")

(yunge-key-define yunge-embark-debug-map
                  yunge-embark-debug-bindings)

(fset 'yunge-embark-debug-map yunge-embark-debug-map)

(defconst yunge-embark-general-action-bindings
  '(("C-q" embark-toggle-quit nil)
    ("q" nil nil)
    ("w" nil nil)
    ("y" embark-copy-as-kill nil)))

(defconst yunge-embark-buffer-action-bindings
  '(("b" embark-bury-buffer nil)
    ("k" nil nil)
    ("K" nil nil)
    ("q" kill-buffer nil)
    ("Q" embark-kill-buffer-and-window nil)
    ("z" nil nil)))

(defconst yunge-embark-tab-action-bindings
  '(("k" nil nil)
    ("q" tab-bar-close-tab-by-name nil)
    ("s" nil nil)))

(defconst yunge-embark-function-action-bindings
  '(("D" yunge-embark-debug-map nil)
    ("k" nil nil)
    ("K" nil nil)
    ("m" nil nil)
    ("M" nil nil)
    ("t" nil nil)
    ("T" nil nil)))

(defun yunge-embark--setup-keys ()
  "Set up the Embark entry point."
  (yunge-key-define global-map yunge-embark-global-bindings))

(defun yunge-embark--describe-keys ()
  "Describe the Embark entry point to Which-Key."
  (yunge-key-which-key-describe-map
   global-map yunge-embark-global-bindings))

(defun yunge-embark-target-file-at-point ()
  "Target explicit file paths without requiring them to exist.
Keep Embark's structured Dired and Image Dired handling."
  (if (derived-mode-p 'dired-mode
                      'image-dired-thumbnail-mode)
      (embark-target-file-at-point)
    ;; Use FFAP only to delimit text, not to resolve an existing file.
    (let* ((text (ffap-string-at-point 'file))
           (bounds
            (cons (car ffap-string-at-point-region)
                  (cadr ffap-string-at-point-region))))
      (unless (ffap-url-p text)
        ;; Compiler-style :LINE[:COLUMN] locates text within the file;
        ;; it is not part of the file name or its highlighted bounds.
        (when (string-match
               ":[0-9]+\\(?::[0-9]+\\)?\\'" text)
          (setcdr bounds
                  (- (cdr bounds)
                     (- (length text) (match-beginning 0))))
          (setq text (substring text 0 (match-beginning 0))))
        ;; Reject common syntax tokens, then require a directory
        ;; component or file extension as evidence of an explicit path.
        (unless (or (member text '("" "/" "//" "/*" "."))
                    (equal (file-name-base text) text))
          `(file
            ,(abbreviate-file-name (expand-file-name text))
            ,(car bounds) . ,(cdr bounds)))))))

(defun yunge-embark--setup-target-finders ()
  "Use the syntactic explicit file target finder."
  (setq embark-target-finders
        (mapcar
         (lambda (finder)
           (if (eq finder 'embark-target-file-at-point)
               'yunge-embark-target-file-at-point
             finder))
         embark-target-finders)))

(defun yunge-embark--setup-action-keys ()
  "Set up consistent keys in the Embark action maps."
  (yunge-key-define embark-general-map
                    yunge-embark-general-action-bindings)
  (yunge-key-define embark-buffer-map
                    yunge-embark-buffer-action-bindings)
  (yunge-key-define embark-tab-map
                    yunge-embark-tab-action-bindings)
  (yunge-key-define embark-function-map
                    yunge-embark-function-action-bindings))

(elpaca embark-consult)

(elpaca embark
  (yunge-embark--setup-keys)
  (with-eval-after-load 'embark
    (yunge-embark--setup-target-finders)
    (yunge-embark--setup-action-keys))
  (with-eval-after-load 'which-key
    (yunge-embark--describe-keys)))

(provide 'yunge-embark)

;;; yunge-embark.el ends here
