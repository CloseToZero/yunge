;;; yunge-embark.el --- Contextual actions -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function ffap-file-at-point "ffap")
(declare-function ffap-string-at-point "ffap")

(defvar embark-buffer-map)
(defvar embark-function-map)
(defvar embark-general-map)
(defvar embark-tab-map)
(defvar ffap-string-at-point-region)
(defvar thing-at-point-file-name-chars)

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
  (yunge-key-add-which-key-descriptions
   global-map yunge-embark-global-bindings))

(defun yunge-embark--adjust-windows-file-target-bounds (target)
  "Return TARGET with bounds matching a backslash path at point."
  (if (not target)
      nil
    (let* ((text (ffap-string-at-point 'file))
           (bounds
            (cons (car ffap-string-at-point-region)
                  (cadr ffap-string-at-point-region))))
      (if (not (string-search "\\" text))
          target
        ;; FFAP canonicalizes Windows separators.  Normalize only the
        ;; comparison strings, leaving its resolved target unchanged.
        (let* ((file (ffap-file-at-point))
               (compared-text (string-replace "\\" "/" text))
               (compared-file
                (and file (string-replace "\\" "/" file))))
          (if (and
               compared-file
               (or (string-match (regexp-quote compared-file)
                                 compared-text)
                   (string-match
                    (regexp-quote (file-name-base compared-file))
                    compared-text)))
              `(,(car target) ,(cadr target)
                ,(+ (car bounds) (match-beginning 0)) .
                ,(+ (car bounds) (match-end 0)))
            target))))))

(defun yunge-embark--setup-file-target ()
  "Make Embark recognize Windows path separators."
  (when (eq system-type 'windows-nt)
    ;; `filename' omits the native Windows separator.  As a result, Embark
    ;; highlights only part of a backslash path resolved by FFAP.
    (setq thing-at-point-file-name-chars
          (concat thing-at-point-file-name-chars "\\\\"))
    (advice-add 'embark-target-guess-file-at-point :filter-return
                #'yunge-embark--adjust-windows-file-target-bounds)))

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
    (yunge-embark--setup-file-target)
    (yunge-embark--setup-action-keys))
  (with-eval-after-load 'which-key
    (yunge-embark--describe-keys)))

(provide 'yunge-embark)

;;; yunge-embark.el ends here
