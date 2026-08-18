;;; yunge-reader-webview-view.el --- View state -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(defconst yunge-reader-webview--surface-states
  '(detached creating native-ready opening ready failed)
  "Lifecycle states for one logical view's current native surface.")

(defvar yunge-reader-webview--next-view-id 0
  "Last logical WebView identifier allocated by Emacs.")

(defvar yunge-reader-webview--views (make-hash-table :test #'eql)
  "Native WebView surfaces indexed by their current identifier.")

(defvar yunge-reader-webview--logical-views
  (make-hash-table :test #'eq)
  "Live logical WebView records, including temporarily hidden views.")

(defvar-local yunge-reader-webview--buffer-view nil
  "Logical WebView record owned by the current Reader buffer.")

(cl-defstruct (yunge-reader-webview--view
               (:constructor yunge-reader-webview--make-view))
  "One logical EPUB view and its disposable native surface."
  id
  window
  buffer
  (surface-state 'detached)
  native-focused
  focus-release-pending
  destroyed
  persistent
  owns-publication
  bounds
  requested-bounds
  bounds-pending
  publication
  style
  surface-style
  scroll-bar-mode
  surface-scroll-bar-mode
  location
  pending-target
  outline
  outline-ready
  outline-error
  outline-waiters
  selection
  search-result
  path
  open-deadline
  open-timer
  pending-destroys
  destroy-waiters
  destroy-finished
  location-changed-function
  selection-changed-function
  accelerator-function
  scroll-bar-function)

(defun yunge-reader-webview--set-surface-state (view state)
  "Set VIEW's surface STATE after validating the lifecycle value."
  (unless (memq state yunge-reader-webview--surface-states)
    (error "Invalid EPUB surface state: %S" state))
  (setf (yunge-reader-webview--view-surface-state view) state))

(defun yunge-reader-webview--surface-created-p (view)
  "Return whether VIEW has a native surface that accepts requests."
  (memq (yunge-reader-webview--view-surface-state view)
        '(native-ready opening ready failed)))

(defun yunge-reader-webview--surface-ready-p (view)
  "Return whether VIEW's native surface has loaded its publication."
  (eq (yunge-reader-webview--view-surface-state view) 'ready))

(provide 'yunge-reader-webview-view)

;;; yunge-reader-webview-view.el ends here
