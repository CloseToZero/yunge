;;; yunge-reader-webview-view.el --- View state -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(defconst yunge-reader-webview--surface-states
  '(creating native-ready opening ready failed)
  "Lifecycle states for a current native surface.")

(defvar yunge-reader-webview--next-view-id 0
  "Last logical WebView identifier allocated by Emacs.")

(defvar yunge-reader-webview--views (make-hash-table :test #'eql)
  "Native WebView surfaces indexed by their current identifier.")

(defvar yunge-reader-webview--logical-views
  (make-hash-table :test #'eq)
  "Live logical WebView records, including hidden views.")

(defvar-local yunge-reader-webview--buffer-view nil
  "Logical WebView record owned by the current Reader buffer.")

(cl-defstruct (yunge-reader-webview--surface
               (:constructor yunge-reader-webview--make-surface))
  "One disposable native surface for a logical EPUB view."
  id
  window
  (state 'creating)
  native-focused
  focus-release-pending
  bounds
  requested-bounds
  bounds-pending
  appearance
  style
  zoom
  scroll-bar-mode
  open-timer)

(cl-defstruct (yunge-reader-webview--view
               (:constructor yunge-reader-webview--make-view))
  "One logical EPUB view and its disposable native surface."
  buffer
  surface
  destroyed
  persistent
  owns-publication
  layout
  publication
  appearance
  style
  zoom
  scroll-bar-mode
  location
  pending-target
  outline
  outline-ready
  outline-error
  outline-waiters
  selection
  search-result
  path
  pending-destroys
  destroy-waiters
  destroy-finished
  location-changed-function
  selection-changed-function
  accelerator-function
  zoom-changed-function
  appearance-function
  scroll-bar-function
  external-link-function)

(defun yunge-reader-webview--set-surface-state (surface state)
  "Set SURFACE's STATE after validating the lifecycle value."
  (unless (memq state yunge-reader-webview--surface-states)
    (error "Invalid EPUB surface state: %S" state))
  (setf (yunge-reader-webview--surface-state surface) state))

(defun yunge-reader-webview--surface-created-p (surface)
  "Return whether SURFACE accepts native requests."
  (and surface
       (memq (yunge-reader-webview--surface-state surface)
             '(native-ready opening ready failed))))

(defun yunge-reader-webview--surface-ready-p (surface)
  "Return whether SURFACE has loaded its publication."
  (and surface
       (eq (yunge-reader-webview--surface-state surface) 'ready)))

(provide 'yunge-reader-webview-view)

;;; yunge-reader-webview-view.el ends here
