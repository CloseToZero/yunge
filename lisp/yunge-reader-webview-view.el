;;; yunge-reader-webview-view.el --- View state -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)

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

(defvar yunge-reader-webview--operation-surface nil
  "Dynamically selected surface for one native event or request.")

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
  desired-appearance
  appearance
  style
  zoom
  desired-scroll-bar-mode
  scroll-bar-mode
  location
  selection
  search-result
  open-timer)

(cl-defstruct (yunge-reader-webview--view
               (:constructor yunge-reader-webview--make-view))
  "One logical EPUB view and its per-window native surfaces."
  buffer
  surface
  surfaces
  destroyed
  persistent
  owns-publication
  broker-session
  layout
  publication
  renderer-url
  resource-root
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

(defun yunge-reader-webview--view-surface-table (view)
  "Return VIEW's window-indexed native surface table."
  (let ((table
         (or (yunge-reader-webview--view-surfaces view)
             (setf (yunge-reader-webview--view-surfaces view)
                   (make-hash-table :test #'eq)))))
    (when-let* ((surface (yunge-reader-webview--view-surface view))
                (window (yunge-reader-webview--surface-window surface)))
      (unless (gethash window table)
        (puthash window surface table)))
    table))

(defun yunge-reader-webview--view-surface-list (view)
  "Return every native surface belonging to VIEW."
  (let ((active (yunge-reader-webview--view-surface view))
        surfaces)
    (maphash
     (lambda (_window surface) (push surface surfaces))
     (yunge-reader-webview--view-surface-table view))
    ;; A hidden surface normally retains its former window and therefore
    ;; remains in the table.  Keeping the active fallback here also makes
    ;; destruction robust while migrating old in-memory view records that
    ;; predate the per-window registry.
    (when (and active (not (memq active surfaces)))
      (push active surfaces))
    surfaces))

(defun yunge-reader-webview--view-surface-for-window (view window)
  "Return VIEW's native surface for WINDOW, or nil."
  (gethash window (yunge-reader-webview--view-surface-table view)))

(defun yunge-reader-webview--view-surface-for-id (view id)
  "Return VIEW's native surface named by ID, or nil."
  (seq-find
   (lambda (surface)
     (eql id (yunge-reader-webview--surface-id surface)))
   (yunge-reader-webview--view-surface-list view)))

(defun yunge-reader-webview--current-surface (view)
  "Return VIEW's dynamically targeted or active native surface."
  (if (and yunge-reader-webview--operation-surface
           (memq yunge-reader-webview--operation-surface
                 (yunge-reader-webview--view-surface-list view)))
      yunge-reader-webview--operation-surface
    (yunge-reader-webview--view-surface view)))

(defun yunge-reader-webview--register-surface (view surface)
  "Register SURFACE as VIEW's presentation for its window."
  (puthash (yunge-reader-webview--surface-window surface)
           surface
           (yunge-reader-webview--view-surface-table view))
  surface)

(defun yunge-reader-webview--unregister-surface (view surface)
  "Remove SURFACE from VIEW and clear it as active when necessary."
  (let ((table (yunge-reader-webview--view-surface-table view))
        (window (yunge-reader-webview--surface-window surface)))
    (when (eq (gethash window table) surface)
      (remhash window table))
    (when (eq (yunge-reader-webview--view-surface view) surface)
      (setf (yunge-reader-webview--view-surface view) nil)))
  surface)

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
