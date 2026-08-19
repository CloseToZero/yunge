;;; yunge-reader-webview-events.el --- Events -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'subr-x)
(require 'yunge-reader-webview-protocol)
(require 'yunge-reader-webview-view)

(declare-function yunge-reader-webview--dispatch-pending-target
                  "yunge-reader-webview" (view))
(declare-function yunge-reader-webview--finish-outline-waiters
                  "yunge-reader-webview" (view outline error-data))
(declare-function yunge-reader-webview--focus-owning-window
                  "yunge-reader-webview" (view))
(declare-function yunge-reader-webview--record-native-focus
                  "yunge-reader-webview" (view focused))
(declare-function yunge-reader-webview--relay-owning-key
                  "yunge-reader-webview" (view key))
(declare-function yunge-reader-webview--set-buffer-message
                  "yunge-reader-webview" (view message))
(declare-function yunge-reader-webview--set-view-selection
                  "yunge-reader-webview" (view selection))
(declare-function yunge-reader-webview--store-view-location
                  "yunge-reader-webview" (view message &optional quiet))
(declare-function yunge-reader-webview--store-view-outline
                  "yunge-reader-webview" (view message))
(declare-function yunge-reader-webview--sync-view-scroll-bars
                  "yunge-reader-webview" (view))
(declare-function yunge-reader-webview--sync-view-search-result
                  "yunge-reader-webview" (view &optional reveal))
(declare-function yunge-reader-webview--sync-view-appearance
                  "yunge-reader-webview" (view))
(declare-function yunge-reader-webview--sync-view-style
                  "yunge-reader-webview" (view))
(declare-function yunge-reader-webview--sync-view-zoom
                  "yunge-reader-webview" (view))
(declare-function yunge-reader--uri-valid-p "yunge-reader" (uri))

(defvar yunge-reader-webview--process)

(defconst yunge-reader-webview--passive-buffer-message
  (concat "This EPUB view is active in another window.\n\n"
          "Select this window to move the EPUB view here.\n"
          "Use SPC m v for an independent Additional view.")
  "Message visible behind an active EPUB surface in passive windows.")

(defun yunge-reader-webview--event-location (message)
  "Return the validated EPUB locator carried by event MESSAGE."
  (let ((location (alist-get 'location message)))
    (unless (yunge-reader-webview--valid-location-p location)
      (error "Malformed EPUB location event: %S" message))
    (copy-tree location)))

(defun yunge-reader-webview--event-location-user (message)
  "Return whether location event MESSAGE came from direct user movement."
  (let ((user (alist-get 'user message)))
    (unless (and (assq 'user message)
                 (memq user '(nil t)))
      (error "Malformed EPUB location user flag: %S" message))
    user))

(defun yunge-reader-webview--event-outline (message)
  "Return the validated EPUB outline carried by event MESSAGE."
  (let ((outline (alist-get 'outline message)))
    (unless (yunge-reader-webview--valid-outline-p outline)
      (error "Malformed EPUB outline event: %S" message))
    (copy-tree outline)))

(defun yunge-reader-webview--event-selection (message)
  "Return the validated EPUB selection carried by event MESSAGE."
  (unless (assq 'selection message)
    (error "EPUB selection event has no selection field: %S" message))
  (let ((selection (alist-get 'selection message)))
    (unless (or (null selection)
                (yunge-reader-webview--valid-selection-p selection))
      (error "Malformed EPUB selection event: %S" message))
    (and selection (copy-tree selection))))

(defun yunge-reader-webview--event-external-uri (message)
  "Return the validated external URI carried by event MESSAGE."
  (let ((uri (alist-get 'uri message)))
    (unless (yunge-reader--uri-valid-p uri)
      (error "Malformed EPUB external-link event: %S" message))
    uri))

(defun yunge-reader-webview--handle-event (process message)
  "Validate and route one asynchronous WebView MESSAGE from PROCESS."
  (unless (eq process yunge-reader-webview--process)
    (error "WebView event belongs to an obsolete process"))
  (let ((event (alist-get 'event message))
        (id (alist-get 'view message)))
    (unless (and (stringp event) (integerp id))
      (error "Malformed Yunge Reader WebView event: %S" message))
    (pcase event
      ("accelerator"
       (let ((key (alist-get 'key message)))
         (unless (member key yunge-reader-webview--accelerators)
           (error "Malformed WebView accelerator event: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views))
                     (buffer (yunge-reader-webview--view-buffer view))
                     ((buffer-live-p buffer)))
           (with-current-buffer buffer
             (condition-case error-data
                 (if (member
                      key yunge-reader-webview--owning-accelerators)
                     (yunge-reader-webview--relay-owning-key view key)
                   (when-let*
                       ((function
                         (yunge-reader-webview--view-accelerator-function
                          view)))
                     (funcall function view key)))
               (quit nil)
               (error
                (display-warning
                 'yunge-reader
                 (format "Could not run EPUB key %s: %s"
                         key (error-message-string error-data))
                 :warning))))
           (when (member key '("C-g" "<escape>"))
             (yunge-reader-webview--focus-owning-window view)))))
      ("focus-gained"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--record-native-focus view t)))
      ("focus-lost"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--record-native-focus view nil)))
      ("publication-ready"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--store-view-location view message t)
         (yunge-reader-webview--set-surface-state
          (yunge-reader-webview--view-surface view) 'ready)
         (yunge-reader-webview--set-view-selection view nil)
         (yunge-reader-webview--sync-view-appearance view)
         (yunge-reader-webview--sync-view-style view)
         (yunge-reader-webview--sync-view-zoom view)
         (yunge-reader-webview--sync-view-scroll-bars view)
         (yunge-reader-webview--sync-view-search-result view)
         (yunge-reader-webview--store-view-outline view message)
         (yunge-reader-webview--dispatch-pending-target view)
         (yunge-reader-webview--set-buffer-message
          view yunge-reader-webview--passive-buffer-message)))
      ("location"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--store-view-location view message)))
      ("selection"
       (when-let* ((view (gethash id yunge-reader-webview--views)))
         (yunge-reader-webview--set-view-selection
          view (yunge-reader-webview--event-selection message))))
      ("external-link"
       (let ((uri (yunge-reader-webview--event-external-uri message)))
         (when-let* ((view (gethash id yunge-reader-webview--views))
                     (buffer (yunge-reader-webview--view-buffer view))
                     ((buffer-live-p buffer))
                     (function
                      (yunge-reader-webview--view-external-link-function
                       view)))
           (with-current-buffer buffer
             (condition-case error-data
                 (funcall function view uri)
               (quit nil)
               (error
                (display-warning
                 'yunge-reader
                 (format "Could not open EPUB link: %s"
                         (error-message-string error-data))
                 :warning)))))))
      ("navigation-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB navigation error: %S" message))
         (display-warning 'yunge-reader detail :warning)))
      ("appearance-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB appearance error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (setf (yunge-reader-webview--surface-appearance
                  (yunge-reader-webview--view-surface view))
                 nil))
         (display-warning 'yunge-reader detail :warning)))
      ("style-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB style error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (setf (yunge-reader-webview--surface-style
                  (yunge-reader-webview--view-surface view))
                 nil))
         (display-warning 'yunge-reader detail :warning)))
      ("zoom-changed"
       (let ((scale (alist-get 'scale message)))
         (unless (and (numberp scale) (> scale 0))
           (error "Malformed EPUB zoom event: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views))
                     (function
                      (yunge-reader-webview--view-zoom-changed-function
                       view)))
           (condition-case error-data
               (funcall function view scale)
             (error
              (display-warning
               'yunge-reader
               (format "Could not update EPUB zoom: %s"
                       (error-message-string error-data))
               :warning))))))
      ("zoom-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB zoom error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (setf (yunge-reader-webview--surface-zoom
                  (yunge-reader-webview--view-surface view))
                 nil))
         (display-warning 'yunge-reader detail :warning)))
      ("scroll-bars-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB scroll bar error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (setf
            (yunge-reader-webview--surface-scroll-bar-mode
             (yunge-reader-webview--view-surface view))
            nil))
         (display-warning 'yunge-reader detail :warning)))
      ("publication-error"
       (let ((detail (alist-get 'message message)))
         (unless (stringp detail)
           (error "Malformed EPUB renderer error: %S" message))
         (when-let* ((view (gethash id yunge-reader-webview--views)))
           (let ((surface
                  (yunge-reader-webview--view-surface view)))
             (yunge-reader-webview--set-surface-state surface 'failed)
             (setf (yunge-reader-webview--surface-appearance surface) nil
                   (yunge-reader-webview--surface-style surface) nil
                   (yunge-reader-webview--surface-zoom surface) nil
                   (yunge-reader-webview--surface-scroll-bar-mode
                    surface)
                   nil))
           (setf
                 (yunge-reader-webview--view-pending-target view) nil
                 (yunge-reader-webview--view-outline-error view)
                 (list 'error detail))
           (yunge-reader-webview--set-view-selection view nil)
           (yunge-reader-webview--finish-outline-waiters
            view nil (list 'error detail))
           (yunge-reader-webview--set-buffer-message view detail))
         (display-warning 'yunge-reader detail :warning)))
      (_
       (error "Unsupported Yunge Reader WebView event: %s" event)))))

(provide 'yunge-reader-webview-events)

;;; yunge-reader-webview-events.el ends here
