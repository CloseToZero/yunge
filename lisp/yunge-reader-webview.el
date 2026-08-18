;;; yunge-reader-webview.el --- WebView integration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-reader)
(require 'yunge-reader-webview-protocol)
(require 'yunge-reader-webview-view)
(require 'yunge-reader-webview-service)
(require 'yunge-reader-webview-renderer)
(require 'yunge-reader-webview-surface)

(define-derived-mode yunge-reader-webview-spike-mode special-mode
  "Yunge-WebView"
  "Major mode used behind a native child WebView spike."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (add-hook 'kill-buffer-hook
            #'yunge-reader-webview--kill-buffer nil t))

(defun yunge-reader-webview--style-complete
    (view id style _result error-data)
  "Finish applying STYLE to VIEW surface ID."
  (when (yunge-reader-webview--surface-current-p view id)
    (when error-data
      (when (equal style
                   (yunge-reader-webview--view-surface-style view))
        (setf (yunge-reader-webview--view-surface-style view) nil))
      (display-warning
       'yunge-reader (error-message-string error-data) :warning))))

(defun yunge-reader-webview--sync-view-style (view)
  "Send VIEW's desired style to its ready native surface."
  (let ((style (yunge-reader-webview--view-style view)))
    (when (and style
               (yunge-reader-webview--surface-ready-p view)
               (yunge-reader-webview--surface-current-p
                view (yunge-reader-webview--view-id view))
               (not
                (equal style
                       (yunge-reader-webview--view-surface-style view))))
      (let ((id (yunge-reader-webview--view-id view))
            (requested (copy-tree style)))
        (setf (yunge-reader-webview--view-surface-style view)
              (copy-tree requested))
        (yunge-reader-webview--set-native-view-style
         view requested
         (apply-partially
          #'yunge-reader-webview--style-complete
          view id requested))))))

(defun yunge-reader-webview--set-view-style (view style)
  "Set logical VIEW's desired STYLE and synchronize its live surface."
  (when (or (null view)
            (yunge-reader-webview--view-destroyed view))
    (error "Cannot style a dead EPUB view"))
  (when (eq (yunge-reader-webview--view-layout view) 'fixed)
    (error "Cannot apply reflow style to a fixed-layout EPUB view"))
  (let ((style (copy-tree
                (yunge-reader-webview--check-style style))))
    (setf (yunge-reader-webview--view-style view) style)
    (yunge-reader-webview--sync-view-style view)
    style))

(defun yunge-reader-webview--zoom-complete
    (view id zoom _result error-data)
  "Finish applying ZOOM to VIEW surface ID."
  (when (and error-data
             (yunge-reader-webview--surface-current-p view id))
    (when (equal zoom
                 (yunge-reader-webview--view-surface-zoom view))
      (setf (yunge-reader-webview--view-surface-zoom view) nil))
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

(defun yunge-reader-webview--sync-view-zoom (view)
  "Send VIEW's desired fixed zoom to its ready native surface."
  (let ((zoom (yunge-reader-webview--view-zoom view)))
    (when (and zoom
               (yunge-reader-webview--surface-ready-p view)
               (yunge-reader-webview--surface-current-p
                view (yunge-reader-webview--view-id view))
               (not
                (equal zoom
                       (yunge-reader-webview--view-surface-zoom view))))
      (let ((id (yunge-reader-webview--view-id view)))
        (setf (yunge-reader-webview--view-surface-zoom view) zoom)
        (yunge-reader-webview--set-native-view-zoom
         view zoom
         (apply-partially
          #'yunge-reader-webview--zoom-complete view id zoom))))))

(defun yunge-reader-webview--set-view-zoom (view zoom)
  "Set logical VIEW's desired fixed-layout ZOOM and synchronize it."
  (when (or (null view)
            (yunge-reader-webview--view-destroyed view))
    (error "Cannot zoom a dead EPUB view"))
  (unless (eq (yunge-reader-webview--view-layout view) 'fixed)
    (error "Cannot apply fixed zoom to a reflowable EPUB view"))
  (setq zoom (yunge-reader-webview--check-fixed-zoom zoom))
  (setf (yunge-reader-webview--view-zoom view) zoom)
  (yunge-reader-webview--sync-view-zoom view)
  zoom)

(defun yunge-reader-webview--scroll-bar-complete
    (view id mode _result error-data)
  "Finish applying scroll bar MODE to VIEW surface ID."
  (when (and error-data
             (yunge-reader-webview--surface-current-p view id))
    (when (eq mode
              (yunge-reader-webview--view-surface-scroll-bar-mode view))
      (setf (yunge-reader-webview--view-surface-scroll-bar-mode view)
            nil))
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

(defun yunge-reader-webview--sync-view-scroll-bars (view)
  "Send VIEW's resolved scroll bar mode to its ready native surface."
  (let ((mode (yunge-reader-webview--view-scroll-bar-mode view)))
    (when (and mode
               (yunge-reader-webview--surface-ready-p view)
               (yunge-reader-webview--surface-current-p
                view (yunge-reader-webview--view-id view))
               (not
                (eq mode
                    (yunge-reader-webview--view-surface-scroll-bar-mode
                     view))))
      (let ((id (yunge-reader-webview--view-id view)))
        (setf (yunge-reader-webview--view-surface-scroll-bar-mode view)
              mode)
        (yunge-reader-webview--set-native-scroll-bar-mode
         view mode
         (apply-partially
          #'yunge-reader-webview--scroll-bar-complete view id mode))))))

(defun yunge-reader-webview--resolved-scroll-bar-mode (view window)
  "Return VIEW's resolved scroll bar mode in WINDOW."
  (yunge-reader-webview--check-scroll-bar-mode
   (if-let* ((function
              (yunge-reader-webview--view-scroll-bar-function view)))
       (funcall function window)
     'visible)))

(defun yunge-reader-webview--update-scroll-bar-mode (view window)
  "Resolve VIEW's scroll bar mode for WINDOW and synchronize it."
  (let ((mode
         (yunge-reader-webview--resolved-scroll-bar-mode view window)))
    (unless (eq mode (yunge-reader-webview--view-scroll-bar-mode view))
      (setf (yunge-reader-webview--view-scroll-bar-mode view) mode)
      (yunge-reader-webview--sync-view-scroll-bars view))))

(defun yunge-reader-webview--queue-view-target (view target)
  "Queue one transient EPUB TARGET until VIEW's surface is ready."
  (setf (yunge-reader-webview--view-pending-target view)
        (copy-tree (yunge-reader-webview--check-target target))))

(defun yunge-reader-webview--pending-target-complete
    (_result error-data)
  "Report ERROR-DATA from a queued EPUB navigation target."
  (when error-data
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

(defun yunge-reader-webview--dispatch-pending-target (view)
  "Navigate VIEW to its queued transient target, if any."
  (when-let* ((target
               (prog1 (yunge-reader-webview--view-pending-target view)
                 (setf (yunge-reader-webview--view-pending-target view)
                       nil))))
    (yunge-reader-webview--navigate-view
     view "go-to"
     #'yunge-reader-webview--pending-target-complete
     target)))

(defun yunge-reader-webview--set-buffer-message (view message)
  "Replace VIEW's backing buffer contents with MESSAGE."
  (let ((buffer (yunge-reader-webview--view-buffer view)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert message "\n")
          (set-buffer-modified-p nil))))))

(defun yunge-reader-webview--store-view-location
    (view message &optional initial)
  "Store VIEW's locator from MESSAGE and notify its logical owner.
An INITIAL publication location has no direct-user origin."
  (let ((location (yunge-reader-webview--event-location message))
        (user
         (unless initial
           (yunge-reader-webview--event-location-user message))))
    (setf (yunge-reader-webview--view-location view) location)
    (when-let* ((function
                 (yunge-reader-webview--view-location-changed-function
                  view)))
      (condition-case error-data
          (funcall function view user)
        (error
         (display-warning
          'yunge-reader
          (format "Could not update EPUB location: %s"
                  (error-message-string error-data))
          :warning))))))

(defun yunge-reader-webview--set-view-selection (view selection)
  "Set VIEW's validated SELECTION and notify its logical owner."
  (unless (or (null selection)
              (yunge-reader-webview--valid-selection-p selection))
    (error "Invalid EPUB view selection: %S" selection))
  (unless (equal selection
                 (yunge-reader-webview--view-selection view))
    (setf (yunge-reader-webview--view-selection view)
          (and selection (copy-tree selection)))
    (when-let* ((function
                 (yunge-reader-webview--view-selection-changed-function
                  view)))
      (condition-case error-data
          (funcall function view)
        (error
         (display-warning
          'yunge-reader
          (format "Could not record EPUB selection: %s"
                  (error-message-string error-data))
          :warning)))))
  (yunge-reader-webview--view-selection view))

(defun yunge-reader-webview--clear-view-selection (view)
  "Ask live native VIEW to clear its publication selection."
  (when (and (integerp (yunge-reader-webview--view-id view))
             (yunge-reader-webview--surface-ready-p view)
             (not (yunge-reader-webview--view-destroyed view))
             (process-live-p yunge-reader-webview--process))
    (yunge-reader-webview--request
     "view-clear-selection"
     `((view . ,(yunge-reader-webview--view-id view)))
     (lambda (_result _error-data)))
    t))

(defun yunge-reader-webview--search-result-complete
    (view id selection _result error-data)
  "Report failure to apply SELECTION to VIEW surface ID."
  (when (and error-data
             (yunge-reader-webview--surface-current-p view id)
             (equal selection
                    (yunge-reader-webview--view-search-result view)))
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

(defun yunge-reader-webview--sync-view-search-result (view &optional reveal)
  "Apply logical VIEW's desired search result to its ready surface.
When REVEAL is non-nil, navigate to the result before painting it."
  (when (and (integerp (yunge-reader-webview--view-id view))
             (yunge-reader-webview--surface-ready-p view)
             (not (yunge-reader-webview--view-destroyed view))
             (process-live-p yunge-reader-webview--process))
    (let ((id (yunge-reader-webview--view-id view))
          (selection
           (copy-tree
            (yunge-reader-webview--view-search-result view))))
      (yunge-reader-webview--request
       "view-search-result"
       `((view . ,id)
         (selection . ,selection)
         (reveal . ,(if reveal t :false)))
       (apply-partially
        #'yunge-reader-webview--search-result-complete
        view id selection)))
    t))

(defun yunge-reader-webview--set-view-search-result (view selection)
  "Set logical VIEW's desired native search-result SELECTION."
  (when (or (null view)
            (yunge-reader-webview--view-destroyed view))
    (error "Cannot set a search result on a dead EPUB view"))
  (unless (or (null selection)
              (yunge-reader-webview--valid-selection-p selection))
    (error "Invalid EPUB search result selection: %S" selection))
  (setf (yunge-reader-webview--view-search-result view)
        (and selection (copy-tree selection)))
  (yunge-reader-webview--sync-view-search-result view t)
  selection)

(defun yunge-reader-webview--focus-owning-window (view)
  "Return native focus from VIEW to its owning live Emacs window."
  (yunge-reader-webview--request-parent-focus view)
  (when-let* ((window (yunge-reader-webview--view-window view))
              ((window-live-p window)))
    (select-window window)
    (select-frame-set-input-focus (window-frame window))))

(defun yunge-reader-webview--request-parent-focus (view)
  "Ask live native VIEW to return keyboard focus to its parent frame."
  (when (and (integerp (yunge-reader-webview--view-id view))
             (yunge-reader-webview--surface-created-p view)
             (not (yunge-reader-webview--view-focus-release-pending view))
             (process-live-p yunge-reader-webview--process))
    (let ((id (yunge-reader-webview--view-id view)))
      (setf (yunge-reader-webview--view-focus-release-pending view) t)
      (yunge-reader-webview--request
       "view-focus-parent" `((view . ,id))
       (lambda (_result error-data)
         (when (yunge-reader-webview--surface-current-p view id)
           (setf
            (yunge-reader-webview--view-focus-release-pending view) nil)
           (if error-data
               (display-warning
                'yunge-reader
                (error-message-string error-data)
                :warning)
             (setf (yunge-reader-webview--view-native-focused view)
                   nil))))))
    t))

(defun yunge-reader-webview--record-native-focus (view focused)
  "Record whether VIEW has native focus and synchronize Emacs selection."
  (setf (yunge-reader-webview--view-native-focused view) focused
        (yunge-reader-webview--view-focus-release-pending view) nil)
  (when (and focused
             (window-live-p (yunge-reader-webview--view-window view))
             (not (eq (selected-window)
                      (yunge-reader-webview--view-window view))))
    (select-window (yunge-reader-webview--view-window view))))

(defun yunge-reader-webview--relay-owning-key (view key)
  "Return focus from VIEW and enqueue normalized Emacs KEY."
  (unless (member key '("SPC" "M-m"))
    (error "Invalid WebView owning key: %s" key))
  (yunge-reader-webview--focus-owning-window view)
  (setq unread-command-events
        (append (listify-key-sequence (kbd key))
                unread-command-events)))

(defun yunge-reader-webview--finish-outline-waiters
    (view outline error-data)
  "Complete VIEW's outline waiters with OUTLINE or ERROR-DATA."
  (let ((waiters
         (prog1 (yunge-reader-webview--view-outline-waiters view)
           (setf (yunge-reader-webview--view-outline-waiters view)
                 nil))))
    (dolist (complete waiters)
      (funcall complete (and outline (copy-tree outline)) error-data))))

(defun yunge-reader-webview--store-view-outline (view message)
  "Store VIEW's bounded outline from publication-ready MESSAGE."
  (let ((outline (yunge-reader-webview--event-outline message)))
    (setf (yunge-reader-webview--view-outline view) outline
          (yunge-reader-webview--view-outline-ready view) t
          (yunge-reader-webview--view-outline-error view) nil)
    (yunge-reader-webview--finish-outline-waiters view outline nil)))

(defun yunge-reader-webview--request-view-outline (view complete)
  "Invoke COMPLETE with VIEW's outline when its publication is ready."
  (unless (functionp complete)
    (error "Invalid EPUB outline completion: %S" complete))
  (cond
   ((or (null view)
        (yunge-reader-webview--view-destroyed view))
    (funcall complete nil
             '(error "The EPUB view is no longer live")))
   ((yunge-reader-webview--view-outline-ready view)
    (funcall complete
             (copy-tree (yunge-reader-webview--view-outline view))
             nil))
   ((yunge-reader-webview--view-outline-error view)
    (funcall complete nil
             (copy-tree
              (yunge-reader-webview--view-outline-error view))))
   (t
    (setf (yunge-reader-webview--view-outline-waiters view)
          (append
           (yunge-reader-webview--view-outline-waiters view)
           (list complete))))))

(defun yunge-reader-webview--install-hooks ()
  "Install hooks that keep native views aligned with Emacs windows."
  (add-hook 'window-size-change-functions
            #'yunge-reader-webview--sync-views)
  (add-hook 'window-state-change-functions
            #'yunge-reader-webview--sync-views)
  (add-hook 'window-buffer-change-functions
            #'yunge-reader-webview--sync-views)
  (add-hook 'window-selection-change-functions
            #'yunge-reader-webview--window-selection-changed))

(defun yunge-reader-webview--remove-hooks ()
  "Remove native view synchronization hooks."
  (remove-hook 'window-size-change-functions
               #'yunge-reader-webview--sync-views)
  (remove-hook 'window-state-change-functions
               #'yunge-reader-webview--sync-views)
  (remove-hook 'window-buffer-change-functions
               #'yunge-reader-webview--sync-views)
  (remove-hook 'window-selection-change-functions
               #'yunge-reader-webview--window-selection-changed))

(defun yunge-reader-webview--register-view (view)
  "Register logical VIEW and synchronize its native surface."
  (puthash view t yunge-reader-webview--logical-views)
  (yunge-reader-webview--install-hooks)
  (yunge-reader-webview--sync-view view)
  view)

(defun yunge-reader-webview--unregister-view (view)
  "Forget logical VIEW and remove global hooks when none remain."
  (remhash view yunge-reader-webview--logical-views)
  (when (zerop (hash-table-count
                yunge-reader-webview--logical-views))
    (yunge-reader-webview--remove-hooks)))

(defun yunge-reader-webview--visible-window (view)
  "Return VIEW's active presentation window, if any."
  (let ((buffer (yunge-reader-webview--view-buffer view)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (yunge-reader--presentation-window)))))

(defun yunge-reader-webview--sync-view (view)
  "Synchronize logical VIEW with its currently visible window."
  (unless (yunge-reader-webview--view-destroyed view)
    (let ((window (yunge-reader-webview--visible-window view))
          (current (yunge-reader-webview--view-window view))
          (id (yunge-reader-webview--view-id view)))
      (cond
       ((and window id (eq window current))
        (yunge-reader-webview--update-scroll-bar-mode view window)
        (setf (yunge-reader-webview--view-requested-bounds view)
              (yunge-reader-webview--window-bounds window))
        (yunge-reader-webview--send-latest-bounds view))
       ((and window id current
             (window-live-p current)
             (eq (window-frame window) (window-frame current)))
        (setf (yunge-reader-webview--view-window view) window)
        (yunge-reader-webview--update-scroll-bar-mode view window)
        (setf (yunge-reader-webview--view-requested-bounds view)
              (yunge-reader-webview--window-bounds window))
        (yunge-reader-webview--send-latest-bounds view))
       (id
        (yunge-reader-webview--release-surface view)
        (if window
            (yunge-reader-webview--start-surface view window)
          (unless (yunge-reader-webview--view-persistent view)
            (yunge-reader-webview--destroy-view view))))
       (window
        (yunge-reader-webview--start-surface view window))
       ((not (yunge-reader-webview--view-persistent view))
        (yunge-reader-webview--destroy-view view))))))

(defun yunge-reader-webview--sync-views (&rest _ignored)
  "Synchronize every logical view after an Emacs window change."
  (let (views)
    (maphash
     (lambda (view _present)
       (push view views))
     yunge-reader-webview--logical-views)
    (dolist (view views)
      (yunge-reader-webview--sync-view view))))

(defun yunge-reader-webview--window-selection-changed (&rest _ignored)
  "Move native surfaces and synchronize focus after window selection."
  (yunge-reader-webview--sync-views)
  (yunge-reader-webview--sync-native-focus))

(defun yunge-reader-webview--sync-native-focus (&rest _ignored)
  "Release any focused native child whose Emacs window is not selected."
  (maphash
   (lambda (view _present)
     (when (and (yunge-reader-webview--view-native-focused view)
                (not (eq (selected-window)
                         (yunge-reader-webview--view-window view))))
       (yunge-reader-webview--request-parent-focus view)))
   yunge-reader-webview--logical-views))

(defun yunge-reader-webview--close-owned-publication (publication)
  "Close PUBLICATION when the WebView helper is still live."
  (when (and publication
             (process-live-p yunge-reader-webview--process))
    (yunge-reader-webview--close-publication
     publication (lambda (_result _error-data)))))

(defun yunge-reader-webview--finish-view-destroy (view)
  "Finish permanent destruction of logical VIEW."
  (unless (yunge-reader-webview--view-destroy-finished view)
    (setf (yunge-reader-webview--view-destroy-finished view) t
          (yunge-reader-webview--view-pending-target view) nil
          (yunge-reader-webview--view-search-result view) nil)
    (yunge-reader-webview--finish-outline-waiters
     view nil '(error "The EPUB view was destroyed before its outline loaded"))
    (let ((publication
           (prog1 (yunge-reader-webview--view-publication view)
             (setf (yunge-reader-webview--view-publication view) nil))))
      (when (yunge-reader-webview--view-owns-publication view)
        (yunge-reader-webview--close-owned-publication publication)))
    (let ((waiters
           (prog1
               (yunge-reader-webview--view-destroy-waiters view)
             (setf (yunge-reader-webview--view-destroy-waiters view)
                   nil))))
      (dolist (complete waiters)
        (funcall complete)))))

(defun yunge-reader-webview--maybe-finish-view-destroy (view)
  "Finish destroyed VIEW after all obsolete surfaces are gone."
  (when (and (yunge-reader-webview--view-destroyed view)
             (null (yunge-reader-webview--view-id view))
             (null
              (yunge-reader-webview--view-pending-destroys view)))
    (yunge-reader-webview--finish-view-destroy view)))

(defun yunge-reader-webview--destroy-view (view &optional complete)
  "Destroy logical VIEW and invoke COMPLETE after its surface is gone."
  (cond
   ((yunge-reader-webview--view-destroy-finished view)
    (when complete
      (funcall complete)))
   ((yunge-reader-webview--view-destroyed view)
    (when complete
      (setf (yunge-reader-webview--view-destroy-waiters view)
            (append
             (yunge-reader-webview--view-destroy-waiters view)
             (list complete)))))
   (t
    (when complete
      (setf (yunge-reader-webview--view-destroy-waiters view)
            (list complete)))
    (setf (yunge-reader-webview--view-destroyed view) t)
    (yunge-reader-webview--unregister-view view)
    (yunge-reader-webview--release-surface view)
    (yunge-reader-webview--maybe-finish-view-destroy view)))
  view)

(defun yunge-reader-webview--forget-all-views ()
  "Forget all native views without sending protocol messages."
  (let (views)
    (maphash
     (lambda (view _present)
       (push view views))
     yunge-reader-webview--logical-views)
    (dolist (view views)
      (yunge-reader-webview--cancel-open-timer view)
      (yunge-reader-webview--set-surface-state view 'detached)
      (setf (yunge-reader-webview--view-id view) nil
            (yunge-reader-webview--view-window view) nil
            (yunge-reader-webview--view-native-focused view) nil
            (yunge-reader-webview--view-focus-release-pending view) nil
            (yunge-reader-webview--view-destroyed view) t
            (yunge-reader-webview--view-surface-style view) nil
            (yunge-reader-webview--view-surface-zoom view) nil
            (yunge-reader-webview--view-surface-scroll-bar-mode view) nil
            (yunge-reader-webview--view-pending-destroys view) nil)
      (yunge-reader-webview--set-view-selection view nil)
      (yunge-reader-webview--finish-view-destroy view)))
  (clrhash yunge-reader-webview--views)
  (clrhash yunge-reader-webview--logical-views)
  (yunge-reader-webview--remove-hooks))

(add-hook 'yunge-reader-webview-service-stopped-hook
          #'yunge-reader-webview--forget-all-views)

(defun yunge-reader-webview--kill-buffer ()
  "Destroy the native view owned by the current buffer."
  (when yunge-reader-webview--buffer-view
    (yunge-reader-webview--destroy-view
     yunge-reader-webview--buffer-view)))

(defun yunge-reader-webview--current-ready-view ()
  "Return the current buffer's ready EPUB WebView."
  (let ((view yunge-reader-webview--buffer-view))
    (unless (and view
                 (not (yunge-reader-webview--view-destroyed view))
                 (yunge-reader-webview--surface-ready-p view))
      (user-error "The current buffer has no ready EPUB view"))
    view))

(cl-defun yunge-reader-webview--attach-shared-publication
    (publication layout
     &key location location-changed-function selection-changed-function
     accelerator-function style zoom zoom-changed-function
     scroll-bar-function external-link-function)
  "Attach shared PUBLICATION with Reader LAYOUT to the current buffer.
Restore bounded LOCATION when supplied.  Invoke LOCATION-CHANGED-FUNCTION
with the logical view whenever its renderer reports a stable location.
Invoke SELECTION-CHANGED-FUNCTION whenever its logical selection changes.
Invoke ACCELERATOR-FUNCTION with the view and a normalized key when the
focused native child forwards one.  STYLE or ZOOM is copied into the view.
Invoke ZOOM-CHANGED-FUNCTION with the view and its effective fixed scale.
SCROLL-BAR-FUNCTION resolves its mode for the owning Emacs window.
Invoke EXTERNAL-LINK-FUNCTION with the view and a validated absolute URI."
  (unless (and (integerp publication) (> publication 0))
    (error "Invalid EPUB publication ID: %S" publication))
  (unless (memq layout '(fixed reflow))
    (error "Invalid EPUB publication layout: %S" layout))
  (when (and (eq layout 'fixed) style)
    (error "Fixed-layout EPUB views do not accept reflow style"))
  (when (and (eq layout 'reflow) zoom)
    (error "Reflowable EPUB views do not accept fixed zoom"))
  (when (and (eq layout 'reflow) zoom-changed-function)
    (error "Reflowable EPUB views do not report fixed zoom"))
  (when (eq layout 'fixed)
    (setq zoom
          (yunge-reader-webview--check-fixed-zoom
           (or zoom 'fit-page))))
  (when location
    (yunge-reader-webview--check-location location))
  (when style
    (yunge-reader-webview--check-style style))
  (when (and location-changed-function
             (not (functionp location-changed-function)))
    (error "Invalid EPUB location callback: %S"
           location-changed-function))
  (when (and selection-changed-function
             (not (functionp selection-changed-function)))
    (error "Invalid EPUB selection callback: %S"
           selection-changed-function))
  (when (and accelerator-function
             (not (functionp accelerator-function)))
    (error "Invalid EPUB accelerator callback: %S"
           accelerator-function))
  (when (and zoom-changed-function
             (not (functionp zoom-changed-function)))
    (error "Invalid EPUB zoom callback: %S"
           zoom-changed-function))
  (when (and scroll-bar-function
             (not (functionp scroll-bar-function)))
    (error "Invalid EPUB scroll bar callback: %S"
           scroll-bar-function))
  (when (and external-link-function
             (not (functionp external-link-function)))
    (error "Invalid EPUB external-link callback: %S"
           external-link-function))
  (when (and yunge-reader-webview--buffer-view
             (not (yunge-reader-webview--view-destroyed
                   yunge-reader-webview--buffer-view)))
    (error "Current buffer already owns an EPUB view"))
  (let ((view
         (yunge-reader-webview--make-view
          :buffer (current-buffer)
          :persistent t
          :publication publication
          :layout layout
          :style (and style (copy-tree style))
          :zoom zoom
          :location (and location (copy-tree location))
          :location-changed-function location-changed-function
          :selection-changed-function selection-changed-function
          :accelerator-function accelerator-function
          :zoom-changed-function zoom-changed-function
          :scroll-bar-function scroll-bar-function
          :external-link-function external-link-function)))
    (setq yunge-reader-webview--buffer-view view)
    (yunge-reader-webview--register-view view)
    view))

(defun yunge-reader-webview--detach-shared-publication
    (&optional complete)
  "Detach the current buffer's shared EPUB view and invoke COMPLETE."
  (let ((view yunge-reader-webview--buffer-view))
    (setq yunge-reader-webview--buffer-view nil)
    (if view
        (yunge-reader-webview--destroy-view view complete)
      (when complete
        (funcall complete)))))

(defun yunge-reader-webview--navigation-complete (_result error-data)
  "Report an asynchronous EPUB navigation ERROR-DATA."
  (when error-data
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

;;;###autoload
(defun yunge-reader-webview-previous-screen ()
  "Move the current EPUB spike view backward by one screen."
  (interactive)
  (yunge-reader-webview--navigate-view
   (yunge-reader-webview--current-ready-view)
   "previous-screen"
   #'yunge-reader-webview--navigation-complete))

;;;###autoload
(defun yunge-reader-webview-next-screen ()
  "Move the current EPUB spike view forward by one screen."
  (interactive)
  (yunge-reader-webview--navigate-view
   (yunge-reader-webview--current-ready-view)
   "next-screen"
   #'yunge-reader-webview--navigation-complete))

(defun yunge-reader-webview--publication-open-complete
    (view result error-data)
  "Finish opening VIEW's publication from native RESULT."
  (if error-data
      (progn
        (yunge-reader-webview--set-buffer-message
         view (error-message-string error-data))
        (yunge-reader-webview--destroy-view view)
        (display-warning
         'yunge-reader (error-message-string error-data) :warning))
    (let ((publication (alist-get 'publication result)))
      (unless (and (integerp publication) (> publication 0))
        (error "Malformed EPUB publication result: %S" result))
      (if (yunge-reader-webview--view-destroyed view)
          (yunge-reader-webview--close-owned-publication publication)
        (setf (yunge-reader-webview--view-publication view) publication
              (yunge-reader-webview--view-owns-publication view) t)
        (yunge-reader-webview--register-view view)))))

;;;###autoload
(defun yunge-reader-webview-spike (&optional window)
  "Embed a selectable reflowable WebView test page in WINDOW.
This command is an architecture spike, not an EPUB reader yet."
  (interactive)
  (unless (display-graphic-p)
    (user-error "The WebView spike requires a graphical display"))
  (unless (eq system-type 'windows-nt)
    (user-error "The current WebView spike supports Windows only"))
  (let* ((window (or window (selected-window)))
         (buffer
          (generate-new-buffer
           "*Yunge Reader WebView*"))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer)))
    (with-current-buffer buffer
      (yunge-reader-webview-spike-mode)
      (setq yunge-reader-webview--buffer-view view)
      (let ((inhibit-read-only t))
        (insert "Creating native WebView...\n")
        (set-buffer-modified-p nil)))
    (set-window-buffer window buffer)
    (yunge-reader-webview--register-view view)
    buffer))

;;;###autoload
(defun yunge-reader-webview-epub-spike
    (file &optional window location)
  "Open local EPUB FILE at LOCATION in a native child WebView in WINDOW.
This manual architecture spike does not register EPUB file associations or
save a durable reading position."
  (interactive "fEPUB file: ")
  (unless (display-graphic-p)
    (user-error "The EPUB WebView spike requires a graphical display"))
  (unless (eq system-type 'windows-nt)
    (user-error "The current EPUB WebView spike supports Windows only"))
  (setq file (expand-file-name file))
  (when (file-remote-p file)
    (user-error "The EPUB WebView spike accepts local files only"))
  (unless (and (file-regular-p file) (file-readable-p file))
    (user-error "EPUB file is not readable: %s" file))
  (when location
    (yunge-reader-webview--check-location location))
  (let* ((window (or window (selected-window)))
         (buffer
          (generate-new-buffer
           (format "*Yunge EPUB %s*" (file-name-nondirectory file))))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer
           :location (and location (copy-tree location))
           :path file)))
    (with-current-buffer buffer
      (yunge-reader-webview-spike-mode)
      (setq yunge-reader-webview--buffer-view view)
      (let ((inhibit-read-only t))
        (insert "Validating EPUB publication...\n")
        (set-buffer-modified-p nil)))
    (set-window-buffer window buffer)
    (yunge-reader-webview--open-publication
     file
     (apply-partially
      #'yunge-reader-webview--publication-open-complete view))
    buffer))

(provide 'yunge-reader-webview)

;;; yunge-reader-webview.el ends here
