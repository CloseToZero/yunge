;;; yunge-reader-pdf-backend.el --- PDF backend lifecycle -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'password-cache)
(require 'subr-x)
(require 'yunge-reader)
(require 'yunge-reader-native)
(require 'yunge-reader-pdf-protocol)

(defcustom yunge-reader-pdf-password-attempts 3
  "Maximum number of passwords prompted for one PDF open."
  :type 'natnum
  :group 'yunge-reader)


(cl-defstruct yunge-reader-pdf-handle
  "A process-local PDF document bound to one native helper session."
  session
  id
  identity
  recovering
  waiters
  closed)


(defun yunge-reader-pdf--password-cache-key (file)
  "Return the in-memory password cache key for PDF FILE."
  (list 'yunge-reader-pdf (file-truename file)))

(defun yunge-reader-pdf--password-error-p (error-data)
  "Return non-nil when ERROR-DATA requests a PDF password."
  (eq (car-safe error-data)
      'yunge-reader-native-pdf-password-error))

(defun yunge-reader-pdf--password-prompt-current-p
    (buffer generation window state)
  "Return whether a password prompt still belongs to BUFFER's open."
  (and (buffer-live-p buffer)
       (window-live-p window)
       (eq (selected-window) window)
       (not (active-minibuffer-window))
       (with-current-buffer buffer
         (and (= generation yunge-reader--open-generation)
              (yunge-reader--window-state-current-p window state)))))

(defun yunge-reader-pdf--open-properties (result)
  "Return Reader document properties represented by native RESULT."
  (list
   :layout 'fixed
   :metadata
   (list :page-count (alist-get 'page-count result)
         :pages (alist-get 'pages result))))

(defun yunge-reader-pdf--file-identity (file)
  "Return the stable local-file identity used to recover PDF FILE."
  (let* ((absolute (expand-file-name file))
         (attributes (file-attributes absolute 'string)))
    (list
     (or (ignore-errors (file-truename absolute)) absolute)
     (and attributes (file-attribute-size attributes))
     (and attributes (file-attribute-modification-time attributes))
     (and attributes (file-attribute-file-identifier attributes)))))

(defun yunge-reader-pdf--open-in-session
    (file session buffer generation window state complete
          &optional operation-task)
  "Open PDF FILE in native SESSION and call COMPLETE.
BUFFER, GENERATION, WINDOW, and STATE guard any password prompt.  COMPLETE
receives the raw native open result and nil, or nil and an error value."
  (let* ((key (yunge-reader-pdf--password-cache-key file))
         (cached-password (password-read-from-cache key))
         finished)
    (cl-labels
        ((finish (result error-data password owned)
           (unless finished
             (setq finished t)
             (when (and owned (stringp password))
               (if (and (not error-data) password-cache)
                   (password-cache-add key password)
                 (clear-string password)))
             (funcall complete result error-data)))
         (prompt (attempt last-error)
           (if (or (>= attempt yunge-reader-pdf-password-attempts)
                   (not
                    (yunge-reader-pdf--password-prompt-current-p
                     buffer generation window state)))
               (finish nil last-error nil nil)
             (condition-case prompt-error
                 (let ((password
                        (read-passwd
                         (if (zerop attempt)
                             (format "Password for %s: "
                                     (file-name-nondirectory file))
                           (format "Incorrect password for %s: "
                                   (file-name-nondirectory file))))))
                   (request password (1+ attempt) nil t))
               (quit
                (finish
                 nil '(error "PDF password entry cancelled") nil nil))
               (error (finish nil prompt-error nil nil)))))
         (request (password attempt cached owned)
           (condition-case request-error
               (let ((child
                      (yunge-reader-native-request-in-session
                       session
                       "open"
                       (append
                        (list (cons 'path file))
                        (when password (list (cons 'password password))))
                       (lambda (result native-error)
                         (cond
                          ((and native-error
                                (yunge-reader-pdf--password-error-p
                                 native-error))
                           (when cached
                             (password-cache-remove key))
                           (when (and owned (stringp password))
                             (clear-string password))
                           (prompt attempt native-error))
                          (native-error
                           (finish nil native-error password owned))
                          (t (finish result nil password owned)))))))
                 (when operation-task
                   (yunge-reader-task-adopt-child operation-task child))
                 child)
             (error (finish nil request-error password owned)))))
      (request cached-password 0 (and cached-password t) nil))))

(defun yunge-reader-pdf--open (file complete)
  "Open PDF FILE and call COMPLETE using the reader driver contract."
  (let* ((buffer (current-buffer))
         (operation-task yunge-reader--request-task)
         (generation yunge-reader--open-generation)
         (window (yunge-reader--place-window))
         (state (and window (yunge-reader--window-state window)))
         identity
         session
         acquired
         finished)
    (cl-labels
        ((finish (result error-data)
           (unless finished
             (setq finished t)
             (if error-data
                 (progn
                   (when acquired
                     (setq acquired nil)
                     (yunge-reader-native-release))
                   (funcall complete nil nil error-data))
               (funcall
                complete
                (make-yunge-reader-pdf-handle
                 :session session
                 :id (alist-get 'document result)
                 :identity identity)
                (yunge-reader-pdf--open-properties result)
                nil)))))
      (condition-case acquire-error
          (progn
            (setq identity (yunge-reader-pdf--file-identity file))
            (setq session (yunge-reader-native-acquire))
            (setq acquired t)
            (if operation-task
                (yunge-reader-pdf--open-in-session
                 file session buffer generation window state #'finish
                 operation-task)
              (yunge-reader-pdf--open-in-session
               file session buffer generation window state #'finish)))
        (error (finish nil acquire-error))))))


(defun yunge-reader-pdf--session-error-p (error-data)
  "Return whether ERROR-DATA reports a lost native helper session."
  (eq (car-safe error-data) 'yunge-reader-native-session-lost))

(defun yunge-reader-pdf--stopped-error-p (error-data)
  "Return whether ERROR-DATA reports an intentional helper stop."
  (eq (car-safe error-data) 'yunge-reader-native-session-stopped))

(defun yunge-reader-pdf--recovery-error (message)
  "Return a native-session recovery error containing MESSAGE."
  (list 'yunge-reader-native-session-lost message))

(defun yunge-reader-pdf--finish-recovery (handle error-data)
  "Finish HANDLE recovery and notify every waiter with ERROR-DATA."
  (let ((waiters (nreverse (yunge-reader-pdf-handle-waiters handle))))
    (setf (yunge-reader-pdf-handle-recovering handle) nil
          (yunge-reader-pdf-handle-waiters handle) nil)
    (dolist (waiter waiters)
      (condition-case callback-error
          (funcall waiter error-data)
        (error
         (display-warning
          'yunge-reader
          (format "PDF recovery callback failed: %s"
                  (error-message-string callback-error))
          :warning))))))

(defun yunge-reader-pdf--close-native (session id)
  "Best-effort close native PDF ID belonging to SESSION."
  (when (and (integerp id)
             (yunge-reader-native-session-live-p session))
    (condition-case nil
        (progn
          (yunge-reader-native-cancel-document-requests
           session id "The PDF document was closed")
          (yunge-reader-native-request-in-session
           session "close" (list (cons 'document id)) #'ignore))
      (error nil))))

(defun yunge-reader-pdf--owns-document-p (handle document)
  "Return whether HANDLE still belongs to live DOCUMENT."
  (and (not (yunge-reader-pdf-handle-closed handle))
       (yunge-reader--document-live-p document)))

(defun yunge-reader-pdf--recovery-context (document view)
  "Return password-prompt context for DOCUMENT, preferring VIEW."
  (let ((buffer (yunge-reader--document-view document view)))
    (if (not buffer)
        (list nil 0 nil nil)
      (with-current-buffer buffer
        (let ((window (yunge-reader--place-window)))
          (list
           buffer
           yunge-reader--open-generation
           window
           (and window (yunge-reader--window-state window))))))))

(defun yunge-reader-pdf--recover-complete
    (document handle session identity result error-data)
  "Finish reopening DOCUMENT's HANDLE in SESSION.
IDENTITY is the file identity accepted before the open request."
  (if error-data
      (yunge-reader-pdf--finish-recovery handle error-data)
    (let ((id (alist-get 'document result)))
      (condition-case recovery-error
          (progn
            (unless (integerp id)
              (error "Recovered PDF has an invalid native handle: %S" id))
            (unless (yunge-reader-pdf--owns-document-p handle document)
              (error "The Reader buffer closed during PDF recovery"))
            (unless
                (equal
                 identity
                 (yunge-reader-pdf--file-identity
                  (yunge-reader-document-file document)))
              (error "The PDF changed on disk during recovery"))
            (setf (yunge-reader-pdf-handle-session handle) session
                  (yunge-reader-pdf-handle-id handle) id)
            (yunge-reader-pdf--finish-recovery handle nil))
        (error
         (yunge-reader-pdf--close-native session id)
         (yunge-reader-pdf--finish-recovery
          handle recovery-error))))))

(defun yunge-reader-pdf--start-recovery (document handle view)
  "Start recovery for DOCUMENT and HANDLE on behalf of VIEW."
  (condition-case recovery-error
      (progn
        (unless (yunge-reader-pdf--owns-document-p handle document)
          (error "The Reader buffer no longer owns this PDF"))
        (let* ((file (yunge-reader-document-file document))
               (identity (yunge-reader-pdf--file-identity file)))
          (unless (equal identity
                         (yunge-reader-pdf-handle-identity handle))
            (error "The PDF changed on disk; close and reopen it"))
          (yunge-reader-native-start)
          (let* ((session (yunge-reader-native-current-session))
                 (context
                  (yunge-reader-pdf--recovery-context document view)))
            (unless session
              (error "The Yunge Reader helper did not start"))
            (apply
             #'yunge-reader-pdf--open-in-session
             file session
             (append
              context
              (list
               (lambda (result error-data)
                 (yunge-reader-pdf--recover-complete
                  document handle session identity result error-data))))))))
    (error
     (yunge-reader-pdf--finish-recovery handle recovery-error))))

(defun yunge-reader-pdf--ensure-handle (document view complete)
  "Call COMPLETE after DOCUMENT has a live native handle.
COMPLETE receives nil on success or an error value.  Concurrent recovery
requests for the same document share one native open.  VIEW owns any prompt."
  (let ((handle (yunge-reader-document-handle document)))
    (cond
     ((not (yunge-reader-pdf-handle-p handle))
      (funcall complete '(error "Invalid PDF document handle")))
     ((yunge-reader-pdf-handle-closed handle)
      (funcall
       complete
       (yunge-reader-pdf--recovery-error
        "The PDF document is already closed")))
     ((yunge-reader-native-session-live-p
       (yunge-reader-pdf-handle-session handle))
      (funcall complete nil))
     ((yunge-reader-pdf-handle-recovering handle)
      (push complete (yunge-reader-pdf-handle-waiters handle)))
     (t
      (setf (yunge-reader-pdf-handle-recovering handle) t
            (yunge-reader-pdf-handle-waiters handle) (list complete))
      (yunge-reader-pdf--start-recovery document handle view)))))

(defun yunge-reader-pdf--close (document)
  "Close PDF DOCUMENT and release its native service lease."
  (let ((released nil))
    (cl-labels
        ((release ()
           (unless released
             (setq released t)
             (yunge-reader-native-release))))
      (condition-case error-data
          (let ((handle (yunge-reader-document-handle document)))
            (unless (yunge-reader-pdf-handle-p handle)
              (error "Invalid PDF document handle: %S" handle))
            (unless (yunge-reader-pdf-handle-closed handle)
              (setf (yunge-reader-pdf-handle-closed handle) t)
              (when (yunge-reader-pdf-handle-recovering handle)
                (yunge-reader-pdf--finish-recovery
                 handle
                 (yunge-reader-pdf--recovery-error
                  "The PDF closed during native recovery")))
              (if (not
                   (yunge-reader-native-session-live-p
                    (yunge-reader-pdf-handle-session handle)))
                  (release)
                (let ((session
                       (yunge-reader-pdf-handle-session handle))
                      (id (yunge-reader-pdf-handle-id handle)))
                  (yunge-reader-native-cancel-document-requests
                   session id "The PDF document was closed")
                  (yunge-reader-native-request-in-session
                   session
                   "close"
                   (list (cons 'document id))
                   (lambda (_result _native-error)
                     (release)))))))
        (error
         (release)
         (signal (car error-data) (cdr error-data)))))))

(defun yunge-reader-pdf--native-request
    (session operation parameters complete revision)
  "Send one native PDF request, carrying REVISION when present."
  (if revision
      (yunge-reader-native-request-in-session
       session operation parameters complete :revision revision)
    (yunge-reader-native-request-in-session
     session operation parameters complete)))

(defun yunge-reader-pdf--dispatch
    (document operation arguments complete &optional operation-task)
  "Dispatch one PDF DOCUMENT OPERATION with ARGUMENTS to COMPLETE."
  (let* ((handle (yunge-reader-document-handle document))
         (valid (yunge-reader-pdf-handle-p handle))
         (session
          (and valid (yunge-reader-pdf-handle-session handle)))
         (id (and valid (yunge-reader-pdf-handle-id handle)))
         (revision
          (and (yunge-reader-task-p operation-task)
               (yunge-reader-task-revision operation-task))))
    (unless valid
      (error "Invalid PDF document handle: %S" handle))
    (pcase operation
      ('outline
       (yunge-reader-pdf--native-request
        session
        "outline"
        (list (cons 'document id))
        (lambda (result native-error)
          (funcall complete
                   (and result
                        (yunge-reader-pdf--native-outline
                         document result))
                   native-error))
        revision))
      ('page-info
       (yunge-reader-pdf--native-request
        session
        "page-info"
        (list (cons 'document id)
              (cons 'page (plist-get arguments :page)))
        complete
        revision))
      ('page-links
       (let ((page (plist-get arguments :page)))
         (yunge-reader-pdf--native-request
          session
          "page-links"
          (list (cons 'document id)
                (cons 'page page))
          (lambda (result native-error)
            (funcall complete
                     (and result
                          (yunge-reader-pdf--native-page-links
                           document page result))
                     native-error))
          revision)))
      ('page-text
       (yunge-reader-pdf--native-request
        session
        "page-text"
        (list (cons 'document id)
              (cons 'page (plist-get arguments :page)))
        complete
        revision))
      ('render-page
       (yunge-reader-pdf--native-request
        session
        "render-page"
        (list (cons 'document id)
              (cons 'page (plist-get arguments :page))
              (cons 'width (plist-get arguments :width))
              (cons 'appearance
                    (yunge-reader-pdf--native-appearance
                     (plist-get arguments :appearance)))
              (cons 'cache-key
                    (plist-get arguments :cache-key)))
        complete
        revision))
      ('search
       (let* ((direction (plist-get arguments :direction))
              (cursor-value (plist-get arguments :cursor))
              (cursor
               (yunge-reader-pdf--search-cursor-value cursor-value))
              (origin (plist-get arguments :origin)))
         (when (and cursor-value (null cursor))
           (error "Invalid generic PDF search cursor: %S" cursor-value))
         (when (and origin
                    (not
                     (yunge-reader-pdf--native-search-position-p origin)))
           (error "Invalid native PDF search origin: %S" origin))
         (unless (memq direction '(forward backward))
           (error "Invalid PDF search direction: %S" direction))
         (when (and origin cursor)
           (error "PDF search origin and cursor are mutually exclusive"))
         (yunge-reader-pdf--native-request
          session
          "search"
          (append
           (list
            (cons 'document id)
            (cons 'query (plist-get arguments :query))
            (cons 'case-sensitive
                  (if (plist-get arguments :case-sensitive) t :false))
            (cons 'direction (symbol-name direction))
            (cons 'match-limit (plist-get arguments :match-limit))
            (cons 'page-limit (plist-get arguments :page-limit)))
           (when origin
             (list (cons 'origin origin)))
           (when cursor
             (list (cons 'cursor cursor))))
          (lambda (result native-error)
            (funcall complete
                     (and result
                          (yunge-reader-pdf--native-search-batch result))
                     native-error))
          revision)))
      ('selection-text
       (let* ((start (plist-get arguments :start))
              (end (plist-get arguments :end))
              (cursor (plist-get arguments :cursor))
              (start-parameters
               (yunge-reader-pdf--indexed-position-parameters start))
              (end-parameters
               (yunge-reader-pdf--indexed-position-parameters end))
              (cursor-parameters
               (and cursor
                    (yunge-reader-pdf--indexed-position-parameters
                     cursor)))
              (unit-limit (plist-get arguments :unit-limit))
              (character-limit
               (plist-get arguments :character-limit)))
          (if (not (and start-parameters end-parameters
                        (or (null cursor) cursor-parameters)))
              (funcall
               complete nil
               (yunge-reader-pdf--native-error
                "PDF selection endpoints must be indexed positions"))
            (yunge-reader-pdf--native-request
             session
             "selection-text"
             (append
              (list (cons 'document id)
                    (cons 'start start-parameters)
                    (cons 'end end-parameters))
              (when cursor-parameters
                (list (cons 'cursor cursor-parameters)))
              (when unit-limit
                (list (cons 'page-limit unit-limit)))
              (when character-limit
                (list (cons 'character-limit character-limit))))
             (lambda (result native-error)
               (funcall complete
                        (and result
                             (yunge-reader-pdf--native-selection-batch
                              result))
                        native-error))
             revision))))
      (_
       (funcall
        complete nil
        (yunge-reader-pdf--native-error
         (format "Unsupported PDF operation: %S" operation)))))))

(defun yunge-reader-pdf--request
    (document operation arguments complete)
  "Dispatch a recoverable PDF DOCUMENT OPERATION to COMPLETE."
  (let ((view (current-buffer))
        (operation-task yunge-reader--request-task)
        finished)
    (cl-labels
        ((finish (value error-data)
           (unless finished
             (setq finished t)
             (funcall complete value error-data)))
         (send (retry)
           (when (or (null operation-task)
                     (yunge-reader-task-active-p operation-task))
             (yunge-reader-pdf--ensure-handle
              document view
              (lambda (recovery-error)
                (when (or (null operation-task)
                          (yunge-reader-task-active-p operation-task))
                  (if recovery-error
                      (finish nil recovery-error)
                    (condition-case request-error
                        (let ((child
                               (yunge-reader-pdf--dispatch
                                document operation arguments
                                (lambda (value error-data)
                                  (if (and retry
                                           (yunge-reader-pdf--session-error-p
                                            error-data))
                                      (send nil)
                                    (finish value error-data)))
                                operation-task)))
                          (when operation-task
                            (yunge-reader-task-adopt-child
                             operation-task child))
                          child)
                      (error (finish nil request-error))))))))))
      (send t))))

(defun yunge-reader-pdf--request-outline
    (document arguments complete)
  "Request DOCUMENT's outline with ARGUMENTS through COMPLETE."
  (yunge-reader-pdf--request document 'outline arguments complete))

(defun yunge-reader-pdf--request-selection-text-capability
    (document arguments complete)
  "Request selected text from DOCUMENT with ARGUMENTS through COMPLETE."
  (unless (yunge-reader-selection-text-request-p arguments)
    (error "Invalid PDF selection text request: %S" arguments))
  (yunge-reader-pdf--request
   document 'selection-text
   (list
    :start (yunge-reader-selection-text-request-start arguments)
    :end (yunge-reader-selection-text-request-end arguments)
    :cursor (yunge-reader-selection-text-request-cursor arguments)
    :unit-limit (yunge-reader-selection-text-request-unit-limit arguments)
    :character-limit
    (yunge-reader-selection-text-request-character-limit arguments))
   complete))

(provide 'yunge-reader-pdf-backend)

;;; yunge-reader-pdf-backend.el ends here
