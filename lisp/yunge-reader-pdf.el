;;; yunge-reader-pdf.el --- PDF reader -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'password-cache)
(require 'seq)
(require 'svg)
(require 'subr-x)
(require 'yunge-key)
(require 'yunge-reader)
(require 'yunge-reader-native)

(defcustom yunge-reader-pdf-page-margin 24
  "Pixel margin reserved around a rendered PDF page."
  :type 'natnum
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-page-gap 16
  "Vertical pixel gap between pages in the continuous PDF roll."
  :type 'natnum
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-center-pages t
  "Whether to center PDF pages in their Reader window."
  :type 'boolean
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-prefetch-pages 1
  "Number of pages to prefetch on each side of the visible PDF roll."
  :type 'natnum
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-selection-color "#f6d32d"
  "Color painted behind selected PDF characters."
  :type 'color
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-selection-opacity 0.38
  "Opacity used to paint selected PDF characters."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-search-color "#ff7800"
  "Color painted behind the current PDF search match."
  :type 'color
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-search-opacity 0.46
  "Opacity used to paint the current PDF search match."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-password-attempts 3
  "Maximum number of passwords prompted for one PDF open."
  :type 'natnum
  :group 'yunge-reader)

(defconst yunge-reader-pdf--points-to-pixels (/ 96.0 72.0)
  "Nominal conversion from PDF points to screen pixels at scale one.")

(defconst yunge-reader-pdf-link-maximum-items 4096
  "Maximum number of links accepted from one PDF page response.")

(cl-defstruct yunge-reader-pdf-handle
  "A process-local PDF document bound to one native helper session."
  session
  id
  identity
  recovering
  waiters
  closed)

(cl-defstruct yunge-reader-pdf-link
  "One PDF page link with disposable hit geometry."
  page
  index
  bounds
  label
  action)

(cl-defstruct yunge-reader-pdf-link-data
  "One bounded page of PDF links."
  page
  links
  truncated)

(cl-defstruct yunge-reader-pdf--prefetch-task
  "One replaceable, low-priority PDF prefetch task."
  document
  kind
  page
  width
  generation)

(defvar-local yunge-reader-pdf-page 0
  "Zero-based page currently displayed in the PDF adapter.")

(defvar-local yunge-reader-pdf--generation 0
  "Generation used to reject late PDF rendering completions.")

(defvar-local yunge-reader-pdf--page-infos nil
  "Vector of canonical geometry for every PDF page.")

(defvar-local yunge-reader-pdf--page-positions nil
  "Vector mapping zero-based PDF pages to buffer positions.")

(defvar-local yunge-reader-pdf--render-results nil
  "Cache mapping page and pixel width to native render results.")

(defvar-local yunge-reader-pdf--render-pending nil
  "Map page and width render keys to request generations in flight.")

(defvar-local yunge-reader-pdf--displayed-pages nil
  "Pages currently painted as images in any live view window.")

(defvar-local yunge-reader-pdf--updating-visible nil
  "Non-nil while PDF roll virtualization is updating display slots.")

(defvar-local yunge-reader-pdf--pending-location nil
  "Stable PDF position waiting for a live viewport window.")

(defvar-local yunge-reader-pdf--resize-timer nil
  "Idle timer coalescing changes to the current PDF viewport size.")

(defvar-local yunge-reader-pdf--pending-resize nil
  "Latest PDF viewport resize waiting for redisplay to settle.")

(defvar-local yunge-reader-pdf--text-cache nil
  "Page-indexed cache of canonical PDF text geometry.")

(defvar-local yunge-reader-pdf--text-pending nil
  "Page-indexed set of outstanding PDF text requests.")

(defvar-local yunge-reader-pdf--link-cache nil
  "Page-indexed cache of PDF links.")

(defvar-local yunge-reader-pdf--link-pending nil
  "Page-indexed map of callbacks awaiting PDF links.")

(defvar-local yunge-reader-pdf--link-activation-generation 0
  "Generation used to reject late interactive PDF link completions.")

(defvar-local yunge-reader-pdf--working-pages nil
  "Pages retained by the current visible PDF working set.")

(defvar-local yunge-reader-pdf--prefetch-queue nil
  "Replaceable PDF prefetch tasks waiting behind the active task.")

(defvar-local yunge-reader-pdf--prefetch-active nil
  "The one PDF prefetch task currently owned by the native helper.")

(defvar-local yunge-reader-pdf--prefetch-running nil
  "Non-nil while the PDF prefetch scheduler is dispatching tasks.")

(defvar-keymap yunge-reader-pdf--image-map
  "<mouse-1>" #'yunge-reader-pdf-select-at-mouse
  "C-<mouse-1>" #'yunge-reader-pdf-activate-at-mouse
  "<drag-mouse-1>" #'yunge-reader-pdf-select-with-mouse)

(defconst yunge-reader-pdf-normal-bindings
  '(("RET" yunge-reader-pdf-follow-link "follow link")
    ("C-d" yunge-reader-pdf-scroll-down "scroll down")
    ("C-u" yunge-reader-pdf-scroll-up "scroll up")
    ("G" yunge-reader-pdf-last-page "last page")
    ("J" yunge-reader-pdf-next-page "next page")
    ("K" yunge-reader-pdf-previous-page "previous page")
    ("gg" yunge-reader-pdf-first-page "first page")
    ("gp" yunge-reader-pdf-goto-page "go to page")
    ("gr" yunge-reader-refresh "refresh")
    ("j" yunge-reader-pdf-scroll-down-line "scroll down one line")
    ("k" yunge-reader-pdf-scroll-up-line "scroll up one line"))
  "Normal-state bindings for the PDF view adapter.")

(defvar-keymap yunge-reader-pdf-view-mode-map
  "RET" #'yunge-reader-pdf-follow-link
  "C-d" #'yunge-reader-pdf-scroll-down
  "C-u" #'yunge-reader-pdf-scroll-up
  "G" #'yunge-reader-pdf-last-page
  "J" #'yunge-reader-pdf-next-page
  "K" #'yunge-reader-pdf-previous-page
  "<next>" #'scroll-up-command
  "<prior>" #'scroll-down-command
  "g g" #'yunge-reader-pdf-first-page
  "g p" #'yunge-reader-pdf-goto-page
  "g r" #'yunge-reader-refresh
  "j" #'yunge-reader-pdf-scroll-down-line
  "k" #'yunge-reader-pdf-scroll-up-line)

(define-minor-mode yunge-reader-pdf-view-mode
  "Display a fixed-layout PDF through the Yunge Reader PDF driver."
  :init-value nil
  :lighter " PDF"
  :keymap yunge-reader-pdf-view-mode-map
  (if yunge-reader-pdf-view-mode
      (progn
        (setq-local yunge-reader-pdf-page 0)
        (setq-local line-spacing yunge-reader-pdf-page-gap)
        (setq-local yunge-reader-pdf--render-results
                    (make-hash-table :test #'equal))
        (setq-local yunge-reader-pdf--render-pending
                    (make-hash-table :test #'equal))
        (setq-local yunge-reader-pdf--text-cache
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--text-pending
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--link-cache
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--link-pending
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--link-activation-generation 0)
        (setq-local yunge-reader-pdf--working-pages nil)
        (setq-local yunge-reader-pdf--prefetch-queue nil)
        (setq-local yunge-reader-pdf--prefetch-active nil)
        (setq-local yunge-reader-pdf--prefetch-running nil)
        (setq-local yunge-reader-pdf--pending-location nil)
        (setq-local yunge-reader-pdf--resize-timer nil)
        (setq-local yunge-reader-pdf--pending-resize nil)
        (add-hook 'yunge-reader-refresh-hook
                  #'yunge-reader-pdf--refresh nil t)
        (add-hook 'yunge-reader-view-role-change-hook
                  #'yunge-reader-pdf--update-header nil t)
        (add-hook 'yunge-reader-search-result-hook
                  #'yunge-reader-pdf--search-result-changed nil t)
        (add-hook 'window-size-change-functions
                  #'yunge-reader-pdf--window-size-change nil t)
        (add-hook 'window-scroll-functions
                  #'yunge-reader-pdf--window-scrolled nil t)
        (add-hook 'kill-buffer-hook
                  #'yunge-reader-pdf--cancel-resize nil t))
    (yunge-reader-pdf--cancel-resize)
    (remove-hook 'yunge-reader-refresh-hook
                 #'yunge-reader-pdf--refresh t)
    (remove-hook 'yunge-reader-view-role-change-hook
                 #'yunge-reader-pdf--update-header t)
    (remove-hook 'yunge-reader-search-result-hook
                 #'yunge-reader-pdf--search-result-changed t)
    (remove-hook 'window-size-change-functions
                 #'yunge-reader-pdf--window-size-change t)
    (remove-hook 'window-scroll-functions
                 #'yunge-reader-pdf--window-scrolled t)
    (remove-hook 'kill-buffer-hook
                 #'yunge-reader-pdf--cancel-resize t)
    (kill-local-variable 'line-spacing)
    (setq yunge-reader-pdf--page-infos nil
          yunge-reader-pdf--page-positions nil
          yunge-reader-pdf--render-results nil
          yunge-reader-pdf--render-pending nil
          yunge-reader-pdf--displayed-pages nil
          yunge-reader-pdf--pending-location nil
          yunge-reader-pdf--resize-timer nil
          yunge-reader-pdf--pending-resize nil
          yunge-reader-pdf--text-cache nil
          yunge-reader-pdf--text-pending nil
          yunge-reader-pdf--link-cache nil
          yunge-reader-pdf--link-pending nil
          yunge-reader-pdf--link-activation-generation 0
          yunge-reader-pdf--working-pages nil
          yunge-reader-pdf--prefetch-queue nil
          yunge-reader-pdf--prefetch-active nil
          yunge-reader-pdf--prefetch-running nil)))

(with-eval-after-load 'evil
  (yunge-key-evil-define-minor-mode
   'normal 'yunge-reader-pdf-view-mode
   yunge-reader-pdf-normal-bindings))

(defun yunge-reader-pdf--match-p (file)
  "Return whether FILE has a PDF extension."
  (string-equal (downcase (or (file-name-extension file) "")) "pdf"))

(defun yunge-reader-pdf--native-error (message)
  "Return an Emacs error value containing MESSAGE."
  (list 'error message))

(defun yunge-reader-pdf--native-position (value)
  "Return a stable reader position represented by native VALUE."
  (let ((page (alist-get 'page value))
        (offset (alist-get 'offset value)))
    (when (and (natnump page) (natnump offset))
      (make-yunge-reader-position :unit page :offset offset))))

(defun yunge-reader-pdf--native-search-result (value)
  "Return a generic search result represented by native VALUE."
  (let ((start
         (yunge-reader-pdf--native-position
          (alist-get 'start value)))
        (end
         (yunge-reader-pdf--native-position
          (alist-get 'end value))))
    (when (and start end)
      (make-yunge-reader-search-result
       :start start
       :end end
       :text (alist-get 'text value)
       :before (alist-get 'before value)
       :after (alist-get 'after value)))))

(defun yunge-reader-pdf--native-search-batch (value)
  "Return a generic search batch represented by native VALUE."
  (let ((results
         (mapcar #'yunge-reader-pdf--native-search-result
                 (alist-get 'matches value)))
        (cursor-value (alist-get 'cursor value)))
    (when (cl-every #'identity results)
      (make-yunge-reader-search-batch
       :results results
       :cursor (and cursor-value
                    (yunge-reader-pdf--native-position cursor-value))
       :done (eq (alist-get 'done value) t)))))

(defun yunge-reader-pdf--native-selection-batch (value)
  "Return a generic selection batch represented by native VALUE."
  (let ((cursor-value (alist-get 'cursor value)))
    (make-yunge-reader-selection-batch
     :text (alist-get 'text value)
     :cursor (and cursor-value
                  (yunge-reader-pdf--native-position cursor-value))
     :done (eq (alist-get 'done value) t))))

(defun yunge-reader-pdf--indexed-position-parameters (position)
  "Return native indexed parameters for reader POSITION, or nil."
  (let ((page (and (yunge-reader-position-p position)
                   (yunge-reader-position-unit position)))
        (offset (and (yunge-reader-position-p position)
                     (yunge-reader-position-offset position))))
    (when (and (natnump page) (natnump offset))
      (list (cons 'page page) (cons 'offset offset)))))

(defun yunge-reader-pdf--outline-page-info (document page)
  "Return metadata for zero-based PAGE in PDF DOCUMENT."
  (let* ((metadata (yunge-reader-document-metadata document))
         (pages (plist-get metadata :pages)))
    (and (natnump page)
         (listp pages)
         (< page (length pages))
         (nth page pages))))

(defun yunge-reader-pdf--outline-view-state (destination)
  "Return generic zoom mode and scale for native DESTINATION."
  (let ((view (alist-get 'view destination))
        (zoom (alist-get 'zoom destination)))
    (pcase view
      ("xyz"
       (if (and (numberp zoom) (> zoom 0))
           (cons 'manual zoom)
         '(nil)))
      ((or "fit" "fit-bounds") '(fit-page))
      ((or "fit-horizontal" "fit-bounds-horizontal")
       '(fit-width))
      (_ '(nil)))))

(defun yunge-reader-pdf--native-location-action
    (document destination)
  "Return a generic location action for PDF DESTINATION in DOCUMENT."
  (let* ((page (alist-get 'page destination))
         (page-info
          (yunge-reader-pdf--outline-page-info document page))
         (height (and page-info (alist-get 'height page-info)))
         (x (alist-get 'x destination))
         (y (alist-get 'y destination))
         (view-state
          (yunge-reader-pdf--outline-view-state destination)))
    (when (and page-info
               (numberp height)
               (> height 0)
               (or (null x) (numberp x))
               (or (null y) (numberp y)))
      (make-yunge-reader-action
       :type 'location
       :position
       (make-yunge-reader-position
        :unit page
        :x (or x 0.0)
        :y (or y height))
       :zoom-mode (car view-state)
       :scale (cdr view-state)))))

(defun yunge-reader-pdf--native-outline-item (document value)
  "Return a generic outline item represented by native VALUE."
  (let ((title (alist-get 'title value))
        (depth (alist-get 'depth value))
        (destination (alist-get 'destination value)))
    (when (and (stringp title)
               (not (string-empty-p (string-trim title)))
               (natnump depth)
               (or (null destination)
                   (listp destination)))
      (let ((action
             (and destination
                  (yunge-reader-pdf--native-location-action
                   document destination))))
        (when (or (null destination) action)
          (make-yunge-reader-outline-item
           :title title
           :depth depth
           :action action))))))

(defun yunge-reader-pdf--native-outline (document value)
  "Return a generic PDF outline represented by native VALUE."
  (let* ((items-entry (assq 'items value))
         (native-items (cdr items-entry))
         (items
          (and (listp native-items)
               (mapcar
                (lambda (item)
                  (yunge-reader-pdf--native-outline-item
                   document item))
                native-items))))
    (when (and items-entry
               (listp native-items)
               (cl-every #'identity items))
      (make-yunge-reader-outline-data
       :items items
       :truncated (eq (alist-get 'truncated value) t)))))

(defun yunge-reader-pdf--native-link-bounds (value)
  "Return validated canonical PDF link bounds represented by VALUE."
  (let ((left (alist-get 'left value))
        (bottom (alist-get 'bottom value))
        (right (alist-get 'right value))
        (top (alist-get 'top value)))
    (when (and (numberp left)
               (numberp bottom)
               (numberp right)
               (numberp top)
               (< left right)
               (< bottom top))
      (list
       (cons 'left left)
       (cons 'bottom bottom)
       (cons 'right right)
       (cons 'top top)))))

(defun yunge-reader-pdf--native-link-action (document value)
  "Return a generic Reader action represented by native link VALUE."
  (let ((action
         (pcase (alist-get 'type value)
           ("location"
            (when-let* ((destination
                         (alist-get 'destination value)))
              (yunge-reader-pdf--native-location-action
               document destination)))
           ("uri"
            (make-yunge-reader-action
             :type 'uri
             :uri (alist-get 'uri value))))))
    (and (yunge-reader--action-valid-p action) action)))

(defun yunge-reader-pdf--native-page-link
    (document page index value)
  "Return PAGE link INDEX represented by native VALUE in DOCUMENT."
  (let ((bounds
         (yunge-reader-pdf--native-link-bounds
          (alist-get 'bounds value)))
        (native-action (alist-get 'action value))
        (label (alist-get 'label value)))
    (when (and bounds
               (listp native-action)
               (or (null label)
                   (and (stringp label)
                        (not (string-empty-p label)))))
      (when-let* ((action
                   (yunge-reader-pdf--native-link-action
                    document native-action)))
        (make-yunge-reader-pdf-link
         :page page
         :index index
         :bounds bounds
         :label label
         :action action)))))

(defun yunge-reader-pdf--native-page-links
    (document expected-page value)
  "Return PDF links for EXPECTED-PAGE represented by native VALUE."
  (let* ((page (alist-get 'page value))
         (links-entry (assq 'links value))
         (native-links (cdr links-entry))
         (links
          (and (listp native-links)
               (cl-loop
                for item in native-links
                for index from 0
                collect
                (yunge-reader-pdf--native-page-link
                 document page index item)))))
    (when (and (eql page expected-page)
               links-entry
               (listp native-links)
               (<= (length native-links)
                   yunge-reader-pdf-link-maximum-items)
               (cl-every #'identity links))
      (make-yunge-reader-pdf-link-data
       :page page
       :links links
       :truncated (eq (alist-get 'truncated value) t)))))

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
    (file session buffer generation window state complete)
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
               (yunge-reader-native-request-in-session
                session
                "open"
                (append
                 (list (cons 'path file))
                 (when password (list (cons 'password password))))
                (lambda (result native-error)
                  (cond
                   ((and native-error
                         (yunge-reader-pdf--password-error-p native-error))
                    (when cached
                      (password-cache-remove key))
                    (when (and owned (stringp password))
                      (clear-string password))
                    (prompt attempt native-error))
                   (native-error
                    (finish nil native-error password owned))
                   (t (finish result nil password owned)))))
             (error (finish nil request-error password owned)))))
      (request cached-password 0 (and cached-password t) nil))))

(defun yunge-reader-pdf--open (file complete)
  "Open PDF FILE and call COMPLETE using the reader driver contract."
  (let* ((buffer (current-buffer))
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
            (yunge-reader-pdf--open-in-session
             file session buffer generation window state #'finish))
        (error (finish nil acquire-error))))))

(defun yunge-reader-pdf--attach (_document)
  "Attach the PDF view adapter to the current Reader buffer."
  (yunge-reader-pdf-view-mode 1))

(defun yunge-reader-pdf--detach (_document)
  "Detach the PDF view adapter from the current Reader buffer."
  (yunge-reader-pdf-view-mode -1))

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
        (yunge-reader-native-request-in-session
         session "close" (list (cons 'document id)) #'ignore)
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
                (yunge-reader-native-request-in-session
                 (yunge-reader-pdf-handle-session handle)
                 "close"
                 (list
                  (cons 'document
                        (yunge-reader-pdf-handle-id handle)))
                 (lambda (_result _native-error)
                   (release))))))
        (error
         (release)
         (signal (car error-data) (cdr error-data)))))))

(defun yunge-reader-pdf--dispatch
    (document operation arguments complete)
  "Dispatch one PDF DOCUMENT OPERATION with ARGUMENTS to COMPLETE."
  (let* ((handle (yunge-reader-document-handle document))
         (valid (yunge-reader-pdf-handle-p handle))
         (session
          (and valid (yunge-reader-pdf-handle-session handle)))
         (id (and valid (yunge-reader-pdf-handle-id handle))))
    (unless valid
      (error "Invalid PDF document handle: %S" handle))
    (pcase operation
      ('outline
       (yunge-reader-native-request-in-session
        session
        "outline"
        (list (cons 'document id))
        (lambda (result native-error)
          (funcall complete
                   (and result
                        (yunge-reader-pdf--native-outline
                         document result))
                   native-error))))
      ('page-info
       (yunge-reader-native-request-in-session
        session
        "page-info"
        (list (cons 'document id)
              (cons 'page (plist-get arguments :page)))
        complete))
      ('page-links
       (let ((page (plist-get arguments :page)))
         (yunge-reader-native-request-in-session
          session
          "page-links"
          (list (cons 'document id)
                (cons 'page page))
          (lambda (result native-error)
            (funcall complete
                     (and result
                          (yunge-reader-pdf--native-page-links
                           document page result))
                     native-error)))))
      ('page-text
       (yunge-reader-native-request-in-session
        session
        "page-text"
        (list (cons 'document id)
              (cons 'page (plist-get arguments :page)))
        complete))
      ('render-page
       (yunge-reader-native-request-in-session
        session
        "render-page"
        (list (cons 'document id)
              (cons 'page (plist-get arguments :page))
              (cons 'width (plist-get arguments :width))
              (cons 'cache-key
                    (plist-get arguments :cache-key)))
        complete))
      ('search
       (let ((cursor (plist-get arguments :cursor)))
         (yunge-reader-native-request-in-session
          session
          "search"
          (append
           (list
            (cons 'document id)
            (cons 'query (plist-get arguments :query))
            (cons 'case-sensitive
                  (if (plist-get arguments :case-sensitive) t :false))
            (cons 'match-limit (plist-get arguments :match-limit))
            (cons 'page-limit (plist-get arguments :page-limit)))
           (when cursor
             (list
              (cons
               'cursor
               (list
                (cons 'page (yunge-reader-position-unit cursor))
                (cons 'offset
                      (yunge-reader-position-offset cursor)))))))
          (lambda (result native-error)
            (funcall complete
                     (and result
                          (yunge-reader-pdf--native-search-batch result))
                     native-error)))))
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
            (yunge-reader-native-request-in-session
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
                        native-error))))))
      (_
       (funcall
        complete nil
        (yunge-reader-pdf--native-error
         (format "Unsupported PDF operation: %S" operation)))))))

(defun yunge-reader-pdf--request
    (document operation arguments complete)
  "Dispatch a recoverable PDF DOCUMENT OPERATION to COMPLETE."
  (let ((view (current-buffer))
        finished)
    (cl-labels
        ((finish (value error-data)
           (unless finished
             (setq finished t)
             (funcall complete value error-data)))
         (send (retry)
           (yunge-reader-pdf--ensure-handle
            document view
            (lambda (recovery-error)
              (if recovery-error
                  (finish nil recovery-error)
                (condition-case request-error
                    (yunge-reader-pdf--dispatch
                     document operation arguments
                     (lambda (value error-data)
                       (if (and retry
                                (yunge-reader-pdf--session-error-p
                                 error-data))
                           (send nil)
                         (finish value error-data))))
                  (error (finish nil request-error))))))))
      (send t))))

(defun yunge-reader-pdf-register ()
  "Register the PDF driver."
  (yunge-reader-register-driver
   'pdf
   :match #'yunge-reader-pdf--match-p
   :open #'yunge-reader-pdf--open
   :close #'yunge-reader-pdf--close
   :attach #'yunge-reader-pdf--attach
   :detach #'yunge-reader-pdf--detach
   :request #'yunge-reader-pdf--request
   :location #'yunge-reader-pdf--location
   :restore #'yunge-reader-pdf--restore-location))

;;;###autoload
(defun yunge-reader-pdf-mode ()
  "Read the PDF visited by the current buffer with Yunge Reader."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a PDF file"))
  (yunge-reader-pdf-register)
  (yunge-reader-visit-file buffer-file-name))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.pdf\\'" . yunge-reader-pdf-mode))

;;;###autoload
(defun yunge-reader-pdf-open (file)
  "Open PDF FILE explicitly with Yunge Reader."
  (interactive "fRead PDF: ")
  (yunge-reader-pdf-register)
  (yunge-reader-open file))

(defun yunge-reader-pdf--page-count ()
  "Return the current PDF page count, or zero."
  (or
   (and yunge-reader-document
        (plist-get
         (yunge-reader-document-metadata yunge-reader-document)
         :page-count))
   0))

(defun yunge-reader-pdf--page-info (page)
  "Return canonical geometry for zero-based PDF PAGE."
  (and (vectorp yunge-reader-pdf--page-infos)
       (natnump page)
       (< page (length yunge-reader-pdf--page-infos))
       (aref yunge-reader-pdf--page-infos page)))

(defun yunge-reader-pdf--load-page-infos ()
  "Load and validate page geometry from the current document metadata."
  (let* ((metadata
          (yunge-reader-document-metadata yunge-reader-document))
         (count (plist-get metadata :page-count))
         (pages (plist-get metadata :pages)))
    (unless (and (natnump count)
                 (listp pages)
                 (= (length pages) count))
      (error "PDF page geometry metadata is incomplete"))
    (setq yunge-reader-pdf--page-infos (vconcat pages))))

(defun yunge-reader-pdf--viewport-window ()
  "Return a live window suitable for measuring the current PDF view."
  (or (get-buffer-window (current-buffer) t)
      (selected-window)))

(defun yunge-reader-pdf--target-width
    (page-info &optional window suppress-scale)
  "Return target pixel width for PAGE-INFO in WINDOW.
When SUPPRESS-SCALE is non-nil, do not update the shared effective scale."
  (let* ((window (or window (yunge-reader-pdf--viewport-window)))
         (page-width (alist-get 'width page-info))
         (page-height (alist-get 'height page-info))
         (margin (* 2 yunge-reader-pdf-page-margin))
         (available-width
          (max 16 (- (window-body-width window t) margin)))
         (available-height
          (max 16 (- (window-body-height window t) margin)))
         (width
          (pcase yunge-reader-zoom-mode
            ('fit-width available-width)
            ('fit-page
             (min available-width
                  (floor
                   (* available-height
                      (/ page-width page-height)))))
            (_
             (round
              (* page-width
                 yunge-reader-pdf--points-to-pixels
                 yunge-reader-scale))))))
    (setq width (max 16 (min 8192 width)))
    (unless suppress-scale
      (yunge-reader-set-effective-scale
       (/ width
          (* page-width yunge-reader-pdf--points-to-pixels))))
    width))

(defun yunge-reader-pdf--pixel-size (page-info width)
  "Return the rendered pixel size for PAGE-INFO at WIDTH."
  (let ((page-width (alist-get 'width page-info))
        (page-height (alist-get 'height page-info)))
    (unless (and (numberp page-width) (> page-width 0)
                 (numberp page-height) (> page-height 0))
      (error "PDF page geometry must be positive"))
    (cons width (max 1 (round (* width (/ page-height page-width)))))))

(defun yunge-reader-pdf--display-width (page)
  "Return the pixel width currently painted for PAGE, or nil."
  (when-let* ((position (yunge-reader-pdf--page-position page))
              (_ (< position (point-max)))
              (width
               (get-text-property
                position 'yunge-reader-pdf-display-width)))
    (and (natnump width) (> width 0) width)))

(defun yunge-reader-pdf--location (_document window)
  "Return the stable PDF position visible at the top left of WINDOW."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (> (yunge-reader-pdf--page-count) 0)
             yunge-reader-pdf--page-positions)
    (let* ((page
            (or (yunge-reader-pdf--page-at-position
                 (window-start window))
                yunge-reader-pdf-page))
           (page
            (max 0 (min (1- (yunge-reader-pdf--page-count)) page)))
           (page-info (yunge-reader-pdf--page-info page))
           (width
            (or (yunge-reader-pdf--display-width page)
                (yunge-reader-pdf--page-width page window)))
           (size (yunge-reader-pdf--pixel-size page-info width))
           (pixel-width (car size))
           (pixel-height (cdr size))
           (page-width (alist-get 'width page-info))
           (page-height (alist-get 'height page-info))
           (vertical
            (max 0 (min pixel-height
                        (or (window-vscroll window t) 0))))
           (column-width
            (max 1 (frame-char-width (window-frame window))))
           (horizontal
            (max 0
                 (min pixel-width
                      (* (window-hscroll window) column-width)))))
      (make-yunge-reader-position
       :unit page
       :x (* page-width (/ (float horizontal) pixel-width))
       :y (* page-height
             (- 1.0 (/ (float vertical) pixel-height)))))))

(defun yunge-reader-pdf--apply-pending-location (window)
  "Apply a pending stable PDF location to live WINDOW."
  (when (and yunge-reader-pdf--pending-location
             (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (> (yunge-reader-pdf--page-count) 0)
             yunge-reader-pdf--page-positions)
    (let* ((location yunge-reader-pdf--pending-location)
           (page
            (max
             0
             (min (1- (yunge-reader-pdf--page-count))
                  (yunge-reader-position-unit location))))
           (page-info (yunge-reader-pdf--page-info page))
           (page-width (alist-get 'width page-info))
           (page-height (alist-get 'height page-info))
           (width (yunge-reader-pdf--page-width page window))
           (size (yunge-reader-pdf--pixel-size page-info width))
           (pixel-width (car size))
           (pixel-height (cdr size))
           (x
            (max 0.0
                 (min page-width
                      (or (yunge-reader-position-x location) 0.0))))
           (y
            (max 0.0
                 (min page-height
                      (or (yunge-reader-position-y location)
                          page-height))))
           (body-width (window-body-width window t))
           (body-height (window-body-height window t))
           (vertical
            (max
             0
             (min (max 0 (- pixel-height body-height))
                  (round
                   (* pixel-height (- 1.0 (/ y page-height)))))))
           (horizontal-pixels
            (max
             0
             (min (max 0 (- pixel-width body-width))
                  (round (* pixel-width (/ x page-width))))))
           (column-width
            (max 1 (frame-char-width (window-frame window))))
           (position (yunge-reader-pdf--page-position page)))
      (setq yunge-reader-pdf--pending-location nil
            yunge-reader-pdf-page page)
      (goto-char position)
      (set-window-start window position t)
      (set-window-vscroll window vertical t)
      (set-window-hscroll
       window (floor (/ (float horizontal-pixels) column-width)))
      t)))

(defun yunge-reader-pdf--restore-location
    (_document location window)
  "Accept stable PDF LOCATION for restoration in WINDOW."
  (when (and (yunge-reader-position-p location)
             (natnump (yunge-reader-position-unit location))
             (> (yunge-reader-pdf--page-count) 0)
             yunge-reader-pdf--page-positions)
    (let* ((page
            (max
             0
             (min (1- (yunge-reader-pdf--page-count))
                  (yunge-reader-position-unit location))))
           (position (yunge-reader-pdf--page-position page))
           (restored (copy-yunge-reader-position location))
           (live-window (yunge-reader--place-window window)))
      (setf (yunge-reader-position-unit restored) page)
      (setq yunge-reader-pdf--pending-location restored
            yunge-reader-pdf-page page)
      (goto-char position)
      (when live-window
        (set-window-start live-window position t))
      (yunge-reader-pdf--update-visible-pages live-window)
      t)))

(defun yunge-reader-pdf--cache-key (page width)
  "Return an immutable render cache key for PAGE at WIDTH."
  (let* ((file (yunge-reader-document-file yunge-reader-document))
         (attributes (file-attributes file 'string)))
    (secure-hash
     'sha256
     (prin1-to-string
      (list
       (file-truename file)
       (file-attribute-size attributes)
       (float-time (file-attribute-modification-time attributes))
       page width
       yunge-reader-native-pdfium-api
       (yunge-reader-native--build-id))))))

(defun yunge-reader-pdf--position-before-p (left right)
  "Return non-nil when document position LEFT precedes RIGHT."
  (let ((left-unit (yunge-reader-position-unit left))
        (right-unit (yunge-reader-position-unit right))
        (left-offset (yunge-reader-position-offset left))
        (right-offset (yunge-reader-position-offset right)))
    (or (< left-unit right-unit)
        (and (= left-unit right-unit)
             (<= left-offset right-offset)))))

(defun yunge-reader-pdf--ordered-selection ()
  "Return the current selection endpoints in document order."
  (when yunge-reader-selection
    (yunge-reader-pdf--ordered-range
     (yunge-reader-selection-start yunge-reader-selection)
     (yunge-reader-selection-end yunge-reader-selection))))

(defun yunge-reader-pdf--ordered-range (start end)
  "Return document positions START and END in document order."
  (if (yunge-reader-pdf--position-before-p start end)
      (cons start end)
    (cons end start)))

(defun yunge-reader-pdf--range-offsets
    (page text-layer start end)
  "Return inclusive START through END offsets on PAGE and TEXT-LAYER."
  (pcase-let* ((`(,start . ,end)
                (yunge-reader-pdf--ordered-range start end))
               (start-page (yunge-reader-position-unit start))
               (end-page (yunge-reader-position-unit end))
               (characters (alist-get 'characters text-layer)))
    (when (and (<= start-page page)
               (<= page end-page)
               characters)
      (cons
       (if (= page start-page)
           (yunge-reader-position-offset start)
         0)
       (if (= page end-page)
           (yunge-reader-position-offset end)
         (apply #'max
                (mapcar
                 (lambda (character)
                   (alist-get 'index character))
                 characters)))))))

(defun yunge-reader-pdf--selection-offsets (page text-layer)
  "Return selected inclusive offsets for PAGE and TEXT-LAYER."
  (when-let* ((selection (yunge-reader-pdf--ordered-selection)))
    (yunge-reader-pdf--range-offsets
     page text-layer (car selection) (cdr selection))))

(defun yunge-reader-pdf--search-offsets (page text-layer)
  "Return current search match offsets for PAGE and TEXT-LAYER."
  (when yunge-reader-search-result
    (yunge-reader-pdf--range-offsets
     page text-layer
     (yunge-reader-search-result-start yunge-reader-search-result)
     (yunge-reader-search-result-end yunge-reader-search-result))))

(defun yunge-reader-pdf--convex-quad-p (quad)
  "Return non-nil when QUAD is strictly convex and outline-ordered."
  (let ((orientation 0)
        (valid t))
    (dotimes (index 4)
      (let* ((first (nth index quad))
             (second (nth (mod (1+ index) 4) quad))
             (third (nth (mod (+ index 2) 4) quad))
             (cross
              (- (* (- (alist-get 'x second)
                       (alist-get 'x first))
                    (- (alist-get 'y third)
                       (alist-get 'y second)))
                 (* (- (alist-get 'y second)
                       (alist-get 'y first))
                    (- (alist-get 'x third)
                       (alist-get 'x second))))))
        (if (< (abs cross) 0.000001)
            (setq valid nil)
          (let ((sign (if (> cross 0) 1 -1)))
            (if (= orientation 0)
                (setq orientation sign)
              (unless (= orientation sign)
                (setq valid nil)))))))
    valid))

(defun yunge-reader-pdf--quad-points (character)
  "Return CHARACTER's valid canonical quadrilateral, or nil."
  (let ((quad (alist-get 'quad character)))
    (when (and (listp quad)
               (ignore-errors (= (length quad) 4))
               (cl-every
                (lambda (point)
                  (and (listp point)
                       (numberp (alist-get 'x point))
                       (numberp (alist-get 'y point))))
                quad)
               (yunge-reader-pdf--convex-quad-p quad))
      quad)))

(defun yunge-reader-pdf--svg-quad
    (quad page-width page-height pixel-width pixel-height)
  "Project canonical QUAD to SVG coordinates."
  (mapcar
   (lambda (point)
     (cons
      (* pixel-width
         (/ (alist-get 'x point) page-width))
      (* pixel-height
         (/ (- page-height (alist-get 'y point))
            page-height))))
   quad))

(defun yunge-reader-pdf--paint-bounds
    (svg bounds page-width page-height pixel-width pixel-height
         &optional color opacity)
  "Paint canonical BOUNDS onto SVG."
  (let* ((left (alist-get 'left bounds))
         (bottom (alist-get 'bottom bounds))
         (right (alist-get 'right bounds))
         (top (alist-get 'top bounds))
         (x (* pixel-width (/ left page-width)))
         (y (* pixel-height (/ (- page-height top) page-height)))
         (width
          (max 1 (* pixel-width (/ (- right left) page-width))))
         (height
          (max 1 (* pixel-height (/ (- top bottom) page-height)))))
    (svg-rectangle
     svg x y width height
     :fill-color (or color yunge-reader-pdf-selection-color)
     :fill-opacity (or opacity yunge-reader-pdf-selection-opacity))))

(defun yunge-reader-pdf--paint-character
    (svg character page-width page-height pixel-width pixel-height
         &optional color opacity)
  "Paint CHARACTER geometry onto SVG."
  (if-let* ((quad (yunge-reader-pdf--quad-points character)))
      (svg-polygon
       svg
       (yunge-reader-pdf--svg-quad
        quad page-width page-height pixel-width pixel-height)
       :fill-color (or color yunge-reader-pdf-selection-color)
       :fill-opacity (or opacity yunge-reader-pdf-selection-opacity))
    (when-let* ((bounds (alist-get 'bounds character)))
      (yunge-reader-pdf--paint-bounds
       svg bounds page-width page-height pixel-width pixel-height
       color opacity))))

(defun yunge-reader-pdf--paint-range
    (svg range text-layer page-info pixel-width pixel-height color opacity)
  "Paint inclusive text RANGE onto SVG with COLOR and OPACITY."
  (let ((page-width (alist-get 'width page-info))
        (page-height (alist-get 'height page-info)))
    (dolist (character (alist-get 'characters text-layer))
      (let ((index (alist-get 'index character)))
        (when (and (<= (car range) index)
                   (<= index (cdr range))
                   (not (alist-get 'generated character)))
          (yunge-reader-pdf--paint-character
           svg character page-width page-height
           pixel-width pixel-height color opacity))))))

(defun yunge-reader-pdf--paint-selection
    (svg page page-info text-layer pixel-width pixel-height)
  "Paint PAGE selection onto SVG using PAGE-INFO and TEXT-LAYER."
  (when-let* ((range
               (yunge-reader-pdf--selection-offsets page text-layer)))
    (yunge-reader-pdf--paint-range
     svg range text-layer page-info pixel-width pixel-height
     yunge-reader-pdf-selection-color
     yunge-reader-pdf-selection-opacity)))

(defun yunge-reader-pdf--paint-search
    (svg page page-info text-layer pixel-width pixel-height)
  "Paint PAGE's current search match onto SVG."
  (when-let* ((range
               (yunge-reader-pdf--search-offsets page text-layer)))
    (yunge-reader-pdf--paint-range
     svg range text-layer page-info pixel-width pixel-height
     yunge-reader-pdf-search-color yunge-reader-pdf-search-opacity)))

(defun yunge-reader-pdf--render-key (page width)
  "Return the in-memory render key for PAGE and WIDTH."
  (cons page width))

(defun yunge-reader-pdf--nearest-render-entry (page width)
  "Return the nearest cached render entry for PAGE at WIDTH.
The returned value has the render key as its car and the native result as its
cdr."
  (when (hash-table-p yunge-reader-pdf--render-results)
    (let (nearest nearest-distance)
      (maphash
       (lambda (key result)
         (when (and (consp key)
                    (eql (car key) page)
                    (natnump (cdr key)))
           (let ((distance (abs (- (cdr key) width))))
             (when (or (null nearest-distance)
                       (< distance nearest-distance))
               (setq nearest (cons key result)
                     nearest-distance distance)))))
       yunge-reader-pdf--render-results)
      nearest)))

(defun yunge-reader-pdf--display-image-object (page width &optional entry)
  "Return an Emacs image object for PAGE displayed at WIDTH.
Use the nearest cached render ENTRY while an exact render is unavailable."
  (let* ((entry
          (or entry
              (yunge-reader-pdf--nearest-render-entry page width)))
         (render-key (car-safe entry))
         (result (cdr-safe entry))
         (path (alist-get 'path result))
         (page-info (yunge-reader-pdf--page-info page))
         (fallback
          (and (consp render-key) (/= (cdr render-key) width)))
         (target-size
          (and page-info
               (yunge-reader-pdf--pixel-size page-info width)))
         (pixel-width
          (if fallback
              (car-safe target-size)
            (alist-get 'pixel-width result)))
         (pixel-height
          (if fallback
              (cdr-safe target-size)
            (alist-get 'pixel-height result)))
         (text-layer
          (and yunge-reader-pdf--text-cache
               (gethash page yunge-reader-pdf--text-cache))))
    (when path
      (if (and text-layer
               pixel-width
               pixel-height
               (or (yunge-reader-pdf--selection-offsets page text-layer)
                   (yunge-reader-pdf--search-offsets page text-layer)))
          (let ((svg (svg-create pixel-width pixel-height)))
            (svg-embed svg path "image/png" nil
                       :x 0 :y 0
                       :width pixel-width
                       :height pixel-height)
            (yunge-reader-pdf--paint-selection
             svg page page-info text-layer pixel-width pixel-height)
            (yunge-reader-pdf--paint-search
             svg page page-info text-layer pixel-width pixel-height)
            (svg-image svg))
        (if fallback
            (and pixel-width pixel-height
                 (create-image path nil nil
                               :width pixel-width
                               :height pixel-height
                               :transform-smoothing t))
          (create-image path nil nil))))))

(defun yunge-reader-pdf--page-width (page &optional window)
  "Return the target render width for PAGE in WINDOW."
  (yunge-reader-pdf--target-width
   (yunge-reader-pdf--page-info page)
   window
   (/= page yunge-reader-pdf-page)))

(defun yunge-reader-pdf--placeholder (page width)
  "Return a stable placeholder display for PAGE at WIDTH."
  (pcase-let ((`(,_ . ,height)
               (yunge-reader-pdf--pixel-size
                (yunge-reader-pdf--page-info page) width)))
    `(space . (:width (,width) :height (,height)))))

(defun yunge-reader-pdf--page-prefix (width)
  "Return the line prefix that centers a PDF display of WIDTH pixels."
  (when yunge-reader-pdf-center-pages
    `(space :align-to (- center (,(/ width 2))))))

(defun yunge-reader-pdf--page-position (page)
  "Return the buffer position holding zero-based PDF PAGE."
  (and (vectorp yunge-reader-pdf--page-positions)
       (natnump page)
       (< page (length yunge-reader-pdf--page-positions))
       (aref yunge-reader-pdf--page-positions page)))

(defun yunge-reader-pdf--search-page-p (page)
  "Return non-nil when the current search result intersects PAGE."
  (when yunge-reader-search-result
    (let* ((range
            (yunge-reader-pdf--ordered-range
             (yunge-reader-search-result-start
              yunge-reader-search-result)
             (yunge-reader-search-result-end
              yunge-reader-search-result)))
           (start-page (yunge-reader-position-unit (car range)))
           (end-page (yunge-reader-position-unit (cdr range))))
      (and (<= start-page page) (<= page end-page)))))

(defun yunge-reader-pdf--character-center (character)
  "Return canonical center of CHARACTER geometry, or nil."
  (if-let* ((quad (yunge-reader-pdf--quad-points character)))
      (cons
       (/ (apply #'+
                 (mapcar (lambda (point) (alist-get 'x point)) quad))
          4.0)
       (/ (apply #'+
                 (mapcar (lambda (point) (alist-get 'y point)) quad))
          4.0))
    (when-let* ((bounds (alist-get 'bounds character)))
      (cons
       (/ (+ (alist-get 'left bounds)
             (alist-get 'right bounds))
          2.0)
       (/ (+ (alist-get 'bottom bounds)
             (alist-get 'top bounds))
          2.0)))))

(defun yunge-reader-pdf--search-character (page text-layer)
  "Return the first drawable search character on PAGE's TEXT-LAYER."
  (when-let* ((range
               (yunge-reader-pdf--search-offsets page text-layer)))
    (seq-find
     (lambda (character)
       (let ((index (alist-get 'index character)))
         (and (<= (car range) index)
              (<= index (cdr range))
              (not (alist-get 'generated character))
              (yunge-reader-pdf--character-center character))))
     (alist-get 'characters text-layer))))

(defun yunge-reader-pdf--scroll-to-search-result ()
  "Align the current PDF search result inside its reader window."
  (when yunge-reader-search-result
    (let* ((page
            (yunge-reader-position-unit
             (yunge-reader-search-result-start
              yunge-reader-search-result)))
           (text-layer
            (and yunge-reader-pdf--text-cache
                 (gethash page yunge-reader-pdf--text-cache)))
           (character
            (and text-layer
                 (yunge-reader-pdf--search-character page text-layer)))
           (center
            (and character
                 (yunge-reader-pdf--character-center character)))
           (page-info (and center (yunge-reader-pdf--page-info page)))
           (position (and center (yunge-reader-pdf--page-position page)))
           (window (get-buffer-window (current-buffer) t)))
      (when (and (= page yunge-reader-pdf-page)
                 page-info position (window-live-p window))
        (let* ((width (yunge-reader-pdf--page-width page window))
               (size (yunge-reader-pdf--pixel-size page-info width))
               (pixel-width (car size))
               (pixel-height (cdr size))
               (page-width (alist-get 'width page-info))
               (page-height (alist-get 'height page-info))
               (target-x (* pixel-width (/ (car center) page-width)))
               (target-y
                (* pixel-height
                   (- 1.0 (/ (cdr center) page-height))))
               (body-width (window-body-width window t))
               (body-height (window-body-height window t))
               (vertical
                (max 0
                     (min (max 0 (- pixel-height body-height))
                          (round (- target-y (/ body-height 3.0))))))
               (horizontal-pixels
                (max 0
                     (min (max 0 (- pixel-width body-width))
                          (round (- target-x (/ body-width 3.0))))))
               (column-width
                (max 1 (frame-char-width (window-frame window)))))
          (goto-char position)
          (set-window-start window position t)
          (set-window-vscroll window vertical t)
          (set-window-hscroll
           window
           (floor (/ (float horizontal-pixels) column-width))))))))

(defun yunge-reader-pdf--search-result-changed ()
  "Repaint and visit the current PDF search result."
  (if (not yunge-reader-search-result)
      (when yunge-reader-pdf--displayed-pages
        (yunge-reader-pdf--paint-pages
         yunge-reader-pdf--displayed-pages))
    (let ((page
           (yunge-reader-position-unit
            (yunge-reader-search-result-start
             yunge-reader-search-result))))
      (when (and (natnump page)
                 (< page (yunge-reader-pdf--page-count)))
        (yunge-reader-pdf--set-page page)
        (yunge-reader-pdf--request-text page)
        (yunge-reader-pdf--paint-pages
         yunge-reader-pdf--displayed-pages)
        (yunge-reader-pdf--scroll-to-search-result)))))

(defun yunge-reader-pdf--paint-page (page &optional width)
  "Paint PAGE at WIDTH, using its current width when WIDTH is nil."
  (when-let* ((position (yunge-reader-pdf--page-position page)))
    (let* ((width
            (or width
                (yunge-reader-pdf--display-width page)
                (yunge-reader-pdf--page-width page)))
           (entry (yunge-reader-pdf--nearest-render-entry page width))
           (display
            (if (and entry
                     (memq page yunge-reader-pdf--displayed-pages))
                (condition-case image-error
                    (or (yunge-reader-pdf--display-image-object
                         page width entry)
                        (error "Emacs rejected the rendered PDF image"))
                  (error
                   (display-warning
                    'yunge-reader
                    (format "Could not display PDF page %d: %s"
                            (1+ page)
                            (error-message-string image-error))
                    :warning)
                   (yunge-reader-pdf--placeholder page width)))
              (yunge-reader-pdf--placeholder page width)))
            (inhibit-read-only t))
      (with-silent-modifications
        (add-text-properties
         position (1+ position)
         (list 'display display
               'line-prefix (yunge-reader-pdf--page-prefix width)
               'yunge-reader-pdf-display-width width))))))

(defun yunge-reader-pdf--paint-pages (pages &optional window)
  "Paint PAGES for WINDOW and virtualize all former live images."
  (let ((former yunge-reader-pdf--displayed-pages))
    (setq yunge-reader-pdf--displayed-pages pages)
    (dolist (page (cl-remove-duplicates (append former pages)))
      (yunge-reader-pdf--paint-page
       page
       (and window (yunge-reader-pdf--page-width page window))))))

(defun yunge-reader-pdf--build-roll ()
  "Build one stable buffer slot for every PDF page."
  (let* ((count (yunge-reader-pdf--page-count))
         (positions (make-vector count nil))
         (inhibit-read-only t))
    (erase-buffer)
    (dotimes (page count)
      (aset positions page (point))
      (insert
       (propertize
        " "
        'yunge-reader-pdf-page page
        'keymap yunge-reader-pdf--image-map
        'pointer 'text
        'help-echo
        (concat
         "Mouse-1 selects text; Ctrl-Mouse-1 follows a link; "
         "drag selects across pages")))
      (unless (= page (1- count))
        (insert (propertize "\n" 'yunge-reader-pdf-page page))))
    (setq yunge-reader-pdf--page-positions positions)
    (set-buffer-modified-p nil)))

(defun yunge-reader-pdf--page-at-position (position)
  "Return the PDF page associated with buffer POSITION."
  (when (integer-or-marker-p position)
    (get-text-property position 'yunge-reader-pdf-page)))

(defun yunge-reader-pdf--window-pages (window)
  "Return PDF pages intersecting live WINDOW."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let ((position (window-start window))
          (end (or (window-end window t) (point-max)))
          pages)
      (let ((limit (min (point-max) (1+ end))))
        (while (< position limit)
          (when-let* ((page
                       (yunge-reader-pdf--page-at-position position)))
            (cl-pushnew page pages))
          (setq position
                (or (next-single-property-change
                     position 'yunge-reader-pdf-page nil limit)
                    limit))))
      (nreverse pages))))

(defun yunge-reader-pdf--visible-pages ()
  "Return sorted PDF pages visible in any window for this buffer."
  (let (pages)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (dolist (page (yunge-reader-pdf--window-pages window))
        (cl-pushnew page pages)))
    (sort (or pages (list yunge-reader-pdf-page)) #'<)))

(defun yunge-reader-pdf--prefetch-range (pages)
  "Expand visible PAGES by `yunge-reader-pdf-prefetch-pages'."
  (let ((count (yunge-reader-pdf--page-count))
        expanded)
    (dolist (page pages)
      (cl-loop
       for candidate from (- page yunge-reader-pdf-prefetch-pages)
       to (+ page yunge-reader-pdf-prefetch-pages)
       when (and (>= candidate 0) (< candidate count))
       do (cl-pushnew candidate expanded)))
    (sort expanded #'<)))

(defun yunge-reader-pdf--prefetch-task-same-p (left right)
  "Return whether prefetch tasks LEFT and RIGHT describe the same work."
  (and left right
       (eq (yunge-reader-pdf--prefetch-task-document left)
           (yunge-reader-pdf--prefetch-task-document right))
       (eq (yunge-reader-pdf--prefetch-task-kind left)
           (yunge-reader-pdf--prefetch-task-kind right))
       (eql (yunge-reader-pdf--prefetch-task-page left)
            (yunge-reader-pdf--prefetch-task-page right))
       (equal (yunge-reader-pdf--prefetch-task-width left)
              (yunge-reader-pdf--prefetch-task-width right))))

(defun yunge-reader-pdf--retain-page-p (page)
  "Return whether PAGE belongs to the current in-memory working set."
  (or (null yunge-reader-pdf--working-pages)
      (memq page yunge-reader-pdf--working-pages)))

(defun yunge-reader-pdf--retain-render-p (page width)
  "Return whether PAGE at WIDTH belongs to the current render set."
  (or (null yunge-reader-pdf--working-pages)
      (and (memq page yunge-reader-pdf--working-pages)
           (yunge-reader-pdf--page-info page)
           (= width
              (or (yunge-reader-pdf--display-width page)
                  (yunge-reader-pdf--page-width page))))))

(defun yunge-reader-pdf--prune-cache (table retain)
  "Remove entries from hash TABLE unless RETAIN accepts their key."
  (when (hash-table-p table)
    (let (removed)
      (maphash
       (lambda (key _value)
         (unless (funcall retain key)
           (push key removed)))
       table)
      (dolist (key removed)
        (remhash key table)))))

(defun yunge-reader-pdf--prune-working-set (pages tasks)
  "Retain only PAGES and their current render TASKS in memory."
  (let ((render-keys (make-hash-table :test #'equal))
        (render-widths (make-hash-table :test #'eql)))
    (dolist (task tasks)
      (when (eq (yunge-reader-pdf--prefetch-task-kind task) 'render)
        (let ((page (yunge-reader-pdf--prefetch-task-page task))
              (width (yunge-reader-pdf--prefetch-task-width task)))
          (puthash (yunge-reader-pdf--render-key page width)
                   t render-keys)
          (puthash page width render-widths))))
    ;; Keep one old render only until each working page has its exact target.
    (maphash
     (lambda (page width)
       (unless (gethash (yunge-reader-pdf--render-key page width)
                        yunge-reader-pdf--render-results)
         (when-let* ((entry
                      (yunge-reader-pdf--nearest-render-entry page width)))
           (puthash (car entry) t render-keys))))
     render-widths)
    (yunge-reader-pdf--prune-cache
     yunge-reader-pdf--render-results
     (lambda (key) (gethash key render-keys)))
    (yunge-reader-pdf--prune-cache
     yunge-reader-pdf--text-cache
     (lambda (page) (memq page pages)))
    (yunge-reader-pdf--prune-cache
     yunge-reader-pdf--link-cache
     (lambda (page) (memq page pages)))))

(defun yunge-reader-pdf--prune-page-renders (page width)
  "Retain PAGE's WIDTH render and leave other pages unchanged."
  (yunge-reader-pdf--prune-cache
   yunge-reader-pdf--render-results
   (lambda (key)
     (or (/= (car key) page)
         (= (cdr key) width)))))

(defun yunge-reader-pdf--prefetch-task-needed-p (task)
  "Return whether TASK is still useful to the current PDF view."
  (let ((document
         (yunge-reader-pdf--prefetch-task-document task))
        (kind (yunge-reader-pdf--prefetch-task-kind task))
        (page (yunge-reader-pdf--prefetch-task-page task))
        (width (yunge-reader-pdf--prefetch-task-width task)))
    (and yunge-reader-pdf-view-mode
         (eq document yunge-reader-document)
         (memq page yunge-reader-pdf--working-pages)
         (pcase kind
           ('render
            (not
             (gethash (yunge-reader-pdf--render-key page width)
                      yunge-reader-pdf--render-results)))
           ('text (not (gethash page yunge-reader-pdf--text-cache)))
           ('links (not (gethash page yunge-reader-pdf--link-cache)))
           (_ nil)))))

(defun yunge-reader-pdf--dispatch-prefetch-task (task)
  "Dispatch low-priority PDF prefetch TASK and return its state."
  (pcase (yunge-reader-pdf--prefetch-task-kind task)
    ('render
     (yunge-reader-pdf--request-render
      (yunge-reader-pdf--prefetch-task-generation task)
      (yunge-reader-pdf--prefetch-task-page task)
      (yunge-reader-pdf--prefetch-task-width task)))
    ('text
     (yunge-reader-pdf--request-text
      (yunge-reader-pdf--prefetch-task-page task)))
    ('links
     (yunge-reader-pdf--request-links
      (yunge-reader-pdf--prefetch-task-page task)))
    (_ 'cached)))

(defun yunge-reader-pdf--run-prefetch ()
  "Dispatch at most one current PDF prefetch task."
  (unless yunge-reader-pdf--prefetch-running
    (let ((yunge-reader-pdf--prefetch-running t))
      (while (and (not yunge-reader-pdf--prefetch-active)
                  yunge-reader-pdf--prefetch-queue)
        (let ((task (pop yunge-reader-pdf--prefetch-queue)))
          (when (yunge-reader-pdf--prefetch-task-needed-p task)
            (setq yunge-reader-pdf--prefetch-active task)
            (condition-case error-data
                (when
                    (eq (yunge-reader-pdf--dispatch-prefetch-task task)
                        'cached)
                  (setq yunge-reader-pdf--prefetch-active nil))
              (error
               (setq yunge-reader-pdf--prefetch-active nil)
               (display-warning
                'yunge-reader
                (format "Could not prefetch PDF page %d: %s"
                        (1+ (yunge-reader-pdf--prefetch-task-page task))
                        (error-message-string error-data))
                :warning)))))))))

(defun yunge-reader-pdf--finish-prefetch
    (document kind page &optional width error-data)
  "Finish DOCUMENT prefetch for KIND, PAGE, WIDTH, and ERROR-DATA."
  (let ((task yunge-reader-pdf--prefetch-active))
    (when (and task
               (eq document
                   (yunge-reader-pdf--prefetch-task-document task))
               (eq kind (yunge-reader-pdf--prefetch-task-kind task))
               (eql page (yunge-reader-pdf--prefetch-task-page task))
               (or (not (eq kind 'render))
                   (equal width
                          (yunge-reader-pdf--prefetch-task-width task))))
      (setq yunge-reader-pdf--prefetch-active nil)
      (if (yunge-reader-pdf--stopped-error-p error-data)
          (setq yunge-reader-pdf--prefetch-queue nil)
        (yunge-reader-pdf--run-prefetch)))))

(defun yunge-reader-pdf--prefetch-tasks (pages &optional window)
  "Return image-first background tasks for PDF PAGES in WINDOW."
  (let ((document yunge-reader-document)
        (generation yunge-reader-pdf--generation))
    (append
     (mapcar
      (lambda (page)
        (make-yunge-reader-pdf--prefetch-task
         :document document
         :kind 'render
         :page page
         :width (yunge-reader-pdf--page-width page window)
         :generation generation))
      pages)
     (mapcar
      (lambda (page)
        (make-yunge-reader-pdf--prefetch-task
         :document document :kind 'text :page page))
      pages)
     (mapcar
      (lambda (page)
        (make-yunge-reader-pdf--prefetch-task
         :document document :kind 'links :page page))
      pages))))

(defun yunge-reader-pdf--update-header ()
  "Update the continuous PDF roll header."
  (let ((role
         (pcase (yunge-reader-view-role)
           ('primary "Primary")
           ('additional "Additional")
           (_ "Reader"))))
    (setq header-line-format
          (format " %s  Page %d/%d  %.0f%%  Continuous "
                  role
                  (1+ yunge-reader-pdf-page)
                  (yunge-reader-pdf--page-count)
                  (* 100 yunge-reader-effective-scale)))))

(defun yunge-reader-pdf--render-complete
    (buffer document generation page width result error-data)
  "Store one rendered PAGE result in BUFFER for GENERATION and WIDTH."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unwind-protect
          (when (eq document yunge-reader-document)
            (let* ((render-key
                    (yunge-reader-pdf--render-key page width))
                   (pending
                    (and
                     (hash-table-p yunge-reader-pdf--render-pending)
                     (gethash render-key
                              yunge-reader-pdf--render-pending))))
              (when (and (eq (car-safe pending) document)
                         (eql (cdr-safe pending) generation))
                (remhash render-key yunge-reader-pdf--render-pending)))
            (if error-data
                (when (and (not
                            (yunge-reader-pdf--stopped-error-p
                             error-data))
                           yunge-reader-pdf-view-mode
                           (yunge-reader-pdf--retain-render-p page width))
                  (display-warning
                   'yunge-reader
                   (format "Could not render PDF page %d: %s"
                           (1+ page)
                           (error-message-string error-data))
                   :warning))
              (when (and
                     (hash-table-p yunge-reader-pdf--render-results)
                     (yunge-reader-pdf--retain-render-p page width))
                (puthash (yunge-reader-pdf--render-key page width)
                         result yunge-reader-pdf--render-results)
                (yunge-reader-pdf--prune-page-renders page width))
              (when (and yunge-reader-pdf-view-mode
                         (yunge-reader-pdf--retain-render-p page width)
                         (memq page yunge-reader-pdf--displayed-pages))
                (yunge-reader-pdf--paint-page page width)
                (when (yunge-reader-pdf--search-page-p page)
                  (yunge-reader-pdf--scroll-to-search-result)))))
        (yunge-reader-pdf--finish-prefetch
         document 'render page width error-data)))))

(defun yunge-reader-pdf--request-render
    (generation page &optional width)
  "Request a render of PAGE for GENERATION unless it is cached."
  (let* ((width (or width (yunge-reader-pdf--page-width page)))
         (render-key (yunge-reader-pdf--render-key page width))
         (document yunge-reader-document))
    (cond
     ((gethash render-key yunge-reader-pdf--render-results) 'cached)
     ((gethash render-key yunge-reader-pdf--render-pending) 'pending)
     (t
      (let ((buffer (current-buffer)))
        (puthash
         render-key (cons document generation)
         yunge-reader-pdf--render-pending)
        (condition-case error-data
            (yunge-reader-request
             'render-page
             (list :page page
                   :width width
                   :cache-key (yunge-reader-pdf--cache-key page width))
             (lambda (result request-error)
               (yunge-reader-pdf--render-complete
                buffer document generation page width
                result request-error)))
          (error
           (remhash render-key yunge-reader-pdf--render-pending)
           (signal (car error-data) (cdr error-data))))
        'started)))))

(defun yunge-reader-pdf--text-complete
    (buffer document page result error-data)
  "Store PAGE text RESULT in BUFFER, or report ERROR-DATA."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unwind-protect
          (when (eq document yunge-reader-document)
            (when (and (hash-table-p yunge-reader-pdf--text-pending)
                       (eq (car-safe
                            (gethash page
                                     yunge-reader-pdf--text-pending))
                           document))
              (remhash page yunge-reader-pdf--text-pending))
            (if error-data
                (when (and (not
                            (yunge-reader-pdf--stopped-error-p
                             error-data))
                           (yunge-reader-pdf--retain-page-p page))
                  (display-warning
                   'yunge-reader
                   (format "Could not load PDF page text: %s"
                           (error-message-string error-data))
                   :warning))
              (when (and (hash-table-p yunge-reader-pdf--text-cache)
                         (yunge-reader-pdf--retain-page-p page))
                (puthash page result yunge-reader-pdf--text-cache))
              (when (and (memq page yunge-reader-pdf--displayed-pages)
                         (or yunge-reader-selection
                             (yunge-reader-pdf--search-page-p page)))
                (yunge-reader-pdf--paint-page page))
              (when (yunge-reader-pdf--search-page-p page)
                (yunge-reader-pdf--scroll-to-search-result))))
        (yunge-reader-pdf--finish-prefetch
         document 'text page nil error-data)))))

(defun yunge-reader-pdf--request-text (page)
  "Request and cache canonical text geometry for PAGE."
  (cond
   ((gethash page yunge-reader-pdf--text-cache) 'cached)
   ((gethash page yunge-reader-pdf--text-pending) 'pending)
   (t
    (let ((buffer (current-buffer))
          (document yunge-reader-document))
      (puthash page (list document) yunge-reader-pdf--text-pending)
      (condition-case error-data
          (yunge-reader-request
           'page-text (list :page page)
           (lambda (result request-error)
             (yunge-reader-pdf--text-complete
              buffer document page result request-error)))
        (error
         (remhash page yunge-reader-pdf--text-pending)
         (signal (car error-data) (cdr error-data))))
      'started))))

(defun yunge-reader-pdf--link-complete
    (buffer document page result error-data)
  "Store PAGE link RESULT for DOCUMENT in BUFFER and notify waiters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unwind-protect
          (when (eq document yunge-reader-document)
            (let ((callbacks
                   (and (hash-table-p yunge-reader-pdf--link-pending)
                        (gethash page yunge-reader-pdf--link-pending))))
              (when (hash-table-p yunge-reader-pdf--link-pending)
                (remhash page yunge-reader-pdf--link-pending))
              (unless (or error-data
                          (yunge-reader-pdf-link-data-p result))
                (setq error-data
                      (list
                       'error
                       "Reader driver returned invalid PDF link data")))
              (if error-data
                  (when (and (not
                              (yunge-reader-pdf--stopped-error-p
                               error-data))
                             (yunge-reader-pdf--retain-page-p page))
                    (display-warning
                     'yunge-reader
                     (format "Could not load PDF page links: %s"
                             (error-message-string error-data))
                     :warning))
                (when (and (hash-table-p yunge-reader-pdf--link-cache)
                           (yunge-reader-pdf--retain-page-p page))
                  (puthash page result yunge-reader-pdf--link-cache)))
              (dolist (callback (delq nil callbacks))
                (condition-case callback-error
                    (funcall callback result error-data)
                  (error
                   (display-warning
                    'yunge-reader
                    (format "Could not finish PDF link action: %s"
                            (error-message-string callback-error))
                    :warning))))))
        (yunge-reader-pdf--finish-prefetch
         document 'links page nil error-data)))))

(defun yunge-reader-pdf--request-links (page &optional complete)
  "Request and cache PDF links for PAGE, then call COMPLETE."
  (if-let* ((cached
             (and (hash-table-p yunge-reader-pdf--link-cache)
                  (gethash page yunge-reader-pdf--link-cache))))
      (progn
        (when complete
          (funcall complete cached nil))
        'cached)
    (let ((pending
           (and (hash-table-p yunge-reader-pdf--link-pending)
                (gethash page yunge-reader-pdf--link-pending))))
      (if pending
          (progn
            (when complete
              (puthash
               page (cons complete pending)
               yunge-reader-pdf--link-pending))
            'pending)
        (let ((buffer (current-buffer))
              (document yunge-reader-document))
          (puthash page (list complete)
                   yunge-reader-pdf--link-pending)
          (condition-case error-data
              (yunge-reader-request
               'page-links (list :page page)
               (lambda (result request-error)
                 (yunge-reader-pdf--link-complete
                  buffer document page result request-error)))
            (error
             (remhash page yunge-reader-pdf--link-pending)
             (signal (car error-data) (cdr error-data))))
          'started)))))

(defun yunge-reader-pdf--queue-pages (pages &optional window)
  "Replace low-priority PDF work for PAGES viewed in WINDOW."
  (let* ((pages (cl-remove-duplicates (copy-sequence pages)))
         (tasks (yunge-reader-pdf--prefetch-tasks pages window)))
    (setq yunge-reader-pdf--working-pages pages)
    (yunge-reader-pdf--prune-working-set pages tasks)
    (setq yunge-reader-pdf--prefetch-queue
          (if yunge-reader-pdf--prefetch-active
              (seq-remove
               (lambda (task)
                 (yunge-reader-pdf--prefetch-task-same-p
                  task yunge-reader-pdf--prefetch-active))
               tasks)
            tasks))
    (yunge-reader-pdf--run-prefetch)))

(defun yunge-reader-pdf--sync-current-page (&optional window)
  "Update the current page from WINDOW's topmost roll slot."
  (let* ((window
          (or window (get-buffer-window (current-buffer) t)))
         (page
          (and window
               (yunge-reader-pdf--page-at-position
                (window-start window)))))
    (when (natnump page)
      (setq yunge-reader-pdf-page page))))

(defun yunge-reader-pdf--update-visible-pages (&optional window)
  "Virtualize the PDF roll and queue pages visible around WINDOW."
  (when (and yunge-reader-pdf-view-mode
             yunge-reader-document
             yunge-reader-pdf--page-positions
             (not yunge-reader-pdf--updating-visible))
    (let ((yunge-reader-pdf--updating-visible t)
          (window
           (or window (get-buffer-window (current-buffer) t))))
      (unless (yunge-reader-pdf--apply-pending-location window)
        (yunge-reader-pdf--sync-current-page window))
      (yunge-reader-pdf--target-width
       (yunge-reader-pdf--page-info yunge-reader-pdf-page)
       window)
      (let ((visible (yunge-reader-pdf--visible-pages)))
        (yunge-reader-pdf--paint-pages visible window)
        (yunge-reader-pdf--queue-pages
         (yunge-reader-pdf--prefetch-range visible) window))
      (yunge-reader-pdf--update-header)
      (yunge-reader-record-place window))))

(defun yunge-reader-pdf--refresh (&optional window location)
  "Refresh the PDF roll in WINDOW, then restore stable LOCATION."
  (when (and yunge-reader-pdf-view-mode yunge-reader-document)
    (setq window
          (or (and (window-live-p window)
                   (eq (window-buffer window) (current-buffer))
                   window)
              (get-buffer-window (current-buffer) t)))
    (cl-incf yunge-reader-pdf--generation)
    (unless yunge-reader-pdf--page-infos
      (yunge-reader-pdf--load-page-infos))
    (if (zerop (yunge-reader-pdf--page-count))
        (yunge-reader--display-status "This PDF contains no pages")
      (unless yunge-reader-pdf--page-positions
        (yunge-reader-pdf--build-roll))
      (when (and (yunge-reader-position-p location)
                 (not yunge-reader-pdf--pending-location))
        (setq yunge-reader-pdf--pending-location
              (copy-yunge-reader-position location)))
      (setq yunge-reader-pdf--displayed-pages nil)
      (dotimes (page (yunge-reader-pdf--page-count))
        (yunge-reader-pdf--paint-page
         page
         (and window (yunge-reader-pdf--page-width page window))))
      (yunge-reader-pdf--update-visible-pages window)
      (yunge-reader-pdf--scroll-to-search-result))))

(defun yunge-reader-pdf--pixel-to-page-point
    (page x y display-width display-height)
  "Convert PAGE display pixel X and Y to canonical PDF coordinates."
  (let* ((page-info (yunge-reader-pdf--page-info page))
         (page-width (alist-get 'width page-info))
         (page-height (alist-get 'height page-info)))
    (unless (and (numberp page-width) (> page-width 0)
                 (numberp page-height) (> page-height 0)
                 (numberp display-width) (> display-width 0)
                 (numberp display-height) (> display-height 0))
      (user-error "PDF page geometry is unavailable"))
    (setq x (max 0 (min display-width x))
          y (max 0 (min display-height y)))
    (cons (* page-width (/ (float x) display-width))
          (* page-height
             (- 1.0 (/ (float y) display-height))))))

(defun yunge-reader-pdf--bounds-distance (x y bounds)
  "Return squared distance from canonical X and Y to BOUNDS."
  (let* ((left (alist-get 'left bounds))
         (bottom (alist-get 'bottom bounds))
         (right (alist-get 'right bounds))
         (top (alist-get 'top bounds))
         (dx (cond ((< x left) (- left x))
                   ((> x right) (- x right))
                   (t 0.0)))
         (dy (cond ((< y bottom) (- bottom y))
                   ((> y top) (- y top))
                   (t 0.0))))
    (+ (* dx dx) (* dy dy))))

(defun yunge-reader-pdf--segment-distance (x y start end)
  "Return squared distance from X and Y to segment START through END."
  (let* ((start-x (alist-get 'x start))
         (start-y (alist-get 'y start))
         (delta-x (- (alist-get 'x end) start-x))
         (delta-y (- (alist-get 'y end) start-y))
         (length-squared
          (+ (* delta-x delta-x) (* delta-y delta-y)))
         (ratio
          (if (> length-squared 0)
              (max 0.0
                   (min 1.0
                        (/ (+ (* (- x start-x) delta-x)
                              (* (- y start-y) delta-y))
                           length-squared)))
            0.0))
         (nearest-x (+ start-x (* ratio delta-x)))
         (nearest-y (+ start-y (* ratio delta-y)))
         (distance-x (- x nearest-x))
         (distance-y (- y nearest-y)))
    (+ (* distance-x distance-x)
       (* distance-y distance-y))))

(defun yunge-reader-pdf--quad-contains-p (x y quad)
  "Return non-nil when canonical point X and Y lies in convex QUAD."
  (let ((orientation 0)
        (inside t))
    (dotimes (index 4)
      (let* ((start (nth index quad))
             (end (nth (mod (1+ index) 4) quad))
             (cross
              (- (* (- (alist-get 'x end)
                       (alist-get 'x start))
                    (- y (alist-get 'y start)))
                 (* (- (alist-get 'y end)
                       (alist-get 'y start))
                    (- x (alist-get 'x start))))))
        (unless (< (abs cross) 0.000001)
          (let ((sign (if (> cross 0) 1 -1)))
            (if (= orientation 0)
                (setq orientation sign)
              (unless (= orientation sign)
                (setq inside nil)))))))
    inside))

(defun yunge-reader-pdf--quad-distance (x y quad)
  "Return squared distance from canonical X and Y to convex QUAD."
  (if (yunge-reader-pdf--quad-contains-p x y quad)
      0.0
    (let ((distance most-positive-fixnum))
      (dotimes (index 4)
        (setq distance
              (min
               distance
               (yunge-reader-pdf--segment-distance
                x y
                (nth index quad)
                (nth (mod (1+ index) 4) quad)))))
      distance)))

(defun yunge-reader-pdf--character-distance (x y character)
  "Return squared distance from X and Y to CHARACTER geometry."
  (if-let* ((quad (yunge-reader-pdf--quad-points character)))
      (yunge-reader-pdf--quad-distance x y quad)
    (when-let* ((bounds (alist-get 'bounds character)))
      (yunge-reader-pdf--bounds-distance x y bounds))))

(defun yunge-reader-pdf--hit-character (page point text-layer)
  "Return PAGE's TEXT-LAYER character nearest canonical POINT."
  (let* ((x (car point))
         (y (cdr point))
         (page-info (yunge-reader-pdf--page-info page))
         (page-width (alist-get 'width page-info))
         (page-height (alist-get 'height page-info))
         (tolerance
          (max 12.0 (* 0.03 (min page-width page-height))))
         best
         best-distance)
    (dolist (character (alist-get 'characters text-layer))
      (unless (or (alist-get 'generated character)
                  (string-empty-p
                   (or (alist-get 'text character) "")))
        (let ((distance
               (yunge-reader-pdf--character-distance x y character)))
          (when (and distance
                     (or (null best-distance)
                         (< distance best-distance)))
            (setq best character
                  best-distance distance)))))
    (when (and best-distance
               (<= best-distance (* tolerance tolerance)))
      best)))

(defun yunge-reader-pdf--link-contains-p (link point)
  "Return non-nil when LINK contains canonical PDF POINT."
  (let ((bounds (yunge-reader-pdf-link-bounds link))
        (x (car point))
        (y (cdr point)))
    (and (<= (alist-get 'left bounds) x)
         (<= x (alist-get 'right bounds))
         (<= (alist-get 'bottom bounds) y)
         (<= y (alist-get 'top bounds)))))

(defun yunge-reader-pdf--link-at-point (page point data)
  "Return PAGE link containing canonical POINT in DATA."
  (when (and (yunge-reader-pdf-link-data-p data)
             (= page (yunge-reader-pdf-link-data-page data)))
    (seq-find
     (lambda (link)
       (yunge-reader-pdf--link-contains-p link point))
     (yunge-reader-pdf-link-data-links data))))

(defun yunge-reader-pdf--page-label (page)
  "Return the display label for zero-based PDF PAGE."
  (or (alist-get 'label (yunge-reader-pdf--page-info page))
      (number-to-string (1+ page))))

(defun yunge-reader-pdf--link-target-label (action)
  "Return a compact target label for PDF link ACTION."
  (pcase (yunge-reader-action-type action)
    ('location
     (format "page %s"
             (yunge-reader-pdf--page-label
              (yunge-reader-position-unit
               (yunge-reader-action-position action)))))
    ('uri
     (truncate-string-to-width
      (yunge-reader-action-uri action) 80 nil nil t))))

(defun yunge-reader-pdf--link-label (link)
  "Return one completion label for PDF LINK."
  (let* ((action (yunge-reader-pdf-link-action link))
         (source (yunge-reader-pdf-link-page link))
         (text
          (or (yunge-reader-pdf-link-label link)
              (format "link %d"
                      (1+ (yunge-reader-pdf-link-index link))))))
    (truncate-string-to-width
     (format "Page %s: %s -> %s"
             (yunge-reader-pdf--page-label source)
             text
             (yunge-reader-pdf--link-target-label action))
     120 nil nil t)))

(defun yunge-reader-pdf--link-candidates (pages)
  "Return unique completion candidates for cached links on PAGES."
  (let ((counts (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (used (make-hash-table :test #'equal))
        labeled)
    (dolist (page pages)
      (when-let* ((data
                   (gethash page yunge-reader-pdf--link-cache)))
        (dolist (link (yunge-reader-pdf-link-data-links data))
          (let ((label (yunge-reader-pdf--link-label link)))
            (push (cons label link) labeled)
            (puthash label (1+ (gethash label counts 0)) counts)))))
    (mapcar
     (lambda (candidate)
       (let* ((label (car candidate))
              (index (1+ (gethash label seen 0))))
         (puthash label index seen)
         (let* ((base
                 (if (> (gethash label counts) 1)
                     (format "%s [%d]" label index)
                   label))
                (unique base)
                (suffix 2))
           (while (gethash unique used)
             (setq unique (format "%s [%d]" base suffix)
                   suffix (1+ suffix)))
           (puthash unique t used)
           (cons unique (cdr candidate)))))
     (nreverse labeled))))

(defun yunge-reader-pdf--follow-location-link (link)
  "Follow PDF location LINK through the generic Reader action layer."
  (yunge-reader--follow-action
   (yunge-reader-pdf-link-action link))
  t)

(defun yunge-reader-pdf--follow-link (link)
  "Follow PDF LINK through the generic Reader action layer."
  (let ((action (yunge-reader-pdf-link-action link)))
    (if (eq (yunge-reader-action-type action) 'location)
        (yunge-reader-pdf--follow-location-link link)
      (yunge-reader--follow-action action)))
  (message "Link: %s" (yunge-reader-pdf--link-label link))
  t)

(defun yunge-reader-pdf--select-link (pages)
  "Choose and follow one cached PDF link from PAGES."
  (let ((candidates (yunge-reader-pdf--link-candidates pages))
        (truncated
         (seq-some
          (lambda (page)
            (when-let* ((data
                         (gethash page yunge-reader-pdf--link-cache)))
              (yunge-reader-pdf-link-data-truncated data)))
          pages)))
    (if (null candidates)
        (message "The visible PDF pages have no links")
      (let* ((completion-extra-properties
              '(:category yunge-reader-link))
             (choice
              (completing-read
               (if truncated "Links (truncated): " "Links: ")
               candidates nil t))
             (link (cdr (assoc choice candidates))))
        (when link
          (yunge-reader-pdf--follow-link link))))))

(defun yunge-reader-pdf--link-prompt-current-p
    (document generation window state)
  "Return whether a pending link prompt still belongs to the current view."
  (and (eq document yunge-reader-document)
       (= generation yunge-reader-pdf--link-activation-generation)
       (eq (selected-window) window)
       (not (active-minibuffer-window))
       (yunge-reader--window-state-current-p window state)))

(defun yunge-reader-pdf--finish-link-prompt
    (buffer document generation window state pages loaded)
  "Finish a link prompt for BUFFER when its captured view is unchanged."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and
             (= generation
                yunge-reader-pdf--link-activation-generation)
             (eq document yunge-reader-document))
        (if
            (yunge-reader-pdf--link-prompt-current-p
             document generation window state)
            (condition-case error-data
                (yunge-reader-pdf--select-link pages)
              (quit nil)
              (error
               (display-warning
                'yunge-reader
                (format "Could not follow PDF link: %s"
                        (error-message-string error-data))
                :warning)))
          (when loaded
            (message "PDF links loaded; press RET to open them")))))))

(defun yunge-reader-pdf-follow-link ()
  "Choose a link from the PDF pages visible in this window."
  (interactive)
  (unless yunge-reader-document
    (user-error "This reader buffer has no open document"))
  (let* ((window (yunge-reader--place-window))
         (pages (and window (yunge-reader-pdf--window-pages window))))
    (unless window
      (user-error "The Reader buffer is not displayed in a live window"))
    (setq pages (or pages (list yunge-reader-pdf-page)))
    (let* ((buffer (current-buffer))
           (document yunge-reader-document)
           (state (yunge-reader--window-state window))
           (generation
            (cl-incf yunge-reader-pdf--link-activation-generation))
           (missing
            (seq-remove
             (lambda (page)
               (gethash page yunge-reader-pdf--link-cache))
             pages)))
      (if (null missing)
          (yunge-reader-pdf--select-link pages)
        (let ((remaining (length missing))
              loaded)
          (message "Loading PDF links...")
          (dolist (page missing)
            (yunge-reader-pdf--request-links
             page
             (lambda (_result error-data)
               (unless error-data
                 (setq loaded t))
               (cl-decf remaining)
               (when (zerop remaining)
                 (yunge-reader-pdf--finish-link-prompt
                  buffer document generation window state
                  pages loaded))))))))))

(defun yunge-reader-pdf--event-page-point (position)
  "Return PAGE and canonical point represented by mouse POSITION."
  (let* ((buffer-position (posn-point position))
         (page (yunge-reader-pdf--page-at-position buffer-position))
         (object-point (posn-object-x-y position))
         (object-size (posn-object-width-height position)))
    (unless (and (natnump page) object-point object-size)
      (user-error "Place both PDF selection endpoints on page images"))
    (list
     :page page
     :point
     (yunge-reader-pdf--pixel-to-page-point
      page
      (car object-point) (cdr object-point)
      (car object-size) (cdr object-size)))))

(defun yunge-reader-pdf--event-page-points (event single)
  "Return start and end page points for mouse EVENT.
When SINGLE is non-nil, use the event start for both points."
  (let* ((start (event-start event))
         (end (if single start (or (event-end event) start)))
         (window (posn-window start)))
    (unless (and (windowp window)
                 (eq window (posn-window end)))
      (user-error "Keep the PDF selection in one reader window"))
    (list
     (yunge-reader-pdf--event-page-point start)
     (yunge-reader-pdf--event-page-point end))))

(defun yunge-reader-pdf--select-points (start-location end-location)
  "Select PDF characters at START-LOCATION and END-LOCATION."
  (let* ((start-page (plist-get start-location :page))
         (end-page (plist-get end-location :page))
         (start-point (plist-get start-location :point))
         (end-point (plist-get end-location :point))
         (start-layer
          (and yunge-reader-pdf--text-cache
               (gethash start-page yunge-reader-pdf--text-cache)))
         (end-layer
          (and yunge-reader-pdf--text-cache
               (gethash end-page yunge-reader-pdf--text-cache))))
    (unless (and start-layer end-layer)
      (user-error "PDF text geometry is still loading"))
    (let ((start-character
           (yunge-reader-pdf--hit-character
            start-page start-point start-layer))
          (end-character
           (yunge-reader-pdf--hit-character
            end-page end-point end-layer)))
      (unless (and start-character end-character)
        (setq yunge-reader-selection nil)
        (yunge-reader-pdf--paint-pages
         yunge-reader-pdf--displayed-pages)
        (user-error "No selectable PDF text near the pointer"))
      (let ((start-index (alist-get 'index start-character))
            (end-index (alist-get 'index end-character)))
        (yunge-reader-set-selection
         (make-yunge-reader-position
          :unit start-page
          :offset start-index
          :x (car start-point)
          :y (cdr start-point))
         (make-yunge-reader-position
          :unit end-page
          :offset end-index
          :x (car end-point)
          :y (cdr end-point)))
        (yunge-reader-pdf--paint-pages
         yunge-reader-pdf--displayed-pages)
        (if (= start-page end-page)
            (message "Selected %d PDF character%s"
                     (1+ (abs (- start-index end-index)))
                     (if (= start-index end-index) "" "s"))
          (message "Selected PDF text across %d pages"
                   (1+ (abs (- start-page end-page)))))))))

(defun yunge-reader-pdf--select-mouse-event (event single)
  "Handle PDF mouse selection EVENT, using one point when SINGLE."
  (let* ((position (event-start event))
         (window (posn-window position)))
    (unless (windowp window)
      (user-error "The mouse event is outside a PDF window"))
    (with-current-buffer (window-buffer window)
      (pcase-let ((`(,start-point ,end-point)
                   (yunge-reader-pdf--event-page-points
                    event single)))
        (yunge-reader-pdf--select-points
         start-point end-point)))))

(defun yunge-reader-pdf--activate-page-point (location data)
  "Follow a link at LOCATION in DATA, returning nil when none exists."
  (let* ((page (plist-get location :page))
         (point (plist-get location :point))
         (link (yunge-reader-pdf--link-at-point page point data)))
    (if link
        (yunge-reader-pdf--follow-link link)
      (message "There is no PDF link at this position")
      nil)))

(defun yunge-reader-pdf-activate-at-mouse (event)
  "Follow a PDF link at modified mouse EVENT, if one exists."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position)))
    (unless (windowp window)
      (user-error "The mouse event is outside a PDF window"))
    (select-window window)
    (with-current-buffer (window-buffer window)
      (let* ((location
              (yunge-reader-pdf--event-page-point position))
             (page (plist-get location :page))
             (cached
              (and (hash-table-p yunge-reader-pdf--link-cache)
                   (gethash page yunge-reader-pdf--link-cache)))
             (buffer (current-buffer))
             (document yunge-reader-document)
             (state (yunge-reader--window-state window))
             (generation
              (cl-incf yunge-reader-pdf--link-activation-generation)))
        (if cached
            (yunge-reader-pdf--activate-page-point location cached)
          (yunge-reader-pdf--request-links
           page
           (lambda (result _error-data)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when
                     (yunge-reader-pdf--link-prompt-current-p
                      document generation window state)
                   (condition-case error-data
                       (yunge-reader-pdf--activate-page-point
                        location result)
                     (error
                      (display-warning
                       'yunge-reader
                       (format "Could not activate PDF link: %s"
                               (error-message-string error-data))
                       :warning)))))))))))))

(defun yunge-reader-pdf-select-at-mouse (event)
  "Select the PDF character at mouse EVENT."
  (interactive "e")
  (yunge-reader-pdf--select-mouse-event event t))

(defun yunge-reader-pdf-select-with-mouse (event)
  "Select the PDF character range described by drag EVENT."
  (interactive "e")
  (yunge-reader-pdf--select-mouse-event event nil))

(defun yunge-reader-pdf--cancel-resize ()
  "Cancel a pending PDF viewport resize for the current buffer."
  (when (timerp yunge-reader-pdf--resize-timer)
    (cancel-timer yunge-reader-pdf--resize-timer))
  (setq yunge-reader-pdf--resize-timer nil
        yunge-reader-pdf--pending-resize nil))

(defun yunge-reader-pdf--finish-resize (buffer)
  "Refresh BUFFER after its latest PDF viewport resize settles."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((state yunge-reader-pdf--pending-resize)
             (document (plist-get state :document))
             (window (plist-get state :window))
             (location (plist-get state :location)))
        (setq yunge-reader-pdf--resize-timer nil
              yunge-reader-pdf--pending-resize nil)
        (when (and yunge-reader-pdf-view-mode
                   (eq document yunge-reader-document)
                   (window-live-p window)
                   (eq (window-buffer window) buffer))
          (if (memq yunge-reader-zoom-mode '(fit-width fit-page))
              (yunge-reader-pdf--refresh window location)
            (yunge-reader-pdf--update-visible-pages window)))))))

(defun yunge-reader-pdf--window-size-change (window)
  "Schedule viewport work after WINDOW changes its body size."
  (when (and yunge-reader-document
             (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let* ((former yunge-reader-pdf--pending-resize)
           (same-view
            (and (eq (plist-get former :document)
                     yunge-reader-document)
                 (eq (plist-get former :window) window)))
           (location
            (or (and same-view (plist-get former :location))
                (and (not yunge-reader-pdf--pending-location)
                     (ignore-errors
                       (yunge-reader-pdf--location
                        yunge-reader-document window))))))
      (setq yunge-reader-pdf--pending-resize
            (list :document yunge-reader-document
                  :window window
                  :width (window-body-width window t)
                  :height (window-body-height window t)
                  :location location))
      (unless (timerp yunge-reader-pdf--resize-timer)
        (setq yunge-reader-pdf--resize-timer
              (run-with-idle-timer
               0 nil #'yunge-reader-pdf--finish-resize
               (current-buffer)))))))

(defun yunge-reader-pdf--window-scrolled (window _start)
  "Update PDF virtualization after WINDOW scrolls."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (yunge-reader-pdf--update-visible-pages window)))

(defun yunge-reader-pdf--set-page (page)
  "Display zero-based PDF PAGE."
  (let ((count (yunge-reader-pdf--page-count)))
    (unless (> count 0)
      (user-error "This PDF has no pages"))
    (setq yunge-reader-pdf--pending-location nil
          yunge-reader-pdf-page
          (max 0 (min (1- count) page)))
    (let ((position
           (yunge-reader-pdf--page-position yunge-reader-pdf-page))
          (window (get-buffer-window (current-buffer) t)))
      (when position
        (goto-char position)
        (when (window-live-p window)
          (set-window-start window position t)
          (set-window-vscroll window 0 t))))
    (yunge-reader-pdf--update-visible-pages)
    yunge-reader-pdf-page))

(defun yunge-reader-pdf--scroll-half-window (direction count)
  "Scroll in DIRECTION by half a window COUNT times."
  (let ((function
         (if (eq direction 'up)
             #'pixel-scroll-precision-scroll-up-page
           #'pixel-scroll-precision-scroll-down-page)))
    (dotimes (_ (abs count))
      (funcall
       function
       (max 1 (/ (window-text-height nil t) 2))))
    (yunge-reader-pdf--update-visible-pages (selected-window))))

(defun yunge-reader-pdf-scroll-up (&optional count)
  "Scroll backward by half a PDF window COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (yunge-reader-pdf--scroll-half-window
   (if (< count 0) 'down 'up) count))

(defun yunge-reader-pdf-scroll-down (&optional count)
  "Scroll forward by half a PDF window COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (yunge-reader-pdf--scroll-half-window
   (if (< count 0) 'up 'down) count))

(defun yunge-reader-pdf--scroll-line (direction count)
  "Scroll in DIRECTION by one screen line COUNT times."
  (let ((function
         (if (eq direction 'up)
             #'pixel-scroll-precision-scroll-up-page
           #'pixel-scroll-precision-scroll-down-page))
        (pixels
         (max 1 (frame-char-height (window-frame)))))
    (dotimes (_ (abs count))
      (funcall function pixels))
    (yunge-reader-pdf--update-visible-pages (selected-window))))

(defun yunge-reader-pdf-scroll-up-line (&optional count)
  "Scroll backward by one PDF screen line COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (yunge-reader-pdf--scroll-line
   (if (< count 0) 'down 'up) count))

(defun yunge-reader-pdf-scroll-down-line (&optional count)
  "Scroll forward by one PDF screen line COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (yunge-reader-pdf--scroll-line
   (if (< count 0) 'up 'down) count))

(defun yunge-reader-pdf-next-page (&optional count)
  "Move forward COUNT PDF pages, defaulting to one."
  (interactive "p")
  (yunge-reader-pdf--set-page
   (+ yunge-reader-pdf-page (or count 1))))

(defun yunge-reader-pdf-previous-page (&optional count)
  "Move backward COUNT PDF pages, defaulting to one."
  (interactive "p")
  (yunge-reader-pdf-next-page (- (or count 1))))

(defun yunge-reader-pdf-first-page ()
  "Move to the first page in the PDF view."
  (interactive)
  (yunge-reader-pdf--set-page 0))

(defun yunge-reader-pdf-last-page ()
  "Move to the last page in the PDF view."
  (interactive)
  (yunge-reader-pdf--set-page
   (1- (yunge-reader-pdf--page-count))))

(defun yunge-reader-pdf-goto-page (page)
  "Go to one-based PDF PAGE."
  (interactive
   (list
    (read-number
     (format "Page (1-%d): " (yunge-reader-pdf--page-count))
     (1+ yunge-reader-pdf-page))))
  (yunge-reader-pdf--set-page (1- page)))

(dolist (command
         '(yunge-reader-pdf-first-page
           yunge-reader-pdf-last-page
           yunge-reader-pdf-goto-page
           yunge-reader-pdf--follow-location-link))
  (yunge-jump-history-track-command command))

(provide 'yunge-reader-pdf)

;;; yunge-reader-pdf.el ends here
