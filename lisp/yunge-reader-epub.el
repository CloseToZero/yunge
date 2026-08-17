;;; yunge-reader-epub.el --- EPUB reader -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-key)
(require 'yunge-reader)
(require 'yunge-reader-webview)

(cl-defstruct yunge-reader-epub-handle
  "One shared native EPUB publication."
  publication
  metadata
  pending-detaches
  closing
  closed)

(defun yunge-reader-epub--set-scroll-bar-mode (symbol value)
  "Set SYMBOL to VALUE and synchronize live EPUB views."
  (set-default symbol value)
  (when (boundp 'yunge-reader-webview--logical-views)
    (yunge-reader-webview--sync-views)))

(defcustom yunge-reader-epub-scroll-bar-mode 'follow-emacs
  "How native EPUB views display their spine-item scroll bars.
When set to `follow-emacs', use the owning frame's actual vertical scroll bar
state.  `hidden' and `visible' override the frame state without changing
scrolling behavior."
  :type '(choice
          (const :tag "Follow Emacs" follow-emacs)
          (const :tag "Always hidden" hidden)
          (const :tag "Always visible" visible))
  :set #'yunge-reader-epub--set-scroll-bar-mode
  :group 'yunge-reader)

(defcustom yunge-reader-epub-default-font-scale 1.0
  "Default font scale for reflowable EPUB views."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-epub-default-line-height 1.6
  "Default unitless line height for reflowable EPUB views."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-epub-default-content-width 720
  "Default maximum EPUB content width in CSS pixels."
  :type 'integer
  :group 'yunge-reader)

(defcustom yunge-reader-epub-default-side-padding 7.0
  "Default EPUB side padding as a percentage of the view width."
  :type 'number
  :group 'yunge-reader)

(defconst yunge-reader-epub-minimum-font-scale 0.5
  "Smallest font scale accepted by the EPUB renderer.")

(defconst yunge-reader-epub-maximum-font-scale 3.0
  "Largest font scale accepted by the EPUB renderer.")

(defvar-local yunge-reader-epub--native-selection-sync nil
  "Whether native selection state is being mapped into Reader state.")

(defconst yunge-reader-epub-normal-bindings
  '(("C-d" yunge-reader-epub-next-screen "next screen")
    ("C-u" yunge-reader-epub-previous-screen "previous screen")
    ("J" yunge-reader-epub-next-screen "next screen")
    ("K" yunge-reader-epub-previous-screen "previous screen"))
  "Normal-state bindings for reflowable EPUB views.")

(defvar-keymap yunge-reader-epub-view-mode-map
  "C-d" #'yunge-reader-epub-next-screen
  "C-u" #'yunge-reader-epub-previous-screen
  "J" #'yunge-reader-epub-next-screen
  "K" #'yunge-reader-epub-previous-screen
  "<next>" #'yunge-reader-epub-next-screen
  "<prior>" #'yunge-reader-epub-previous-screen)

(define-minor-mode yunge-reader-epub-view-mode
  "Display a reflowable EPUB through a native WebView."
  :init-value nil
  :lighter " EPUB"
  :keymap yunge-reader-epub-view-mode-map
  (if yunge-reader-epub-view-mode
      (progn
        (add-hook 'yunge-reader-refresh-hook
                  #'yunge-reader-epub--refresh nil t)
        (add-hook 'yunge-reader-selection-change-hook
                  #'yunge-reader-epub--selection-state-changed nil t)
        (add-hook 'yunge-reader-view-role-change-hook
                  #'yunge-reader-epub--update-header nil t))
    (remove-hook 'yunge-reader-refresh-hook
                 #'yunge-reader-epub--refresh t)
    (remove-hook 'yunge-reader-selection-change-hook
                 #'yunge-reader-epub--selection-state-changed t)
    (remove-hook 'yunge-reader-view-role-change-hook
                 #'yunge-reader-epub--update-header t)
    (kill-local-variable 'yunge-reader-default-scale)
    (kill-local-variable 'yunge-reader-minimum-scale)
    (kill-local-variable 'yunge-reader-maximum-scale)
    (setq header-line-format nil)))

(with-eval-after-load 'evil
  (yunge-key-evil-define-minor-mode
   'normal 'yunge-reader-epub-view-mode
   yunge-reader-epub-normal-bindings))

(defun yunge-reader-epub--match-p (file)
  "Return whether FILE has an EPUB extension."
  (string-equal
   (downcase (or (file-name-extension file) "")) "epub"))

(defun yunge-reader-epub--supported-p ()
  "Return whether the current Emacs can host the EPUB WebView."
  (and (display-graphic-p) (eq system-type 'windows-nt)))

(defun yunge-reader-epub--native-error (message)
  "Return an Emacs error value containing MESSAGE."
  (list 'error message))

(defun yunge-reader-epub--metadata (result)
  "Return bounded Reader metadata parsed from native RESULT."
  (let ((metadata (alist-get 'metadata result)))
    (list
     :title (alist-get 'title metadata)
     :language (alist-get 'language metadata)
     :identifier (alist-get 'identifier metadata)
     :version (alist-get 'version metadata)
     :entry-count (alist-get 'entry-count result))))

(defun yunge-reader-epub--open-complete
    (complete result error-data)
  "Call COMPLETE after validating native EPUB RESULT or ERROR-DATA."
  (if error-data
      (funcall complete nil nil error-data)
    (condition-case validation-error
        (let ((publication (alist-get 'publication result)))
          (unless (and (integerp publication) (> publication 0))
            (error "Malformed EPUB publication result: %S" result))
          (let ((metadata (yunge-reader-epub--metadata result)))
            (funcall
             complete
             (make-yunge-reader-epub-handle
              :publication publication
              :metadata metadata
              :pending-detaches 0)
             (list :layout 'reflow :metadata metadata)
             nil)))
      (error
       (when-let* ((publication (alist-get 'publication result))
                   ((integerp publication))
                   ((> publication 0)))
         (yunge-reader-webview--close-owned-publication publication))
       (funcall complete nil nil validation-error)))))

(defun yunge-reader-epub--open (file complete)
  "Open EPUB FILE and invoke COMPLETE with its shared handle."
  (cond
   ((not (yunge-reader-epub--supported-p))
    (funcall
     complete nil nil
     (yunge-reader-epub--native-error
      "EPUB reading currently requires graphical Windows Emacs")))
   ((or (file-remote-p file)
        (not (file-regular-p file))
        (not (file-readable-p file)))
    (funcall
     complete nil nil
     (yunge-reader-epub--native-error
      (format "EPUB file is not readable: %s" file))))
   (t
    (yunge-reader-webview--open-publication
     file
     (apply-partially
      #'yunge-reader-epub--open-complete complete)))))

(defun yunge-reader-epub--close-complete
    (handle _result error-data)
  "Finish closing HANDLE and report ERROR-DATA."
  (if error-data
      (display-warning
       'yunge-reader (error-message-string error-data) :warning)
    (setf (yunge-reader-epub-handle-closed handle) t)))

(defun yunge-reader-epub--maybe-close (handle)
  "Close HANDLE once every native view detach has completed."
  (when (and (yunge-reader-epub-handle-closing handle)
             (zerop
              (yunge-reader-epub-handle-pending-detaches handle))
             (not (yunge-reader-epub-handle-closed handle)))
    (if (process-live-p yunge-reader-webview--process)
        (yunge-reader-webview--close-publication
         (yunge-reader-epub-handle-publication handle)
         (apply-partially
          #'yunge-reader-epub--close-complete handle))
      (setf (yunge-reader-epub-handle-closed handle) t))))

(defun yunge-reader-epub--close (document)
  "Close the shared EPUB resource owned by DOCUMENT."
  (let ((handle (yunge-reader-document-handle document)))
    (unless (yunge-reader-epub-handle-closing handle)
      (setf (yunge-reader-epub-handle-closing handle) t)
      (yunge-reader-epub--maybe-close handle))))

(defun yunge-reader-epub--update-header ()
  "Update the current EPUB view header."
  (let* ((metadata
          (and yunge-reader-document
               (yunge-reader-document-metadata
                yunge-reader-document)))
         (title (or (plist-get metadata :title)
                    (and yunge-reader-document
                         (file-name-nondirectory
                          (yunge-reader-document-file
                           yunge-reader-document)))
                    "EPUB"))
         (role
          (pcase (yunge-reader-view-role)
            ('primary "Primary")
            ('additional "Additional")
            (_ "Reader")))
         (font-percent
          (round
           (* 100
              (or yunge-reader-effective-scale
                  yunge-reader-scale
                  yunge-reader-epub-default-font-scale)))))
    (setq header-line-format
          (format " %s  EPUB  Font %d%%  %s "
                  role font-percent title))))

(defun yunge-reader-epub--location-changed (view)
  "Record the stable location reported by EPUB VIEW."
  (when-let* ((buffer (yunge-reader-webview--view-buffer view))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (and yunge-reader-epub-view-mode
                 (eq view yunge-reader-webview--buffer-view))
        (yunge-reader-record-place
         (yunge-reader-webview--view-window view))))))

(defun yunge-reader-epub--selection-position (href cfi)
  "Return a Reader position for EPUB HREF and collapsed CFI."
  (make-yunge-reader-position :unit href :offset cfi))

(defun yunge-reader-epub--selection-changed (view)
  "Synchronize EPUB VIEW's native selection with its Reader buffer."
  (when-let* ((buffer (yunge-reader-webview--view-buffer view))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (and yunge-reader-epub-view-mode
                 (eq view yunge-reader-webview--buffer-view))
        (let ((yunge-reader-epub--native-selection-sync t))
          (if-let* ((selection
                     (yunge-reader-webview--view-selection view)))
              (let ((href (alist-get 'href selection)))
                (yunge-reader-set-selection
                 (yunge-reader-epub--selection-position
                  href (alist-get 'start selection))
                 (yunge-reader-epub--selection-position
                  href (alist-get 'end selection))))
            (when yunge-reader-selection
              (yunge-reader-clear-selection t))))))))

(defun yunge-reader-epub--selection-state-changed ()
  "Synchronize a cleared Reader selection with the native EPUB view."
  (when (and yunge-reader-epub-view-mode
             (not yunge-reader-epub--native-selection-sync)
             (null yunge-reader-selection)
             yunge-reader-webview--buffer-view)
    (yunge-reader-webview--clear-view-selection
     yunge-reader-webview--buffer-view)))

(defun yunge-reader-epub--font-scale (scale)
  "Return EPUB font SCALE restricted to renderer bounds."
  (unless (and (numberp scale) (= scale scale))
    (error "Invalid EPUB font scale: %S" scale))
  (max yunge-reader-epub-minimum-font-scale
       (min yunge-reader-epub-maximum-font-scale scale)))

(defun yunge-reader-epub--initial-font-scale ()
  "Return the font scale to use before the EPUB surface opens."
  (yunge-reader-epub--font-scale
   (cond
    ((null yunge-reader--pending-place)
     yunge-reader-epub-default-font-scale)
    ((eq (plist-get yunge-reader--pending-place :zoom-mode)
         'manual)
     (plist-get yunge-reader--pending-place :scale))
    (t
     (error
      "EPUB place has an unsupported zoom mode: %S"
      (plist-get yunge-reader--pending-place :zoom-mode))))))

(defun yunge-reader-epub--configure-zoom ()
  "Configure Reader zoom state for one EPUB view and return its scale."
  (let ((default
         (yunge-reader-epub--font-scale
          yunge-reader-epub-default-font-scale))
        (scale (yunge-reader-epub--initial-font-scale)))
    (setq-local yunge-reader-default-scale default)
    (setq-local yunge-reader-minimum-scale
                yunge-reader-epub-minimum-font-scale)
    (setq-local yunge-reader-maximum-scale
                yunge-reader-epub-maximum-font-scale)
    (setq yunge-reader-zoom-mode 'manual
          yunge-reader-scale scale
          yunge-reader-effective-scale scale)
    scale))

(defun yunge-reader-epub--default-style (&optional font-scale)
  "Return a fresh EPUB style using optional FONT-SCALE."
  (yunge-reader-webview--check-style
   `((font-scale
      . ,(yunge-reader-epub--font-scale
          (or font-scale yunge-reader-epub-default-font-scale)))
     (line-height . ,yunge-reader-epub-default-line-height)
     (content-width . ,yunge-reader-epub-default-content-width)
     (side-padding . ,yunge-reader-epub-default-side-padding))))

(defun yunge-reader-epub--scroll-bar-mode (window)
  "Return the resolved EPUB scroll bar mode for WINDOW."
  (pcase yunge-reader-epub-scroll-bar-mode
    ('follow-emacs
     (if (frame-parameter (window-frame window)
                          'vertical-scroll-bars)
         'visible
       'hidden))
    ((or 'hidden 'visible) yunge-reader-epub-scroll-bar-mode)
    (_
     (error "Invalid EPUB scroll bar mode: %S"
            yunge-reader-epub-scroll-bar-mode))))

(defun yunge-reader-epub--attach (document)
  "Attach a persistent EPUB WebView for DOCUMENT."
  (let ((handle (yunge-reader-document-handle document)))
    (unless (and (yunge-reader-epub-handle-p handle)
                  (not (yunge-reader-epub-handle-closing handle)))
      (error "EPUB document has no live publication"))
    (let ((font-scale (yunge-reader-epub--configure-zoom)))
      (yunge-reader-epub-view-mode 1)
      (yunge-reader-epub--update-header)
      (yunge-reader-webview--attach-shared-publication
       (yunge-reader-epub-handle-publication handle)
       nil #'yunge-reader-epub--location-changed
       #'yunge-reader-epub--selection-changed
       #'yunge-reader-epub--accelerator
       (yunge-reader-epub--default-style font-scale)
       #'yunge-reader-epub--scroll-bar-mode))))

(defun yunge-reader-epub--detach-complete (handle)
  "Finish one native view detach belonging to HANDLE."
  (setf (yunge-reader-epub-handle-pending-detaches handle)
        (max 0
             (1- (yunge-reader-epub-handle-pending-detaches
                  handle))))
  (yunge-reader-epub--maybe-close handle))

(defun yunge-reader-epub--detach (document)
  "Detach the current EPUB view from DOCUMENT."
  (let ((handle (yunge-reader-document-handle document)))
    (when yunge-reader-webview--buffer-view
      (cl-incf (yunge-reader-epub-handle-pending-detaches handle))
      (yunge-reader-webview--detach-shared-publication
       (apply-partially
        #'yunge-reader-epub--detach-complete handle)))
    (yunge-reader-epub-view-mode -1)))

(defun yunge-reader-epub--locator-position (location)
  "Return the Reader position represented by EPUB LOCATION."
  (when (yunge-reader-webview--valid-location-p location)
    (make-yunge-reader-position
     :unit (alist-get 'href location)
     :offset (alist-get 'cfi location)
     :x (alist-get 'fraction location))))

(defun yunge-reader-epub--position-locator (position)
  "Return an EPUB locator represented by Reader POSITION."
  (when (yunge-reader-position-p position)
    (let ((location
           (append
            (list
             (cons 'cfi (yunge-reader-position-offset position))
             (cons 'href (yunge-reader-position-unit position)))
            (when (numberp (yunge-reader-position-x position))
              (list
               (cons 'fraction
                     (yunge-reader-position-x position)))))))
      (and (yunge-reader-webview--valid-location-p location)
           location))))

(defun yunge-reader-epub--position-target (position)
  "Return the EPUB navigation target represented by POSITION."
  (or
   (yunge-reader-epub--position-locator position)
   (when (and (yunge-reader-position-p position)
              (null (yunge-reader-position-offset position))
              (null (yunge-reader-position-x position))
              (null (yunge-reader-position-y position)))
     (let ((target
            (list (cons 'href
                        (yunge-reader-position-unit position)))))
       (and (yunge-reader-webview--valid-target-p target)
            target)))))

(defun yunge-reader-epub--outline-position (href)
  "Return a one-shot Reader navigation position for EPUB HREF."
  (when (yunge-reader-webview--valid-target-href-p href)
    (make-yunge-reader-position :unit href)))

(defun yunge-reader-epub--native-outline-item (value)
  "Return a generic outline item represented by renderer VALUE."
  (when (yunge-reader-webview--valid-outline-item-p value)
    (let* ((href (alist-get 'href value))
           (position (and href
                          (yunge-reader-epub--outline-position href))))
      (make-yunge-reader-outline-item
       :title (alist-get 'title value)
       :depth (alist-get 'depth value)
       :action
       (and position
            (make-yunge-reader-action
             :type 'location :position position))))))

(defun yunge-reader-epub--native-outline (value)
  "Return a generic EPUB outline represented by renderer VALUE."
  (when (yunge-reader-webview--valid-outline-p value)
    (make-yunge-reader-outline-data
     :items
     (mapcar #'yunge-reader-epub--native-outline-item
             (alist-get 'items value))
     :truncated (eq (alist-get 'truncated value) t))))

(defun yunge-reader-epub--location (_document _window)
  "Return the current stable EPUB position."
  (when-let* ((view yunge-reader-webview--buffer-view)
              ((not (yunge-reader-webview--view-destroyed view))))
    (yunge-reader-epub--locator-position
     (yunge-reader-webview--view-location view))))

(defun yunge-reader-epub--restore-complete (_result error-data)
  "Report an asynchronous EPUB restore ERROR-DATA."
  (when error-data
    (display-warning
     'yunge-reader (error-message-string error-data) :warning)))

(defun yunge-reader-epub--restore-location
    (_document position _window)
  "Restore EPUB POSITION in the current buffer's logical view."
  (when-let* ((view yunge-reader-webview--buffer-view)
              (target
               (yunge-reader-epub--position-target position)))
    (let ((stable
           (yunge-reader-webview--valid-location-p target))
          (ready
           (yunge-reader-webview--view-publication-ready view)))
      (cond
       (stable
        (setf (yunge-reader-webview--view-location view)
              (copy-tree target)
              (yunge-reader-webview--view-pending-target view) nil)
        (when ready
          (yunge-reader-webview--navigate-view
           view "go-to" #'yunge-reader-epub--restore-complete target))
        t)
       (t
        (if ready
            (progn
              (setf (yunge-reader-webview--view-pending-target view)
                    nil)
              (yunge-reader-webview--navigate-view
               view "go-to" #'yunge-reader-epub--restore-complete
               target))
          (yunge-reader-webview--queue-view-target view target))
        :deferred)))))

(defun yunge-reader-epub--refresh ()
  "Synchronize the current EPUB surface with its Reader window."
  (when-let* ((view yunge-reader-webview--buffer-view)
              ((not (yunge-reader-webview--view-destroyed view))))
    (unless (eq yunge-reader-zoom-mode 'manual)
      (error "EPUB views require manual Reader zoom mode"))
    (let* ((scale
            (yunge-reader-epub--font-scale yunge-reader-scale))
           (style
            (copy-tree
             (or (yunge-reader-webview--view-style view)
                 (yunge-reader-epub--default-style scale)))))
      (setq yunge-reader-scale scale
            yunge-reader-effective-scale scale)
      (setcdr (assq 'font-scale style) scale)
      (yunge-reader-webview--set-view-style view style)
      (yunge-reader-webview--sync-view view)
      (yunge-reader-epub--update-header)
      (yunge-reader-record-place
       (yunge-reader-webview--view-window view)))))

(defun yunge-reader-epub--outline-complete
    (complete value error-data)
  "Call COMPLETE with generic outline VALUE or ERROR-DATA."
  (if error-data
      (funcall complete nil error-data)
    (let ((outline (yunge-reader-epub--native-outline value)))
      (if outline
          (funcall complete outline nil)
        (funcall
         complete nil
         (yunge-reader-epub--native-error
          "The EPUB renderer returned an invalid outline"))))))

(defun yunge-reader-epub--document-view (document)
  "Return DOCUMENT's current compatible EPUB view, or nil."
  (let ((handle (yunge-reader-document-handle document))
        (view yunge-reader-webview--buffer-view))
    (and
     (yunge-reader-epub-handle-p handle)
     view
     (not (yunge-reader-webview--view-destroyed view))
     (eql (yunge-reader-epub-handle-publication handle)
          (yunge-reader-webview--view-publication view))
     view)))

(defun yunge-reader-epub--selection-range (start end)
  "Return a native same-spine selection for Reader START and END."
  (when (and (yunge-reader-position-p start)
             (yunge-reader-position-p end)
             (null (yunge-reader-position-x start))
             (null (yunge-reader-position-y start))
             (null (yunge-reader-position-x end))
             (null (yunge-reader-position-y end)))
    (let ((selection
           `((href . ,(yunge-reader-position-unit start))
             (start . ,(yunge-reader-position-offset start))
             (end . ,(yunge-reader-position-offset end)))))
      (and
       (equal (yunge-reader-position-unit start)
              (yunge-reader-position-unit end))
       (yunge-reader-webview--valid-selection-p selection)
       selection))))

(defun yunge-reader-epub--selection-cursor-offset (cursor href)
  "Return CURSOR's transient Unicode offset for HREF, or nil."
  (if (null cursor)
      0
    (and
     (yunge-reader-position-p cursor)
     (equal (yunge-reader-position-unit cursor) href)
     (natnump (yunge-reader-position-offset cursor))
     (null (yunge-reader-position-x cursor))
     (null (yunge-reader-position-y cursor))
     (yunge-reader-position-offset cursor))))

(defun yunge-reader-epub--selection-text-complete
    (href complete result error-data)
  "Map native selection text RESULT for HREF through COMPLETE."
  (if error-data
      (funcall complete nil error-data)
    (let ((done (eq (alist-get 'done result) t)))
      (funcall
       complete
       (make-yunge-reader-selection-batch
        :text (alist-get 'text result)
        :cursor
        (unless done
          (make-yunge-reader-position
           :unit href
           :offset (alist-get 'next-offset result)))
        :done done)
       nil))))

(defun yunge-reader-epub--request-selection-text
    (document arguments complete)
  "Request one generic selection text batch for DOCUMENT."
  (let* ((start (plist-get arguments :start))
         (end (plist-get arguments :end))
         (cursor (plist-get arguments :cursor))
         (unit-limit (plist-get arguments :unit-limit))
         (character-limit (plist-get arguments :character-limit))
         (selection (yunge-reader-epub--selection-range start end))
         (href (and selection (alist-get 'href selection)))
         (offset
          (and selection
               (yunge-reader-epub--selection-cursor-offset
                cursor href)))
         (view (yunge-reader-epub--document-view document)))
    (cond
     ((not selection)
      (funcall
       complete nil
       (yunge-reader-epub--native-error
        "EPUB selection endpoints must share one valid spine")))
     ((not (and (integerp unit-limit) (<= 1 unit-limit 64)))
      (funcall
       complete nil
       (yunge-reader-epub--native-error
        "EPUB selection unit limit must be between 1 and 64")))
     ((null offset)
      (funcall
       complete nil
       (yunge-reader-epub--native-error
        "EPUB selection cursor must be an offset in the selected spine")))
     ((not view)
      (funcall
       complete nil
       (yunge-reader-epub--native-error
        "The current EPUB view cannot provide selection text")))
     ((not
       (let ((current
              (yunge-reader-webview--view-selection view)))
         (and current
              (equal (alist-get 'href selection)
                     (alist-get 'href current))
              (equal (alist-get 'start selection)
                     (alist-get 'start current))
              (equal (alist-get 'end selection)
                     (alist-get 'end current)))))
      (funcall
       complete nil
       (yunge-reader-epub--native-error
        "The EPUB selection changed before it could be copied")))
     (t
      (yunge-reader-webview--request-selection-text
       view selection offset character-limit
       (apply-partially
        #'yunge-reader-epub--selection-text-complete
        href complete))))))

(defun yunge-reader-epub--request
    (document operation arguments complete)
  "Dispatch one EPUB DOCUMENT OPERATION through COMPLETE."
  (pcase operation
    ('outline
     (let ((view (yunge-reader-epub--document-view document)))
       (if view
           (yunge-reader-webview--request-view-outline
            view
            (apply-partially
             #'yunge-reader-epub--outline-complete complete))
         (funcall
          complete nil
          (yunge-reader-epub--native-error
           "The current EPUB view cannot provide its outline")))))
    ('selection-text
     (yunge-reader-epub--request-selection-text
      document arguments complete))
    (_
     (funcall
      complete nil
      (yunge-reader-epub--native-error
       (format "Unsupported EPUB operation: %S" operation))))))

(defun yunge-reader-epub--navigate (command)
  "Run semantic EPUB navigation COMMAND in the current view."
  (yunge-reader-webview--navigate-view
   (yunge-reader-webview--current-ready-view)
   command #'yunge-reader-epub--restore-complete))

(defun yunge-reader-epub--accelerator (view key)
  "Run normalized WebView KEY for EPUB VIEW through active Emacs maps."
  (when (and (eq view yunge-reader-webview--buffer-view)
             yunge-reader-epub-view-mode)
    (when-let* ((command (key-binding (kbd key) t))
                ((commandp command)))
      (let ((this-command command)
            (real-this-command command))
        (call-interactively command)))))

(defun yunge-reader-epub-next-screen (&optional count)
  "Move forward COUNT EPUB screens."
  (interactive "p")
  (setq count (or count 1))
  (if (< count 0)
      (yunge-reader-epub-previous-screen (- count))
    (dotimes (_ count)
      (yunge-reader-epub--navigate "next-screen"))))

(defun yunge-reader-epub-previous-screen (&optional count)
  "Move backward COUNT EPUB screens."
  (interactive "p")
  (setq count (or count 1))
  (if (< count 0)
      (yunge-reader-epub-next-screen (- count))
    (dotimes (_ count)
      (yunge-reader-epub--navigate "previous-screen"))))

(defun yunge-reader-epub-register ()
  "Register the EPUB driver."
  (yunge-reader-register-driver
   'epub
   :match #'yunge-reader-epub--match-p
   :open #'yunge-reader-epub--open
   :close #'yunge-reader-epub--close
   :attach #'yunge-reader-epub--attach
   :detach #'yunge-reader-epub--detach
   :request #'yunge-reader-epub--request
   :location #'yunge-reader-epub--location
   :restore #'yunge-reader-epub--restore-location))

;;;###autoload
(defun yunge-reader-epub-mode ()
  "Read the EPUB visited by the current buffer with Yunge Reader."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting an EPUB file"))
  (yunge-reader-epub-register)
  (yunge-reader-visit-file buffer-file-name))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.epub\\'" . yunge-reader-epub-mode))

;;;###autoload
(defun yunge-reader-epub-open (file)
  "Open EPUB FILE explicitly with Yunge Reader."
  (interactive "fRead EPUB: ")
  (yunge-reader-epub-register)
  (yunge-reader-open file))

(provide 'yunge-reader-epub)

;;; yunge-reader-epub.el ends here
