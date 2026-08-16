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
        (add-hook 'yunge-reader-view-role-change-hook
                  #'yunge-reader-epub--update-header nil t))
    (remove-hook 'yunge-reader-refresh-hook
                 #'yunge-reader-epub--refresh t)
    (remove-hook 'yunge-reader-view-role-change-hook
                 #'yunge-reader-epub--update-header t)
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
            (_ "Reader"))))
    (setq header-line-format
          (format " %s  EPUB  %s " role title))))

(defun yunge-reader-epub--location-changed (view)
  "Record the stable location reported by EPUB VIEW."
  (when-let* ((buffer (yunge-reader-webview--view-buffer view))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (and yunge-reader-epub-view-mode
                 (eq view yunge-reader-webview--buffer-view))
        (yunge-reader-record-place
         (yunge-reader-webview--view-window view))))))

(defun yunge-reader-epub--default-style ()
  "Return a fresh, validated default EPUB reading style."
  (yunge-reader-webview--check-style
   `((font-scale . ,yunge-reader-epub-default-font-scale)
     (line-height . ,yunge-reader-epub-default-line-height)
     (content-width . ,yunge-reader-epub-default-content-width)
     (side-padding . ,yunge-reader-epub-default-side-padding))))

(defun yunge-reader-epub--attach (document)
  "Attach a persistent EPUB WebView for DOCUMENT."
  (let ((handle (yunge-reader-document-handle document)))
    (unless (and (yunge-reader-epub-handle-p handle)
                 (not (yunge-reader-epub-handle-closing handle)))
      (error "EPUB document has no live publication"))
    (yunge-reader-epub-view-mode 1)
    (yunge-reader-epub--update-header)
    (yunge-reader-webview--attach-shared-publication
     (yunge-reader-epub-handle-publication handle)
     nil #'yunge-reader-epub--location-changed
     #'yunge-reader-epub--accelerator
     (yunge-reader-epub--default-style))))

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
  (when yunge-reader-webview--buffer-view
    (yunge-reader-webview--sync-view
     yunge-reader-webview--buffer-view)))

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

(defun yunge-reader-epub--request
    (document operation _arguments complete)
  "Dispatch one EPUB DOCUMENT OPERATION through COMPLETE."
  (pcase operation
    ('outline
     (let* ((handle (yunge-reader-document-handle document))
            (view yunge-reader-webview--buffer-view))
       (if (and (yunge-reader-epub-handle-p handle)
                view
                (not (yunge-reader-webview--view-destroyed view))
                (= (yunge-reader-epub-handle-publication handle)
                   (yunge-reader-webview--view-publication view)))
           (yunge-reader-webview--request-view-outline
            view
            (apply-partially
             #'yunge-reader-epub--outline-complete complete))
         (funcall
          complete nil
          (yunge-reader-epub--native-error
           "The current EPUB view cannot provide its outline")))))
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
      (call-interactively command))))

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
