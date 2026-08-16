;;; yunge-reader.el --- Extensible document reading -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'yunge-jump-history)
(require 'yunge-key)

(declare-function browse-url "browse-url" (url &rest arguments))
(declare-function evil-set-initial-state "evil-core" (mode state))

(defgroup yunge-reader nil
  "Read fixed-layout and reflowable documents."
  :group 'applications)

(defcustom yunge-reader-default-scale 1.0
  "Manual scale restored by `yunge-reader-zoom-reset'."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-minimum-scale 0.25
  "Smallest manual document scale."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-maximum-scale 8.0
  "Largest manual document scale."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-zoom-factor 1.2
  "Factor applied by each zoom step."
  :type 'number
  :group 'yunge-reader)

(defcustom yunge-reader-search-match-limit 64
  "Maximum search matches requested from one reader batch."
  :type '(integer :tag "Matches" 1 200)
  :group 'yunge-reader)

(defcustom yunge-reader-search-page-limit 8
  "Maximum document units scanned by one reader search batch."
  :type '(integer :tag "Units" 1 64)
  :group 'yunge-reader)

(defcustom yunge-reader-copy-unit-limit 8
  "Maximum document units read by one selection text batch."
  :type '(integer :tag "Units" 1 64)
  :group 'yunge-reader)

(defcustom yunge-reader-copy-character-limit 16384
  "Maximum indexed characters read by one selection text batch."
  :type '(integer :tag "Characters" 1 65536)
  :group 'yunge-reader)

(defcustom yunge-reader-place-limit 1000
  "Maximum number of durable document places to retain."
  :type '(integer :tag "Places" 1)
  :group 'yunge-reader)

(defcustom yunge-reader-uri-schemes '("https" "http" "mailto")
  "URI schemes that document actions may open through `browse-url'."
  :type '(repeat (string :tag "Scheme"))
  :group 'yunge-reader)

(defconst yunge-reader-uri-maximum-bytes 4096
  "Maximum encoded size accepted for one document URI action.")

(defconst yunge-reader-place-version 1
  "Current durable Reader place format version.")

(define-error 'yunge-reader-no-driver
  "No Yunge Reader driver accepts the document")

(cl-defstruct (yunge-reader-driver
               (:constructor yunge-reader--make-driver))
  "A document format implementation.
MATCH-FUNCTION receives an absolute file name.  OPEN-FUNCTION receives that
file and a completion function, which it calls with HANDLE, a properties
plist, and an error value.  CLOSE-FUNCTION receives a `yunge-reader-document'.
REQUEST-FUNCTION receives a document, operation, argument plist, and a
completion function, which it calls with a value and an error value.
LOCATION-FUNCTION receives a document and window and returns a stable
`yunge-reader-position'.  RESTORE-FUNCTION receives a document, position,
and window and returns non-nil after accepting the location."
  name
  match-function
  open-function
  close-function
  request-function
  location-function
  restore-function)

(cl-defstruct yunge-reader-document
  "An open document owned by one reader driver."
  file
  driver
  handle
  layout
  metadata)

(cl-defstruct yunge-reader-position
  "A stable position in a document.
UNIT names a page or reflowable content unit.  OFFSET is a text offset or
driver-defined stable anchor.  X and Y are optional coordinates in the
unscaled coordinate system of UNIT."
  unit
  offset
  x
  y)

(cl-defstruct yunge-reader-selection
  "A logical document selection independent of its painted highlight."
  start
  end
  text)

(cl-defstruct yunge-reader-selection-batch
  "One concatenable batch of selected document text."
  text
  cursor
  done)

(cl-defstruct yunge-reader-search-result
  "One driver-neutral document search result."
  start
  end
  text
  before
  after)

(cl-defstruct yunge-reader-search-batch
  "One bounded batch of driver search results."
  results
  cursor
  done)

(cl-defstruct yunge-reader-action
  "One format-independent action exposed by a document."
  type
  position
  zoom-mode
  scale
  uri)

(cl-defstruct yunge-reader-outline-item
  "One entry in a flattened document outline."
  title
  depth
  action)

(cl-defstruct yunge-reader-outline-data
  "One bounded document outline returned by a driver."
  items
  truncated)

(defconst yunge-reader-outline-maximum-items 10000
  "Maximum number of outline entries accepted from one driver response.")

(defvar yunge-reader-drivers nil
  "Registered `yunge-reader-driver' objects in precedence order.")

(defvar yunge-reader-saved-places nil
  "Most recently used durable Reader places.
Each entry maps a canonical file name to versioned, printable place data.")

(defvar-local yunge-reader-document nil
  "Document displayed by the current reader buffer.")

(defvar-local yunge-reader--opening-file nil
  "Absolute file currently being opened, or nil.")

(defvar-local yunge-reader--open-generation 0
  "Generation used to reject late document-open completions.")

(defvar-local yunge-reader--pending-place nil
  "Durable place waiting for the current document to finish opening.")

(defvar-local yunge-reader--place-recording-enabled nil
  "Whether the current document may replace its durable place.")

(defvar-local yunge-reader--restoring-place nil
  "Whether the current view is restoring a durable place.")

(defvar-local yunge-reader-zoom-mode 'fit-width
  "Current zoom mode: `manual', `fit-width', or `fit-page'.")

(defvar-local yunge-reader-scale 1.0
  "Manual document scale used when `yunge-reader-zoom-mode' is `manual'.")

(defvar-local yunge-reader-effective-scale nil
  "Scale most recently resolved by the active view adapter.")

(defvar-local yunge-reader-selection nil
  "Current logical `yunge-reader-selection', or nil.")

(defvar-local yunge-reader--copy-generation 0
  "Generation used to reject late selection text completions.")

(defvar-local yunge-reader--copy-pending nil
  "Non-nil while the current selection is being copied in batches.")

(defvar-local yunge-reader-search-query nil
  "Literal query active in the current reader buffer, or nil.")

(defvar-local yunge-reader-search-results nil
  "Search results loaded so far in document order.")

(defvar-local yunge-reader-search-result nil
  "Current `yunge-reader-search-result', or nil.")

(defvar-local yunge-reader-search-result-hook nil
  "Hook run after `yunge-reader-search-result' changes.")

(defvar-local yunge-reader--search-case-sensitive nil
  "Whether the active reader search distinguishes case.")

(defvar-local yunge-reader--search-index nil
  "Zero-based index of the current result in loaded search results.")

(defvar-local yunge-reader--search-cursor nil
  "Driver-neutral cursor for the next search batch.")

(defvar-local yunge-reader--search-done nil
  "Non-nil after the active search reaches the document end.")

(defvar-local yunge-reader--search-pending nil
  "Non-nil while one search batch is outstanding.")

(defvar-local yunge-reader--search-generation 0
  "Generation used to reject late reader search completions.")

(defvar-local yunge-reader--outline nil
  "Cached `yunge-reader-outline-data' for the current document.")

(defvar-local yunge-reader--outline-loaded nil
  "Whether the current document outline has finished loading.")

(defvar-local yunge-reader--outline-pending nil
  "Non-nil while one document outline request is outstanding.")

(defvar-local yunge-reader--outline-generation 0
  "Generation used to reject late document outline completions.")

(defvar-local yunge-reader-refresh-hook nil
  "Hook run after the current reader view becomes invalid.
Drivers or view adapters use this buffer-local hook to request visible
artifacts.  Functions run in the reader buffer without arguments.")

(defconst yunge-reader-normal-bindings
  '(("+" yunge-reader-zoom-in "zoom in")
    ("-" yunge-reader-zoom-out "zoom out")
    ("=" yunge-reader-zoom-reset "reset zoom")
    ("/" yunge-reader-search "search")
    ("N" yunge-reader-search-previous "previous match")
    ("P" yunge-reader-fit-page "fit page")
    ("W" yunge-reader-fit-width "fit width")
    ("gr" yunge-reader-refresh "refresh")
    ("n" yunge-reader-search-next "next match")
    ("o" yunge-reader-outline "outline")
    ("q" quit-window "quit")
    ("y" yunge-reader-copy-selection "copy selection"))
  "Normal-state bindings shared by Yunge Reader adapters.")

(defvar-keymap yunge-reader-mode-map
  :parent special-mode-map
  "+" #'yunge-reader-zoom-in
  "-" #'yunge-reader-zoom-out
  "=" #'yunge-reader-zoom-reset
  "/" #'yunge-reader-search
  "N" #'yunge-reader-search-previous
  "P" #'yunge-reader-fit-page
  "W" #'yunge-reader-fit-width
  "C-g" #'yunge-reader-clear-selection
  "g r" #'yunge-reader-refresh
  "n" #'yunge-reader-search-next
  "o" #'yunge-reader-outline
  "q" #'quit-window
  "y" #'yunge-reader-copy-selection)

(define-derived-mode yunge-reader-mode special-mode "Yunge Reader"
  "Major mode shared by Yunge Reader document adapters."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (setq-local yunge-reader-scale yunge-reader-default-scale)
  (setq-local yunge-reader--copy-generation 0)
  (setq-local yunge-reader--copy-pending nil)
  (add-hook 'kill-buffer-hook #'yunge-reader--close-document nil t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'yunge-reader-mode 'normal)
  (yunge-key-evil-define 'normal yunge-reader-mode-map
                         yunge-reader-normal-bindings))

(cl-defun yunge-reader-register-driver
    (name &key match open close request location restore)
  "Register a reader driver NAME.
MATCH, OPEN, CLOSE, and REQUEST follow the contracts documented by
`yunge-reader-driver'.  LOCATION and RESTORE are an optional pair.
Registering NAME again atomically replaces its old definition and gives the
new definition highest precedence."
  (unless (symbolp name)
    (error "Reader driver name must be a symbol: %S" name))
  (dolist (function (list match open close request))
    (unless (functionp function)
      (error "Reader driver %s has a non-function member: %S"
             name function)))
  (unless (eq (null location) (null restore))
    (error "Reader driver %s must define both location functions" name))
  (dolist (function (delq nil (list location restore)))
    (unless (functionp function)
      (error "Reader driver %s has a non-function member: %S"
             name function)))
  (let ((driver
         (yunge-reader--make-driver
          :name name
          :match-function match
          :open-function open
          :close-function close
          :request-function request
          :location-function location
          :restore-function restore)))
    (setq yunge-reader-drivers
          (cons
           driver
           (seq-remove
            (lambda (candidate)
              (eq (yunge-reader-driver-name candidate) name))
            yunge-reader-drivers)))
    driver))

(defun yunge-reader-unregister-driver (name)
  "Unregister reader driver NAME."
  (setq yunge-reader-drivers
        (seq-remove
         (lambda (driver)
           (eq (yunge-reader-driver-name driver) name))
         yunge-reader-drivers)))

(defun yunge-reader-driver-for-file (file)
  "Return the first registered driver accepting FILE, or nil."
  (let ((absolute (expand-file-name file)))
    (seq-find
     (lambda (driver)
       (funcall (yunge-reader-driver-match-function driver) absolute))
     yunge-reader-drivers)))

(defun yunge-reader--place-file-key (file)
  "Return the canonical durable-place key for FILE."
  (let ((absolute (expand-file-name file)))
    (or (ignore-errors (file-truename absolute)) absolute)))

(defun yunge-reader--position-data (position)
  "Return printable durable data for reader POSITION."
  (list
   :unit (copy-tree (yunge-reader-position-unit position) t)
   :offset (copy-tree (yunge-reader-position-offset position) t)
   :x (yunge-reader-position-x position)
   :y (yunge-reader-position-y position)))

(defun yunge-reader--position-data-p (data)
  "Return whether DATA represents a durable reader position."
  (and (listp data)
       (plist-member data :unit)
       (let ((x (plist-get data :x))
             (y (plist-get data :y)))
         (and (or (null x) (numberp x))
              (or (null y) (numberp y))))))

(defun yunge-reader--position-from-data (data)
  "Return the reader position represented by durable DATA."
  (when (yunge-reader--position-data-p data)
    (make-yunge-reader-position
     :unit (copy-tree (plist-get data :unit) t)
     :offset (copy-tree (plist-get data :offset) t)
     :x (plist-get data :x)
     :y (plist-get data :y))))

(defun yunge-reader--make-place (driver position)
  "Return a printable place for DRIVER at POSITION."
  (list
   :version yunge-reader-place-version
   :driver (yunge-reader-driver-name driver)
   :position (yunge-reader--position-data position)
   :zoom-mode yunge-reader-zoom-mode
   :scale yunge-reader-scale))

(defun yunge-reader--place-p (place driver)
  "Return whether PLACE is valid for DRIVER."
  (and (listp place)
       (equal (plist-get place :version)
              yunge-reader-place-version)
       (eq (plist-get place :driver)
           (yunge-reader-driver-name driver))
       (yunge-reader--position-data-p
        (plist-get place :position))
       (memq (plist-get place :zoom-mode)
             '(manual fit-width fit-page))
       (let ((scale (plist-get place :scale)))
         (and (numberp scale) (> scale 0)))))

(defun yunge-reader--saved-place (file driver)
  "Return the valid durable place for FILE and DRIVER, or nil."
  (let* ((key (yunge-reader--place-file-key file))
         (place (cdr (assoc key yunge-reader-saved-places))))
    (when (yunge-reader--place-p place driver)
      (copy-tree place t))))

(defun yunge-reader--store-place (file place)
  "Store durable PLACE for FILE as the most recent Reader place."
  (let ((key (yunge-reader--place-file-key file)))
    (setq yunge-reader-saved-places
          (cons
           (cons key (copy-tree place t))
           (seq-remove
            (lambda (entry) (equal (car-safe entry) key))
            yunge-reader-saved-places)))
    (when (> (length yunge-reader-saved-places)
             yunge-reader-place-limit)
      (setcdr (nthcdr (1- yunge-reader-place-limit)
                      yunge-reader-saved-places)
              nil))))

(defun yunge-reader--place-window (&optional window)
  "Return a live WINDOW displaying the current Reader buffer."
  (let ((window
         (or window
             (and (eq (window-buffer (selected-window))
                      (current-buffer))
                  (selected-window))
             (get-buffer-window (current-buffer) t))))
    (and (window-live-p window)
         (eq (window-buffer window) (current-buffer))
         window)))

(defun yunge-reader--current-place (&optional window)
  "Return the current Reader place viewed in WINDOW, or nil."
  (when yunge-reader-document
    (let* ((driver
            (yunge-reader-document-driver yunge-reader-document))
           (location
            (yunge-reader-driver-location-function driver))
           (window (yunge-reader--place-window window)))
      (when (and location window)
        (when-let* ((position
                     (funcall location yunge-reader-document window)))
          (unless (yunge-reader-position-p position)
            (error "Reader driver returned an invalid place: %S"
                   position))
          (yunge-reader--make-place driver position))))))

(defun yunge-reader-record-place (&optional window)
  "Record the current durable Reader place as viewed in WINDOW.
Do nothing until document opening and any prior place restoration commit."
  (when (and yunge-reader--place-recording-enabled
             (not yunge-reader--restoring-place)
             yunge-reader-document)
    (condition-case error-data
        (when-let* ((place (yunge-reader--current-place window)))
          (yunge-reader--store-place
           (yunge-reader-document-file yunge-reader-document)
           place))
      (error
       (display-warning
        'yunge-reader
        (format "Could not remember Reader place: %s"
                (error-message-string error-data))
        :warning)))))

(defun yunge-reader--restore-view-state (place)
  "Restore generic zoom state from durable PLACE."
  (setq yunge-reader-zoom-mode (plist-get place :zoom-mode)
        yunge-reader-scale
        (yunge-reader--clamp-scale (plist-get place :scale))
        yunge-reader-effective-scale nil))

(defun yunge-reader--apply-place (place &optional window)
  "Apply validated Reader PLACE in WINDOW and return whether it succeeded."
  (let* ((driver
          (yunge-reader-document-driver yunge-reader-document))
         (restore (yunge-reader-driver-restore-function driver)))
    (when (and restore (yunge-reader--place-p place driver))
      (yunge-reader--restore-view-state place)
      (yunge-reader-refresh)
      (and
       (funcall
        restore yunge-reader-document
        (yunge-reader--position-from-data
         (plist-get place :position))
        (yunge-reader--place-window window))
       t))))

(defun yunge-reader--restore-live-place (place window)
  "Restore live Reader PLACE in WINDOW without committing partial state."
  (let ((origin (yunge-reader--current-place window))
        (recording yunge-reader--place-recording-enabled)
        accepted
        failure)
    (let ((yunge-reader--restoring-place t))
      (setq yunge-reader--place-recording-enabled nil)
      (unwind-protect
          (condition-case error-data
              (setq accepted
                    (yunge-reader--apply-place place window))
            (error (setq failure error-data)))
        (unless accepted
          (when origin
            (ignore-errors
              (yunge-reader--apply-place origin window))))
        (setq yunge-reader--place-recording-enabled recording)))
    (when failure
      (signal (car failure) (cdr failure)))
    (when accepted
      (yunge-reader-record-place window))
    accepted))

(defun yunge-reader--restore-open-place ()
  "Build the opened view, restore its pending place, and permit writes."
  (let* ((place yunge-reader--pending-place)
         (accepted t)
         (yunge-reader--restoring-place t))
    (setq yunge-reader--place-recording-enabled nil)
    (when place
      (setq accepted
            (yunge-reader--apply-place place)))
    (unless place
      (yunge-reader-refresh))
    (setq yunge-reader--pending-place nil)
    (when accepted
      (setq yunge-reader--place-recording-enabled t))
    accepted))

(defun yunge-reader--buffer-file (buffer)
  "Return the document file associated with reader BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (and yunge-reader-document
               (yunge-reader-document-file yunge-reader-document))
          yunge-reader--opening-file))))

(defun yunge-reader--existing-buffer (file)
  "Return a live reader buffer for FILE, or nil."
  (seq-find
   (lambda (buffer)
     (let ((buffer-file (yunge-reader--buffer-file buffer)))
       (and buffer-file
            (with-current-buffer buffer
              (derived-mode-p 'yunge-reader-mode))
            ;; Reader drivers may accept virtual or not-yet-existing files,
            ;; for which `file-equal-p' cannot establish identity.
            (or (ignore-errors (file-equal-p file buffer-file))
                (equal file (expand-file-name buffer-file))))))
   (buffer-list)))

(defun yunge-reader--jump-target (window _position)
  "Capture the current Reader location as an immutable jump target."
  (when (and yunge-reader--place-recording-enabled
             (not yunge-reader--restoring-place)
             yunge-reader-document)
    (when-let* ((place (yunge-reader--current-place window)))
      (list
       :file (yunge-reader-document-file yunge-reader-document)
       :place (copy-tree place t)))))

(defun yunge-reader--same-jump-target-p (left right)
  "Return whether Reader jump targets LEFT and RIGHT are equivalent."
  (let ((left-file (plist-get left :file))
        (right-file (plist-get right :file)))
    (and (stringp left-file)
         (stringp right-file)
         (equal
          (yunge-reader--place-file-key left-file)
          (yunge-reader--place-file-key right-file))
         (equal (plist-get left :place) (plist-get right :place)))))

(defun yunge-reader--window-point (window)
  "Return WINDOW's point, including its live selected-window point."
  (if (eq window (selected-window))
      (with-current-buffer (window-buffer window)
        (point))
    (window-point window)))

(defun yunge-reader--window-state (window)
  "Capture WINDOW state needed to undo a failed Reader visit."
  (list
   :buffer (window-buffer window)
   :point (yunge-reader--window-point window)
   :start (window-start window)
   :vscroll (window-vscroll window t)
   :hscroll (window-hscroll window)))

(defun yunge-reader--window-state-current-p (window state)
  "Return non-nil when WINDOW still has captured STATE."
  (and
   (window-live-p window)
   (eq (window-buffer window) (plist-get state :buffer))
   (= (yunge-reader--window-point window)
      (plist-get state :point))
   (= (window-start window) (plist-get state :start))
   (= (window-vscroll window t) (plist-get state :vscroll))
   (= (window-hscroll window) (plist-get state :hscroll))))

(defun yunge-reader--restore-window-state (window state)
  "Restore WINDOW from captured STATE when its buffer remains live."
  (let ((buffer (plist-get state :buffer)))
    (when (and (window-live-p window) (buffer-live-p buffer))
      (set-window-buffer window buffer)
      (set-window-point window (plist-get state :point))
      (set-window-start window (plist-get state :start) t)
      (set-window-vscroll window (plist-get state :vscroll) t)
      (set-window-hscroll window (plist-get state :hscroll))
      t)))

(defun yunge-reader--display-jump-place (buffer place window)
  "Display live Reader BUFFER at PLACE in WINDOW."
  (when (and (buffer-live-p buffer) (window-live-p window))
    (select-window window)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (and yunge-reader-document
           (yunge-reader--restore-live-place place window)))))

(defun yunge-reader--visit-new-jump-target
    (file driver place window origin-state complete)
  "Open FILE with DRIVER at PLACE in WINDOW, then call COMPLETE."
  (let ((buffer
         (generate-new-buffer
          (format "*Reader: %s*" (file-name-nondirectory file)))))
    (with-current-buffer buffer
      (yunge-reader-mode))
    (condition-case error-data
        (yunge-reader--begin-open
         buffer driver file place
         (lambda (opened)
           (let ((window-unchanged
                  (yunge-reader--window-state-current-p
                   window origin-state))
                 displayed)
             (when (and opened window-unchanged)
               (condition-case nil
                   (setq displayed
                         (yunge-reader--display-jump-place
                          buffer place window))
                 (error nil)))
             (unless displayed
               (when window-unchanged
                 (yunge-reader--restore-window-state
                  window origin-state))
               (when (buffer-live-p buffer)
                 (kill-buffer buffer)))
             (funcall
              complete
              (cond
               (displayed t)
               ((not window-unchanged) :cancel))))))
      (error
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (signal (car error-data) (cdr error-data))))))

(defun yunge-reader--visit-jump-target (value window complete)
  "Visit Reader jump target VALUE in WINDOW, then call COMPLETE."
  (let* ((file (plist-get value :file))
         (place (plist-get value :place))
         (driver (and (stringp file)
                      (yunge-reader-driver-for-file file))))
    (if (not (and driver
                  (yunge-reader--place-p place driver)
                  (window-live-p window)))
        (funcall complete nil)
      (let ((origin-state (yunge-reader--window-state window))
            (existing (yunge-reader--existing-buffer file)))
        (if existing
            (let ((displayed
                   (condition-case nil
                       (with-current-buffer existing
                         (and yunge-reader-document
                              (yunge-reader--display-jump-place
                               existing place window)))
                     (error nil))))
              (unless displayed
                (yunge-reader--restore-window-state window origin-state))
              (funcall complete (and displayed t)))
          (yunge-reader--visit-new-jump-target
           file driver place window origin-state complete))))))

(defun yunge-reader--display-status (format-string &rest arguments)
  "Replace the current reader buffer with formatted status text."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (apply #'format format-string arguments) "\n")))

(defun yunge-reader--close-handle (driver file handle properties)
  "Close HANDLE returned for FILE by DRIVER using PROPERTIES."
  (when handle
    (condition-case error-data
        (funcall
         (yunge-reader-driver-close-function driver)
         (make-yunge-reader-document
          :file file
          :driver driver
          :handle handle
          :layout (plist-get properties :layout)
          :metadata (plist-get properties :metadata)))
      (error
       (display-warning
        'yunge-reader
        (format "Could not close late reader document: %s"
                (error-message-string error-data))
        :warning)))))

(defun yunge-reader--finish-open
    (buffer generation driver file handle properties error-data complete)
  "Finish opening FILE in BUFFER for GENERATION.
DRIVER produced HANDLE, PROPERTIES, and ERROR-DATA.  Call COMPLETE with
non-nil after the initial view and pending place are ready."
  (if (not (and (buffer-live-p buffer)
                (with-current-buffer buffer
                  (= generation yunge-reader--open-generation))))
      (progn
        (yunge-reader--close-handle driver file handle properties)
        (when complete
          (funcall complete nil)))
    (with-current-buffer buffer
      (setq yunge-reader--opening-file nil)
      (if error-data
          (progn
            (setq yunge-reader--pending-place nil
                  yunge-reader--place-recording-enabled nil)
            (yunge-reader--display-status
             "Could not open %s:\n\n%s"
             file (error-message-string error-data))
            (when complete
              (funcall complete nil)))
        (let ((layout (plist-get properties :layout)))
          (unless (memq layout '(fixed reflow))
            (setq error-data
                  (list 'error
                        (format "Driver returned invalid layout: %S"
                                layout))))
          (if error-data
              (progn
                (setq yunge-reader--pending-place nil
                      yunge-reader--place-recording-enabled nil)
                (yunge-reader--close-handle
                 driver file handle properties)
                (yunge-reader--display-status
                 "Could not open %s:\n\n%s"
                 file (error-message-string error-data))
                (when complete
                  (funcall complete nil)))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file
                   :driver driver
                   :handle handle
                   :layout layout
                   :metadata (plist-get properties :metadata)))
            (yunge-reader--display-status
             "%s\n\nLayout: %s\nDriver: %s"
             (file-name-nondirectory file)
             layout
             (yunge-reader-driver-name driver))
            (let (accepted restore-error)
              (condition-case error-data
                  (setq accepted
                        (yunge-reader--restore-open-place))
                (error (setq restore-error error-data)))
              (if restore-error
                  (progn
                    (setq yunge-reader--pending-place nil
                          yunge-reader--place-recording-enabled nil)
                    (yunge-reader--display-status
                     "Could not prepare %s:\n\n%s"
                     file (error-message-string restore-error)))
                (when accepted
                  (yunge-reader-record-place)))
              (when complete
                (funcall complete accepted)))))))))

(defun yunge-reader--begin-open
    (buffer driver file &optional place complete)
  "Ask DRIVER to open FILE for reader BUFFER.
Restore explicit PLACE instead of the saved place.  Call COMPLETE with
non-nil only after opening and restoration succeed."
  (when (and place (not (yunge-reader--place-p place driver)))
    (error "Reader jump contains an invalid place: %S" place))
  (unless (or (null complete) (functionp complete))
    (error "Reader open completion must be a function: %S" complete))
  (with-current-buffer buffer
    (setq yunge-reader--opening-file file
          yunge-reader--pending-place
          (or (and place (copy-tree place t))
              (yunge-reader--saved-place file driver))
          yunge-reader--place-recording-enabled nil
          yunge-reader--outline nil
          yunge-reader--outline-loaded nil
          yunge-reader--outline-pending nil)
    (cl-incf yunge-reader--open-generation)
    (cl-incf yunge-reader--outline-generation)
    (yunge-reader--display-status "Opening %s..." file)
    (let ((generation yunge-reader--open-generation)
          completed)
      (condition-case error-data
          (funcall
           (yunge-reader-driver-open-function driver)
           file
           (lambda (handle properties open-error)
             (if completed
                 (progn
                   (yunge-reader--close-handle
                    driver file handle properties)
                   (display-warning
                    'yunge-reader
                    (format "Reader driver %s completed open twice"
                            (yunge-reader-driver-name driver))
                    :warning))
               (setq completed t)
               (yunge-reader--finish-open
                buffer generation driver file handle properties
                open-error complete))))
        (error
         (unless completed
           (setq completed t)
           (yunge-reader--finish-open
            buffer generation driver file nil nil error-data
            complete)))))))

;;;###autoload
(defun yunge-reader-open (file)
  "Open FILE with its registered Yunge Reader driver.
Return the reader buffer immediately; drivers may finish opening
asynchronously."
  (interactive "fRead document: ")
  (setq file (expand-file-name file))
  (if-let* ((existing (yunge-reader--existing-buffer file)))
      (progn
        (pop-to-buffer existing)
        existing)
    (let ((driver (yunge-reader-driver-for-file file)))
      (unless driver
        (signal 'yunge-reader-no-driver (list file)))
      (let ((buffer
             (generate-new-buffer
              (format "*Reader: %s*" (file-name-nondirectory file)))))
        (with-current-buffer buffer
          (yunge-reader-mode))
        (pop-to-buffer buffer)
        (yunge-reader--begin-open buffer driver file)
        buffer))))

(defun yunge-reader--close-document ()
  "Close the native or external document owned by the current buffer."
  (cl-incf yunge-reader--open-generation)
  (cl-incf yunge-reader--search-generation)
  (cl-incf yunge-reader--outline-generation)
  (cl-incf yunge-reader--copy-generation)
  (setq yunge-reader--outline nil
        yunge-reader--outline-loaded nil
        yunge-reader--outline-pending nil
        yunge-reader--copy-pending nil)
  (when yunge-reader-document
    (yunge-reader-record-place)
    (let* ((document yunge-reader-document)
           (driver (yunge-reader-document-driver document)))
      (setq yunge-reader-document nil
            yunge-reader--pending-place nil
            yunge-reader--place-recording-enabled nil)
      (condition-case error-data
          (funcall (yunge-reader-driver-close-function driver) document)
        (error
         (display-warning
          'yunge-reader
          (format "Could not close reader document: %s"
                  (error-message-string error-data))
          :warning))))))

(defun yunge-reader-request (operation arguments complete)
  "Request OPERATION with ARGUMENTS for the current document.
COMPLETE is called exactly once by the driver with a value and error value."
  (unless yunge-reader-document
    (user-error "This reader buffer has no open document"))
  (unless (functionp complete)
    (error "Reader completion must be a function: %S" complete))
  (let ((driver (yunge-reader-document-driver yunge-reader-document))
        completed)
    (condition-case error-data
        (funcall
         (yunge-reader-driver-request-function driver)
         yunge-reader-document operation arguments
         (lambda (value request-error)
           (unless completed
             (setq completed t)
             (funcall complete value request-error))))
      (error
       (unless completed
         (setq completed t)
         (funcall complete nil error-data))))))

(defun yunge-reader--uri-scheme (uri)
  "Return URI's lowercase explicit scheme, or nil."
  (when (and (stringp uri)
             (string-match
              "\\`\\([A-Za-z][A-Za-z0-9+.-]*\\):" uri))
    (downcase (match-string 1 uri))))

(defun yunge-reader--uri-valid-p (uri)
  "Return non-nil when URI is bounded and structurally safe to open."
  (and (stringp uri)
       (not (string-empty-p uri))
       (<= (string-bytes uri) yunge-reader-uri-maximum-bytes)
       (not (string-match-p "[[:space:][:cntrl:]]" uri))
       (yunge-reader--uri-scheme uri)))

(defun yunge-reader--uri-allowed-p (uri)
  "Return non-nil when URI uses an allowed document action scheme."
  (when-let* ((scheme (yunge-reader--uri-scheme uri)))
    (seq-some
     (lambda (allowed)
       (and (stringp allowed)
            (string-equal scheme (downcase allowed))))
     yunge-reader-uri-schemes)))

(defun yunge-reader--location-action-valid-p (action)
  "Return non-nil when ACTION is a valid location action."
  (and (eq (yunge-reader-action-type action) 'location)
       (yunge-reader-position-p
        (yunge-reader-action-position action))
       (memq (yunge-reader-action-zoom-mode action)
             '(nil manual fit-width fit-page))
       (let ((scale (yunge-reader-action-scale action)))
         (or (null scale)
             (and (numberp scale) (> scale 0))))
       (null (yunge-reader-action-uri action))))

(defun yunge-reader--uri-action-valid-p (action)
  "Return non-nil when ACTION is a structurally valid URI action."
  (and (eq (yunge-reader-action-type action) 'uri)
       (null (yunge-reader-action-position action))
       (null (yunge-reader-action-zoom-mode action))
       (null (yunge-reader-action-scale action))
       (yunge-reader--uri-valid-p
        (yunge-reader-action-uri action))))

(defun yunge-reader--action-valid-p (action)
  "Return non-nil when ACTION is supported by the Reader core."
  (and (yunge-reader-action-p action)
       (or (yunge-reader--location-action-valid-p action)
           (yunge-reader--uri-action-valid-p action))))

(defun yunge-reader--outline-item-valid-p (item)
  "Return non-nil when ITEM follows the generic outline contract."
  (and (yunge-reader-outline-item-p item)
       (stringp (yunge-reader-outline-item-title item))
       (not
        (string-empty-p
         (string-trim
          (yunge-reader-outline-item-title item))))
       (natnump (yunge-reader-outline-item-depth item))
       (let ((action (yunge-reader-outline-item-action item)))
         (or (null action)
             (and (yunge-reader-action-p action)
                  (yunge-reader--location-action-valid-p action))))))

(defun yunge-reader--outline-valid-p (outline)
  "Return non-nil when OUTLINE follows the generic outline contract."
  (and (yunge-reader-outline-data-p outline)
       (proper-list-p (yunge-reader-outline-data-items outline))
       (<= (length (yunge-reader-outline-data-items outline))
           yunge-reader-outline-maximum-items)
       (cl-every #'yunge-reader--outline-item-valid-p
                 (yunge-reader-outline-data-items outline))
       (memq (yunge-reader-outline-data-truncated outline)
             '(nil t))))

(defun yunge-reader--outline-label (item)
  "Return a compact hierarchy label for outline ITEM."
  (concat
   (make-string
    (* 2 (min 20 (yunge-reader-outline-item-depth item)))
    ?\s)
   (yunge-reader-outline-item-title item)))

(defun yunge-reader--outline-candidates (outline)
  "Return unique completion candidates for actionable OUTLINE entries."
  (let ((counts (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (used (make-hash-table :test #'equal))
        labeled)
    (dolist (item (yunge-reader-outline-data-items outline))
      (when (yunge-reader-outline-item-action item)
        (let ((label (yunge-reader--outline-label item)))
          (push (cons label item) labeled)
          (puthash label (1+ (gethash label counts 0)) counts))))
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

(defun yunge-reader--action-place (action)
  "Return a Reader place for location ACTION."
  (let* ((driver
          (yunge-reader-document-driver yunge-reader-document))
         (place
          (yunge-reader--make-place
           driver (yunge-reader-action-position action))))
    (when-let* ((mode (yunge-reader-action-zoom-mode action)))
      (setq place (plist-put place :zoom-mode mode)))
    (when-let* ((scale (yunge-reader-action-scale action)))
      (setq place (plist-put place :scale scale)))
    place))

(defun yunge-reader--follow-location-action (action)
  "Follow location ACTION and return non-nil on success."
  (let ((window (yunge-reader--place-window)))
    (unless window
      (user-error "The Reader buffer is not displayed in a live window"))
    (unless
        (yunge-reader--restore-live-place
         (yunge-reader--action-place action) window)
      (user-error "The Reader driver rejected the destination"))
    t))

(defun yunge-reader--follow-uri-action (action)
  "Open URI ACTION through the configured safe scheme policy."
  (let* ((uri (yunge-reader-action-uri action))
         (scheme (yunge-reader--uri-scheme uri)))
    (unless (yunge-reader--uri-allowed-p uri)
      (user-error "Document URI scheme is not allowed: %s"
                  (or scheme "none")))
    (require 'browse-url)
    (browse-url uri)
    (message "Opened document URI: %s"
             (truncate-string-to-width uri 120 nil nil t))
    t))

(defun yunge-reader--follow-action (action)
  "Follow supported Reader ACTION and return non-nil on success."
  (unless (yunge-reader--action-valid-p action)
    (user-error "This document action has no supported destination"))
  (pcase (yunge-reader-action-type action)
    ('location (yunge-reader--follow-location-action action))
    ('uri (yunge-reader--follow-uri-action action))))

(defun yunge-reader--follow-outline-item (item)
  "Follow the location action carried by outline ITEM."
  (let ((action (yunge-reader-outline-item-action item)))
    (unless action
      (user-error "This outline entry has no supported destination"))
    (yunge-reader--follow-action action)
    (message "Outline: %s" (yunge-reader-outline-item-title item))
    t))

(defun yunge-reader--select-outline-item (outline)
  "Prompt for and follow one actionable item from OUTLINE."
  (let ((candidates (yunge-reader--outline-candidates outline)))
    (if (null candidates)
        (message "This document has no usable outline destinations")
      (let* ((completion-extra-properties
              '(:category yunge-reader-outline))
             (choice
              (completing-read
               (if (yunge-reader-outline-data-truncated outline)
                   "Outline (truncated): "
                 "Outline: ")
               candidates nil t))
             (item (cdr (assoc choice candidates))))
        (when item
          (yunge-reader--follow-outline-item item))))))

(defun yunge-reader--complete-outline
    (buffer document generation window state value error-data)
  "Complete an outline request made from BUFFER and WINDOW."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (= generation yunge-reader--outline-generation)
                 (eq document yunge-reader-document))
        (setq yunge-reader--outline-pending nil)
        (cond
         (error-data
          (display-warning
           'yunge-reader
           (format "Could not load document outline: %s"
                   (error-message-string error-data))
           :warning))
         ((not (yunge-reader--outline-valid-p value))
          (display-warning
           'yunge-reader
           "Reader driver returned an invalid document outline"
           :warning))
         (t
          (setq yunge-reader--outline value
                yunge-reader--outline-loaded t)
          (if (and (eq (selected-window) window)
                   (not (active-minibuffer-window))
                   (yunge-reader--window-state-current-p window state))
              (condition-case error-data
                  (yunge-reader--select-outline-item value)
                (quit nil)
                (error
                 (display-warning
                  'yunge-reader
                  (format "Could not follow document outline: %s"
                          (error-message-string error-data))
                  :warning)))
            (message "Document outline loaded; press o to open it"))))))))

(defun yunge-reader-outline ()
  "Choose a destination from the current document outline."
  (interactive)
  (unless yunge-reader-document
    (user-error "This reader buffer has no open document"))
  (cond
   (yunge-reader--outline-loaded
    (yunge-reader--select-outline-item yunge-reader--outline))
   (yunge-reader--outline-pending
    (message "Document outline is still loading"))
   (t
    (let* ((buffer (current-buffer))
           (document yunge-reader-document)
           (generation yunge-reader--outline-generation)
           (window (yunge-reader--place-window)))
      (unless window
        (user-error "The Reader buffer is not displayed in a live window"))
      (let ((state (yunge-reader--window-state window)))
        (setq yunge-reader--outline-pending t)
        (yunge-reader-request
         'outline nil
         (lambda (value error-data)
           (yunge-reader--complete-outline
            buffer document generation window state
            value error-data))))))))

(defun yunge-reader--search-smart-case-p (query)
  "Return non-nil when QUERY contains an uppercase character."
  (not (equal query (downcase query))))

(defun yunge-reader--search-result-valid-p (result)
  "Return non-nil when RESULT has stable reader endpoints."
  (and (yunge-reader-search-result-p result)
       (yunge-reader-position-p
        (yunge-reader-search-result-start result))
       (yunge-reader-position-p
        (yunge-reader-search-result-end result))))

(defun yunge-reader--search-batch-valid-p (batch)
  "Return non-nil when BATCH follows the generic search contract."
  (and (yunge-reader-search-batch-p batch)
       (proper-list-p (yunge-reader-search-batch-results batch))
       (cl-every #'yunge-reader--search-result-valid-p
                 (yunge-reader-search-batch-results batch))
       (or (null (yunge-reader-search-batch-cursor batch))
           (yunge-reader-position-p
            (yunge-reader-search-batch-cursor batch)))))

(defun yunge-reader--search-context (result)
  "Return one compact display context for RESULT."
  (truncate-string-to-width
   (string-trim
    (replace-regexp-in-string
     "[[:space:]]+" " "
     (concat (or (yunge-reader-search-result-before result) "")
             (or (yunge-reader-search-result-text result) "")
             (or (yunge-reader-search-result-after result) ""))))
   100 nil nil t))

(defun yunge-reader--set-search-index (index)
  "Make loaded search result INDEX current and notify the view."
  (let ((result (nth index yunge-reader-search-results)))
    (unless result
      (error "Reader search result index is unavailable: %S" index))
    (when-let* ((window (get-buffer-window (current-buffer) t)))
      (yunge-jump-history-record window))
    (setq yunge-reader--search-index index
          yunge-reader-search-result result)
    (run-hooks 'yunge-reader-search-result-hook)
    (message
     "Match %d%s: %s"
     (1+ index)
     (if yunge-reader--search-done
         (format "/%d" (length yunge-reader-search-results))
       "+")
     (yunge-reader--search-context result))
    result))

(defun yunge-reader--schedule-search-request
    (buffer generation intent)
  "Schedule BUFFER's next search request for GENERATION and INTENT."
  (run-at-time
   0 nil
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (= generation yunge-reader--search-generation)
           (yunge-reader--request-search-batch intent)))))))

(defun yunge-reader--complete-search-batch
    (buffer document generation intent old-count old-cursor value error-data)
  "Complete one search request in BUFFER for DOCUMENT and GENERATION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (= generation yunge-reader--search-generation)
                 (eq document yunge-reader-document))
        (setq yunge-reader--search-pending nil)
        (cond
         (error-data
          (display-warning
           'yunge-reader
           (format "Could not search document: %s"
                   (error-message-string error-data))
           :warning))
         ((not (yunge-reader--search-batch-valid-p value))
          (display-warning
           'yunge-reader
           "Reader driver returned an invalid search batch"
           :warning))
         (t
          (let* ((new-results
                  (yunge-reader-search-batch-results value))
                 (cursor (yunge-reader-search-batch-cursor value))
                 (done (yunge-reader-search-batch-done value)))
            (setq yunge-reader-search-results
                  (append yunge-reader-search-results new-results)
                  yunge-reader--search-cursor cursor
                  yunge-reader--search-done done)
            (cond
             ((and (memq intent '(first next)) new-results)
              (yunge-reader--set-search-index old-count))
             ((and (eq intent 'last) done
                   yunge-reader-search-results)
              (yunge-reader--set-search-index
               (1- (length yunge-reader-search-results))))
             ((and done yunge-reader-search-results
                   (eq intent 'next))
              (yunge-reader--set-search-index 0))
             (done
              (message "No matches for: %s"
                       yunge-reader-search-query))
             ((equal cursor old-cursor)
              (display-warning
               'yunge-reader
               "Reader search cursor did not advance"
               :warning))
             (t
              (yunge-reader--schedule-search-request
               buffer generation intent))))))))))

(defun yunge-reader--request-search-batch (intent)
  "Request the next search batch needed for INTENT."
  (unless yunge-reader--search-pending
    (let ((buffer (current-buffer))
          (document yunge-reader-document)
          (generation yunge-reader--search-generation)
          (old-count (length yunge-reader-search-results))
          (old-cursor yunge-reader--search-cursor))
      (setq yunge-reader--search-pending t)
      (yunge-reader-request
       'search
       (list :query yunge-reader-search-query
             :case-sensitive yunge-reader--search-case-sensitive
             :cursor yunge-reader--search-cursor
             :match-limit yunge-reader-search-match-limit
             :page-limit yunge-reader-search-page-limit)
       (lambda (value error-data)
         (yunge-reader--complete-search-batch
          buffer document generation intent old-count old-cursor
          value error-data))))))

(defun yunge-reader-search (query)
  "Search the current document for literal QUERY.
Case is ignored unless QUERY contains an uppercase character."
  (interactive
   (list (read-string "Search document: " yunge-reader-search-query)))
  (unless yunge-reader-document
    (user-error "This reader buffer has no open document"))
  (when (string-empty-p query)
    (user-error "Search query must not be empty"))
  (cl-incf yunge-reader--search-generation)
  (setq yunge-reader-search-query query
        yunge-reader-search-results nil
        yunge-reader-search-result nil
        yunge-reader--search-case-sensitive
        (yunge-reader--search-smart-case-p query)
        yunge-reader--search-index nil
        yunge-reader--search-cursor nil
        yunge-reader--search-done nil
        yunge-reader--search-pending nil)
  (run-hooks 'yunge-reader-search-result-hook)
  (message "Searching for: %s" query)
  (yunge-reader--request-search-batch 'first))

(defun yunge-reader-search-next ()
  "Visit the next match for the active document search."
  (interactive)
  (unless yunge-reader-search-query
    (user-error "There is no active document search"))
  (when yunge-reader--search-pending
    (user-error "Document search is still loading"))
  (cond
   ((and (natnump yunge-reader--search-index)
         (< (1+ yunge-reader--search-index)
            (length yunge-reader-search-results)))
    (yunge-reader--set-search-index
     (1+ yunge-reader--search-index)))
   (yunge-reader--search-done
    (if yunge-reader-search-results
        (yunge-reader--set-search-index 0)
      (message "No matches for: %s" yunge-reader-search-query)))
   (t
    (yunge-reader--request-search-batch 'next))))

(defun yunge-reader-search-previous ()
  "Visit the previous match for the active document search.
At the first loaded match, finish the bounded search before wrapping."
  (interactive)
  (unless yunge-reader-search-query
    (user-error "There is no active document search"))
  (when yunge-reader--search-pending
    (user-error "Document search is still loading"))
  (cond
   ((and (natnump yunge-reader--search-index)
         (> yunge-reader--search-index 0))
    (yunge-reader--set-search-index
     (1- yunge-reader--search-index)))
   (yunge-reader--search-done
    (if yunge-reader-search-results
        (yunge-reader--set-search-index
         (1- (length yunge-reader-search-results)))
      (message "No matches for: %s" yunge-reader-search-query)))
   (t
    (yunge-reader--request-search-batch 'last))))

(defun yunge-reader-clear-search ()
  "Clear the active document search and its view highlight."
  (interactive)
  (cl-incf yunge-reader--search-generation)
  (setq yunge-reader-search-query nil
        yunge-reader-search-results nil
        yunge-reader-search-result nil
        yunge-reader--search-index nil
        yunge-reader--search-cursor nil
        yunge-reader--search-done nil
        yunge-reader--search-pending nil)
  (run-hooks 'yunge-reader-search-result-hook)
  (message "Cleared document search"))

(defun yunge-reader-refresh ()
  "Invalidate and request the current reader view again."
  (interactive)
  (run-hooks 'yunge-reader-refresh-hook))

(defun yunge-reader--clamp-scale (scale)
  "Return SCALE restricted to the configured manual zoom range."
  (max yunge-reader-minimum-scale
       (min yunge-reader-maximum-scale scale)))

(defun yunge-reader--set-manual-scale (scale)
  "Set manual SCALE and refresh the current reader view."
  (setq yunge-reader-scale (yunge-reader--clamp-scale scale)
        yunge-reader-zoom-mode 'manual
        yunge-reader-effective-scale yunge-reader-scale)
  (yunge-reader-refresh)
  yunge-reader-scale)

(defun yunge-reader-zoom-in (&optional count)
  "Zoom in COUNT steps, defaulting to one."
  (interactive "p")
  (let ((base (or yunge-reader-effective-scale yunge-reader-scale)))
    (yunge-reader--set-manual-scale
     (* base (expt yunge-reader-zoom-factor (or count 1))))))

(defun yunge-reader-zoom-out (&optional count)
  "Zoom out COUNT steps, defaulting to one."
  (interactive "p")
  (let ((base (or yunge-reader-effective-scale yunge-reader-scale)))
    (yunge-reader--set-manual-scale
     (/ base (expt yunge-reader-zoom-factor (or count 1))))))

(defun yunge-reader-zoom-reset ()
  "Restore `yunge-reader-default-scale' in manual zoom mode."
  (interactive)
  (yunge-reader--set-manual-scale yunge-reader-default-scale))

(defun yunge-reader--set-fit-mode (mode)
  "Use fit MODE and refresh the current reader view."
  (setq yunge-reader-zoom-mode mode
        yunge-reader-effective-scale nil)
  (yunge-reader-refresh)
  mode)

(defun yunge-reader-fit-width ()
  "Fit document content to the selected window's width."
  (interactive)
  (yunge-reader--set-fit-mode 'fit-width))

(defun yunge-reader-fit-page ()
  "Fit one complete document unit inside the selected window."
  (interactive)
  (yunge-reader--set-fit-mode 'fit-page))

(defun yunge-reader-set-effective-scale (scale)
  "Record SCALE resolved by the active reader view adapter."
  (unless (and (numberp scale) (> scale 0))
    (error "Reader effective scale must be positive: %S" scale))
  (setq yunge-reader-effective-scale scale))

(defun yunge-reader-set-selection (start end &optional text)
  "Select the logical document range from START through END.
START and END are `yunge-reader-position' objects.  TEXT may be supplied by a
driver that already resolved the selected glyphs."
  (unless (and (yunge-reader-position-p start)
               (yunge-reader-position-p end))
    (error "Reader selection endpoints must be reader positions"))
  (cl-incf yunge-reader--copy-generation)
  (setq yunge-reader--copy-pending nil)
  (setq yunge-reader-selection
        (make-yunge-reader-selection
         :start start :end end :text text)))

(defun yunge-reader-clear-selection ()
  "Clear the logical selection in the current reader buffer."
  (interactive)
  (cl-incf yunge-reader--copy-generation)
  (setq yunge-reader-selection nil
        yunge-reader--copy-pending nil)
  (yunge-reader-refresh))

(defun yunge-reader--selection-batch-valid-p (batch)
  "Return non-nil when BATCH follows the selection text contract."
  (when (yunge-reader-selection-batch-p batch)
    (let ((cursor (yunge-reader-selection-batch-cursor batch))
          (done (yunge-reader-selection-batch-done batch)))
      (and (stringp (yunge-reader-selection-batch-text batch))
           (memq done '(nil t))
           (if done
               (null cursor)
             (yunge-reader-position-p cursor))))))

(defun yunge-reader--copy-current-p
    (document selection generation)
  "Return whether DOCUMENT copy of SELECTION at GENERATION is current."
  (and yunge-reader--copy-pending
       (= generation yunge-reader--copy-generation)
       (eq document yunge-reader-document)
       (eq selection yunge-reader-selection)))

(defun yunge-reader--copy-text (text)
  "Put nonempty selected TEXT in the kill ring."
  (unless (and (stringp text) (not (string-empty-p text)))
    (user-error "The document selection contains no text"))
  (kill-new text)
  (message "Copied document text")
  text)

(defun yunge-reader--schedule-selection-batch
    (buffer document selection generation cursor fragments)
  "Schedule the next selection batch for BUFFER and DOCUMENT."
  (run-at-time
   0 nil
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (yunge-reader--copy-current-p
                document selection generation)
           (yunge-reader--request-selection-batch
            buffer document selection generation cursor fragments)))))))

(defun yunge-reader--complete-selection-batch
    (buffer document selection generation old-cursor fragments
            value error-data)
  "Complete one selection text request made from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (yunge-reader--copy-current-p
             document selection generation)
        (cond
         (error-data
          (setq yunge-reader--copy-pending nil)
          (display-warning
           'yunge-reader
           (format "Could not copy document text: %s"
                   (error-message-string error-data))
           :warning))
         ((not (yunge-reader--selection-batch-valid-p value))
          (setq yunge-reader--copy-pending nil)
          (display-warning
           'yunge-reader
           "Reader driver returned an invalid selection text batch"
           :warning))
         (t
          (let* ((text (yunge-reader-selection-batch-text value))
                 (cursor (yunge-reader-selection-batch-cursor value))
                 (done (yunge-reader-selection-batch-done value))
                 (fragments (cons text fragments)))
            (cond
             (done
              (let ((complete-text
                     (mapconcat #'identity
                                (nreverse fragments) "")))
                (setq yunge-reader--copy-pending nil)
                (condition-case copy-error
                    (progn
                      (yunge-reader--copy-text complete-text)
                      (setf (yunge-reader-selection-text selection)
                            complete-text))
                  (error
                   (display-warning
                    'yunge-reader
                    (format "Could not copy document text: %s"
                            (error-message-string copy-error))
                    :warning)))))
             ((equal cursor old-cursor)
              (setq yunge-reader--copy-pending nil)
              (display-warning
               'yunge-reader
               "Reader selection text cursor did not advance"
               :warning))
             (t
              (yunge-reader--schedule-selection-batch
               buffer document selection generation cursor
               fragments))))))))))

(defun yunge-reader--request-selection-batch
    (buffer document selection generation cursor fragments)
  "Request one bounded text batch for SELECTION in DOCUMENT."
  (yunge-reader-request
   'selection-text
   (list :start (yunge-reader-selection-start selection)
         :end (yunge-reader-selection-end selection)
         :cursor cursor
         :unit-limit yunge-reader-copy-unit-limit
         :character-limit yunge-reader-copy-character-limit)
   (lambda (value error-data)
     (yunge-reader--complete-selection-batch
      buffer document selection generation cursor fragments
      value error-data))))

(defun yunge-reader-copy-selection ()
  "Copy the current logical document selection.
Ask the active driver for text when the selection does not already carry it."
  (interactive)
  (unless yunge-reader-selection
    (user-error "There is no document selection"))
  (cond
   ((yunge-reader-selection-text yunge-reader-selection)
    (cl-incf yunge-reader--copy-generation)
    (setq yunge-reader--copy-pending nil)
    (yunge-reader--copy-text
     (yunge-reader-selection-text yunge-reader-selection)))
   (yunge-reader--copy-pending
    (message "Document selection is still being copied"))
   (t
    (let ((buffer (current-buffer))
          (document yunge-reader-document)
          (selection yunge-reader-selection)
          (generation (cl-incf yunge-reader--copy-generation)))
      (unless document
        (user-error "This reader buffer has no open document"))
      (setq yunge-reader--copy-pending t)
      (message "Copying document text...")
      (yunge-reader--request-selection-batch
       buffer document selection generation nil nil)))))

(yunge-jump-history-register-target
 'reader
 :capture #'yunge-reader--jump-target
 :same #'yunge-reader--same-jump-target-p
 :visit #'yunge-reader--visit-jump-target)

(yunge-jump-history-track-command 'yunge-reader--follow-outline-item)

(provide 'yunge-reader)

;;; yunge-reader.el ends here
