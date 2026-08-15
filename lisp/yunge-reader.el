;;; yunge-reader.el --- Extensible document reading -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'yunge-key)

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

(defcustom yunge-reader-place-limit 1000
  "Maximum number of durable document places to retain."
  :type '(integer :tag "Places" 1)
  :group 'yunge-reader)

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
  "q" #'quit-window
  "y" #'yunge-reader-copy-selection)

(define-derived-mode yunge-reader-mode special-mode "Yunge Reader"
  "Major mode shared by Yunge Reader document adapters."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (setq-local yunge-reader-scale yunge-reader-default-scale)
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

(defun yunge-reader-record-place (&optional window)
  "Record the current durable Reader place as viewed in WINDOW.
Do nothing until document opening and any prior place restoration commit."
  (when (and yunge-reader--place-recording-enabled
             (not yunge-reader--restoring-place)
             yunge-reader-document)
    (let* ((driver
            (yunge-reader-document-driver yunge-reader-document))
           (location
            (yunge-reader-driver-location-function driver))
           (window (yunge-reader--place-window window)))
      (when (and location window)
        (condition-case error-data
            (when-let* ((position
                         (funcall location yunge-reader-document window)))
              (unless (yunge-reader-position-p position)
                (error "Reader driver returned an invalid place: %S"
                       position))
              (yunge-reader--store-place
               (yunge-reader-document-file yunge-reader-document)
               (yunge-reader--make-place driver position)))
          (error
           (display-warning
            'yunge-reader
            (format "Could not remember Reader place: %s"
                    (error-message-string error-data))
            :warning)))))))

(defun yunge-reader--restore-view-state (place)
  "Restore generic zoom state from durable PLACE."
  (setq yunge-reader-zoom-mode (plist-get place :zoom-mode)
        yunge-reader-scale
        (yunge-reader--clamp-scale (plist-get place :scale))
        yunge-reader-effective-scale nil))

(defun yunge-reader--restore-open-place ()
  "Build the opened view, restore its pending place, and permit writes."
  (let* ((place yunge-reader--pending-place)
         (driver
          (yunge-reader-document-driver yunge-reader-document))
         (restore (yunge-reader-driver-restore-function driver))
         (accepted t)
         (yunge-reader--restoring-place t))
    (setq yunge-reader--place-recording-enabled nil)
    (when place
      (yunge-reader--restore-view-state place))
    (yunge-reader-refresh)
    (when place
      (setq accepted
            (and restore
                 (funcall
                  restore yunge-reader-document
                  (yunge-reader--position-from-data
                   (plist-get place :position))
                  (yunge-reader--place-window)))))
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
    (buffer generation driver file handle properties error-data)
  "Finish opening FILE in BUFFER for GENERATION.
DRIVER produced HANDLE, PROPERTIES, and ERROR-DATA."
  (if (not (and (buffer-live-p buffer)
                (with-current-buffer buffer
                  (= generation yunge-reader--open-generation))))
      (yunge-reader--close-handle driver file handle properties)
    (with-current-buffer buffer
      (setq yunge-reader--opening-file nil)
      (if error-data
          (progn
            (setq yunge-reader--pending-place nil
                  yunge-reader--place-recording-enabled nil)
            (yunge-reader--display-status
             "Could not open %s:\n\n%s"
             file (error-message-string error-data)))
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
                 file (error-message-string error-data)))
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
            (when (yunge-reader--restore-open-place)
              (yunge-reader-record-place))))))))

(defun yunge-reader--begin-open (buffer driver file)
  "Ask DRIVER to open FILE for reader BUFFER."
  (with-current-buffer buffer
    (setq yunge-reader--opening-file file
          yunge-reader--pending-place
          (yunge-reader--saved-place file driver)
          yunge-reader--place-recording-enabled nil)
    (cl-incf yunge-reader--open-generation)
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
                open-error))))
        (error
         (unless completed
           (setq completed t)
           (yunge-reader--finish-open
            buffer generation driver file nil nil error-data)))))))

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
  (setq yunge-reader-selection
        (make-yunge-reader-selection
         :start start :end end :text text)))

(defun yunge-reader-clear-selection ()
  "Clear the logical selection in the current reader buffer."
  (interactive)
  (setq yunge-reader-selection nil)
  (yunge-reader-refresh))

(defun yunge-reader--selection-result-text (value)
  "Return selected text represented by driver VALUE, or nil."
  (cond
   ((stringp value) value)
   ((and (listp value) (stringp (plist-get value :text)))
    (plist-get value :text))))

(defun yunge-reader--copy-text (text)
  "Put nonempty selected TEXT in the kill ring."
  (unless (and (stringp text) (not (string-empty-p text)))
    (user-error "The document selection contains no text"))
  (kill-new text)
  (message "Copied document text")
  text)

(defun yunge-reader-copy-selection ()
  "Copy the current logical document selection.
Ask the active driver for text when the selection does not already carry it."
  (interactive)
  (unless yunge-reader-selection
    (user-error "There is no document selection"))
  (if-let* ((text (yunge-reader-selection-text yunge-reader-selection)))
      (yunge-reader--copy-text text)
    (let ((buffer (current-buffer))
          (selection yunge-reader-selection))
      (yunge-reader-request
       'selection-text
       (list :start (yunge-reader-selection-start selection)
             :end (yunge-reader-selection-end selection))
       (lambda (value error-data)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (if error-data
                 (display-warning
                  'yunge-reader
                  (format "Could not copy document text: %s"
                          (error-message-string error-data))
                  :warning)
               (let ((text (yunge-reader--selection-result-text value)))
                 (when (eq selection yunge-reader-selection)
                   (setf (yunge-reader-selection-text selection) text))
                 (yunge-reader--copy-text text))))))))))

(provide 'yunge-reader)

;;; yunge-reader.el ends here
