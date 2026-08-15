;;; yunge-reader.el --- Extensible document reading -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

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

(define-error 'yunge-reader-no-driver
  "No Yunge Reader driver accepts the document")

(cl-defstruct (yunge-reader-driver
               (:constructor yunge-reader--make-driver))
  "A document format implementation.
MATCH-FUNCTION receives an absolute file name.  OPEN-FUNCTION receives that
file and a completion function, which it calls with HANDLE, a properties
plist, and an error value.  CLOSE-FUNCTION receives a `yunge-reader-document'.
REQUEST-FUNCTION receives a document, operation, argument plist, and a
completion function, which it calls with a value and an error value."
  name
  match-function
  open-function
  close-function
  request-function)

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

(defvar yunge-reader-drivers nil
  "Registered `yunge-reader-driver' objects in precedence order.")

(defvar-local yunge-reader-document nil
  "Document displayed by the current reader buffer.")

(defvar-local yunge-reader--opening-file nil
  "Absolute file currently being opened, or nil.")

(defvar-local yunge-reader--open-generation 0
  "Generation used to reject late document-open completions.")

(defvar-local yunge-reader-zoom-mode 'fit-width
  "Current zoom mode: `manual', `fit-width', or `fit-page'.")

(defvar-local yunge-reader-scale 1.0
  "Manual document scale used when `yunge-reader-zoom-mode' is `manual'.")

(defvar-local yunge-reader-effective-scale nil
  "Scale most recently resolved by the active view adapter.")

(defvar-local yunge-reader-selection nil
  "Current logical `yunge-reader-selection', or nil.")

(defvar-local yunge-reader-refresh-hook nil
  "Hook run after the current reader view becomes invalid.
Drivers or view adapters use this buffer-local hook to request visible
artifacts.  Functions run in the reader buffer without arguments.")

(defvar-keymap yunge-reader-mode-map
  :parent special-mode-map
  "+" #'yunge-reader-zoom-in
  "=" #'yunge-reader-zoom-in
  "-" #'yunge-reader-zoom-out
  "0" #'yunge-reader-zoom-reset
  "w" #'yunge-reader-fit-width
  "p" #'yunge-reader-fit-page
  "g" #'yunge-reader-refresh
  "M-w" #'yunge-reader-copy-selection
  "C-g" #'yunge-reader-clear-selection
  "q" #'quit-window)

(define-derived-mode yunge-reader-mode special-mode "Yunge Reader"
  "Major mode shared by Yunge Reader document adapters."
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (setq-local yunge-reader-scale yunge-reader-default-scale)
  (add-hook 'kill-buffer-hook #'yunge-reader--close-document nil t))

(cl-defun yunge-reader-register-driver
    (name &key match open close request)
  "Register a reader driver NAME.
MATCH, OPEN, CLOSE, and REQUEST follow the contracts documented by
`yunge-reader-driver'.  Registering NAME again atomically replaces its old
definition and gives the new definition highest precedence."
  (unless (symbolp name)
    (error "Reader driver name must be a symbol: %S" name))
  (dolist (function (list match open close request))
    (unless (functionp function)
      (error "Reader driver %s has a non-function member: %S"
             name function)))
  (let ((driver
         (yunge-reader--make-driver
          :name name
          :match-function match
          :open-function open
          :close-function close
          :request-function request)))
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
          (yunge-reader--display-status
           "Could not open %s:\n\n%s"
           file (error-message-string error-data))
        (let ((layout (plist-get properties :layout)))
          (unless (memq layout '(fixed reflow))
            (setq error-data
                  (list 'error
                        (format "Driver returned invalid layout: %S"
                                layout))))
          (if error-data
              (progn
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
            (yunge-reader-refresh)))))))

(defun yunge-reader--begin-open (buffer driver file)
  "Ask DRIVER to open FILE for reader BUFFER."
  (with-current-buffer buffer
    (setq yunge-reader--opening-file file)
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
  (when yunge-reader-document
    (let* ((document yunge-reader-document)
           (driver (yunge-reader-document-driver document)))
      (setq yunge-reader-document nil)
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
