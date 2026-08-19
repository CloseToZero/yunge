;;; yunge-reader.el --- Extensible document reading -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'yunge-jump-history)
(require 'yunge-key)

(declare-function browse-url "browse-url" (url &rest arguments))
(declare-function evil-refresh-cursor
                  "evil-common" (&optional state buffer))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-state-property
                  "evil-common" (state property &optional value))
(declare-function yunge-reader-outline-create-buffer
                  "yunge-reader-outline"
                  (reader window entry document &optional outline))
(declare-function yunge-reader-outline-display-buffer
                  "yunge-reader-outline" (buffer))
(declare-function yunge-reader-outline-set-data
                  "yunge-reader-outline" (outline))
(declare-function yunge-reader-outline-set-status
                  "yunge-reader-outline" (status))
(declare-function yunge-reader-outline-set-target
                  "yunge-reader-outline"
                  (reader window entry document))

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

(defcustom yunge-reader-default-appearances
  '((pdf . original)
    (epub . original))
  "Default appearance for each Reader document format.
An omitted format also defaults to `original'."
  :type '(alist
          :key-type (symbol :tag "Format")
          :value-type
          (choice
           (const :tag "Original" original)
           (const :tag "Follow Emacs" follow-emacs)))
  :group 'yunge-reader)

(defcustom yunge-reader-uri-schemes '("https" "http" "mailto")
  "URI schemes that document actions may open through `browse-url'."
  :type '(repeat (string :tag "Scheme"))
  :group 'yunge-reader)

(defconst yunge-reader-uri-maximum-bytes 4096
  "Maximum encoded size accepted for one document URI action.")

(defconst yunge-reader-place-version 1
  "Current durable Reader place format version.")

(defconst yunge-reader-appearances '(original follow-emacs)
  "Appearance values accepted by Reader documents.")

(defun yunge-reader--face-color (face attribute frame fallback)
  "Return FACE ATTRIBUTE on FRAME as RGB hex, or FALLBACK."
  (let* ((value
          (and (facep face)
               (face-attribute face attribute frame 'default)))
         (rgb (and value (color-values value frame))))
    (if rgb
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (component)
                         (round component 257))
                       rgb))
      fallback)))

(define-error 'yunge-reader-no-driver
  "No Yunge Reader driver accepts the document")

(cl-defstruct (yunge-reader-driver
               (:constructor yunge-reader--make-driver))
  "A document format implementation.
MATCH-FUNCTION receives an absolute file name.  OPEN-FUNCTION receives that
file and a completion function, which it calls with HANDLE, a properties
plist, and an error value.  CLOSE-FUNCTION receives a `yunge-reader-document'.
ATTACH-FUNCTION and DETACH-FUNCTION receive a document in the Reader buffer
whose format-specific view they initialize or tear down.  REQUEST-FUNCTION
receives a document, operation, argument plist, and a completion function,
which it calls with a value and an error value.
LOCATION-FUNCTION receives a document and window and returns a stable
`yunge-reader-position'.  RESTORE-FUNCTION receives a document, position,
and window and returns non-nil after accepting the location."
  name
  match-function
  open-function
  close-function
  attach-function
  detach-function
  request-function
  location-function
  restore-function)

(cl-defstruct yunge-reader-document
  "An open document resource owned by one reader driver."
  key
  file
  driver
  handle
  layout
  metadata)

(cl-defstruct (yunge-reader--document-entry
               (:constructor yunge-reader--make-document-entry))
  "One canonical document resource and its attached Reader views."
  key
  file
  driver
  state
  document
  requests
  views
  primary-view
  active-view
  outline
  outline-loaded
  outline-pending
  outline-waiters)

(cl-defstruct (yunge-reader--view-request
               (:constructor yunge-reader--make-view-request))
  "One Reader buffer waiting to attach to a document resource."
  buffer
  generation
  complete
  completed)

(cl-defstruct (yunge-reader--outline-waiter
               (:constructor yunge-reader--make-outline-waiter))
  "One Reader view waiting for a shared document outline."
  buffer
  generation
  outline-buffer)

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

(cl-defstruct yunge-reader-search-cursor
  "Opaque driver-owned continuation for one directional search run."
  value)

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

(defvar yunge-reader--document-registry
  (make-hash-table :test #'equal)
  "Map canonical document keys to live resource entries.")

(defvar yunge-reader-saved-places nil
  "Most recently used durable Reader places.
Each entry maps a canonical file name to versioned, printable place data.")

(defvar yunge-reader-saved-appearance-overrides nil
  "Durable Reader appearance overrides.
Each entry maps a canonical file name to `original' or `follow-emacs'.")

(defvar-local yunge-reader-document nil
  "Document displayed by the current reader buffer.")

(defvar-local yunge-reader--document-entry nil
  "Shared resource entry attached to the current Reader buffer.")

(defvar-local yunge-reader--view-attached nil
  "Whether the current buffer owns an attached driver view.")

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

(defvar-local yunge-reader--active-presentation nil
  "Live window currently presenting this logical Reader view.")

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

(defvar yunge-reader-search-history nil
  "Minibuffer history for document search queries.")

(defvar-local yunge-reader-search-results nil
  "Search results loaded in the active run's navigation order.")

(defvar-local yunge-reader-search-result nil
  "Current `yunge-reader-search-result', or nil.")

(defvar-local yunge-reader-search-highlight-visible nil
  "Whether the active search may display its current result highlight.")

(defvar-local yunge-reader-search-result-hook nil
  "Hook run after the search result or its visibility changes.")

(defvar-local yunge-reader-selection-change-hook nil
  "Hook run after the logical document selection changes.")

(defvar-local yunge-reader--search-case-sensitive nil
  "Whether the active reader search distinguishes case.")

(defvar-local yunge-reader--search-index nil
  "Zero-based index of the current result in loaded search results.")

(defvar-local yunge-reader--search-cursor nil
  "Opaque `yunge-reader-search-cursor' for the next search batch.")

(defvar-local yunge-reader--search-direction nil
  "Direction of the active search run: `forward' or `backward'.")

(defvar-local yunge-reader--search-origin nil
  "Stable Reader position from which the active search run began.")

(defvar-local yunge-reader--search-wrapped nil
  "Non-nil when the active run restarted at a document boundary.")

(defvar-local yunge-reader--search-detached nil
  "Non-nil after manual reading movement detached search navigation.")

(defvar-local yunge-reader--search-done nil
  "Non-nil after the active search reaches the document end.")

(defvar-local yunge-reader--search-pending nil
  "Non-nil while one search batch is outstanding.")

(defvar-local yunge-reader--search-in-flight 0
  "Number of physical search requests not yet completed.")

(defvar-local yunge-reader--search-navigation-intent nil
  "Pending search navigation direction: `forward' or `backward'.")

(defvar-local yunge-reader--search-generation 0
  "Generation used to reject late reader search completions.")

(defvar-local yunge-reader--outline-generation 0
  "Generation used to reject late document outline completions.")

(defvar-local yunge-reader--outline-buffer nil
  "Auxiliary outline buffer owned by the current Reader view.")

(defvar-local yunge-reader-refresh-hook nil
  "Hook run after the current reader view becomes invalid.
Drivers or view adapters use this buffer-local hook to request visible
artifacts.  Functions run in the reader buffer without arguments.")

(defvar-local yunge-reader-view-role-change-hook nil
  "Hook run after the current Reader view changes role.
Functions run in the affected Reader buffer without arguments.  A view
adapter should update role-dependent presentation without rebuilding its
document contents.")

(defvar-local yunge-reader-appearance-change-hook nil
  "Hook run after the current Reader view's effective appearance changes.
Functions run in the affected Reader buffer without arguments.")

(defconst yunge-reader-appearance-bindings
  '(("d" yunge-reader-set-document-appearance "set book")
    ("D" yunge-reader-set-default-appearance "set format default")
    ("u" yunge-reader-unset-document-appearance "inherit default")))

(defvar-keymap yunge-reader-appearance-map
  :doc "Keymap for Reader appearance commands.")

(yunge-key-define yunge-reader-appearance-map
                  yunge-reader-appearance-bindings)

(defconst yunge-reader-command-bindings
  `(("a" ,yunge-reader-appearance-map "appearance")
    ("p" yunge-reader-make-primary "make primary")
    ("v" yunge-reader-new-view "new view")))

(defvar-keymap yunge-reader-command-map
  :doc "Keymap for Reader view commands.")

(yunge-key-define yunge-reader-command-map
                  yunge-reader-command-bindings)

(defconst yunge-reader-normal-bindings
  `(("+" yunge-reader-zoom-in "zoom in")
    ("-" yunge-reader-zoom-out "zoom out")
    ("=" yunge-reader-zoom-reset "reset zoom")
    ("/" yunge-reader-search "search")
    ("N" yunge-reader-search-previous "previous match")
    ("P" yunge-reader-fit-page "fit page")
    ("W" yunge-reader-fit-width "fit width")
    ("gr" yunge-reader-refresh "refresh")
    ("n" yunge-reader-search-next "next match")
    ("o" yunge-reader-outline "outline")
    ("y" yunge-reader-copy-selection "copy selection")
    ([localleader] ,yunge-reader-command-map nil))
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
  "C-g" #'yunge-reader-keyboard-quit
  "<escape>" #'yunge-reader-escape
  "g r" #'yunge-reader-refresh
  "n" #'yunge-reader-search-next
  "o" #'yunge-reader-outline
  "q" #'undefined
  "y" #'yunge-reader-copy-selection)

(defun yunge-reader--hide-evil-cursor ()
  "Keep Evil from restoring a visible cursor in this Reader buffer."
  (when (fboundp 'evil-state-property)
    (dolist (entry (evil-state-property t :cursor))
      (when (and (symbolp (cdr entry)) (boundp (cdr entry)))
        (set (make-local-variable (cdr entry)) '(nil))))
    (when (and (bound-and-true-p evil-local-mode)
               (fboundp 'evil-refresh-cursor))
      (evil-refresh-cursor
       (and (boundp 'evil-state) (symbol-value 'evil-state))
       (current-buffer)))))

(defun yunge-reader--dismiss-after-force-normal-state
    (&rest _arguments)
  "Dismiss Reader highlights after an interactive Evil quit."
  (when (and (eq this-command 'evil-force-normal-state)
             (derived-mode-p 'yunge-reader-mode))
    (yunge-reader--clear-transient-highlights)))

(define-derived-mode yunge-reader-mode special-mode "Yunge Reader"
  "Major mode shared by Yunge Reader document adapters."
  (auto-save-mode -1)
  (setq-local cursor-type nil)
  (yunge-reader--hide-evil-cursor)
  (setq-local truncate-lines t)
  (setq-local yunge-reader-scale yunge-reader-default-scale)
  (setq-local yunge-reader--document-entry nil)
  (setq-local yunge-reader--view-attached nil)
  (setq-local yunge-reader--active-presentation nil)
  (setq-local yunge-reader--outline-buffer nil)
  (setq-local yunge-reader--copy-generation 0)
  (setq-local yunge-reader--copy-pending nil)
  (add-hook 'post-command-hook
            #'yunge-reader--note-view-activity nil t)
  (add-hook 'change-major-mode-hook
            #'yunge-reader--close-document nil t)
  (add-hook 'kill-buffer-hook #'yunge-reader--close-document nil t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'yunge-reader-mode 'normal)
  (yunge-key-evil-define 'normal yunge-reader-mode-map
                         yunge-reader-normal-bindings)
  (yunge-key-evil-define
   '(insert replace) yunge-reader-mode-map
   '(("<escape>" evil-force-normal-state nil)
     ("C-[" evil-force-normal-state nil)))
  (yunge-key-evil-define
   'normal yunge-reader-mode-map
   '(("q" evil-record-macro nil)))
  (advice-add
   'evil-force-normal-state :after
   #'yunge-reader--dismiss-after-force-normal-state)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'yunge-reader-mode)
        (yunge-reader--hide-evil-cursor)))))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-reader-appearance-map yunge-reader-appearance-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-reader-command-map yunge-reader-command-bindings))

(cl-defun yunge-reader-register-driver
    (name &key match open close attach detach request location restore)
  "Register a reader driver NAME.
MATCH, OPEN, CLOSE, and REQUEST follow the contracts documented by
`yunge-reader-driver'.  ATTACH and DETACH are an optional pair whose omitted
default performs no buffer-specific setup.  LOCATION and RESTORE are another
optional pair.
Registering NAME again atomically replaces its old definition and gives the
new definition highest precedence."
  (unless (symbolp name)
    (error "Reader driver name must be a symbol: %S" name))
  (dolist (function (list match open close request))
    (unless (functionp function)
      (error "Reader driver %s has a non-function member: %S"
             name function)))
  (unless (eq (null attach) (null detach))
    (error "Reader driver %s must define both view functions" name))
  (unless (eq (null location) (null restore))
    (error "Reader driver %s must define both location functions" name))
  (dolist (function (delq nil (list attach detach location restore)))
    (unless (functionp function)
      (error "Reader driver %s has a non-function member: %S"
             name function)))
  (let ((driver
         (yunge-reader--make-driver
          :name name
          :match-function match
          :open-function open
          :close-function close
          :attach-function (or attach #'ignore)
          :detach-function (or detach #'ignore)
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

(defun yunge-reader--appearance-p (appearance)
  "Return whether APPEARANCE is a supported Reader appearance."
  (memq appearance yunge-reader-appearances))

(defun yunge-reader--driver-format (driver)
  "Return the format symbol represented by DRIVER."
  (cond
   ((yunge-reader-driver-p driver)
    (yunge-reader-driver-name driver))
   ((symbolp driver) driver)
   (t (error "Invalid Reader driver: %S" driver))))

(defun yunge-reader--default-appearance (driver)
  "Return DRIVER's effective format default appearance."
  (let ((appearance
         (alist-get (yunge-reader--driver-format driver)
                    yunge-reader-default-appearances)))
    (if (yunge-reader--appearance-p appearance)
        appearance
      'original)))

(defun yunge-reader--saved-appearance-override (file)
  "Return FILE's valid saved appearance override, or nil."
  (let ((appearance
         (cdr
          (assoc (yunge-reader--place-file-key file)
                 yunge-reader-saved-appearance-overrides))))
    (and (yunge-reader--appearance-p appearance) appearance)))

(defun yunge-reader-document-appearance-override (&optional document)
  "Return DOCUMENT's explicit appearance override, or nil.
DOCUMENT defaults to the document in the current Reader buffer."
  (let ((document (or document yunge-reader-document)))
    (when document
      (yunge-reader--saved-appearance-override
       (yunge-reader-document-file document)))))

(defun yunge-reader-effective-appearance (&optional document)
  "Return DOCUMENT's effective Reader appearance.
DOCUMENT defaults to the document in the current Reader buffer."
  (let ((document (or document yunge-reader-document)))
    (unless (yunge-reader-document-p document)
      (error "The current Reader buffer has no document"))
    (or (yunge-reader-document-appearance-override document)
        (yunge-reader--default-appearance
         (yunge-reader-document-driver document)))))

(defun yunge-reader--store-appearance-override (file appearance)
  "Persist APPEARANCE as FILE's explicit override."
  (unless (yunge-reader--appearance-p appearance)
    (error "Invalid Reader appearance: %S" appearance))
  (let ((key (yunge-reader--place-file-key file)))
    (setq yunge-reader-saved-appearance-overrides
          (cons
           (cons key appearance)
           (seq-remove
            (lambda (entry) (equal (car-safe entry) key))
            yunge-reader-saved-appearance-overrides)))))

(defun yunge-reader--unset-appearance-override (file)
  "Remove FILE's explicit appearance override."
  (let ((key (yunge-reader--place-file-key file)))
    (setq yunge-reader-saved-appearance-overrides
          (seq-remove
           (lambda (entry) (equal (car-safe entry) key))
           yunge-reader-saved-appearance-overrides))))

(defun yunge-reader--existing-document-state-entries (entries)
  "Return ENTRIES whose file keys still exist."
  (seq-filter
   (lambda (entry)
     (let ((file (car-safe entry)))
       (and (stringp file) (file-exists-p file))))
   entries))

(defun yunge-reader-cleanup-missing-document-state ()
  "Forget saved state for document files that no longer exist.
This removes both durable places and explicit appearance overrides.  A file
on a disconnected volume is considered missing, so this command is never run
automatically."
  (interactive)
  (let ((place-count (length yunge-reader-saved-places))
        (appearance-count
         (length yunge-reader-saved-appearance-overrides)))
    (setq yunge-reader-saved-places
          (yunge-reader--existing-document-state-entries
           yunge-reader-saved-places)
          yunge-reader-saved-appearance-overrides
          (yunge-reader--existing-document-state-entries
           yunge-reader-saved-appearance-overrides))
    (message
     "Removed %d saved places and %d appearance overrides"
     (- place-count (length yunge-reader-saved-places))
     (- appearance-count
        (length yunge-reader-saved-appearance-overrides)))))

(defun yunge-reader--document-key (file driver)
  "Return the registry key for FILE opened through DRIVER."
  (list (yunge-reader-driver-name driver)
        (yunge-reader--place-file-key file)))

(defun yunge-reader--entry-current-p (entry)
  "Return whether ENTRY is the canonical live registry entry."
  (eq (gethash (yunge-reader--document-entry-key entry)
               yunge-reader--document-registry)
      entry))

(defun yunge-reader--view-owns-entry-p (buffer entry)
  "Return whether live BUFFER is attached to ENTRY."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq yunge-reader--document-entry entry)
              (eq yunge-reader-document
                  (yunge-reader--document-entry-document entry))))))

(defun yunge-reader--notify-view-role-change (buffers)
  "Run role-change hooks safely in live BUFFERS."
  (dolist (buffer
           (delete-dups (delq nil (copy-sequence buffers))))
    (when (buffer-live-p buffer)
      (condition-case error-data
          (with-current-buffer buffer
            (run-hooks 'yunge-reader-view-role-change-hook))
        (error
         (display-warning
          'yunge-reader
          (format "Could not update Reader role in %s: %s"
                  (buffer-name buffer)
                  (error-message-string error-data))
          :warning))))))

(defun yunge-reader--entry-live-views (entry)
  "Return and retain only live Reader views attached to ENTRY."
  (let ((previous-primary
         (yunge-reader--document-entry-primary-view entry))
        (views
         (seq-filter
          (lambda (buffer)
            (yunge-reader--view-owns-entry-p buffer entry))
          (yunge-reader--document-entry-views entry))))
    (setf (yunge-reader--document-entry-views entry) views)
    (unless (memq (yunge-reader--document-entry-primary-view entry)
                  views)
      (setf (yunge-reader--document-entry-primary-view entry)
            (or (and
                 (memq (yunge-reader--document-entry-active-view entry)
                       views)
                 (yunge-reader--document-entry-active-view entry))
                (car views))))
    (unless (memq (yunge-reader--document-entry-active-view entry)
                  views)
      (setf (yunge-reader--document-entry-active-view entry)
            (yunge-reader--document-entry-primary-view entry)))
    (unless (eq previous-primary
                (yunge-reader--document-entry-primary-view entry))
      (yunge-reader--notify-view-role-change
       (list (yunge-reader--document-entry-primary-view entry))))
    views))

(defun yunge-reader--notify-appearance-change (entry)
  "Run appearance hooks safely in every live view of ENTRY."
  (dolist (buffer (yunge-reader--entry-live-views entry))
    (condition-case error-data
        (with-current-buffer buffer
          (run-hooks 'yunge-reader-appearance-change-hook))
      (error
       (display-warning
        'yunge-reader
        (format "Could not update Reader appearance in %s: %s"
                (buffer-name buffer)
                (error-message-string error-data))
        :warning)))))

(defun yunge-reader--notify-format-appearance-change (format)
  "Notify live FORMAT documents that inherit their appearance."
  (maphash
   (lambda (_key entry)
     (when (and
            (eq (yunge-reader--document-entry-state entry) 'ready)
            (eq (yunge-reader--driver-format
                 (yunge-reader--document-entry-driver entry))
                format)
            (not
             (yunge-reader--saved-appearance-override
              (yunge-reader--document-entry-file entry))))
       (yunge-reader--notify-appearance-change entry)))
   yunge-reader--document-registry))

(defun yunge-reader--theme-changed (&optional _theme)
  "Refresh live Reader documents that follow the Emacs theme."
  (maphash
   (lambda (_key entry)
     (let ((document
            (yunge-reader--document-entry-document entry)))
       (when (and
              (eq (yunge-reader--document-entry-state entry) 'ready)
              (yunge-reader-document-p document)
              (eq (yunge-reader-effective-appearance document)
                  'follow-emacs))
         (yunge-reader--notify-appearance-change entry))))
   yunge-reader--document-registry))

(add-hook 'enable-theme-functions #'yunge-reader--theme-changed)
(add-hook 'disable-theme-functions #'yunge-reader--theme-changed)

(defun yunge-reader--entry-view (entry &optional preferred)
  "Return a live view of ENTRY, preferring PREFERRED."
  (let ((views (yunge-reader--entry-live-views entry)))
    (cond
     ((memq preferred views) preferred)
     ((memq (yunge-reader--document-entry-active-view entry) views)
      (yunge-reader--document-entry-active-view entry))
     ((memq (yunge-reader--document-entry-primary-view entry) views)
      (yunge-reader--document-entry-primary-view entry))
     (t (car views)))))

(defun yunge-reader--entry-for-document (document)
  "Return the canonical live registry entry owning DOCUMENT."
  (when-let* ((key (yunge-reader-document-key document))
              (entry (gethash key yunge-reader--document-registry)))
    (and (eq document (yunge-reader--document-entry-document entry))
         entry)))

(defun yunge-reader--document-view (document &optional preferred)
  "Return a live Reader view of DOCUMENT, preferring PREFERRED."
  (or (when-let* ((entry (yunge-reader--entry-for-document document)))
        (yunge-reader--entry-view entry preferred))
      (and (buffer-live-p preferred)
           (with-current-buffer preferred
             (and (eq document yunge-reader-document) preferred)))
      (seq-find
       (lambda (buffer)
         (and (buffer-live-p buffer)
              (with-current-buffer buffer
                (eq document yunge-reader-document))))
       (buffer-list))))

(defun yunge-reader--document-live-p (document)
  "Return whether DOCUMENT has at least one live Reader view."
  (and (yunge-reader--document-view document) t))

(defun yunge-reader--primary-view-p ()
  "Return whether the current buffer owns durable place updates."
  (or (null yunge-reader--document-entry)
      (eq (current-buffer)
          (yunge-reader--document-entry-primary-view
           yunge-reader--document-entry))))

(defun yunge-reader--ready-view-entry ()
  "Return the canonical ready entry attached to the current view."
  (let ((entry yunge-reader--document-entry))
    (when (and yunge-reader-document
               entry
               (eq (yunge-reader--document-entry-state entry) 'ready)
               (yunge-reader--entry-current-p entry)
               (memq (current-buffer)
                     (yunge-reader--entry-live-views entry)))
      entry)))

(defun yunge-reader-view-role ()
  "Return the current Reader view role, or nil outside a ready view.
The possible roles are `primary' and `additional'."
  (when-let* ((entry (yunge-reader--ready-view-entry)))
    (if (eq (current-buffer)
            (yunge-reader--document-entry-primary-view entry))
        'primary
      'additional)))

(defun yunge-reader--appearance-label (appearance)
  "Return a user-facing label for APPEARANCE."
  (pcase appearance
    ('original "Original")
    ('follow-emacs "Follow Emacs")
    (_ (error "Invalid Reader appearance: %S" appearance))))

(defun yunge-reader--format-label (format)
  "Return a user-facing label for FORMAT."
  (upcase (symbol-name format)))

(defun yunge-reader--read-appearance (prompt default)
  "Read an appearance with PROMPT and DEFAULT."
  (let* ((choices
          (mapcar
           (lambda (appearance)
             (cons (yunge-reader--appearance-label appearance)
                   appearance))
           yunge-reader-appearances))
         (answer
          (completing-read
           prompt choices nil t nil nil
           (yunge-reader--appearance-label default))))
    (or (cdr (assoc-string answer choices))
        (error "Invalid Reader appearance: %s" answer))))

(defun yunge-reader--read-default-appearance-arguments ()
  "Read a format and appearance for the default appearance command."
  (let* ((current-format
          (and yunge-reader-document
               (yunge-reader--driver-format
                (yunge-reader-document-driver yunge-reader-document))))
         (formats
          (delete-dups
           (append
            (mapcar #'car yunge-reader-default-appearances)
            (mapcar #'yunge-reader-driver-name yunge-reader-drivers))))
         (choices
          (mapcar
           (lambda (format)
             (cons (yunge-reader--format-label format) format))
           formats))
         (format
          (or current-format
              (let ((answer
                     (completing-read
                      "Document format: " choices nil t)))
                (cdr (assoc-string answer choices)))))
         (default (yunge-reader--default-appearance format)))
    (unless format
      (user-error "No Reader document formats are available"))
    (list
     format
     (yunge-reader--read-appearance
      (format "%s default appearance: "
              (yunge-reader--format-label format))
      default))))

(defun yunge-reader--set-default-appearance
    (format appearance &optional persist)
  "Set FORMAT's default to APPEARANCE.
When PERSIST is non-nil, save the setting through Customize."
  (unless (symbolp format)
    (error "Invalid Reader format: %S" format))
  (unless (yunge-reader--appearance-p appearance)
    (error "Invalid Reader appearance: %S" appearance))
  (let ((old (yunge-reader--default-appearance format))
        (updated (copy-tree yunge-reader-default-appearances)))
    (setf (alist-get format updated) appearance)
    (if persist
        (customize-save-variable
         'yunge-reader-default-appearances updated)
      (setq yunge-reader-default-appearances updated))
    (unless (eq old appearance)
      (yunge-reader--notify-format-appearance-change format))
    appearance))

(defun yunge-reader-set-default-appearance (format appearance)
  "Persist APPEARANCE as FORMAT's default.
An explicit override on the current book remains unchanged."
  (interactive (yunge-reader--read-default-appearance-arguments))
  (yunge-reader--set-default-appearance format appearance t)
  (let* ((document
          (and yunge-reader-document
               (eq format
                   (yunge-reader--driver-format
                    (yunge-reader-document-driver
                     yunge-reader-document)))
               yunge-reader-document))
         (override
          (and document
               (yunge-reader-document-appearance-override document))))
    (cond
     ((and override (not (eq override appearance)))
      (message
       (concat
        "%s default is now %s; this book remains %s because it has "
        "a document override.  Use M-x "
        "yunge-reader-unset-document-appearance to inherit the default")
       (yunge-reader--format-label format)
       (yunge-reader--appearance-label appearance)
       (yunge-reader--appearance-label override)))
     (override
      (message "%s default is now %s; this book keeps its matching override"
               (yunge-reader--format-label format)
               (yunge-reader--appearance-label appearance)))
     (t
      (message "%s default appearance is now %s"
               (yunge-reader--format-label format)
               (yunge-reader--appearance-label appearance))))))

(defun yunge-reader-set-document-appearance (appearance)
  "Persist APPEARANCE as an override for the current document."
  (interactive
   (list
    (yunge-reader--read-appearance
     "Book appearance: "
     (yunge-reader-effective-appearance))))
  (let* ((entry (yunge-reader--ready-view-entry))
         (document yunge-reader-document))
    (unless entry
      (user-error "This Reader view has no ready document"))
    (let ((old (yunge-reader-effective-appearance document)))
      (yunge-reader--store-appearance-override
       (yunge-reader-document-file document) appearance)
      (unless (eq old appearance)
        (yunge-reader--notify-appearance-change entry)))
    (message "This book now uses %s"
             (yunge-reader--appearance-label appearance))))

(defun yunge-reader-unset-document-appearance ()
  "Remove the current document's appearance override."
  (interactive)
  (let* ((entry (yunge-reader--ready-view-entry))
         (document yunge-reader-document))
    (unless entry
      (user-error "This Reader view has no ready document"))
    (if-let* ((override
               (yunge-reader-document-appearance-override document)))
        (let ((file (yunge-reader-document-file document)))
          (yunge-reader--unset-appearance-override file)
          (let ((inherited
                 (yunge-reader-effective-appearance document)))
            (unless (eq override inherited)
              (yunge-reader--notify-appearance-change entry))
            (message "This book now inherits %s"
                     (yunge-reader--appearance-label inherited))))
      (message "This book already inherits its format default"))))

(defun yunge-reader--note-view-activity ()
  "Remember the current presentation and document view as active."
  (yunge-reader--activate-presentation)
  (when (and yunge-reader-document
             yunge-reader--document-entry
             (yunge-reader--entry-current-p
              yunge-reader--document-entry))
    (setf (yunge-reader--document-entry-active-view
           yunge-reader--document-entry)
          (current-buffer))))

(defun yunge-reader--complete-view-request (request success)
  "Complete REQUEST once with SUCCESS."
  (unless (yunge-reader--view-request-completed request)
    (setf (yunge-reader--view-request-completed request) t)
    (when-let* ((complete
                 (yunge-reader--view-request-complete request)))
      (funcall complete success))))

(defun yunge-reader--view-request-current-p (entry request)
  "Return whether REQUEST still belongs to ENTRY and its Reader buffer."
  (let ((buffer (yunge-reader--view-request-buffer request))
        (generation (yunge-reader--view-request-generation request)))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (eq yunge-reader--document-entry entry)
                (= generation yunge-reader--open-generation))))))

(defun yunge-reader--entry-pending-view (entry)
  "Return the first live Reader buffer waiting on ENTRY."
  (when-let* ((request
               (seq-find
                (lambda (candidate)
                  (yunge-reader--view-request-current-p
                   entry candidate))
                (yunge-reader--document-entry-requests entry))))
    (yunge-reader--view-request-buffer request)))

(defun yunge-reader--registered-buffer (file)
  "Return the primary or opening Reader buffer registered for FILE."
  (let ((file-key (yunge-reader--place-file-key file))
        found)
    (maphash
     (lambda (key entry)
       (when (and (not found)
                  (equal (cadr key) file-key))
         (setq found
               (or (yunge-reader--entry-view
                    entry
                    (yunge-reader--document-entry-primary-view entry))
                   (yunge-reader--entry-pending-view entry)))))
     yunge-reader--document-registry)
    found))

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

(defun yunge-reader--presentation-window-p (window)
  "Return whether WINDOW presents the current Reader buffer."
  (and (window-live-p window)
       (eq (window-buffer window) (current-buffer))))

(defun yunge-reader--activate-presentation (&optional window)
  "Make WINDOW the current logical view's active presentation.
WINDOW defaults to the selected window.  Return the accepted window, or
nil when it does not display the current Reader buffer."
  (let ((window (or window (selected-window))))
    (when (yunge-reader--presentation-window-p window)
      (setq yunge-reader--active-presentation window))))

(defun yunge-reader--presentation-window ()
  "Return and retain the active window for this logical Reader view."
  (or (yunge-reader--activate-presentation)
      (and (yunge-reader--presentation-window-p
            yunge-reader--active-presentation)
           yunge-reader--active-presentation)
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (setq yunge-reader--active-presentation window))))

(defun yunge-reader--active-presentation-p (window)
  "Return whether WINDOW is the current view's active presentation."
  (and window (eq window (yunge-reader--presentation-window))))

(defun yunge-reader--place-window (&optional window)
  "Return a live presentation WINDOW for the current Reader buffer.
An explicit WINDOW may be an inactive presentation.  Without one, return
the active presentation."
  (if window
      (and (yunge-reader--presentation-window-p window) window)
    (yunge-reader--presentation-window)))

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

(defun yunge-reader--current-position (&optional window)
  "Return the current stable Reader position viewed in WINDOW, or nil."
  (when-let* ((place (yunge-reader--current-place window)))
    (yunge-reader--position-from-data (plist-get place :position))))

(defun yunge-reader--stable-place ()
  "Return the current committed view place, or nil."
  (when yunge-reader--place-recording-enabled
    (when-let* ((window (yunge-reader--place-window)))
      (yunge-reader--current-place window))))

(defun yunge-reader-record-place (&optional window)
  "Record the current durable Reader place as viewed in WINDOW.
Do nothing until document opening and any prior place restoration commit.
Only the primary view's active presentation may replace the durable place."
  (let ((window (yunge-reader--place-window window)))
    (when (and window
               (yunge-reader--active-presentation-p window)
               yunge-reader--place-recording-enabled
               (not yunge-reader--restoring-place)
               (yunge-reader--primary-view-p)
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
          :warning))))))

(defun yunge-reader--restore-view-state (place)
  "Restore generic zoom state from durable PLACE."
  (setq yunge-reader-zoom-mode (plist-get place :zoom-mode)
        yunge-reader-scale
        (yunge-reader--clamp-scale (plist-get place :scale))
        yunge-reader-effective-scale nil))

(defun yunge-reader--apply-place (place &optional window)
  "Apply validated Reader PLACE in WINDOW and return its acceptance value."
  (let* ((driver
          (yunge-reader-document-driver yunge-reader-document))
         (restore (yunge-reader-driver-restore-function driver)))
    (when (and restore (yunge-reader--place-p place driver))
      (yunge-reader--restore-view-state place)
      (yunge-reader-refresh)
      (funcall
       restore yunge-reader-document
       (yunge-reader--position-from-data
        (plist-get place :position))
       (yunge-reader--place-window window)))))

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
    (when (and accepted (not (eq accepted :deferred)))
      (yunge-reader-record-place window))
    (when accepted
      (yunge-reader--detach-search-navigation))
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
  (or
   (yunge-reader--registered-buffer file)
   (seq-find
    (lambda (buffer)
      (let ((buffer-file (yunge-reader--buffer-file buffer)))
        (and buffer-file
             (with-current-buffer buffer
               (derived-mode-p 'yunge-reader-mode))
             ;; Drivers may accept virtual or not-yet-existing files, for
             ;; which `file-equal-p' cannot establish identity.
             (or (ignore-errors (file-equal-p file buffer-file))
                 (equal file (expand-file-name buffer-file))))))
    (buffer-list))))

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
    (insert (apply #'format format-string arguments) "\n")
    (set-buffer-modified-p nil)))

(defun yunge-reader--attach-view (document)
  "Attach DOCUMENT's driver view to the current Reader buffer."
  (let* ((driver (yunge-reader-document-driver document))
         (attach (yunge-reader-driver-attach-function driver)))
    ;; DETACH must be able to clean up a partially attached view when ATTACH
    ;; signals after installing buffer-local state.
    (setq yunge-reader--view-attached t)
    (funcall attach document)))

(defun yunge-reader--detach-view (document)
  "Detach DOCUMENT's driver view from the current Reader buffer."
  (when yunge-reader--view-attached
    (setq yunge-reader--view-attached nil)
    (condition-case error-data
        (funcall
         (yunge-reader-driver-detach-function
          (yunge-reader-document-driver document))
         document)
      (error
       (display-warning
        'yunge-reader
        (format "Could not detach reader view: %s"
                (error-message-string error-data))
        :warning)))))

(defun yunge-reader--close-resource (document warning-format)
  "Close DOCUMENT, reporting failures with WARNING-FORMAT."
  (condition-case error-data
      (funcall
       (yunge-reader-driver-close-function
        (yunge-reader-document-driver document))
       document)
    (error
     (display-warning
      'yunge-reader
      (format warning-format (error-message-string error-data))
      :warning))))

(defun yunge-reader--close-handle (driver file handle properties)
  "Close HANDLE returned for FILE by DRIVER using PROPERTIES."
  (when handle
    (yunge-reader--close-resource
     (make-yunge-reader-document
      :file file
      :driver driver
      :handle handle
      :layout (plist-get properties :layout)
      :metadata (plist-get properties :metadata))
     "Could not close late reader document: %s")))

(defun yunge-reader--remove-entry-request (entry request)
  "Remove REQUEST from ENTRY."
  (setf (yunge-reader--document-entry-requests entry)
        (delq request
              (yunge-reader--document-entry-requests entry))))

(defun yunge-reader--add-entry-view (entry buffer)
  "Attach BUFFER ownership to ready document ENTRY."
  (yunge-reader--entry-live-views entry)
  (unless (memq buffer (yunge-reader--document-entry-views entry))
    (setf (yunge-reader--document-entry-views entry)
          (append (yunge-reader--document-entry-views entry)
                  (list buffer))))
  (unless (memq (yunge-reader--document-entry-primary-view entry)
                (yunge-reader--document-entry-views entry))
    (setf (yunge-reader--document-entry-primary-view entry) buffer))
  (unless (memq (yunge-reader--document-entry-active-view entry)
                (yunge-reader--document-entry-views entry))
    (setf (yunge-reader--document-entry-active-view entry) buffer)))

(defun yunge-reader--remove-entry-view (entry buffer)
  "Remove BUFFER ownership from document ENTRY and promote a survivor."
  (let ((previous-primary
         (yunge-reader--document-entry-primary-view entry))
        (views
         (seq-filter
          (lambda (candidate)
            (and (not (eq candidate buffer))
                 (yunge-reader--view-owns-entry-p candidate entry)))
          (yunge-reader--document-entry-views entry))))
    (setf (yunge-reader--document-entry-views entry) views)
    (when (eq buffer
              (yunge-reader--document-entry-primary-view entry))
      (setf (yunge-reader--document-entry-primary-view entry)
            (or (and
                 (memq (yunge-reader--document-entry-active-view entry)
                       views)
                 (yunge-reader--document-entry-active-view entry))
                (car views))))
    (unless (memq (yunge-reader--document-entry-active-view entry)
                  views)
      (setf (yunge-reader--document-entry-active-view entry)
            (yunge-reader--document-entry-primary-view entry)))
    (unless (eq previous-primary
                (yunge-reader--document-entry-primary-view entry))
      (yunge-reader--notify-view-role-change
       (list (yunge-reader--document-entry-primary-view entry))))))

(defun yunge-reader--release-entry-if-unused (entry)
  "Close ready ENTRY when no attached or pending views remain."
  (when (and (eq (yunge-reader--document-entry-state entry) 'ready)
             (null (yunge-reader--document-entry-requests entry))
             (null (yunge-reader--entry-live-views entry)))
    (when (yunge-reader--entry-current-p entry)
      (remhash (yunge-reader--document-entry-key entry)
               yunge-reader--document-registry))
    (setf (yunge-reader--document-entry-state entry) 'closed)
    (yunge-reader--close-resource
     (yunge-reader--document-entry-document entry)
     "Could not close reader document: %s")))

(defun yunge-reader--fail-view-request (entry request error-data)
  "Fail REQUEST for ENTRY with ERROR-DATA."
  (yunge-reader--remove-entry-request entry request)
  (let ((buffer (yunge-reader--view-request-buffer request)))
    (when (yunge-reader--view-request-current-p entry request)
      (with-current-buffer buffer
        (setq yunge-reader--opening-file nil
              yunge-reader--pending-place nil
              yunge-reader--place-recording-enabled nil
              yunge-reader--document-entry nil)
        (yunge-reader--display-status
         "Could not open %s:\n\n%s"
         (yunge-reader--document-entry-file entry)
         (error-message-string error-data))))
    (yunge-reader--complete-view-request request nil)))

(defun yunge-reader--prepare-view (entry request)
  "Attach and restore REQUEST as one view of ready ENTRY."
  (yunge-reader--remove-entry-request entry request)
  (if (not (yunge-reader--view-request-current-p entry request))
      (yunge-reader--complete-view-request request nil)
    (let ((buffer (yunge-reader--view-request-buffer request))
          (document (yunge-reader--document-entry-document entry)))
      (with-current-buffer buffer
        (setq yunge-reader--opening-file nil
              yunge-reader-document document)
        (yunge-reader--add-entry-view entry buffer)
        (yunge-reader--display-status
         "%s\n\nLayout: %s\nDriver: %s"
         (file-name-nondirectory
          (yunge-reader--document-entry-file entry))
         (yunge-reader-document-layout document)
         (yunge-reader-driver-name
          (yunge-reader-document-driver document)))
        (let (accepted prepare-error)
          (condition-case error-data
              (progn
                (yunge-reader--attach-view document)
                (setq accepted (yunge-reader--restore-open-place)))
            (error (setq prepare-error error-data)))
          (if prepare-error
              (progn
                (setq yunge-reader--pending-place nil
                      yunge-reader--place-recording-enabled nil)
                (yunge-reader--detach-view document)
                (setq yunge-reader-document nil
                      yunge-reader--document-entry nil)
                (yunge-reader--remove-entry-view entry buffer)
                (yunge-reader--display-status
                 "Could not prepare %s:\n\n%s"
                 (yunge-reader--document-entry-file entry)
                 (error-message-string prepare-error)))
            (when accepted
              (yunge-reader-record-place)))
          (yunge-reader--complete-view-request request accepted)))))
  (yunge-reader--release-entry-if-unused entry))

(defun yunge-reader--finish-resource-open
    (entry handle properties error-data)
  "Finish opening the shared resource for ENTRY."
  (let ((layout (plist-get properties :layout)))
    (when (and (not error-data)
               (not (memq layout '(fixed reflow))))
      (setq error-data
            (list 'error
                  (format "Driver returned invalid layout: %S" layout))))
    (cond
     ((not (and (yunge-reader--entry-current-p entry)
                (eq (yunge-reader--document-entry-state entry)
                    'opening)))
      (yunge-reader--close-handle
       (yunge-reader--document-entry-driver entry)
       (yunge-reader--document-entry-file entry)
       handle properties)
      (dolist (request
               (yunge-reader--document-entry-requests entry))
        (yunge-reader--complete-view-request request nil))
      (setf (yunge-reader--document-entry-requests entry) nil))
     (error-data
      (yunge-reader--close-handle
       (yunge-reader--document-entry-driver entry)
       (yunge-reader--document-entry-file entry)
       handle properties)
      (remhash (yunge-reader--document-entry-key entry)
               yunge-reader--document-registry)
      (setf (yunge-reader--document-entry-state entry) 'failed)
      (dolist (request
               (copy-sequence
                (yunge-reader--document-entry-requests entry)))
        (yunge-reader--fail-view-request entry request error-data)))
     (t
      (let ((document
             (make-yunge-reader-document
              :key (yunge-reader--document-entry-key entry)
              :file (yunge-reader--document-entry-file entry)
              :driver (yunge-reader--document-entry-driver entry)
              :handle handle
              :layout layout
              :metadata (plist-get properties :metadata))))
        (setf (yunge-reader--document-entry-document entry) document
              (yunge-reader--document-entry-state entry) 'ready)
        (dolist (request
                 (copy-sequence
                  (yunge-reader--document-entry-requests entry)))
          (yunge-reader--prepare-view entry request))
        (yunge-reader--release-entry-if-unused entry))))))

(defun yunge-reader--start-resource-open (entry)
  "Start the one driver resource open owned by ENTRY."
  (let ((driver (yunge-reader--document-entry-driver entry))
        (file (yunge-reader--document-entry-file entry))
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
             (yunge-reader--finish-resource-open
              entry handle properties open-error))))
      (error
       (unless completed
         (setq completed t)
         (yunge-reader--finish-resource-open
          entry nil nil error-data))))))

(defun yunge-reader--begin-open
    (buffer driver file &optional place complete)
  "Ask DRIVER to open FILE for reader BUFFER.
Restore explicit PLACE instead of the saved place.  Call COMPLETE with
non-nil only after opening and restoration succeed."
  (when (and place (not (yunge-reader--place-p place driver)))
    (error "Reader jump contains an invalid place: %S" place))
  (unless (or (null complete) (functionp complete))
    (error "Reader open completion must be a function: %S" complete))
  (setq file (expand-file-name file))
  (let* ((key (yunge-reader--document-key file driver))
         (registered
          (gethash key yunge-reader--document-registry))
         (entry
          (if (and registered
                   (memq (yunge-reader--document-entry-state registered)
                         '(opening ready)))
              registered
            (let ((created
                   (yunge-reader--make-document-entry
                    :key key
                    :file file
                    :driver driver
                    :state 'opening)))
              (puthash key created yunge-reader--document-registry)
              created)))
         request)
    (with-current-buffer buffer
      (when (or yunge-reader-document yunge-reader--document-entry)
        (error "Reader buffer is already attached to a document"))
      (setq yunge-reader--opening-file file
            yunge-reader--pending-place
            (or (and place (copy-tree place t))
                (yunge-reader--saved-place file driver))
            yunge-reader--place-recording-enabled nil
            yunge-reader--document-entry entry)
      (cl-incf yunge-reader--open-generation)
      (cl-incf yunge-reader--outline-generation)
      (yunge-reader--display-status "Opening %s..." file)
      (setq request
            (yunge-reader--make-view-request
             :buffer buffer
             :generation yunge-reader--open-generation
             :complete complete))
      (setf (yunge-reader--document-entry-requests entry)
            (append (yunge-reader--document-entry-requests entry)
                    (list request)))
      (pcase (yunge-reader--document-entry-state entry)
        ('ready (yunge-reader--prepare-view entry request))
        ('opening
         (when (= (length
                   (yunge-reader--document-entry-requests entry))
                  1)
           (yunge-reader--start-resource-open entry)))))))

(defun yunge-reader--revert-file-buffer (_ignore-auto _noconfirm)
  "Reopen the document visited by the current Reader file buffer."
  (unless buffer-file-name
    (user-error "This Reader buffer is not visiting a file"))
  (yunge-reader-visit-file buffer-file-name))

(defun yunge-reader-visit-file (file)
  "Turn the current file buffer into a Reader view of FILE.
FILE must be the file visited by the current buffer.  Format adapters use this
entry point from their `auto-mode-alist' mode functions."
  (setq file (expand-file-name file))
  (unless (and buffer-file-name
               (equal file (expand-file-name buffer-file-name)))
    (error "Reader file buffer does not visit %s" file))
  (let ((driver (yunge-reader-driver-for-file file)))
    (unless driver
      (signal 'yunge-reader-no-driver (list file)))
    ;; Re-entering this mode runs the old Reader buffer's cleanup hook before
    ;; clearing its local view state.
    (yunge-reader-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (set-buffer-multibyte t))
    (setq-local revert-buffer-function
                #'yunge-reader--revert-file-buffer)
    (set-buffer-modified-p nil)
    (yunge-reader--begin-open (current-buffer) driver file)
    (current-buffer)))

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

(defun yunge-reader-new-view ()
  "Display another Reader view in the active presentation window.
The new buffer starts from the current stable location and zoom state.  It
shares the driver-owned document resource while keeping its view state
independent.  The document's existing primary view is unchanged.  Display
the current buffer in another window first to keep it visible alongside the
new Additional view."
  (interactive)
  (let ((entry (yunge-reader--ready-view-entry)))
    (unless entry
      (user-error "This Reader view has no ready document"))
    (let ((place (yunge-reader--stable-place))
          (source (current-buffer))
          (window (yunge-reader--presentation-window)))
      (unless place
        (user-error "This Reader view has no stable location yet"))
      (unless window
        (user-error "This Reader view has no active presentation"))
      (let* ((origin-state (yunge-reader--window-state window))
             (document
              (yunge-reader--document-entry-document entry))
             (file (yunge-reader-document-file document))
             (driver (yunge-reader-document-driver document))
             (buffer
              (generate-new-buffer
               (format "*Reader: %s*"
                       (file-name-nondirectory file)))))
        (condition-case error-data
            (progn
              (with-current-buffer buffer
                (yunge-reader-mode))
              (select-window window)
              (switch-to-buffer buffer)
              (yunge-reader--begin-open buffer driver file place)
              buffer)
          (error
           (when (and (window-live-p window)
                      (eq (window-buffer window) buffer)
                      (buffer-live-p source))
             (yunge-reader--restore-window-state window origin-state)
             (when (eq window (selected-window))
               (set-buffer source)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (signal (car error-data) (cdr error-data))))))))

(defun yunge-reader-make-primary ()
  "Make the current Reader view own durable place updates.
Capture and save this view's stable place before changing the primary view."
  (interactive)
  (let ((entry (yunge-reader--ready-view-entry)))
    (unless entry
      (user-error "This Reader view has no ready document"))
    (unless (eq (current-buffer)
                (yunge-reader--document-entry-primary-view entry))
      (let ((place (yunge-reader--stable-place))
            (previous-primary
             (yunge-reader--document-entry-primary-view entry)))
        (unless place
          (user-error "This Reader view has no stable location yet"))
        (yunge-reader--store-place
         (yunge-reader--document-entry-file entry) place)
        (setf (yunge-reader--document-entry-primary-view entry)
              (current-buffer)
              (yunge-reader--document-entry-active-view entry)
              (current-buffer))
        (yunge-reader--notify-view-role-change
         (list previous-primary (current-buffer)))
        (message "Current Reader view is now primary")))
    (current-buffer)))

(defun yunge-reader--close-document ()
  "Detach the current view and close its driver-owned document resource."
  (let ((entry yunge-reader--document-entry)
        (document yunge-reader-document)
        cancelled)
    (cl-incf yunge-reader--open-generation)
    (cl-incf yunge-reader--search-generation)
    (cl-incf yunge-reader--outline-generation)
    (cl-incf yunge-reader--copy-generation)
    (setq yunge-reader--copy-pending nil
          yunge-reader--search-pending nil
          yunge-reader--search-navigation-intent nil
          yunge-reader--search-cursor nil
          yunge-reader--search-direction nil
          yunge-reader--search-origin nil
          yunge-reader--search-wrapped nil
          yunge-reader--search-detached nil)
    (when (buffer-live-p yunge-reader--outline-buffer)
      (let ((outline yunge-reader--outline-buffer))
        (setq yunge-reader--outline-buffer nil)
        (kill-buffer outline)))
    (when entry
      (yunge-reader--remove-outline-waiters entry (current-buffer))
      (dolist (request
               (copy-sequence
                (yunge-reader--document-entry-requests entry)))
        (when (eq (current-buffer)
                  (yunge-reader--view-request-buffer request))
          (yunge-reader--remove-entry-request entry request)
          (push request cancelled))))
    (when document
      (yunge-reader-record-place)
      (yunge-reader--detach-view document))
    (setq yunge-reader-document nil
          yunge-reader--document-entry nil
          yunge-reader--outline-buffer nil
          yunge-reader--opening-file nil
          yunge-reader--pending-place nil
          yunge-reader--place-recording-enabled nil)
    (cond
     ((and document entry
           (eq document
               (yunge-reader--document-entry-document entry)))
      (yunge-reader--remove-entry-view entry (current-buffer))
      (yunge-reader--release-entry-if-unused entry))
     (document
      (yunge-reader--close-resource
       document "Could not close reader document: %s"))
     ((and entry
           (eq (yunge-reader--document-entry-state entry) 'opening)
           (null (yunge-reader--document-entry-requests entry)))
      (when (yunge-reader--entry-current-p entry)
        (remhash (yunge-reader--document-entry-key entry)
                 yunge-reader--document-registry))
      (setf (yunge-reader--document-entry-state entry) 'abandoned)))
    (dolist (request cancelled)
      (yunge-reader--complete-view-request request nil))))

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
  (let ((window (yunge-reader--place-window))
        accepted)
    (unless window
      (user-error "The Reader buffer is not displayed in a live window"))
    (unless (setq accepted
                  (yunge-reader--restore-live-place
                   (yunge-reader--action-place action) window))
      (user-error "The Reader driver rejected the destination"))
    accepted))

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
  (let ((action (yunge-reader-outline-item-action item))
        accepted)
    (unless action
      (user-error "This outline entry has no supported destination"))
    (setq accepted (yunge-reader--follow-action action))
    (message "Outline: %s" (yunge-reader-outline-item-title item))
    accepted))

(defun yunge-reader--remove-outline-waiters (entry buffer)
  "Remove outline waiters owned by BUFFER from ENTRY."
  (setf
   (yunge-reader--document-entry-outline-waiters entry)
   (seq-remove
    (lambda (waiter)
      (eq buffer (yunge-reader--outline-waiter-buffer waiter)))
    (yunge-reader--document-entry-outline-waiters entry))))

(defun yunge-reader--add-outline-waiter (entry outline-buffer)
  "Add this Reader view as a waiter for ENTRY.
OUTLINE-BUFFER identifies its exact auxiliary view."
  (yunge-reader--remove-outline-waiters entry (current-buffer))
  (setf
   (yunge-reader--document-entry-outline-waiters entry)
   (append
    (yunge-reader--document-entry-outline-waiters entry)
    (list
     (yunge-reader--make-outline-waiter
      :buffer (current-buffer)
      :generation yunge-reader--outline-generation
      :outline-buffer outline-buffer)))))

(defun yunge-reader--outline-waiter-current-p
    (entry document waiter)
  "Return whether WAITER still belongs to ENTRY and DOCUMENT."
  (let ((buffer (yunge-reader--outline-waiter-buffer waiter))
        (outline
         (yunge-reader--outline-waiter-outline-buffer waiter)))
    (and (buffer-live-p buffer)
         (buffer-live-p outline)
         (with-current-buffer buffer
           (and (= (yunge-reader--outline-waiter-generation waiter)
                   yunge-reader--outline-generation)
                (eq entry yunge-reader--document-entry)
                (eq document yunge-reader-document)
                (eq outline yunge-reader--outline-buffer))))))

(defun yunge-reader--finish-outline-waiter
    (entry document waiter &optional outline status)
  "Update a current WAITER for ENTRY and DOCUMENT.
Render OUTLINE when non-nil; otherwise display STATUS."
  (when (yunge-reader--outline-waiter-current-p
         entry document waiter)
    (with-current-buffer
        (yunge-reader--outline-waiter-outline-buffer waiter)
      (if outline
          (yunge-reader-outline-set-data outline)
        (yunge-reader-outline-set-status status)))))

(defun yunge-reader--complete-outline
    (entry document value error-data)
  "Complete the shared outline request for ENTRY and DOCUMENT."
  (when (and (yunge-reader--entry-current-p entry)
             (eq (yunge-reader--document-entry-state entry) 'ready)
             (eq document
                 (yunge-reader--document-entry-document entry)))
    (let ((waiters
           (yunge-reader--document-entry-outline-waiters entry)))
      (setf (yunge-reader--document-entry-outline-pending entry) nil
            (yunge-reader--document-entry-outline-waiters entry) nil)
      (cond
       (error-data
        (let ((status
               (format "Could not load document outline: %s"
                       (error-message-string error-data))))
          (display-warning 'yunge-reader status :warning)
          (dolist (waiter waiters)
            (yunge-reader--finish-outline-waiter
             entry document waiter nil status))))
       ((not (yunge-reader--outline-valid-p value))
        (let ((status
               "Reader driver returned an invalid document outline"))
          (display-warning 'yunge-reader status :warning)
          (dolist (waiter waiters)
            (yunge-reader--finish-outline-waiter
             entry document waiter nil status))))
       (t
        (setf (yunge-reader--document-entry-outline entry) value
              (yunge-reader--document-entry-outline-loaded entry) t)
        (dolist (waiter waiters)
          (yunge-reader--finish-outline-waiter
           entry document waiter value)))))))

(defun yunge-reader--ensure-outline-buffer
    (entry document window)
  "Return this view's outline buffer for ENTRY and DOCUMENT in WINDOW."
  (require 'yunge-reader-outline)
  (let ((reader (current-buffer))
        (outline yunge-reader--outline-buffer))
    (if (buffer-live-p outline)
        (progn
          (with-current-buffer outline
            (yunge-reader-outline-set-target
             reader window entry document))
          outline)
      (setq yunge-reader--outline-buffer
            (yunge-reader-outline-create-buffer
             reader window entry document)))))

(defun yunge-reader-outline ()
  "Toggle the outline side window for the current Reader view."
  (interactive)
  (unless (yunge-reader--ready-view-entry)
    (user-error "This reader buffer has no open document"))
  (let ((visible
         (and (buffer-live-p yunge-reader--outline-buffer)
              (get-buffer-window yunge-reader--outline-buffer t))))
    (if visible
        (quit-window nil visible)
      (let* ((reader (current-buffer))
             (entry yunge-reader--document-entry)
             (document yunge-reader-document)
             (window (yunge-reader--place-window))
             (loaded
              (yunge-reader--document-entry-outline-loaded entry))
             (pending
              (yunge-reader--document-entry-outline-pending entry)))
        (unless window
          (user-error
           "The Reader buffer is not displayed in a live window"))
        (let ((outline-buffer
               (yunge-reader--ensure-outline-buffer
                entry document window)))
          (if loaded
              (with-current-buffer outline-buffer
                (yunge-reader-outline-set-data
                 (yunge-reader--document-entry-outline entry)))
            (with-current-buffer outline-buffer
              (yunge-reader-outline-set-status
               "Loading document outline...")))
          (yunge-reader-outline-display-buffer outline-buffer)
          (cond
           (loaded nil)
           (pending
            (with-current-buffer reader
              (yunge-reader--add-outline-waiter
               entry outline-buffer)))
           (t
            (with-current-buffer reader
              (yunge-reader--add-outline-waiter
               entry outline-buffer)
              (setf
               (yunge-reader--document-entry-outline-pending entry)
               t)
              (yunge-reader-request
               'outline nil
               (lambda (value error-data)
                 (yunge-reader--complete-outline
                  entry document value error-data)))))))))))

(defun yunge-reader--search-smart-case-p (query)
  "Return non-nil when QUERY contains an uppercase character."
  (not (equal query (downcase query))))

(defun yunge-reader--search-result-valid-p (result)
  "Return non-nil when RESULT follows the generic search contract."
  (and (yunge-reader-search-result-p result)
       (yunge-reader-position-p
        (yunge-reader-search-result-start result))
       (yunge-reader-position-p
        (yunge-reader-search-result-end result))
       (cl-every
        (lambda (value) (or (null value) (stringp value)))
        (list (yunge-reader-search-result-text result)
              (yunge-reader-search-result-before result)
              (yunge-reader-search-result-after result)))))

(defun yunge-reader--search-batch-valid-p (batch)
  "Return non-nil when BATCH follows the generic search contract."
  (and (yunge-reader-search-batch-p batch)
       (proper-list-p (yunge-reader-search-batch-results batch))
       (cl-every #'yunge-reader--search-result-valid-p
                 (yunge-reader-search-batch-results batch))
       (let ((cursor (yunge-reader-search-batch-cursor batch))
             (done (yunge-reader-search-batch-done batch)))
         (and (memq done '(nil t))
              (if done
                  (null cursor)
                (yunge-reader-search-cursor-p cursor))))))

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
    (when yunge-reader-search-highlight-visible
      (when-let* ((window (get-buffer-window (current-buffer) t)))
        (yunge-jump-history-record window)))
    (setq yunge-reader--search-index index
          yunge-reader--search-detached nil
          yunge-reader-search-result result)
    (run-hooks 'yunge-reader-search-result-hook)
    (when yunge-reader-search-highlight-visible
      (message
       "Match %d%s: %s"
       (1+ index)
       (if yunge-reader--search-done
           (format "/%d" (length yunge-reader-search-results))
         "+")
       (yunge-reader--search-context result)))
    result))

(defun yunge-reader--schedule-search-navigation (buffer generation)
  "Resume BUFFER's search navigation for GENERATION asynchronously."
  (run-at-time
   0 nil
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (= generation yunge-reader--search-generation)
           (yunge-reader--drive-search-navigation)))))))

(defun yunge-reader--finish-search-navigation (index)
  "Visit loaded search result INDEX and finish the pending intent."
  (setq yunge-reader--search-navigation-intent nil)
  (yunge-reader--set-search-index index))

(defun yunge-reader--finish-empty-search ()
  "Finish pending search navigation when no results exist."
  (setq yunge-reader--search-navigation-intent nil)
  (message "No matches for: %s" yunge-reader-search-query))

(defun yunge-reader--search-navigation-ready-p ()
  "Return non-nil when the current intent needs no additional batch."
  (and yunge-reader--search-navigation-intent
       (or yunge-reader--search-done
           (and (null yunge-reader--search-index)
                yunge-reader-search-results)
           (and (natnump yunge-reader--search-index)
                (< (1+ yunge-reader--search-index)
                   (length yunge-reader-search-results))))))

(defun yunge-reader--search-result-origin (direction)
  "Return the current result endpoint for search DIRECTION."
  (when yunge-reader-search-result
    (pcase direction
      ('forward
       (yunge-reader-search-result-end yunge-reader-search-result))
      ('backward
       (yunge-reader-search-result-start yunge-reader-search-result))
      (_
       (error "Invalid Reader search direction: %S" direction)))))

(defun yunge-reader--start-search-run
    (direction origin &optional wrapped)
  "Start a DIRECTION search at stable ORIGIN.
When WRAPPED is non-nil, ORIGIN is the corresponding document boundary."
  (unless (memq direction '(forward backward))
    (error "Invalid Reader search direction: %S" direction))
  (cl-incf yunge-reader--search-generation)
  (setq yunge-reader-search-results nil
        yunge-reader-search-result nil
        yunge-reader-search-highlight-visible t
        yunge-reader--search-index nil
        yunge-reader--search-cursor nil
        yunge-reader--search-direction direction
        yunge-reader--search-origin origin
        yunge-reader--search-wrapped wrapped
        yunge-reader--search-detached nil
        yunge-reader--search-done nil
        yunge-reader--search-pending nil
        yunge-reader--search-navigation-intent direction)
  (run-hooks 'yunge-reader-search-result-hook)
  (yunge-reader--drive-search-navigation))

(defun yunge-reader--wrap-search-run ()
  "Continue the active search from its directional document boundary."
  (let ((direction yunge-reader--search-direction))
    (message
     (if (eq direction 'backward)
         "Search wrapped to document end"
       "Search wrapped to document beginning"))
    (yunge-reader--start-search-run direction nil t)))

(defun yunge-reader--drive-search-navigation ()
  "Fulfill the latest search navigation intent without queuing commands."
  (unless yunge-reader--search-pending
    (when yunge-reader--search-navigation-intent
      (cond
       ((and (natnump yunge-reader--search-index)
             (< (1+ yunge-reader--search-index)
                (length yunge-reader-search-results)))
        (yunge-reader--finish-search-navigation
         (1+ yunge-reader--search-index)))
       ((and (null yunge-reader--search-index)
             yunge-reader-search-results)
        (yunge-reader--finish-search-navigation 0))
       (yunge-reader--search-done
        (if (or yunge-reader-search-results
                (and yunge-reader--search-origin
                     (not yunge-reader--search-wrapped)))
            (yunge-reader--wrap-search-run)
          (yunge-reader--finish-empty-search)))
       (t
        (yunge-reader--request-search-batch))))))

(defun yunge-reader--complete-search-batch
    (buffer document generation old-cursor value error-data)
  "Complete BUFFER's search request for DOCUMENT and GENERATION."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq yunge-reader--search-in-flight
            (max 0 (1- yunge-reader--search-in-flight)))
      (if (not (and (= generation yunge-reader--search-generation)
                    (eq document yunge-reader-document)))
          (when (and (zerop yunge-reader--search-in-flight)
                     yunge-reader--search-navigation-intent)
            (yunge-reader--drive-search-navigation))
        (setq yunge-reader--search-pending nil)
        (cond
         (error-data
          (setq yunge-reader--search-navigation-intent nil)
          (display-warning
           'yunge-reader
           (format "Could not search document: %s"
                   (error-message-string error-data))
           :warning))
         ((not (yunge-reader--search-batch-valid-p value))
          (setq yunge-reader--search-navigation-intent nil)
          (display-warning
           'yunge-reader
           "Reader driver returned an invalid search batch"
           :warning))
         (t
          (let ((new-results
                 (yunge-reader-search-batch-results value))
                (cursor (yunge-reader-search-batch-cursor value))
                (done (yunge-reader-search-batch-done value)))
            (setq yunge-reader-search-results
                  (append yunge-reader-search-results new-results)
                  yunge-reader--search-cursor cursor
                  yunge-reader--search-done done)
            (cond
             ((and (not done) (equal cursor old-cursor))
              (setq yunge-reader--search-navigation-intent nil)
              (display-warning
               'yunge-reader
               "Reader search cursor did not advance"
               :warning))
             (yunge-reader--search-navigation-intent
              (if (yunge-reader--search-navigation-ready-p)
                  (yunge-reader--drive-search-navigation)
                (yunge-reader--schedule-search-navigation
                 buffer generation)))))))))))

(defun yunge-reader--request-search-batch ()
  "Request the next batch needed by the current search intent."
  (unless yunge-reader--search-pending
    (if (> yunge-reader--search-in-flight 0)
        (yunge-reader--search-loading-message
         yunge-reader--search-direction)
      (let ((buffer (current-buffer))
            (document yunge-reader-document)
            (generation yunge-reader--search-generation)
            (old-cursor yunge-reader--search-cursor))
        (setq yunge-reader--search-pending t)
        (cl-incf yunge-reader--search-in-flight)
        (yunge-reader-request
         'search
         (list :query yunge-reader-search-query
               :case-sensitive yunge-reader--search-case-sensitive
               :direction yunge-reader--search-direction
               :origin (and (null yunge-reader--search-cursor)
                            yunge-reader--search-origin)
               :cursor yunge-reader--search-cursor
               :match-limit yunge-reader-search-match-limit
               :page-limit yunge-reader-search-page-limit)
         (lambda (value error-data)
           (yunge-reader--complete-search-batch
            buffer document generation old-cursor value error-data)))))))

(defun yunge-reader--search-loading-message (intent)
  "Describe the outstanding search for navigation INTENT."
  (message
   (pcase intent
     ('backward "Searching backward...")
     (_ "Searching forward..."))))

(defun yunge-reader--navigate-search (direction)
  "Navigate in search DIRECTION without accumulating commands."
  (unless yunge-reader-search-query
    (user-error "There is no active document search"))
  (unless (memq direction '(forward backward))
    (error "Invalid Reader search direction: %S" direction))
  (cond
   (yunge-reader--search-detached
    (yunge-reader--start-search-run
     direction (yunge-reader--current-position)))
   ((not (eq direction yunge-reader--search-direction))
    (yunge-reader--start-search-run
     direction
     (or (yunge-reader--search-result-origin direction)
         yunge-reader--search-origin
         (yunge-reader--current-position))))
   (t
    (let ((was-visible yunge-reader-search-highlight-visible))
      (setq yunge-reader-search-highlight-visible t
            yunge-reader--search-navigation-intent direction)
      (unless was-visible
        (run-hooks 'yunge-reader-search-result-hook)))
    (if yunge-reader--search-pending
        (yunge-reader--search-loading-message direction)
      (yunge-reader--drive-search-navigation)))))

(defun yunge-reader--cancel-search-navigation ()
  "Cancel one delayed search jump without discarding discovered results."
  (when yunge-reader--search-navigation-intent
    (setq yunge-reader--search-navigation-intent nil)
    t))

(defun yunge-reader--detach-search-navigation ()
  "Detach an active search from its old result after reading movement."
  (when yunge-reader-search-query
    (setq yunge-reader--search-navigation-intent nil
          yunge-reader--search-detached t)
    t))

(defun yunge-reader-search (query)
  "Search the current document for literal QUERY.
Case is ignored unless QUERY contains an uppercase character."
  (interactive
   (list
    (read-string
     "Search document: " nil 'yunge-reader-search-history)))
  (unless yunge-reader-document
    (user-error "This reader buffer has no open document"))
  (when (string-empty-p query)
    (user-error "Search query must not be empty"))
  (setq yunge-reader-search-query query
        yunge-reader--search-case-sensitive
        (yunge-reader--search-smart-case-p query))
  (message "Searching for: %s" query)
  (yunge-reader--start-search-run
   'forward (yunge-reader--current-position)))

(defun yunge-reader-search-next ()
  "Visit the next match for the active document search."
  (interactive)
  (yunge-reader--navigate-search 'forward))

(defun yunge-reader-search-previous ()
  "Visit the previous match for the active document search."
  (interactive)
  (yunge-reader--navigate-search 'backward))

(defun yunge-reader-clear-search ()
  "Clear the active document search and its view highlight."
  (interactive)
  (cl-incf yunge-reader--search-generation)
  (setq yunge-reader-search-query nil
        yunge-reader-search-results nil
        yunge-reader-search-result nil
        yunge-reader-search-highlight-visible nil
        yunge-reader--search-index nil
        yunge-reader--search-cursor nil
        yunge-reader--search-direction nil
        yunge-reader--search-origin nil
        yunge-reader--search-wrapped nil
        yunge-reader--search-detached nil
        yunge-reader--search-done nil
        yunge-reader--search-pending nil
        yunge-reader--search-navigation-intent nil)
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
  (when (and yunge-reader-document
             (eq (yunge-reader-document-layout
                  yunge-reader-document)
                 'reflow))
    (user-error
     "Fit modes are not available for reflowable documents"))
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
  (let ((selection
         (make-yunge-reader-selection
          :start start :end end :text text)))
    (cl-incf yunge-reader--copy-generation)
    (setq yunge-reader--copy-pending nil)
    (unless (equal selection yunge-reader-selection)
      (setq yunge-reader-selection selection)
      (run-hooks 'yunge-reader-selection-change-hook))))

(defun yunge-reader-clear-selection (&optional defer-refresh)
  "Clear the logical selection in the current reader buffer.
When DEFER-REFRESH is non-nil, leave repainting to the caller."
  (interactive)
  (let ((changed yunge-reader-selection))
    (cl-incf yunge-reader--copy-generation)
    (setq yunge-reader-selection nil
          yunge-reader--copy-pending nil)
    (when changed
      (run-hooks 'yunge-reader-selection-change-hook)))
  (unless defer-refresh
    (yunge-reader-refresh)))

(defun yunge-reader-hide-search-highlight ()
  "Hide the active search highlight without ending its search session."
  (interactive)
  (when yunge-reader-search-highlight-visible
    (setq yunge-reader-search-highlight-visible nil)
    (run-hooks 'yunge-reader-search-result-hook)
    t))

(defun yunge-reader--clear-transient-highlights ()
  "Clear the selection and hide the active search highlight.
Return non-nil when at least one transient highlight was active."
  (let ((selection yunge-reader-selection)
        (search-highlight yunge-reader-search-highlight-visible))
    (when selection
      (yunge-reader-clear-selection search-highlight))
    (when search-highlight
      (yunge-reader-hide-search-highlight))
    (or selection search-highlight)))

(defun yunge-reader-keyboard-quit ()
  "Dismiss Reader highlights or perform the ordinary keyboard quit."
  (interactive)
  (let ((cancelled (yunge-reader--cancel-search-navigation))
        (cleared (yunge-reader--clear-transient-highlights)))
    (unless (or cancelled cleared)
      (keyboard-quit))))

(defun yunge-reader-escape ()
  "Clear Reader highlights or perform the ordinary escape action."
  (interactive)
  (let ((cancelled (yunge-reader--cancel-search-navigation))
        (cleared (yunge-reader--clear-transient-highlights)))
    (unless (or cancelled cleared)
      (keyboard-escape-quit))))

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
