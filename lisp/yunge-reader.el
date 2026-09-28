;;; yunge-reader.el --- Extensible document reading -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'yunge-jump-history)
(require 'yunge-key)
(require 'yunge-reader-model)
(require 'yunge-reader-task)
(require 'yunge-reader-search)
(require 'yunge-reader-selection)

(declare-function browse-url "browse-url" (url &rest arguments))
(declare-function evil-refresh-cursor
                  "evil-common" (&optional state buffer))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-state-property
                  "evil-common" (state property &optional value))
(declare-function yunge-reader-outline-create-buffer
                  "yunge-reader-outline"
                  (reader window document &optional outline))
(declare-function yunge-reader-outline-display-buffer
                  "yunge-reader-outline" (buffer))
(declare-function yunge-reader-outline-set-data
                  "yunge-reader-outline" (outline))
(declare-function yunge-reader-outline-set-status
                  "yunge-reader-outline" (status))
(declare-function yunge-reader-outline-set-target
                  "yunge-reader-outline"
                  (reader window document))

(require 'yunge-reader-state)

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

(cl-defstruct (yunge-reader--document-entry
               (:constructor yunge-reader--make-document-entry))
  "One canonical document resource and its attached Reader views."
  key
  file
  driver
  state
  document
  open-task
  requests
  views
  primary-view
  active-view
  outline
  outline-task)

(cl-defstruct (yunge-reader--view-request
               (:constructor yunge-reader--make-view-request))
  "One Reader buffer waiting to attach to a document resource."
  buffer
  generation
  complete
  completed)

(defvar yunge-reader--request-task nil
  "Dynamically bound composite task for the current driver request.")

(defconst yunge-reader-outline-maximum-items 10000
  "Maximum number of outline entries accepted from one driver response.")

(defvar yunge-reader-drivers nil
  "Registered `yunge-reader-driver' objects in precedence order.")

(defvar yunge-reader--document-registry
  (make-hash-table :test #'equal)
  "Map canonical document keys to live resource entries.")

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
  "Persistent place waiting for the current document to finish opening.")

(defvar-local yunge-reader--last-stable-place nil
  "Last stable place captured while the primary view was visible.")

(defvar-local yunge-reader--place-recording-enabled nil
  "Whether the current document may replace its persistent place.")

(defvar-local yunge-reader--restoring-place nil
  "Whether the current view is restoring a persistent place.")

(defvar-local yunge-reader--active-presentation nil
  "Active Emacs window for this logical Reader view.")

(defvar-local yunge-reader-zoom-mode 'fit-width
  "Current zoom mode: `manual', `fit-width', or `fit-page'.")

(defvar-local yunge-reader-scale 1.0
  "Manual document scale used when `yunge-reader-zoom-mode' is `manual'.")

(defvar-local yunge-reader-effective-scale nil
  "Scale most recently resolved by the active view adapter.")

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

(defvar-keymap yunge-reader-set-mark-map
  :doc "Keymap for setting document-local Reader marks.")

(defvar-keymap yunge-reader-goto-mark-map
  :doc "Keymap for visiting document-local Reader marks.")

(dolist (character (number-sequence ?a ?z))
  (define-key yunge-reader-set-mark-map (vector character)
              #'yunge-reader-set-mark)
  (define-key yunge-reader-goto-mark-map (vector character)
              #'yunge-reader-goto-mark))

(defconst yunge-reader-normal-bindings
  `(("+" yunge-reader-zoom-in "zoom in")
    ("-" yunge-reader-zoom-out "zoom out")
    ("=" yunge-reader-zoom-reset "reset zoom")
    ("/" yunge-reader-search "search")
    ("N" yunge-reader-search-previous "previous match")
    ("P" yunge-reader-fit-page "fit page")
    ("W" yunge-reader-fit-width "fit width")
    ("gr" yunge-reader-refresh "refresh")
    ("'" ,yunge-reader-goto-mark-map "jump mark")
    ("m" ,yunge-reader-set-mark-map "set mark")
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
  "'" yunge-reader-goto-mark-map
  "m" yunge-reader-set-mark-map
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
    (yunge-reader--dismiss-transients)))

(defun yunge-reader--prevent-evil-editing-state
    (function &rest arguments)
  "Run Evil editing-state FUNCTION unless this is a Reader buffer."
  (unless (derived-mode-p 'yunge-reader-mode)
    (apply function arguments)))

(define-derived-mode yunge-reader-mode special-mode "Yunge Reader"
  "Major mode shared by Yunge Reader document adapters."
  (auto-save-mode -1)
  (setq-local cursor-type nil)
  (yunge-reader--hide-evil-cursor)
  (setq-local truncate-lines t)
  (setq-local yunge-navigation-landing-policy 'viewport)
  (setq-local yunge-reader-scale yunge-reader-default-scale)
  (setq-local yunge-reader--document-entry nil)
  (setq-local yunge-reader--view-attached nil)
  (setq-local yunge-reader--active-presentation nil)
  (setq-local yunge-reader--outline-buffer nil)
  (setq-local yunge-reader--last-stable-place nil)
  (add-hook 'post-command-hook
            #'yunge-reader--note-view-activity nil t)
  (add-hook 'change-major-mode-hook
            #'yunge-reader--close-document nil t)
  (add-hook 'kill-buffer-hook #'yunge-reader--close-document nil t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'yunge-reader-mode 'normal)
  (yunge-key-evil-define 'normal yunge-reader-mode-map
                         yunge-reader-normal-bindings)
  (advice-add 'evil-insert-state :around
              #'yunge-reader--prevent-evil-editing-state)
  (advice-add 'evil-replace-state :around
              #'yunge-reader--prevent-evil-editing-state)
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
    (name &key match open close attach detach
          outline outline-index search selection-text location restore)
  "Register a reader driver NAME.
MATCH, OPEN, CLOSE, OUTLINE, SEARCH, and SELECTION-TEXT follow the contracts
documented by `yunge-reader-driver'.  Capabilities are optional; attempting an
unsupported operation completes with an error.  ATTACH and DETACH are an
optional pair whose omitted default performs no buffer-specific setup.
OUTLINE-INDEX locates the current item in a loaded outline.  LOCATION and
RESTORE are another optional pair.
Registering NAME again atomically replaces its old definition and gives the
new definition highest precedence."
  (unless (symbolp name)
    (error "Reader driver name must be a symbol: %S" name))
  (dolist (function (list match open close))
    (unless (functionp function)
      (error "Reader driver %s has a non-function member: %S"
             name function)))
  (dolist (function
           (delq nil (list outline outline-index search selection-text)))
    (unless (functionp function)
      (error "Reader driver %s has a non-function capability: %S"
             name function)))
  (when (and outline-index (null outline))
    (error "Reader driver %s locates an outline it cannot provide" name))
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
          :outline-function outline
          :outline-index-function outline-index
          :search-function search
          :selection-text-function selection-text
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
  "Return the canonical persistent-place key for FILE."
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

(defun yunge-reader--saved-appearance-override (file driver)
  "Return FILE's valid saved DRIVER appearance override, or nil."
  (let ((appearance
         (yunge-reader-state-value
          file (yunge-reader--driver-format driver) :appearance)))
    (and (yunge-reader--appearance-p appearance) appearance)))

(defun yunge-reader-document-appearance-override (&optional document)
  "Return DOCUMENT's explicit appearance override, or nil.
DOCUMENT defaults to the document in the current Reader buffer."
  (let ((document (or document yunge-reader-document)))
    (when document
      (yunge-reader--saved-appearance-override
       (yunge-reader-document-file document)
       (yunge-reader-document-driver document)))))

(defun yunge-reader-effective-appearance (&optional document)
  "Return DOCUMENT's effective Reader appearance.
DOCUMENT defaults to the document in the current Reader buffer."
  (let ((document (or document yunge-reader-document)))
    (unless (yunge-reader-document-p document)
      (error "The current Reader buffer has no document"))
    (or (yunge-reader-document-appearance-override document)
        (yunge-reader--default-appearance
         (yunge-reader-document-driver document)))))

(defun yunge-reader--store-appearance-override (file driver appearance)
  "Persist APPEARANCE as FILE's explicit DRIVER override."
  (unless (yunge-reader--appearance-p appearance)
    (error "Invalid Reader appearance: %S" appearance))
  (yunge-reader-state-put
   file (yunge-reader--driver-format driver) :appearance appearance))

(defun yunge-reader--unset-appearance-override (file driver)
  "Remove FILE's explicit DRIVER appearance override."
  (yunge-reader-state-put
   file (yunge-reader--driver-format driver) :appearance nil))

(defun yunge-reader-cleanup-missing-document-state ()
  "Forget saved state for document files that no longer exist.
This removes entire records whose path aliases are all missing.
A file on a disconnected volume is considered missing, so this command is
never run automatically."
  (interactive)
  (let ((count (yunge-reader-state-cleanup-missing)))
    (message "Removed %d saved Reader document record%s"
             count (if (= count 1) "" "s"))))

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
              (yunge-reader--document-entry-file entry)
              (yunge-reader--document-entry-driver entry))))
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
  "Return whether the current buffer owns persistent place updates."
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
       (yunge-reader-document-file document)
       (yunge-reader-document-driver document)
       appearance)
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
        (let ((file (yunge-reader-document-file document))
              (driver (yunge-reader-document-driver document)))
          (yunge-reader--unset-appearance-override file driver)
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
          (current-buffer)))
  (yunge-reader--cache-current-place))

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
  "Return printable persistent data for reader POSITION."
  (list
   :unit (copy-tree (yunge-reader-position-unit position) t)
   :offset (copy-tree (yunge-reader-position-offset position) t)
   :x (yunge-reader-position-x position)
   :y (yunge-reader-position-y position)))

(defun yunge-reader--position-data-p (data)
  "Return whether DATA represents a persistent reader position."
  (and (listp data)
       (plist-member data :unit)
       (let ((x (plist-get data :x))
             (y (plist-get data :y)))
         (and (or (null x) (numberp x))
              (or (null y) (numberp y))))))

(defun yunge-reader--position-from-data (data)
  "Return the reader position represented by persistent DATA."
  (when (yunge-reader--position-data-p data)
    (make-yunge-reader-position
     :unit (copy-tree (plist-get data :unit) t)
     :offset (copy-tree (plist-get data :offset) t)
     :x (plist-get data :x)
     :y (plist-get data :y))))

(defun yunge-reader--make-place (_driver position)
  "Return a printable place at POSITION."
  (list
   :position (yunge-reader--position-data position)
   :zoom-mode yunge-reader-zoom-mode
   :scale yunge-reader-scale))

(defun yunge-reader--place-p (place _driver)
  "Return whether PLACE has the current persistent shape."
  (and (listp place)
       (yunge-reader--position-data-p
        (plist-get place :position))
       (memq (plist-get place :zoom-mode)
             '(manual fit-width fit-page))
       (let ((scale (plist-get place :scale)))
         (and (numberp scale) (> scale 0)))))

(defun yunge-reader--saved-place (file driver)
  "Return the valid persistent place for FILE and DRIVER, or nil."
  (let ((place
         (yunge-reader-state-value
          file (yunge-reader--driver-format driver) :place)))
    (when (yunge-reader--place-p place driver)
      (copy-tree place t))))

(defun yunge-reader--store-place (file driver place)
  "Store persistent PLACE for FILE and DRIVER as the most recent record."
  (unless (yunge-reader--place-p place driver)
    (error "Invalid Reader place: %S" place))
  (yunge-reader-state-put
   file (yunge-reader--driver-format driver) :place place))

(defun yunge-reader--mark-character-p (character)
  "Return whether CHARACTER names one document-local Reader mark."
  (and (integerp character) (<= ?a character ?z)))

(defun yunge-reader--mark-data-p (mark _driver)
  "Return whether MARK has the current persistent shape."
  (and (listp mark)
       (yunge-reader--position-data-p (plist-get mark :position))))

(defun yunge-reader--make-mark-data (_driver position)
  "Return printable mark data at POSITION."
  (list
   :position (yunge-reader--position-data position)))

(defun yunge-reader--store-mark (file driver character mark)
  "Store document-local CHARACTER MARK for FILE and DRIVER."
  (unless (yunge-reader--mark-character-p character)
    (error "Invalid Reader mark character: %S" character))
  (unless (yunge-reader--mark-data-p mark driver)
    (error "Invalid Reader mark: %S" mark))
  (let* ((format (yunge-reader--driver-format driver))
         (marks (yunge-reader-state-value file format :marks)))
    (setq marks
          (cons
           (cons character (copy-tree mark t))
           (seq-remove
            (lambda (entry) (eq (car-safe entry) character))
            marks)))
    (yunge-reader-state-put file format :marks marks)
    (copy-tree mark t)))

(defun yunge-reader--saved-mark (file driver character)
  "Return FILE's valid DRIVER mark CHARACTER, or nil."
  (when (yunge-reader--mark-character-p character)
    (let* ((marks
            (yunge-reader-state-value
             file (yunge-reader--driver-format driver) :marks))
           (mark (cdr (assq character marks))))
      (when (yunge-reader--mark-data-p mark driver)
        (copy-tree mark t)))))

(defun yunge-reader--presentation-window-p (window)
  "Return whether WINDOW presents the current Reader buffer."
  (and (window-live-p window)
       (eq (window-buffer window) (current-buffer))))

(defun yunge-reader--presentation-windows ()
  "Return every live window presenting the current Reader view."
  (get-buffer-window-list (current-buffer) nil t))

(defun yunge-reader--activate-presentation (&optional window)
  "Make WINDOW the current logical view's active presentation.
WINDOW defaults to the selected window.  Return the accepted window, or
  nil when it does not display the current Reader buffer."
  (let ((window (or window (selected-window))))
    (when (yunge-reader--presentation-window-p window)
      (setq yunge-reader--active-presentation window)
      window)))

(defun yunge-reader--presentation-window ()
  "Return and retain the active window for this logical Reader view."
  (or (yunge-reader--activate-presentation)
      (and (yunge-reader--presentation-window-p
            yunge-reader--active-presentation)
           yunge-reader--active-presentation)
      (when-let* ((window (car (yunge-reader--presentation-windows))))
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

(defun yunge-reader--recordable-primary-p ()
  "Return whether the current primary view may persist its place."
  (and yunge-reader--place-recording-enabled
       (not yunge-reader--restoring-place)
       (yunge-reader--primary-view-p)
       yunge-reader-document))

(defun yunge-reader--cache-current-place (&optional window)
  "Cache the stable place in the active presentation WINDOW.
This is best-effort bookkeeping for a view which may later become hidden."
  (when (yunge-reader--recordable-primary-p)
    (let ((window (yunge-reader--place-window window)))
      (when (and window (yunge-reader--active-presentation-p window))
        (condition-case nil
            (when-let* ((place (yunge-reader--current-place window)))
              (setq yunge-reader--last-stable-place
                    (copy-tree place t)))
          (error nil)))))
  (and yunge-reader--last-stable-place
       (copy-tree yunge-reader--last-stable-place t)))

(defun yunge-reader--stable-place ()
  "Return the current committed view place, or nil."
  (when yunge-reader--place-recording-enabled
    (when-let* ((window (yunge-reader--place-window)))
      (yunge-reader--current-place window))))

(defun yunge-reader-record-place (&optional window)
  "Record the current persistent Reader place as viewed in WINDOW.
Do nothing until document opening and any prior place restoration commit.
Only the primary view's active presentation may replace the persistent place."
  (let* ((explicit-window window)
         (window (yunge-reader--place-window window)))
    (when (yunge-reader--recordable-primary-p)
      (condition-case error-data
          (when-let*
              ((place
                (cond
                 ((and window
                       (yunge-reader--active-presentation-p window))
                  (yunge-reader--current-place window))
                 ((and (null explicit-window) (null window))
                  (and yunge-reader--last-stable-place
                       (copy-tree yunge-reader--last-stable-place t))))))
            (setq yunge-reader--last-stable-place
                  (copy-tree place t))
            (yunge-reader--store-place
             (yunge-reader-document-file yunge-reader-document)
             (yunge-reader-document-driver yunge-reader-document)
             place))
        (error
         (display-warning
          'yunge-reader
          (format "Could not remember Reader place: %s"
                  (error-message-string error-data))
          :warning))))))

(defun yunge-reader--save-open-places ()
  "Commit stable places from open primary views before session state saves."
  (dolist (buffer (reverse (buffer-list)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'yunge-reader-mode)
          (yunge-reader-record-place))))))

(with-eval-after-load 'savehist
  (add-hook 'savehist-save-hook #'yunge-reader--save-open-places))

(defun yunge-reader--restore-view-state (place)
  "Restore generic zoom state from persistent PLACE."
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
      (yunge-reader-search-detach-navigation))
    accepted))

(defun yunge-reader--interactive-mark-character ()
  "Return the lowercase mark represented by `last-command-event'."
  (let ((character (event-basic-type last-command-event)))
    (unless (yunge-reader--mark-character-p character)
      (user-error "Reader marks use lowercase letters a-z"))
    character))

(defun yunge-reader-set-mark (character)
  "Set document-local Reader mark CHARACTER at the current stable position."
  (interactive (list (yunge-reader--interactive-mark-character)))
  (unless (yunge-reader--mark-character-p character)
    (user-error "Reader marks use lowercase letters a-z"))
  (unless yunge-reader-document
    (user-error "This Reader buffer has no open document"))
  (unless (and yunge-reader--place-recording-enabled
               (not yunge-reader--restoring-place))
    (user-error "The current Reader position is not stable yet"))
  (let ((window (yunge-reader--place-window)))
    (unless window
      (user-error "The Reader buffer is not displayed in a live window"))
    (let* ((driver
            (yunge-reader-document-driver yunge-reader-document))
           (position (yunge-reader--current-position window)))
      (unless position
        (user-error "The Reader driver has no stable current position"))
      (yunge-reader--store-mark
       (yunge-reader-document-file yunge-reader-document)
       driver
       character
       (yunge-reader--make-mark-data driver position))
      (message "Reader mark %c set" character)
      character)))

(defun yunge-reader-goto-mark (character)
  "Visit document-local Reader mark CHARACTER using the current view style."
  (interactive (list (yunge-reader--interactive-mark-character)))
  (unless (yunge-reader--mark-character-p character)
    (user-error "Reader marks use lowercase letters a-z"))
  (unless yunge-reader-document
    (user-error "This Reader buffer has no open document"))
  (let* ((driver (yunge-reader-document-driver yunge-reader-document))
         (file (yunge-reader-document-file yunge-reader-document))
         (mark (yunge-reader--saved-mark file driver character))
         (window (yunge-reader--place-window)))
    (unless mark
      (user-error "Reader mark %c is not set" character))
    (unless window
      (user-error "The Reader buffer is not displayed in a live window"))
    (let* ((position
            (yunge-reader--position-from-data
             (plist-get mark :position)))
           (accepted
            (yunge-reader--restore-live-place
             (yunge-reader--make-place driver position) window)))
      (unless accepted
        (user-error "The Reader driver rejected mark %c" character))
      accepted)))

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

(defun yunge-reader--attach-view (document initial-place)
  "Attach DOCUMENT's driver view with INITIAL-PLACE to the current buffer."
  (let* ((driver (yunge-reader-document-driver document))
         (attach (yunge-reader-driver-attach-function driver)))
    ;; DETACH must be able to clean up a partially attached view when ATTACH
    ;; signals after installing buffer-local state.
    (setq yunge-reader--view-attached t)
    (funcall attach document initial-place)))

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
    (when-let* ((task (yunge-reader--document-entry-outline-task entry))
                ((yunge-reader-task-active-p task)))
      (yunge-reader-task-cancel
       task "No Reader view is using this document outline"))
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
                (yunge-reader--attach-view
                 document (and yunge-reader--pending-place
                               (copy-tree yunge-reader--pending-place t)))
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
  (setf (yunge-reader--document-entry-open-task entry) nil)
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
        completed
        task)
    (setq
     task
     (yunge-reader-task-create
      'open
      (lambda (value error-data)
        (yunge-reader--finish-resource-open
         entry (car-safe value) (cadr value) error-data))
      :owner entry))
    (setf (yunge-reader--document-entry-open-task entry) task)
    (condition-case error-data
        (let ((yunge-reader--request-task task))
          (yunge-reader-task-adopt-child
           task
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
                (if (yunge-reader-task-active-p task)
                    (yunge-reader-task-finish
                     task (if open-error 'failed 'completed)
                     (list handle properties) open-error)
                  ;; A driver predating cancellable tasks can still return a
                  ;; handle after the last waiting view disappeared.
                  (yunge-reader--close-handle
                   driver file handle properties)))))))
      (error
       (unless completed
         (setq completed t)
         (yunge-reader-task-finish
          task 'failed nil error-data))))
    task))

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
            yunge-reader--last-stable-place nil
            yunge-reader--place-recording-enabled nil
            yunge-reader--document-entry entry)
      (cl-incf yunge-reader--open-generation)
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
  "Make the current Reader view own persistent place updates.
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
         (yunge-reader--document-entry-file entry)
         (yunge-reader--document-entry-driver entry)
         place)
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
    (yunge-reader-selection-cancel-copy "The Reader view was closed")
    (yunge-reader-search-reset "The Reader view was closed")
    (when (buffer-live-p yunge-reader--outline-buffer)
      (let ((outline yunge-reader--outline-buffer))
        (setq yunge-reader--outline-buffer nil)
        (kill-buffer outline)))
    (when entry
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
          yunge-reader--last-stable-place nil
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
      (setf (yunge-reader--document-entry-state entry) 'abandoned)
      (when-let* ((task
                   (yunge-reader--document-entry-open-task entry))
                  ((yunge-reader-task-active-p task)))
        (yunge-reader-task-cancel
         task "No Reader view is waiting for this document"))))
    (dolist (request cancelled)
      (yunge-reader--complete-view-request request nil))))

(defun yunge-reader--driver-capability (driver operation)
  "Return DRIVER's explicit function for generic OPERATION."
  (pcase operation
    ('outline (yunge-reader-driver-outline-function driver))
    ('search (yunge-reader-driver-search-function driver))
    ('selection-text
     (yunge-reader-driver-selection-text-function driver))
    (_ nil)))

(defun yunge-reader--typed-capability-arguments (operation arguments)
  "Build explicit driver arguments for OPERATION from ARGUMENTS."
  (pcase operation
    ('outline nil)
    ('search
     (make-yunge-reader-search-request
      :query (plist-get arguments :query)
      :case-sensitive (plist-get arguments :case-sensitive)
      :direction (plist-get arguments :direction)
      :origin (plist-get arguments :origin)
      :cursor (plist-get arguments :cursor)
      :match-limit (plist-get arguments :match-limit)
      :unit-limit (plist-get arguments :unit-limit)))
    ('selection-text
     (make-yunge-reader-selection-text-request
      :start (plist-get arguments :start)
      :end (plist-get arguments :end)
      :cursor (plist-get arguments :cursor)
      :unit-limit (plist-get arguments :unit-limit)
      :character-limit (plist-get arguments :character-limit)))
    (_ arguments)))

(cl-defun yunge-reader-request
    (operation arguments complete &key owner timeout revision)
  "Request OPERATION with ARGUMENTS for the current document.
COMPLETE is called exactly once with a value and error value.  Return a
cancellable composite task.  OWNER, TIMEOUT, and REVISION describe that task."
  (unless yunge-reader-document
    (user-error "This reader buffer has no open document"))
  (unless (functionp complete)
    (error "Reader completion must be a function: %S" complete))
  (let* ((driver (yunge-reader-document-driver yunge-reader-document))
         (capability (yunge-reader--driver-capability driver operation))
         (task
          (yunge-reader-task-create
           operation complete
           :owner (or owner (current-buffer))
           :timeout timeout
           :revision revision)))
    (condition-case error-data
        (if (not capability)
            (error "Reader driver %s does not support %s"
                   (yunge-reader-driver-name driver) operation)
          (let ((yunge-reader--request-task task))
            (yunge-reader-task-adopt-child
             task
             (funcall
              capability yunge-reader-document
              (yunge-reader--typed-capability-arguments
               operation arguments)
              (lambda (value request-error)
                (yunge-reader-task-finish
                 task (if request-error 'failed 'completed)
                 value request-error))))))
      (error
       (yunge-reader-task-finish
        task 'failed nil error-data)))
    task))

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

(defun yunge-reader--complete-outline
    (entry document value error-data)
  "Complete the shared outline request for ENTRY and DOCUMENT."
  (setf (yunge-reader--document-entry-outline-task entry) nil)
  (when (and (yunge-reader--entry-current-p entry)
             (eq (yunge-reader--document-entry-state entry) 'ready)
             (eq document
                 (yunge-reader--document-entry-document entry)))
    (let ((status
           (cond
            (error-data
             (format "Could not load document outline: %s"
                     (error-message-string error-data)))
            ((not (yunge-reader--outline-valid-p value))
             "Reader driver returned an invalid document outline"))))
      (if status
          (display-warning 'yunge-reader status :warning)
        (setf (yunge-reader--document-entry-outline entry) value))
      (dolist (reader (yunge-reader--entry-live-views entry))
        (with-current-buffer reader
          (when (buffer-live-p yunge-reader--outline-buffer)
            (with-current-buffer yunge-reader--outline-buffer
              (if status
                  (yunge-reader-outline-set-status status)
                (yunge-reader-outline-set-data value)))))))))

(defun yunge-reader--ensure-outline-buffer (document window)
  "Return this view's outline buffer for DOCUMENT in WINDOW."
  (require 'yunge-reader-outline)
  (let ((reader (current-buffer))
        (outline yunge-reader--outline-buffer))
    (if (buffer-live-p outline)
        (progn
          (with-current-buffer outline
            (yunge-reader-outline-set-target
             reader window document))
          outline)
      (setq yunge-reader--outline-buffer
            (yunge-reader-outline-create-buffer
             reader window document)))))

(defun yunge-reader-outline ()
  "Toggle the outline side window for the current Reader view."
  (interactive)
  (let ((entry (yunge-reader--ready-view-entry)))
    (unless entry
      (user-error "This reader buffer has no open document"))
    (let ((visible
           (and (buffer-live-p yunge-reader--outline-buffer)
                (get-buffer-window yunge-reader--outline-buffer t))))
      (if visible
          (quit-window nil visible)
        (let* ((reader (current-buffer))
               (document yunge-reader-document)
               (window (yunge-reader--place-window))
               (outline-data
                (yunge-reader--document-entry-outline entry))
               (task
                (yunge-reader--document-entry-outline-task entry)))
          (unless window
            (user-error
             "The Reader buffer is not displayed in a live window"))
          (let ((outline-buffer
                 (yunge-reader--ensure-outline-buffer
                  document window)))
            (with-current-buffer outline-buffer
              (if outline-data
                  (yunge-reader-outline-set-data outline-data)
                (yunge-reader-outline-set-status
                 "Loading document outline...")))
            (yunge-reader-outline-display-buffer outline-buffer)
            (unless (or outline-data (yunge-reader-task-active-p task))
              (with-current-buffer reader
                (let (completed)
                  (setq task
                        (yunge-reader-request
                         'outline nil
                         (lambda (value error-data)
                           (setq completed t)
                           (yunge-reader--complete-outline
                            entry document value error-data))
                         :owner entry))
                  ;; A synchronous driver completion must not leave its
                  ;; terminal task registered as active work.
                  (unless completed
                    (setf (yunge-reader--document-entry-outline-task entry)
                          task)))))))))))

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

(defun yunge-reader--dismiss-transients ()
  "Cancel a pending copy, clear the selection, and hide the search highlight.
Return non-nil when any transient Reader activity was dismissed."
  (let ((copy-cancelled
         (yunge-reader-selection-cancel-copy "Reader copy was cancelled"))
        (selection yunge-reader-selection)
        (search-highlight yunge-reader-search-highlight-visible))
    (when selection
      (yunge-reader-clear-selection search-highlight))
    (when search-highlight
      (yunge-reader-hide-search-highlight))
    (or copy-cancelled selection search-highlight)))

(defun yunge-reader-keyboard-quit ()
  "Dismiss Reader highlights or perform the ordinary keyboard quit."
  (interactive)
  (let ((cancelled (yunge-reader-search-cancel-navigation))
        (cleared (yunge-reader--dismiss-transients)))
    (unless (or cancelled cleared)
      (keyboard-quit))))

(defun yunge-reader-escape ()
  "Clear Reader highlights or perform the ordinary escape action."
  (interactive)
  (let ((cancelled (yunge-reader-search-cancel-navigation))
        (cleared (yunge-reader--dismiss-transients)))
    (unless (or cancelled cleared)
      (keyboard-escape-quit))))

(yunge-jump-history-register-target
 'reader
 :capture #'yunge-reader--jump-target
 :same #'yunge-reader--same-jump-target-p
 :visit #'yunge-reader--visit-jump-target)

(yunge-jump-history-track-command 'yunge-reader--follow-outline-item)
(yunge-jump-history-track-command 'yunge-reader-goto-mark)

(provide 'yunge-reader)

;;; yunge-reader.el ends here
