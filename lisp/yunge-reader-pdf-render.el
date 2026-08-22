;;; yunge-reader-pdf-render.el --- PDF render and text pipeline -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'svg)
(require 'yunge-reader)
(require 'yunge-reader-native)
(require 'yunge-reader-pdf-backend)
(require 'yunge-reader-pdf-geometry)
(require 'yunge-reader-pdf-protocol)
(require 'yunge-reader-pdf-viewport)

(declare-function yunge-reader-pdf--make-hit-index
                  "yunge-reader-pdf-interaction" (page text-layer))

(defvar yunge-reader-pdf--image-map)
(defvar yunge-reader-pdf-view-mode)

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

(cl-defstruct yunge-reader-pdf--prefetch-task
  "One replaceable, low-priority PDF prefetch task."
  document
  kind
  page
  width
  appearance
  generation)

(defvar-local yunge-reader-pdf--generation 0
  "Generation used to reject late PDF rendering completions.")

(defvar-local yunge-reader-pdf--render-results nil
  "Cache mapping page, width, and appearance to native render results.")

(defvar-local yunge-reader-pdf--render-pending nil
  "Map render keys to document and generation pairs in flight.")

(defvar-local yunge-reader-pdf--text-cache nil
  "Page-indexed cache of canonical PDF text geometry.")

(defvar-local yunge-reader-pdf--text-hit-cache nil
  "Page-indexed cache of spatial PDF character indexes.")

(defvar-local yunge-reader-pdf--text-pending nil
  "Page-indexed set of outstanding PDF text requests.")

(defvar-local yunge-reader-pdf--link-cache nil
  "Page-indexed cache of PDF links.")

(defvar-local yunge-reader-pdf--link-pending nil
  "Page-indexed map of callbacks awaiting PDF links.")

(defvar-local yunge-reader-pdf--working-pages nil
  "Pages retained by the current visible PDF working set.")

(defvar-local yunge-reader-pdf--prefetch-queue nil
  "Replaceable PDF prefetch tasks waiting behind the active task.")

(defvar-local yunge-reader-pdf--prefetch-active nil
  "The one PDF prefetch task currently owned by the native helper.")

(defvar-local yunge-reader-pdf--prefetch-running nil
  "Non-nil while the PDF prefetch scheduler is dispatching tasks.")


(defun yunge-reader-pdf--render-appearance (&optional window)
  "Return the native PDF appearance resolved for WINDOW."
  (pcase (if (and (yunge-reader-document-p yunge-reader-document)
                  (stringp
                   (yunge-reader-document-file yunge-reader-document)))
             (yunge-reader-effective-appearance)
           'original)
    ('original '((mode . original)))
    ('follow-emacs
     (let* ((window (or window (yunge-reader-pdf--viewport-window)))
            (frame (window-frame window))
            (foreground
             (yunge-reader--face-color
              'default :foreground frame "#000000"))
            (background
             (yunge-reader--face-color
              'default :background frame "#ffffff")))
       `((mode . follow-emacs)
         (foreground . ,foreground)
         (background . ,background))))))

(defun yunge-reader-pdf--highlight-color (face fallback &optional window)
  "Return FACE background in WINDOW for a themed PDF, or FALLBACK."
  (if (eq (alist-get 'mode
                     (yunge-reader-pdf--render-appearance window))
          'follow-emacs)
      (let* ((window (or window (yunge-reader-pdf--viewport-window)))
             (frame (window-frame window)))
        (yunge-reader--face-color face :background frame fallback))
    fallback))

(defun yunge-reader-pdf--appearance-changed ()
  "Refresh PDF rendering after its effective appearance changes."
  (when (and yunge-reader-pdf-view-mode yunge-reader-document)
    (let ((window (yunge-reader-pdf--viewport-window)))
      (yunge-reader-pdf--update-visible-pages window)
      (yunge-reader-pdf--force-redisplay))))

(defun yunge-reader-pdf--nearest-text-offset (page x y)
  "Return PAGE character offset nearest canonical X and Y, or nil."
  (when-let* ((text-layer
               (and yunge-reader-pdf--text-cache
                    (gethash page yunge-reader-pdf--text-cache))))
    (let (best-offset best-distance)
      (dolist (character (alist-get 'characters text-layer))
        (when (yunge-reader-pdf--selectable-character-p character)
          (when-let* ((distance
                       (yunge-reader-pdf--character-distance
                        x y character)))
            (when (or (null best-distance)
                      (< distance best-distance))
              (setq best-offset (alist-get 'index character)
                    best-distance distance)))))
      best-offset)))

(defun yunge-reader-pdf--cache-key (page width appearance)
  "Return an immutable render key for PAGE, WIDTH, and APPEARANCE."
  (let* ((file (yunge-reader-document-file yunge-reader-document))
         (attributes (file-attributes file 'string)))
    (secure-hash
     'sha256
     (prin1-to-string
      (list
       (file-truename file)
       (file-attribute-size attributes)
       (float-time (file-attribute-modification-time attributes))
       page width appearance
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

(defun yunge-reader-pdf--selection-offsets-for
    (selection page text-layer)
  "Return SELECTION's inclusive offsets for PAGE and TEXT-LAYER."
  (when selection
    (pcase-let ((`(,start . ,end)
                 (yunge-reader-pdf--ordered-range
                  (yunge-reader-selection-start selection)
                  (yunge-reader-selection-end selection))))
      (yunge-reader-pdf--range-offsets
       page text-layer start end))))

(defun yunge-reader-pdf--selection-offsets (page text-layer)
  "Return selected inclusive offsets for PAGE and TEXT-LAYER."
  (yunge-reader-pdf--selection-offsets-for
   yunge-reader-selection page text-layer))

(defun yunge-reader-pdf--search-offsets (page text-layer)
  "Return current search match offsets for PAGE and TEXT-LAYER."
  (when (and yunge-reader-search-highlight-visible
             yunge-reader-search-result)
    (yunge-reader-pdf--range-offsets
     page text-layer
     (yunge-reader-search-result-start yunge-reader-search-result)
     (yunge-reader-search-result-end yunge-reader-search-result))))

(defun yunge-reader-pdf--paint-range
    (svg range text-layer page-info pixel-width pixel-height color opacity)
  "Paint inclusive text RANGE onto SVG with COLOR and OPACITY."
  (let ((page-width (alist-get 'width page-info))
        (page-height (alist-get 'height page-info))
        commands run previous-bounds)
    (cl-labels
        ((flush-run
          ()
          (when run
            (setq commands
                  (yunge-reader-pdf--prepend-highlight-path
                   (yunge-reader-pdf--svg-bounds
                    run page-width page-height
                    pixel-width pixel-height)
                   commands)
                  run nil
                  previous-bounds nil))))
      (dolist (character (alist-get 'characters text-layer))
        (let ((index (alist-get 'index character)))
          (when (and (<= (car range) index)
                     (<= index (cdr range))
                     (not (alist-get 'generated character)))
            (let ((quad (yunge-reader-pdf--quad-points character))
                  (bounds (alist-get 'bounds character)))
              (if (and bounds
                       (or (null quad)
                           (yunge-reader-pdf--axis-aligned-quad-p quad)))
                  (if (and previous-bounds
                           (yunge-reader-pdf--same-highlight-run-p
                            previous-bounds bounds))
                      (setq run
                            (yunge-reader-pdf--merge-highlight-bounds
                             run bounds)
                            previous-bounds bounds)
                    (flush-run)
                    (setq run bounds
                          previous-bounds bounds))
                (flush-run)
                (when quad
                  (setq commands
                        (yunge-reader-pdf--prepend-highlight-path
                         (yunge-reader-pdf--svg-quad
                          quad page-width page-height
                          pixel-width pixel-height)
                         commands))))))))
      (flush-run))
    (when commands
      (svg-path
       svg (nreverse commands)
       :fill-color color
       :fill-opacity opacity))))

(defun yunge-reader-pdf--paint-selection
    (svg page page-info text-layer pixel-width pixel-height &optional color)
  "Paint PAGE selection onto SVG using PAGE-INFO and TEXT-LAYER."
  (when-let* ((range
               (yunge-reader-pdf--selection-offsets page text-layer)))
    (yunge-reader-pdf--paint-range
     svg range text-layer page-info pixel-width pixel-height
     (or color yunge-reader-pdf-selection-color)
     yunge-reader-pdf-selection-opacity)))

(defun yunge-reader-pdf--paint-search
    (svg page page-info text-layer pixel-width pixel-height &optional color)
  "Paint PAGE's current search match onto SVG."
  (when-let* ((range
               (yunge-reader-pdf--search-offsets page text-layer)))
    (yunge-reader-pdf--paint-range
     svg range text-layer page-info pixel-width pixel-height
     (or color yunge-reader-pdf-search-color)
     yunge-reader-pdf-search-opacity)))

(defun yunge-reader-pdf--render-key (page width appearance)
  "Return the render key for PAGE, WIDTH, and APPEARANCE."
  (list page width appearance))

(defun yunge-reader-pdf--render-key-page (key)
  "Return the page stored in render KEY."
  (car key))

(defun yunge-reader-pdf--render-key-width (key)
  "Return the width stored in render KEY."
  (cadr key))

(defun yunge-reader-pdf--render-key-appearance (key)
  "Return the appearance stored in render KEY."
  (caddr key))

(defun yunge-reader-pdf--nearest-render-entry (page width appearance)
  "Return the nearest PAGE render for WIDTH and APPEARANCE.
The returned value has the render key as its car and the native result as its
cdr.  A stale appearance is retained only while the exact one is pending."
  (when (hash-table-p yunge-reader-pdf--render-results)
    (let (nearest nearest-rank)
      (maphash
       (lambda (key result)
         (when (and (listp key)
                    (eql (yunge-reader-pdf--render-key-page key) page)
                    (natnump
                     (yunge-reader-pdf--render-key-width key)))
           (let ((rank
                  (cons
                   (if (equal
                        (yunge-reader-pdf--render-key-appearance key)
                        appearance)
                       0 1)
                   (abs
                    (- (yunge-reader-pdf--render-key-width key)
                       width)))))
             (when (or (null nearest-rank)
                       (< (car rank) (car nearest-rank))
                       (and (= (car rank) (car nearest-rank))
                            (< (cdr rank) (cdr nearest-rank))))
               (setq nearest (cons key result)
                     nearest-rank rank)))))
       yunge-reader-pdf--render-results)
      nearest)))

(defun yunge-reader-pdf--display-image-object
    (page width appearance &optional entry window)
  "Return an Emacs image object for PAGE displayed at WIDTH.
Use the nearest cached render ENTRY while an exact render is unavailable."
  (let* ((entry
          (or entry
              (yunge-reader-pdf--nearest-render-entry
               page width appearance)))
         (render-key (car-safe entry))
         (result (cdr-safe entry))
         (path (alist-get 'path result))
         (page-info (yunge-reader-pdf--page-info page))
         (fallback
          (and
           (listp render-key)
           (/= (yunge-reader-pdf--render-key-width render-key) width)))
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
            (svg-embed-base-uri-image
             svg (file-name-nondirectory path)
             :x 0 :y 0
             :width pixel-width
             :height pixel-height)
            (yunge-reader-pdf--paint-selection
             svg page page-info text-layer pixel-width pixel-height
             (yunge-reader-pdf--highlight-color
              'region yunge-reader-pdf-selection-color window))
            (yunge-reader-pdf--paint-search
             svg page page-info text-layer pixel-width pixel-height
             (yunge-reader-pdf--highlight-color
              'isearch yunge-reader-pdf-search-color window))
            (svg-image svg :base-uri path))
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
  (when (and yunge-reader-search-highlight-visible
             yunge-reader-search-result)
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
  (when (and yunge-reader-search-highlight-visible
             yunge-reader-search-result)
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
  (if (not (and yunge-reader-search-highlight-visible
                yunge-reader-search-result))
      (when yunge-reader-pdf--displayed-pages
        (yunge-reader-pdf--paint-pages
         yunge-reader-pdf--displayed-pages)
        (yunge-reader-pdf--force-redisplay))
    (let ((page
           (yunge-reader-position-unit
            (yunge-reader-search-result-start
             yunge-reader-search-result)))
          (yunge-reader-pdf--programmatic-scroll t))
      (when (and (natnump page)
                 (< page (yunge-reader-pdf--page-count)))
        (yunge-reader-pdf--set-page page)
        (yunge-reader-pdf--request-text page)
        (yunge-reader-pdf--paint-pages
         yunge-reader-pdf--displayed-pages)
        (yunge-reader-pdf--scroll-to-search-result)
        (yunge-reader-pdf--force-redisplay)))))

(defun yunge-reader-pdf--paint-page
    (page &optional width appearance window)
  "Paint PAGE at WIDTH and APPEARANCE for WINDOW."
  (when-let* ((position (yunge-reader-pdf--page-position page)))
    (let* ((width
            (or width
                (yunge-reader-pdf--display-width page)
                (yunge-reader-pdf--page-width page)))
           (appearance
            (or appearance
                (yunge-reader-pdf--render-appearance window)))
           (entry
            (yunge-reader-pdf--nearest-render-entry
             page width appearance))
           (display
            (if (and entry
                     (memq page yunge-reader-pdf--displayed-pages))
                (condition-case image-error
                    (or (yunge-reader-pdf--display-image-object
                         page width appearance entry window)
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
  (let ((former yunge-reader-pdf--displayed-pages)
        (appearance (yunge-reader-pdf--render-appearance window)))
    (setq yunge-reader-pdf--displayed-pages pages)
    (dolist (page (cl-remove-duplicates (append former pages)))
      (yunge-reader-pdf--paint-page
       page
       (and window (yunge-reader-pdf--page-width page window))
       appearance window))))

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
        'pointer 'text))
      (unless (= page (1- count))
        (insert (propertize "\n" 'yunge-reader-pdf-page page))))
    (setq yunge-reader-pdf--page-positions positions)
    (set-buffer-modified-p nil)))

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
              (yunge-reader-pdf--prefetch-task-width right))
       (equal (yunge-reader-pdf--prefetch-task-appearance left)
              (yunge-reader-pdf--prefetch-task-appearance right))))

(defun yunge-reader-pdf--retain-page-p (page)
  "Return whether PAGE belongs to the current in-memory working set."
  (or (null yunge-reader-pdf--working-pages)
      (memq page yunge-reader-pdf--working-pages)))

(defun yunge-reader-pdf--retain-render-p (page width appearance)
  "Return whether PAGE, WIDTH, and APPEARANCE are still current."
  (or (null yunge-reader-pdf--working-pages)
      (and (memq page yunge-reader-pdf--working-pages)
           (yunge-reader-pdf--page-info page)
           (= width
              (or (yunge-reader-pdf--display-width page)
                  (yunge-reader-pdf--page-width page)))
           (equal appearance
                  (yunge-reader-pdf--render-appearance)))))

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
              (width (yunge-reader-pdf--prefetch-task-width task))
              (appearance
               (yunge-reader-pdf--prefetch-task-appearance task)))
          (puthash (yunge-reader-pdf--render-key
                    page width appearance)
                   t render-keys)
          (puthash page (cons width appearance) render-widths))))
    ;; Keep one old render only until each working page has its exact target.
    (maphash
     (lambda (page target)
       (let ((width (car target))
             (appearance (cdr target)))
         (unless (gethash (yunge-reader-pdf--render-key
                           page width appearance)
                          yunge-reader-pdf--render-results)
           (when-let* ((entry
                        (yunge-reader-pdf--nearest-render-entry
                         page width appearance)))
             (puthash (car entry) t render-keys)))))
     render-widths)
    (yunge-reader-pdf--prune-cache
     yunge-reader-pdf--render-results
     (lambda (key) (gethash key render-keys)))
    (yunge-reader-pdf--prune-cache
     yunge-reader-pdf--text-cache
     (lambda (page) (memq page pages)))
    (yunge-reader-pdf--prune-cache
     yunge-reader-pdf--text-hit-cache
     (lambda (page) (memq page pages)))
    (yunge-reader-pdf--prune-cache
     yunge-reader-pdf--link-cache
     (lambda (page) (memq page pages)))))

(defun yunge-reader-pdf--prune-page-renders (page width appearance)
  "Retain PAGE's WIDTH and APPEARANCE render."
  (yunge-reader-pdf--prune-cache
   yunge-reader-pdf--render-results
   (lambda (key)
     (or (/= (yunge-reader-pdf--render-key-page key) page)
         (and (= (yunge-reader-pdf--render-key-width key) width)
              (equal
               (yunge-reader-pdf--render-key-appearance key)
               appearance))))))

(defun yunge-reader-pdf--prefetch-task-needed-p (task)
  "Return whether TASK is still useful to the current PDF view."
  (let ((document
         (yunge-reader-pdf--prefetch-task-document task))
        (kind (yunge-reader-pdf--prefetch-task-kind task))
        (page (yunge-reader-pdf--prefetch-task-page task))
        (width (yunge-reader-pdf--prefetch-task-width task))
        (appearance
         (yunge-reader-pdf--prefetch-task-appearance task)))
    (and yunge-reader-pdf-view-mode
         (eq document yunge-reader-document)
         (memq page yunge-reader-pdf--working-pages)
         (pcase kind
           ('render
            (not
             (gethash (yunge-reader-pdf--render-key
                       page width appearance)
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
      (yunge-reader-pdf--prefetch-task-width task)
      (yunge-reader-pdf--prefetch-task-appearance task)))
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
    (document kind page &optional width appearance error-data)
  "Finish DOCUMENT prefetch for KIND, PAGE, WIDTH, and APPEARANCE."
  (let ((task yunge-reader-pdf--prefetch-active))
    (when (and task
               (eq document
                   (yunge-reader-pdf--prefetch-task-document task))
               (eq kind (yunge-reader-pdf--prefetch-task-kind task))
               (eql page (yunge-reader-pdf--prefetch-task-page task))
               (or (not (eq kind 'render))
                   (and
                    (equal
                     width
                     (yunge-reader-pdf--prefetch-task-width task))
                    (equal
                     appearance
                     (yunge-reader-pdf--prefetch-task-appearance task)))))
      (setq yunge-reader-pdf--prefetch-active nil)
      (if (yunge-reader-pdf--stopped-error-p error-data)
          (setq yunge-reader-pdf--prefetch-queue nil)
        (yunge-reader-pdf--run-prefetch)))))

(defun yunge-reader-pdf--prefetch-tasks (pages &optional window)
  "Return image-first background tasks for PDF PAGES in WINDOW."
  (let ((document yunge-reader-document)
        (appearance (yunge-reader-pdf--render-appearance window))
        (generation yunge-reader-pdf--generation))
    (append
     (mapcar
      (lambda (page)
        (make-yunge-reader-pdf--prefetch-task
         :document document
         :kind 'render
         :page page
         :width (yunge-reader-pdf--page-width page window)
         :appearance appearance
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
    (buffer document generation page width appearance result error-data)
  "Store one PAGE render for GENERATION, WIDTH, and APPEARANCE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unwind-protect
          (when (eq document yunge-reader-document)
            (let* ((render-key
                    (yunge-reader-pdf--render-key
                     page width appearance))
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
                           (yunge-reader-pdf--retain-render-p
                            page width appearance))
                  (display-warning
                   'yunge-reader
                   (format "Could not render PDF page %d: %s"
                           (1+ page)
                           (error-message-string error-data))
                   :warning))
              (when (and
                     (hash-table-p yunge-reader-pdf--render-results)
                     (yunge-reader-pdf--retain-render-p
                      page width appearance))
                (puthash (yunge-reader-pdf--render-key
                          page width appearance)
                         result yunge-reader-pdf--render-results)
                (yunge-reader-pdf--prune-page-renders
                 page width appearance))
              (when (and yunge-reader-pdf-view-mode
                         (yunge-reader-pdf--retain-render-p
                          page width appearance)
                         (memq page yunge-reader-pdf--displayed-pages))
                (yunge-reader-pdf--paint-page
                 page width appearance)
                (when (yunge-reader-pdf--search-page-p page)
                  (yunge-reader-pdf--scroll-to-search-result)))))
        (yunge-reader-pdf--finish-prefetch
         document 'render page width appearance error-data)))))

(defun yunge-reader-pdf--request-render
    (generation page &optional width appearance)
  "Request PAGE for GENERATION, WIDTH, and APPEARANCE if uncached."
  (let* ((width (or width (yunge-reader-pdf--page-width page)))
         (appearance
          (or appearance (yunge-reader-pdf--render-appearance)))
         (render-key
          (yunge-reader-pdf--render-key page width appearance))
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
            (yunge-reader-pdf--request
             document 'render-page
             (list :page page
                   :width width
                   :appearance appearance
                   :cache-key
                   (yunge-reader-pdf--cache-key
                    page width appearance))
             (lambda (result request-error)
               (yunge-reader-pdf--render-complete
                buffer document generation page width appearance
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
                (puthash page result yunge-reader-pdf--text-cache)
                (when (hash-table-p yunge-reader-pdf--text-hit-cache)
                  (puthash
                   page
                   (yunge-reader-pdf--make-hit-index page result)
                   yunge-reader-pdf--text-hit-cache)))
              (let ((repainted
                     (when (and
                            (memq page
                                  yunge-reader-pdf--displayed-pages)
                            (or yunge-reader-selection
                                (yunge-reader-pdf--search-page-p page)))
                       (yunge-reader-pdf--paint-page page)
                       t)))
                (when (yunge-reader-pdf--search-page-p page)
                  (yunge-reader-pdf--scroll-to-search-result))
                (when repainted
                  (yunge-reader-pdf--force-redisplay)))))
        (yunge-reader-pdf--finish-prefetch
         document 'text page nil nil error-data)))))

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
          (yunge-reader-pdf--request
           document 'page-text (list :page page)
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
         document 'links page nil nil error-data)))))

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
              (yunge-reader-pdf--request
               document 'page-links (list :page page)
               (lambda (result request-error)
                 (yunge-reader-pdf--link-complete
                  buffer document page result request-error)))
            (error
             (remhash page yunge-reader-pdf--link-pending)
             (signal (car error-data) (cdr error-data))))
          'started)))))


(provide 'yunge-reader-pdf-render)

;;; yunge-reader-pdf-render.el ends here
