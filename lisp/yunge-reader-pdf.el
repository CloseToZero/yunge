;;; yunge-reader-pdf.el --- PDF reader -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
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

(defconst yunge-reader-pdf--points-to-pixels (/ 96.0 72.0)
  "Nominal conversion from PDF points to screen pixels at scale one.")

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

(defvar-local yunge-reader-pdf--text-cache nil
  "Page-indexed cache of canonical PDF text geometry.")

(defvar-local yunge-reader-pdf--text-pending nil
  "Page-indexed set of outstanding PDF text requests.")

(defvar-keymap yunge-reader-pdf--image-map
  "<mouse-1>" #'yunge-reader-pdf-select-at-mouse
  "<drag-mouse-1>" #'yunge-reader-pdf-select-with-mouse)

(defconst yunge-reader-pdf-normal-bindings
  '(("G" yunge-reader-pdf-last-page "last page")
    ("J" yunge-reader-pdf-next-page "next page")
    ("K" yunge-reader-pdf-previous-page "previous page")
    ("gg" yunge-reader-pdf-first-page "first page")
    ("gp" yunge-reader-pdf-goto-page "go to page")
    ("gr" yunge-reader-refresh "refresh"))
  "Normal-state bindings for the PDF view adapter.")

(defvar-keymap yunge-reader-pdf-view-mode-map
  "G" #'yunge-reader-pdf-last-page
  "J" #'yunge-reader-pdf-next-page
  "K" #'yunge-reader-pdf-previous-page
  "<next>" #'scroll-up-command
  "<prior>" #'scroll-down-command
  "g g" #'yunge-reader-pdf-first-page
  "g p" #'yunge-reader-pdf-goto-page
  "g r" #'yunge-reader-refresh)

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
        (add-hook 'yunge-reader-refresh-hook
                  #'yunge-reader-pdf--refresh nil t)
        (add-hook 'yunge-reader-search-result-hook
                  #'yunge-reader-pdf--search-result-changed nil t)
        (add-hook 'window-size-change-functions
                  #'yunge-reader-pdf--window-size-change nil t)
        (add-hook 'window-scroll-functions
                  #'yunge-reader-pdf--window-scrolled nil t))
    (remove-hook 'yunge-reader-refresh-hook
                 #'yunge-reader-pdf--refresh t)
    (remove-hook 'yunge-reader-search-result-hook
                 #'yunge-reader-pdf--search-result-changed t)
    (remove-hook 'window-size-change-functions
                 #'yunge-reader-pdf--window-size-change t)
    (remove-hook 'window-scroll-functions
                 #'yunge-reader-pdf--window-scrolled t)
    (kill-local-variable 'line-spacing)
    (setq yunge-reader-pdf--page-infos nil
          yunge-reader-pdf--page-positions nil
          yunge-reader-pdf--render-results nil
          yunge-reader-pdf--render-pending nil
          yunge-reader-pdf--displayed-pages nil
          yunge-reader-pdf--text-cache nil
          yunge-reader-pdf--text-pending nil)))

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

(defun yunge-reader-pdf--open (file complete)
  "Open PDF FILE and call COMPLETE using the reader driver contract."
  (let ((buffer (current-buffer))
        acquired)
    (yunge-reader-pdf-view-mode 1)
    (condition-case error-data
        (progn
          (yunge-reader-native-acquire)
          (setq acquired t)
          (yunge-reader-native-request
           "open" (list (cons 'path file))
           (lambda (result native-error)
             (if native-error
                 (progn
                   (when acquired
                     (setq acquired nil)
                     (yunge-reader-native-release))
                   (funcall complete nil nil native-error))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq yunge-reader-pdf-page 0)))
               (funcall
                complete
                (alist-get 'document result)
                (list
                 :layout 'fixed
                 :metadata
                 (list :page-count
                       (alist-get 'page-count result)
                       :pages (alist-get 'pages result)))
                nil)))))
      (error
       (when acquired
         (yunge-reader-native-release))
       (funcall complete nil nil error-data)))))

(defun yunge-reader-pdf--close (document)
  "Close PDF DOCUMENT and release its native service lease."
  (let ((released nil))
    (cl-labels
        ((release ()
           (unless released
             (setq released t)
             (yunge-reader-native-release))))
      (if (not (yunge-reader-native-live-p))
          (release)
        (condition-case error-data
            (yunge-reader-native-request
             "close"
             (list
              (cons 'document
                    (yunge-reader-document-handle document)))
             (lambda (_result _native-error)
               (release)))
          (error
           (release)
           (signal (car error-data) (cdr error-data))))))))

(defun yunge-reader-pdf--request
    (document operation arguments complete)
  "Dispatch PDF DOCUMENT OPERATION with ARGUMENTS to COMPLETE."
  (let ((handle (yunge-reader-document-handle document)))
    (pcase operation
      ('page-info
       (yunge-reader-native-request
        "page-info"
        (list (cons 'document handle)
              (cons 'page (plist-get arguments :page)))
        complete))
      ('page-text
       (yunge-reader-native-request
        "page-text"
        (list (cons 'document handle)
              (cons 'page (plist-get arguments :page)))
        complete))
      ('render-page
       (yunge-reader-native-request
        "render-page"
        (list (cons 'document handle)
              (cons 'page (plist-get arguments :page))
              (cons 'width (plist-get arguments :width))
              (cons 'cache-key
                    (plist-get arguments :cache-key)))
         complete))
      ('search
       (let ((cursor (plist-get arguments :cursor)))
         (yunge-reader-native-request
          "search"
          (append
           (list
            (cons 'document handle)
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
              (start-page (yunge-reader-position-unit start))
              (end-page (yunge-reader-position-unit end))
              (start-offset (yunge-reader-position-offset start))
              (end-offset (yunge-reader-position-offset end)))
         (if (not (and (natnump start-page)
                       (natnump end-page)
                       (natnump start-offset)
                       (natnump end-offset)))
             (funcall
              complete nil
              (yunge-reader-pdf--native-error
               "PDF selection endpoints must be indexed positions"))
           (yunge-reader-native-request
            "selection-text"
            (list
             (cons 'document handle)
             (cons 'start
                   (list (cons 'page start-page)
                         (cons 'offset start-offset)))
             (cons 'end
                   (list (cons 'page end-page)
                         (cons 'offset end-offset))))
            (lambda (result native-error)
              (funcall complete
                       (and result (alist-get 'text result))
                       native-error))))))
      (_
       (funcall
        complete nil
        (yunge-reader-pdf--native-error
         (format "Unsupported PDF operation: %S" operation)))))))

(defun yunge-reader-pdf-register ()
  "Register the PDF driver without changing `auto-mode-alist'."
  (yunge-reader-register-driver
   'pdf
   :match #'yunge-reader-pdf--match-p
   :open #'yunge-reader-pdf--open
   :close #'yunge-reader-pdf--close
   :request #'yunge-reader-pdf--request))

;;;###autoload
(defun yunge-reader-pdf-open (file)
  "Open PDF FILE explicitly with Yunge Reader.
This command does not take ownership of ordinary `.pdf' file visits."
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

(defun yunge-reader-pdf--display-image-object (page width)
  "Return an Emacs image object for PAGE rendered at WIDTH."
  (let* ((result
          (gethash (yunge-reader-pdf--render-key page width)
                   yunge-reader-pdf--render-results))
         (path (alist-get 'path result))
         (pixel-width (alist-get 'pixel-width result))
         (pixel-height (alist-get 'pixel-height result))
         (page-info (yunge-reader-pdf--page-info page))
         (text-layer
          (and yunge-reader-pdf--text-cache
               (gethash page yunge-reader-pdf--text-cache))))
    (if (and text-layer
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
      (create-image path nil nil))))

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

(defun yunge-reader-pdf--paint-page (page)
  "Paint PAGE as an image when visible, otherwise as a placeholder."
  (when-let* ((position (yunge-reader-pdf--page-position page)))
    (let* ((width (yunge-reader-pdf--page-width page))
           (result
            (gethash (yunge-reader-pdf--render-key page width)
                     yunge-reader-pdf--render-results))
           (display
            (if (and result
                     (memq page yunge-reader-pdf--displayed-pages))
                (condition-case image-error
                    (or (yunge-reader-pdf--display-image-object page width)
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
      (put-text-property position (1+ position) 'display display))))

(defun yunge-reader-pdf--paint-pages (pages)
  "Paint exactly PAGES as live images and virtualize all former pages."
  (let ((former yunge-reader-pdf--displayed-pages))
    (setq yunge-reader-pdf--displayed-pages pages)
    (dolist (page (cl-remove-duplicates (append former pages)))
      (yunge-reader-pdf--paint-page page))))

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
        'pointer 'hand
        'help-echo
        "Mouse-1 selects a character; drag selects across pages"))
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

(defun yunge-reader-pdf--update-header ()
  "Update the continuous PDF roll header."
  (setq header-line-format
        (format " Page %d/%d  %.0f%%  Continuous "
                (1+ yunge-reader-pdf-page)
                (yunge-reader-pdf--page-count)
                (* 100 yunge-reader-effective-scale))))

(defun yunge-reader-pdf--render-complete
    (buffer generation page width result error-data)
  "Store one rendered PAGE result in BUFFER for GENERATION and WIDTH."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((render-key (yunge-reader-pdf--render-key page width)))
        (when (and (hash-table-p yunge-reader-pdf--render-pending)
                   (eql (gethash render-key
                                 yunge-reader-pdf--render-pending)
                        generation))
          (remhash render-key yunge-reader-pdf--render-pending)))
      (if error-data
          (when (and yunge-reader-pdf-view-mode
                     (yunge-reader-pdf--page-info page)
                     (= width (yunge-reader-pdf--page-width page)))
            (display-warning
             'yunge-reader
             (format "Could not render PDF page %d: %s"
                     (1+ page) (error-message-string error-data))
             :warning))
        (when (hash-table-p yunge-reader-pdf--render-results)
          (puthash (yunge-reader-pdf--render-key page width)
                   result yunge-reader-pdf--render-results))
        (when (and yunge-reader-pdf-view-mode
                   (yunge-reader-pdf--page-info page)
                   (memq page yunge-reader-pdf--displayed-pages)
                   (= width (yunge-reader-pdf--page-width page)))
          (yunge-reader-pdf--paint-page page)
          (when (yunge-reader-pdf--search-page-p page)
            (yunge-reader-pdf--scroll-to-search-result)))))))

(defun yunge-reader-pdf--request-render (generation page)
  "Request a render of PAGE for GENERATION unless it is cached."
  (let* ((width (yunge-reader-pdf--page-width page))
         (render-key (yunge-reader-pdf--render-key page width)))
    (unless (or (gethash render-key yunge-reader-pdf--render-results)
                (gethash render-key yunge-reader-pdf--render-pending))
      (let ((buffer (current-buffer)))
        (puthash render-key generation yunge-reader-pdf--render-pending)
        (yunge-reader-request
         'render-page
         (list :page page
               :width width
               :cache-key (yunge-reader-pdf--cache-key page width))
         (lambda (result error-data)
           (yunge-reader-pdf--render-complete
            buffer generation page width result error-data)))))))

(defun yunge-reader-pdf--text-complete
    (buffer page result error-data)
  "Store PAGE text RESULT in BUFFER, or report ERROR-DATA."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (hash-table-p yunge-reader-pdf--text-pending)
        (remhash page yunge-reader-pdf--text-pending))
      (if error-data
          (display-warning
           'yunge-reader
           (format "Could not load PDF page text: %s"
                   (error-message-string error-data))
           :warning)
        (when (hash-table-p yunge-reader-pdf--text-cache)
          (puthash page result yunge-reader-pdf--text-cache))
        (when (and (memq page yunge-reader-pdf--displayed-pages)
                   (or yunge-reader-selection
                       (yunge-reader-pdf--search-page-p page)))
          (yunge-reader-pdf--paint-page page))
        (when (yunge-reader-pdf--search-page-p page)
          (yunge-reader-pdf--scroll-to-search-result))))))

(defun yunge-reader-pdf--request-text (page)
  "Request and cache canonical text geometry for PAGE."
  (unless (or (gethash page yunge-reader-pdf--text-cache)
              (gethash page yunge-reader-pdf--text-pending))
    (let ((buffer (current-buffer)))
      (puthash page t yunge-reader-pdf--text-pending)
      (yunge-reader-request
       'page-text (list :page page)
       (lambda (result error-data)
         (yunge-reader-pdf--text-complete
          buffer page result error-data))))))

(defun yunge-reader-pdf--queue-pages (pages)
  "Queue render and text work for PAGES in image-first order."
  (let ((generation yunge-reader-pdf--generation))
    (dolist (page pages)
      (yunge-reader-pdf--request-render generation page))
    (dolist (page pages)
      (yunge-reader-pdf--request-text page))))

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
    (let ((yunge-reader-pdf--updating-visible t))
      (yunge-reader-pdf--sync-current-page window)
      (yunge-reader-pdf--target-width
       (yunge-reader-pdf--page-info yunge-reader-pdf-page))
      (let ((visible (yunge-reader-pdf--visible-pages)))
        (yunge-reader-pdf--paint-pages visible)
        (yunge-reader-pdf--queue-pages
         (yunge-reader-pdf--prefetch-range visible)))
      (yunge-reader-pdf--update-header))))

(defun yunge-reader-pdf--refresh ()
  "Refresh the continuous PDF roll at the current zoom."
  (when (and yunge-reader-pdf-view-mode yunge-reader-document)
    (cl-incf yunge-reader-pdf--generation)
    (unless yunge-reader-pdf--page-infos
      (yunge-reader-pdf--load-page-infos))
    (if (zerop (yunge-reader-pdf--page-count))
        (yunge-reader--display-status "This PDF contains no pages")
      (unless yunge-reader-pdf--page-positions
        (yunge-reader-pdf--build-roll))
      (setq yunge-reader-pdf--displayed-pages nil)
      (dotimes (page (yunge-reader-pdf--page-count))
        (yunge-reader-pdf--paint-page page))
      (yunge-reader-pdf--update-visible-pages)
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

(defun yunge-reader-pdf-select-at-mouse (event)
  "Select the PDF character at mouse EVENT."
  (interactive "e")
  (yunge-reader-pdf--select-mouse-event event t))

(defun yunge-reader-pdf-select-with-mouse (event)
  "Select the PDF character range described by drag EVENT."
  (interactive "e")
  (yunge-reader-pdf--select-mouse-event event nil))

(defun yunge-reader-pdf--window-size-change (_window)
  "Refresh fit-mode PDF rendering after the view window changes size."
  (when (and yunge-reader-document
             (memq yunge-reader-zoom-mode '(fit-width fit-page)))
    (yunge-reader-refresh)))

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
    (setq yunge-reader-pdf-page
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

(provide 'yunge-reader-pdf)

;;; yunge-reader-pdf.el ends here
