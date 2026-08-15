;;; yunge-reader-pdf.el --- PDF reader -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'svg)
(require 'subr-x)
(require 'yunge-reader)
(require 'yunge-reader-native)

(defcustom yunge-reader-pdf-page-margin 24
  "Pixel margin reserved around a rendered PDF page."
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

(defconst yunge-reader-pdf--points-to-pixels (/ 96.0 72.0)
  "Nominal conversion from PDF points to screen pixels at scale one.")

(defvar-local yunge-reader-pdf-page 0
  "Zero-based page currently displayed in the PDF adapter.")

(defvar-local yunge-reader-pdf--generation 0
  "Generation used to reject late PDF rendering completions.")

(defvar-local yunge-reader-pdf--page-info nil
  "Native geometry for the page currently being rendered.")

(defvar-local yunge-reader-pdf--render-result nil
  "Native image result for the currently displayed PDF page.")

(defvar-local yunge-reader-pdf--text-cache nil
  "Page-indexed cache of canonical PDF text geometry.")

(defvar-local yunge-reader-pdf--text-pending nil
  "Page-indexed set of outstanding PDF text requests.")

(defvar-keymap yunge-reader-pdf--image-map
  "<mouse-1>" #'yunge-reader-pdf-select-at-mouse
  "<drag-mouse-1>" #'yunge-reader-pdf-select-with-mouse)

(defvar-keymap yunge-reader-pdf-view-mode-map
  "n" #'yunge-reader-pdf-next-page
  "]" #'yunge-reader-pdf-next-page
  "<next>" #'yunge-reader-pdf-next-page
  "b" #'yunge-reader-pdf-previous-page
  "[" #'yunge-reader-pdf-previous-page
  "<prior>" #'yunge-reader-pdf-previous-page
  "G" #'yunge-reader-pdf-goto-page)

(define-minor-mode yunge-reader-pdf-view-mode
  "Display a fixed-layout PDF through the Yunge Reader PDF driver."
  :init-value nil
  :lighter " PDF"
  :keymap yunge-reader-pdf-view-mode-map
  (if yunge-reader-pdf-view-mode
      (progn
        (setq-local yunge-reader-pdf-page 0)
        (setq-local yunge-reader-pdf--text-cache
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--text-pending
                    (make-hash-table :test #'eql))
        (add-hook 'yunge-reader-refresh-hook
                  #'yunge-reader-pdf--refresh nil t)
        (add-hook 'window-size-change-functions
                  #'yunge-reader-pdf--window-size-change nil t))
    (remove-hook 'yunge-reader-refresh-hook
                 #'yunge-reader-pdf--refresh t)
    (remove-hook 'window-size-change-functions
                 #'yunge-reader-pdf--window-size-change t)
    (setq yunge-reader-pdf--render-result nil
          yunge-reader-pdf--text-cache nil
          yunge-reader-pdf--text-pending nil)))

(defun yunge-reader-pdf--match-p (file)
  "Return whether FILE has a PDF extension."
  (string-equal (downcase (or (file-name-extension file) "")) "pdf"))

(defun yunge-reader-pdf--native-error (message)
  "Return an Emacs error value containing MESSAGE."
  (list 'error message))

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
                       (alist-get 'page-count result)))
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
      ('selection-text
       (let* ((start (plist-get arguments :start))
              (end (plist-get arguments :end))
              (start-page (yunge-reader-position-unit start))
              (end-page (yunge-reader-position-unit end)))
         (if (not (and (integerp start-page)
                       (integerp end-page)
                       (= start-page end-page)
                       (integerp (yunge-reader-position-offset start))
                       (integerp (yunge-reader-position-offset end))))
             (funcall
              complete nil
              (yunge-reader-pdf--native-error
               "PDF selection endpoints must be on one page"))
           (yunge-reader-native-request
            "selection-text"
            (list
             (cons 'document handle)
             (cons 'page start-page)
             (cons 'start
                   (yunge-reader-position-offset start))
             (cons 'end
                   (yunge-reader-position-offset end)))
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

(defun yunge-reader-pdf--viewport-window ()
  "Return a live window suitable for measuring the current PDF view."
  (or (get-buffer-window (current-buffer) t)
      (selected-window)))

(defun yunge-reader-pdf--target-width (page-info &optional window)
  "Return target pixel width for PAGE-INFO in WINDOW."
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
    (yunge-reader-set-effective-scale
     (/ width
        (* page-width yunge-reader-pdf--points-to-pixels)))
    width))

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

(defun yunge-reader-pdf--selection-offsets ()
  "Return ordered selected offsets on the current page, or nil."
  (when yunge-reader-selection
    (let* ((start
            (yunge-reader-selection-start yunge-reader-selection))
           (end (yunge-reader-selection-end yunge-reader-selection))
           (start-page (yunge-reader-position-unit start))
           (end-page (yunge-reader-position-unit end))
           (start-offset (yunge-reader-position-offset start))
           (end-offset (yunge-reader-position-offset end)))
      (when (and (eql start-page yunge-reader-pdf-page)
                 (eql end-page yunge-reader-pdf-page)
                 (integerp start-offset)
                 (integerp end-offset))
        (cons (min start-offset end-offset)
              (max start-offset end-offset))))))

(defun yunge-reader-pdf--paint-selection
    (svg text-layer pixel-width pixel-height)
  "Paint the current selection onto SVG using TEXT-LAYER geometry."
  (when-let* ((range (yunge-reader-pdf--selection-offsets)))
    (let ((page-width (alist-get 'width yunge-reader-pdf--page-info))
          (page-height (alist-get 'height yunge-reader-pdf--page-info)))
      (dolist (character (alist-get 'characters text-layer))
        (let ((index (alist-get 'index character))
              (bounds (alist-get 'bounds character)))
          (when (and bounds
                     (<= (car range) index)
                     (<= index (cdr range)))
            (let* ((left (alist-get 'left bounds))
                   (bottom (alist-get 'bottom bounds))
                   (right (alist-get 'right bounds))
                   (top (alist-get 'top bounds))
                   (x (* pixel-width (/ left page-width)))
                   (y (* pixel-height
                         (/ (- page-height top) page-height)))
                   (width (max 1 (* pixel-width
                                    (/ (- right left) page-width))))
                   (height (max 1 (* pixel-height
                                     (/ (- top bottom) page-height)))))
              (svg-rectangle
               svg x y width height
               :fill-color yunge-reader-pdf-selection-color
               :fill-opacity
               yunge-reader-pdf-selection-opacity))))))))

(defun yunge-reader-pdf--display-image-object ()
  "Return an Emacs image object for the current PDF view."
  (let* ((result yunge-reader-pdf--render-result)
         (path (alist-get 'path result))
         (pixel-width (alist-get 'pixel-width result))
         (pixel-height (alist-get 'pixel-height result))
         (text-layer
          (and yunge-reader-pdf--text-cache
               (gethash yunge-reader-pdf-page
                        yunge-reader-pdf--text-cache))))
    (if (and text-layer (yunge-reader-pdf--selection-offsets))
        (let ((svg (svg-create pixel-width pixel-height)))
          (svg-embed svg path "image/png" nil
                     :x 0 :y 0
                     :width pixel-width
                     :height pixel-height)
          (yunge-reader-pdf--paint-selection
           svg text-layer pixel-width pixel-height)
          (svg-image svg))
      (create-image path nil nil))))

(defun yunge-reader-pdf--paint-image ()
  "Paint the current PDF image and logical selection."
  (when yunge-reader-pdf--render-result
    (condition-case image-error
        (let ((image (yunge-reader-pdf--display-image-object)))
          (unless image
            (error "Emacs cannot display the rendered PDF page"))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert
             (propertize
              " "
              'display image
              'keymap yunge-reader-pdf--image-map
              'pointer 'hand
              'help-echo
              "Mouse-1 selects a character; drag selects text"))
            (goto-char (point-min)))
          (setq header-line-format
                (format " Page %d/%d  %.0f%% "
                        (1+ yunge-reader-pdf-page)
                        (yunge-reader-pdf--page-count)
                        (* 100 yunge-reader-effective-scale))))
      (error
       (yunge-reader--display-status
        "Could not display page %d:\n\n%s"
        (1+ yunge-reader-pdf-page)
        (error-message-string image-error))))))

(defun yunge-reader-pdf--display-image
    (generation page result error-data)
  "Display page RESULT for GENERATION and PAGE, or show ERROR-DATA."
  (when (= generation yunge-reader-pdf--generation)
    (if error-data
        (yunge-reader--display-status
         "Could not render page %d:\n\n%s"
         (1+ page) (error-message-string error-data))
      (setq yunge-reader-pdf--render-result result)
      (yunge-reader-pdf--paint-image))))

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
        (when (and (= page yunge-reader-pdf-page)
                   yunge-reader-selection)
          (yunge-reader-pdf--paint-image))))))

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

(defun yunge-reader-pdf--render-with-info
    (generation page page-info error-data)
  "Render PAGE for GENERATION using PAGE-INFO, or show ERROR-DATA."
  (when (= generation yunge-reader-pdf--generation)
    (if error-data
        (yunge-reader--display-status
         "Could not inspect page %d:\n\n%s"
         (1+ page) (error-message-string error-data))
      (setq yunge-reader-pdf--page-info page-info)
      (let ((width (yunge-reader-pdf--target-width page-info))
            (buffer (current-buffer)))
        (yunge-reader-request
         'render-page
         (list :page page
               :width width
               :cache-key
               (yunge-reader-pdf--cache-key page width))
         (lambda (result render-error)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (yunge-reader-pdf--display-image
                generation page result render-error))))))
      (yunge-reader-pdf--request-text page))))

(defun yunge-reader-pdf--refresh ()
  "Request geometry and a rendered image for the current PDF page."
  (when (and yunge-reader-pdf-view-mode yunge-reader-document)
    (cl-incf yunge-reader-pdf--generation)
    (let ((buffer (current-buffer))
          (generation yunge-reader-pdf--generation)
          (page yunge-reader-pdf-page))
      (yunge-reader-request
       'page-info (list :page page)
       (lambda (page-info error-data)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (yunge-reader-pdf--render-with-info
              generation page page-info error-data))))))))

(defun yunge-reader-pdf--pixel-to-page-point
    (x y display-width display-height)
  "Convert display pixel X and Y to canonical PDF page coordinates."
  (let ((page-width (alist-get 'width yunge-reader-pdf--page-info))
        (page-height (alist-get 'height yunge-reader-pdf--page-info)))
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

(defun yunge-reader-pdf--hit-character (point text-layer)
  "Return the TEXT-LAYER character nearest canonical POINT."
  (let* ((x (car point))
         (y (cdr point))
         (page-width (alist-get 'width yunge-reader-pdf--page-info))
         (page-height (alist-get 'height yunge-reader-pdf--page-info))
         (tolerance
          (max 12.0 (* 0.03 (min page-width page-height))))
         best
         best-distance)
    (dolist (character (alist-get 'characters text-layer))
      (when-let* (((not (string-empty-p
                         (or (alist-get 'text character) ""))))
                  (bounds (alist-get 'bounds character)))
        (let ((distance
               (yunge-reader-pdf--bounds-distance x y bounds)))
          (when (or (null best-distance)
                    (< distance best-distance))
            (setq best character
                  best-distance distance)))))
    (when (and best-distance
               (<= best-distance (* tolerance tolerance)))
      best)))

(defun yunge-reader-pdf--event-page-points (event single)
  "Return start and end page points for mouse EVENT.
When SINGLE is non-nil, use the event start for both points."
  (let* ((start (event-start event))
         (end (if single start (or (event-end event) start)))
         (window (posn-window start))
         (object-point (posn-object-x-y start))
         (object-size (posn-object-width-height start))
         (start-window-point (posn-x-y start))
         (end-window-point (posn-x-y end)))
    (unless (and (windowp window)
                 (eq window (posn-window end))
                 object-point object-size
                 start-window-point end-window-point)
      (user-error "Keep the PDF selection inside one page image"))
    (let ((origin-x (- (car start-window-point)
                       (car object-point)))
          (origin-y (- (cdr start-window-point)
                       (cdr object-point))))
      (list
       (yunge-reader-pdf--pixel-to-page-point
        (car object-point) (cdr object-point)
        (car object-size) (cdr object-size))
       (yunge-reader-pdf--pixel-to-page-point
        (- (car end-window-point) origin-x)
        (- (cdr end-window-point) origin-y)
        (car object-size) (cdr object-size))))))

(defun yunge-reader-pdf--select-points (start-point end-point)
  "Select PDF characters nearest START-POINT and END-POINT."
  (let ((text-layer
         (and yunge-reader-pdf--text-cache
              (gethash yunge-reader-pdf-page
                       yunge-reader-pdf--text-cache))))
    (unless text-layer
      (user-error "PDF text geometry is still loading"))
    (let ((start-character
           (yunge-reader-pdf--hit-character start-point text-layer))
          (end-character
           (yunge-reader-pdf--hit-character end-point text-layer)))
      (unless (and start-character end-character)
        (setq yunge-reader-selection nil)
        (yunge-reader-pdf--paint-image)
        (user-error "No selectable PDF text near the pointer"))
      (let ((start-index (alist-get 'index start-character))
            (end-index (alist-get 'index end-character)))
        (yunge-reader-set-selection
         (make-yunge-reader-position
          :unit yunge-reader-pdf-page
          :offset start-index
          :x (car start-point)
          :y (cdr start-point))
         (make-yunge-reader-position
          :unit yunge-reader-pdf-page
          :offset end-index
          :x (car end-point)
          :y (cdr end-point)))
        (yunge-reader-pdf--paint-image)
        (message "Selected %d PDF character%s"
                 (1+ (abs (- start-index end-index)))
                 (if (= start-index end-index) "" "s"))))))

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

(defun yunge-reader-pdf--set-page (page)
  "Display zero-based PDF PAGE."
  (let ((count (yunge-reader-pdf--page-count)))
    (unless (> count 0)
      (user-error "This PDF has no pages"))
    (setq yunge-reader-pdf-page
          (max 0 (min (1- count) page)))
    (setq yunge-reader-selection nil)
    (yunge-reader-refresh)
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
