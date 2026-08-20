;;; yunge-reader-pdf-interaction.el --- PDF interaction -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'yunge-reader)
(require 'yunge-reader-pdf-geometry)
(require 'yunge-reader-pdf-protocol)
(require 'yunge-reader-pdf-viewport)
(require 'yunge-reader-pdf-render)

(defcustom yunge-reader-pdf-selection-drag-threshold 3
  "Pointer travel in pixels before a press starts PDF selection."
  :type 'natnum
  :group 'yunge-reader)

(defvar-local yunge-reader-pdf--link-activation-generation 0
  "Generation used to reject late interactive PDF link completions.")

(defun yunge-reader-pdf--hit-tolerance (page)
  "Return canonical character hit tolerance for PAGE."
  (let* ((page-info (yunge-reader-pdf--page-info page))
         (page-width (alist-get 'width page-info))
         (page-height (alist-get 'height page-info)))
    (if (and (numberp page-width) (numberp page-height))
        (max 12.0 (* 0.03 (min page-width page-height)))
      12.0)))

(defun yunge-reader-pdf--make-hit-index (page text-layer)
  "Build a spatial character hit index for PAGE's TEXT-LAYER."
  (let* ((tolerance (yunge-reader-pdf--hit-tolerance page))
         (cell-size (* 2.0 tolerance))
         (cells (make-hash-table :test #'equal)))
    (dolist (character (alist-get 'characters text-layer))
      (when (yunge-reader-pdf--selectable-character-p character)
        (when-let* ((bounds
                     (yunge-reader-pdf--character-index-bounds
                      character)))
          (let ((left
                 (floor (/ (- (nth 0 bounds) tolerance)
                           cell-size)))
                (bottom
                 (floor (/ (- (nth 1 bounds) tolerance)
                           cell-size)))
                (right
                 (floor (/ (+ (nth 2 bounds) tolerance)
                           cell-size)))
                (top
                 (floor (/ (+ (nth 3 bounds) tolerance)
                           cell-size))))
            (cl-loop for row from bottom to top do
                     (cl-loop for column from left to right do
                              (push character
                                    (gethash (cons column row)
                                             cells))))))))
    (maphash
     (lambda (key characters)
       (puthash key (nreverse characters) cells))
     cells)
    (make-yunge-reader-pdf--hit-index
     :source text-layer
     :tolerance tolerance
     :cell-size cell-size
     :cells cells)))

(defun yunge-reader-pdf--page-hit-index (page text-layer)
  "Return a cached spatial hit index for PAGE's TEXT-LAYER."
  (unless (hash-table-p yunge-reader-pdf--text-hit-cache)
    (setq yunge-reader-pdf--text-hit-cache
          (make-hash-table :test #'eql)))
  (let ((cached (gethash page yunge-reader-pdf--text-hit-cache)))
    (if (and cached
             (eq text-layer
                 (yunge-reader-pdf--hit-index-source cached)))
        cached
      (let ((index
             (yunge-reader-pdf--make-hit-index page text-layer)))
        (puthash page index yunge-reader-pdf--text-hit-cache)
        index))))

(defun yunge-reader-pdf--hit-character (page point text-layer)
  "Return PAGE's TEXT-LAYER character nearest canonical POINT."
  (let* ((x (car point))
         (y (cdr point))
         (index
          (yunge-reader-pdf--page-hit-index page text-layer))
         (tolerance
          (yunge-reader-pdf--hit-index-tolerance index))
         (cell-size
          (yunge-reader-pdf--hit-index-cell-size index))
         (characters
          (gethash
           (cons (floor (/ x cell-size))
                 (floor (/ y cell-size)))
           (yunge-reader-pdf--hit-index-cells index)))
         best
         best-distance)
    (dolist (character characters)
      (let ((distance
             (yunge-reader-pdf--character-distance x y character)))
        (when (and distance
                   (or (null best-distance)
                       (< distance best-distance)))
          (setq best character
                best-distance distance))))
    (when (and best-distance
               (<= best-distance (* tolerance tolerance)))
      best)))

(defun yunge-reader-pdf--link-contains-p (link point)
  "Return non-nil when LINK contains canonical PDF POINT."
  (let ((bounds (yunge-reader-pdf-link-bounds link))
        (x (car point))
        (y (cdr point)))
    (and (<= (alist-get 'left bounds) x)
         (<= x (alist-get 'right bounds))
         (<= (alist-get 'bottom bounds) y)
         (<= y (alist-get 'top bounds)))))

(defun yunge-reader-pdf--link-at-point (page point data)
  "Return PAGE link containing canonical POINT in DATA."
  (when (and (yunge-reader-pdf-link-data-p data)
             (= page (yunge-reader-pdf-link-data-page data)))
    (seq-find
     (lambda (link)
       (yunge-reader-pdf--link-contains-p link point))
     (yunge-reader-pdf-link-data-links data))))

(defun yunge-reader-pdf--page-label (page)
  "Return the display label for zero-based PDF PAGE."
  (or (alist-get 'label (yunge-reader-pdf--page-info page))
      (number-to-string (1+ page))))

(defun yunge-reader-pdf--link-target-label (action)
  "Return a compact target label for PDF link ACTION."
  (pcase (yunge-reader-action-type action)
    ('location
     (format "page %s"
             (yunge-reader-pdf--page-label
              (yunge-reader-position-unit
               (yunge-reader-action-position action)))))
    ('uri
     (truncate-string-to-width
      (yunge-reader-action-uri action) 80 nil nil t))))

(defun yunge-reader-pdf--link-label (link)
  "Return one completion label for PDF LINK."
  (let* ((action (yunge-reader-pdf-link-action link))
         (source (yunge-reader-pdf-link-page link))
         (text
          (or (yunge-reader-pdf-link-label link)
              (format "link %d"
                      (1+ (yunge-reader-pdf-link-index link))))))
    (truncate-string-to-width
     (format "Page %s: %s -> %s"
             (yunge-reader-pdf--page-label source)
             text
             (yunge-reader-pdf--link-target-label action))
     120 nil nil t)))

(defun yunge-reader-pdf--link-candidates (pages)
  "Return unique completion candidates for cached links on PAGES."
  (let ((counts (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (used (make-hash-table :test #'equal))
        labeled)
    (dolist (page pages)
      (when-let* ((data
                   (gethash page yunge-reader-pdf--link-cache)))
        (dolist (link (yunge-reader-pdf-link-data-links data))
          (let ((label (yunge-reader-pdf--link-label link)))
            (push (cons label link) labeled)
            (puthash label (1+ (gethash label counts 0)) counts)))))
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

(defun yunge-reader-pdf--follow-location-link (link)
  "Follow PDF location LINK through the generic Reader action layer."
  (yunge-reader--follow-action
   (yunge-reader-pdf-link-action link))
  t)

(defun yunge-reader-pdf--follow-link (link)
  "Follow PDF LINK through the generic Reader action layer."
  (let ((action (yunge-reader-pdf-link-action link)))
    (if (eq (yunge-reader-action-type action) 'location)
        (yunge-reader-pdf--follow-location-link link)
      (yunge-reader--follow-action action)))
  (message "Link: %s" (yunge-reader-pdf--link-label link))
  t)

(defun yunge-reader-pdf--select-link (pages)
  "Choose and follow one cached PDF link from PAGES."
  (let ((candidates (yunge-reader-pdf--link-candidates pages))
        (truncated
         (seq-some
          (lambda (page)
            (when-let* ((data
                         (gethash page yunge-reader-pdf--link-cache)))
              (yunge-reader-pdf-link-data-truncated data)))
          pages)))
    (if (null candidates)
        (message "The visible PDF pages have no links")
      (let* ((completion-extra-properties
              '(:category yunge-reader-link))
             (choice
              (completing-read
               (if truncated "Links (truncated): " "Links: ")
               candidates nil t))
             (link (cdr (assoc choice candidates))))
        (when link
          (yunge-reader-pdf--follow-link link))))))

(defun yunge-reader-pdf--link-prompt-current-p
    (document generation window state)
  "Return whether a pending link prompt still belongs to the current view."
  (and (eq document yunge-reader-document)
       (= generation yunge-reader-pdf--link-activation-generation)
       (eq (selected-window) window)
       (not (active-minibuffer-window))
       (yunge-reader--window-state-current-p window state)))

(defun yunge-reader-pdf--finish-link-prompt
    (buffer document generation window state pages loaded)
  "Finish a link prompt for BUFFER when its captured view is unchanged."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and
             (= generation
                yunge-reader-pdf--link-activation-generation)
             (eq document yunge-reader-document))
        (if
            (yunge-reader-pdf--link-prompt-current-p
             document generation window state)
            (condition-case error-data
                (yunge-reader-pdf--select-link pages)
              (quit nil)
              (error
               (display-warning
                'yunge-reader
                (format "Could not follow PDF link: %s"
                        (error-message-string error-data))
                :warning)))
          (when loaded
            (message "PDF links loaded; press RET to open them")))))))

(defun yunge-reader-pdf-follow-link ()
  "Choose a link from the PDF pages visible in this window."
  (interactive)
  (unless yunge-reader-document
    (user-error "This reader buffer has no open document"))
  (let* ((window (yunge-reader--place-window))
         (pages (and window (yunge-reader-pdf--window-pages window))))
    (unless window
      (user-error "The Reader buffer is not displayed in a live window"))
    (setq pages (or pages (list yunge-reader-pdf-page)))
    (let* ((buffer (current-buffer))
           (document yunge-reader-document)
           (state (yunge-reader--window-state window))
           (generation
            (cl-incf yunge-reader-pdf--link-activation-generation))
           (missing
            (seq-remove
             (lambda (page)
               (gethash page yunge-reader-pdf--link-cache))
             pages)))
      (if (null missing)
          (yunge-reader-pdf--select-link pages)
        (let ((remaining (length missing))
              loaded)
          (message "Loading PDF links...")
          (dolist (page missing)
            (yunge-reader-pdf--request-links
             page
             (lambda (_result error-data)
               (unless error-data
                 (setq loaded t))
               (cl-decf remaining)
               (when (zerop remaining)
                 (yunge-reader-pdf--finish-link-prompt
                  buffer document generation window state
                  pages loaded))))))))))

(defun yunge-reader-pdf--event-page-point (position &optional noerror)
  "Return PAGE and canonical point represented by mouse POSITION.
When NOERROR is non-nil, return nil for positions outside page images."
  (let* ((buffer-position (posn-point position))
         (page (yunge-reader-pdf--page-at-position buffer-position))
         (image (posn-image position))
         (object-point (posn-object-x-y position))
         (object-size (posn-object-width-height position))
         (x (car-safe object-point))
         (y (cdr-safe object-point))
         (width (car-safe object-size))
         (height (cdr-safe object-size)))
    (if (and (natnump page) image
             (numberp x) (numberp y)
             (numberp width) (> width 0)
             (numberp height) (> height 0)
             (<= 0 x width) (<= 0 y height))
        (list
         :page page
         :point
         (yunge-reader-pdf--pixel-to-page-point
          page x y width height))
      (unless noerror
        (user-error "Keep the PDF selection pointer on a page image")))))

(defun yunge-reader-pdf--same-character-position-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same PDF character."
  (and (equal (yunge-reader-position-unit left)
              (yunge-reader-position-unit right))
       (equal (yunge-reader-position-offset left)
              (yunge-reader-position-offset right))))

(defun yunge-reader-pdf--same-selection-p (left right)
  "Return non-nil when LEFT and RIGHT select the same PDF characters."
  (when (and left right)
    (pcase-let ((`(,left-start . ,left-end)
                 (yunge-reader-pdf--ordered-range
                  (yunge-reader-selection-start left)
                  (yunge-reader-selection-end left)))
                (`(,right-start . ,right-end)
                 (yunge-reader-pdf--ordered-range
                  (yunge-reader-selection-start right)
                  (yunge-reader-selection-end right))))
      (and (yunge-reader-pdf--same-character-position-p
            left-start right-start)
           (yunge-reader-pdf--same-character-position-p
            left-end right-end)))))

(defun yunge-reader-pdf--selection-dirty-pages (old new)
  "Return visible pages whose selection overlay differs in OLD and NEW."
  (seq-filter
   (lambda (page)
     (when-let* ((text-layer
                  (and yunge-reader-pdf--text-cache
                       (gethash page yunge-reader-pdf--text-cache))))
       (not
        (equal
         (yunge-reader-pdf--selection-offsets-for
          old page text-layer)
         (yunge-reader-pdf--selection-offsets-for
          new page text-layer)))))
   yunge-reader-pdf--displayed-pages))

(defun yunge-reader-pdf--repaint-selection (window pages)
  "Repaint selection overlays on PAGES and update live WINDOW."
  (when pages
    (dolist (page pages)
      (yunge-reader-pdf--paint-page page))
    (yunge-reader-pdf--force-redisplay window)))

(defun yunge-reader-pdf--message-selection ()
  "Describe the current PDF selection in the echo area."
  (when yunge-reader-selection
    (let* ((start (yunge-reader-selection-start
                   yunge-reader-selection))
           (end (yunge-reader-selection-end yunge-reader-selection))
           (start-page (yunge-reader-position-unit start))
           (end-page (yunge-reader-position-unit end))
           (start-index (yunge-reader-position-offset start))
           (end-index (yunge-reader-position-offset end)))
      (if (= start-page end-page)
          (message "Selected %d PDF character%s"
                   (1+ (abs (- start-index end-index)))
                   (if (= start-index end-index) "" "s"))
        (message "Selected PDF text across %d pages"
                 (1+ (abs (- start-page end-page))))))))

(defun yunge-reader-pdf--selection-position-at-location (location)
  "Return the PDF character position nearest canonical LOCATION."
  (let* ((page (plist-get location :page))
         (point (plist-get location :point))
         (text-layer
          (and yunge-reader-pdf--text-cache
               (gethash page yunge-reader-pdf--text-cache))))
    (unless text-layer
      (user-error "PDF text geometry is still loading"))
    (when-let* ((character
                 (yunge-reader-pdf--hit-character
                  page point text-layer)))
      (make-yunge-reader-position
       :unit page
       :offset (alist-get 'index character)
       :x (car point)
       :y (cdr point)))))

(defun yunge-reader-pdf--select-points
    (start-location end-location &optional quiet soft window fixed-start)
  "Select PDF characters at START-LOCATION and END-LOCATION.
Suppress the echo message when QUIET is non-nil.  When SOFT is non-nil,
keep the previous selection if either pointer is not near selectable text.
WINDOW is redisplayed immediately after a successful update.  FIXED-START,
when non-nil, is a previously resolved start character position."
  (let ((start
         (or fixed-start
             (yunge-reader-pdf--selection-position-at-location
              start-location)))
        (end
         (yunge-reader-pdf--selection-position-at-location
          end-location)))
      (if (and start end)
          (let* ((previous yunge-reader-selection)
                 (selection
                  (make-yunge-reader-selection
                   :start start
                   :end end)))
            (unless (yunge-reader-pdf--same-selection-p
                     previous selection)
              (yunge-reader-set-selection
               (yunge-reader-selection-start selection)
               (yunge-reader-selection-end selection))
              (yunge-reader-pdf--repaint-selection
               window
               (yunge-reader-pdf--selection-dirty-pages
                previous yunge-reader-selection)))
            (unless quiet
              (yunge-reader-pdf--message-selection))
            t)
        (unless soft
          (let ((previous yunge-reader-selection))
            (when previous
              (yunge-reader-clear-selection t)
              (yunge-reader-pdf--repaint-selection
               window
               (yunge-reader-pdf--selection-dirty-pages
                previous nil))))
          (user-error "No selectable PDF text near the pointer")))))

(defun yunge-reader-pdf--selection-event-position (event)
  "Return the final position represented by mouse EVENT."
  (or (event-end event) (event-start event)))

(defun yunge-reader-pdf--selection-drag-p
    (start-position position)
  "Return non-nil when POSITION moved far enough from START-POSITION."
  (let ((start (posn-x-y start-position))
        (current (posn-x-y position)))
    (and (numberp (car-safe start))
         (numberp (cdr-safe start))
         (numberp (car-safe current))
         (numberp (cdr-safe current))
         (or (> (abs (- (car current) (car start)))
                yunge-reader-pdf-selection-drag-threshold)
             (> (abs (- (cdr current) (cdr start)))
                yunge-reader-pdf-selection-drag-threshold)))))

(defun yunge-reader-pdf--coalesce-motion-events (event)
  "Return latest queued motion EVENT and the first following event."
  (let (pending done)
    (while (not done)
      (let ((next (read-event nil nil 0)))
        (cond
         ((null next)
          (setq done t))
         ((mouse-movement-p next)
          (setq event next))
         (t
          (setq pending next
                done t)))))
    (cons event pending)))

(defun yunge-reader-pdf--clear-click-selection (window)
  "Clear the logical PDF selection after a click in WINDOW."
  (when yunge-reader-selection
    (let ((previous yunge-reader-selection))
      (yunge-reader-clear-selection t)
      (yunge-reader-pdf--repaint-selection
       window
       (yunge-reader-pdf--selection-dirty-pages previous nil)))))

(defun yunge-reader-pdf--track-selection-events
    (start-location start-position window)
  "Track selection from START-LOCATION and START-POSITION in WINDOW."
  (let (dragging done pending fixed-start start-resolved)
    (cl-labels
        ((select-at
          (location)
          (unless start-resolved
            (setq fixed-start
                  (yunge-reader-pdf--selection-position-at-location
                   start-location)
                  start-resolved t))
          (when fixed-start
            (yunge-reader-pdf--select-points
             start-location location t t window fixed-start))))
      (setq mark-active nil)
      (while (not done)
        (let ((next (or pending (read-event))))
          (setq pending nil)
          (when (and next (mouse-movement-p next))
            (pcase-let
                ((`(,latest . ,following)
                  (yunge-reader-pdf--coalesce-motion-events next)))
              (setq next latest
                    pending following)))
          (cond
           ((null next)
            (setq done t))
           ((mouse-movement-p next)
            (let ((position (event-start next)))
              (when (eq window (posn-window position))
                (when (or dragging
                          (yunge-reader-pdf--selection-drag-p
                           start-position position))
                  (setq dragging t)
                  (when-let* ((location
                               (yunge-reader-pdf--event-page-point
                                position t)))
                    (select-at location))))))
           ((memq (event-basic-type next) '(mouse-1 drag-mouse-1))
            (let ((position
                   (yunge-reader-pdf--selection-event-position next)))
              (when (eq window (posn-window position))
                (if (or dragging
                        (yunge-reader-pdf--selection-drag-p
                         start-position position))
                    (progn
                      (setq dragging t)
                      (when-let* ((location
                                   (yunge-reader-pdf--event-page-point
                                    position t)))
                        (select-at location)))
                  (yunge-reader-pdf--clear-click-selection window)))
              (setq done t)))
           (t
            (push next unread-command-events)
            (setq done t))))))
    (when dragging
      (yunge-reader-pdf--message-selection))))

(defun yunge-reader-pdf--track-selection (event)
  "Track PDF text selection continuously from down-mouse EVENT."
  (let* ((start-position (event-start event))
         (window (posn-window start-position)))
    (unless (windowp window)
      (user-error "The mouse event is outside a PDF window"))
    (select-window window)
    (with-current-buffer (window-buffer window)
      (when-let* ((start-location
                   (yunge-reader-pdf--event-page-point
                    start-position t)))
        (track-mouse
          (yunge-reader-pdf--track-selection-events
           start-location start-position window))))))

(defun yunge-reader-pdf--activate-page-point (location data)
  "Follow a link at LOCATION in DATA, returning nil when none exists."
  (let* ((page (plist-get location :page))
         (point (plist-get location :point))
         (link (yunge-reader-pdf--link-at-point page point data)))
    (if link
        (yunge-reader-pdf--follow-link link)
      (message "There is no PDF link at this position")
      nil)))

(defun yunge-reader-pdf-activate-at-mouse (event)
  "Follow a PDF link at modified mouse EVENT, if one exists."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position)))
    (unless (windowp window)
      (user-error "The mouse event is outside a PDF window"))
    (select-window window)
    (with-current-buffer (window-buffer window)
      (let* ((location
              (yunge-reader-pdf--event-page-point position))
             (page (plist-get location :page))
             (cached
              (and (hash-table-p yunge-reader-pdf--link-cache)
                   (gethash page yunge-reader-pdf--link-cache)))
             (buffer (current-buffer))
             (document yunge-reader-document)
             (state (yunge-reader--window-state window))
             (generation
              (cl-incf yunge-reader-pdf--link-activation-generation)))
        (if cached
            (yunge-reader-pdf--activate-page-point location cached)
          (yunge-reader-pdf--request-links
           page
           (lambda (result _error-data)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when
                     (yunge-reader-pdf--link-prompt-current-p
                      document generation window state)
                   (condition-case error-data
                       (yunge-reader-pdf--activate-page-point
                        location result)
                     (error
                      (display-warning
                       'yunge-reader
                       (format "Could not activate PDF link: %s"
                               (error-message-string error-data))
                       :warning)))))))))))))

(defun yunge-reader-pdf-select-with-mouse (event)
  "Select PDF text with live feedback from down-mouse EVENT."
  (interactive "e")
  (yunge-reader-pdf--track-selection event))

(provide 'yunge-reader-pdf-interaction)

;;; yunge-reader-pdf-interaction.el ends here
