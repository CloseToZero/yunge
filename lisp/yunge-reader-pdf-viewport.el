;;; yunge-reader-pdf-viewport.el --- PDF viewport and navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'mwheel)
(require 'pixel-scroll)
(require 'seq)
(require 'yunge-reader)

(defvar yunge-reader-pdf-view-mode)

(defcustom yunge-reader-pdf-page-margin 24
  "Pixel margin reserved around a rendered PDF page."
  :type 'natnum
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-page-gap 16
  "Vertical pixel gap between pages in the continuous PDF roll."
  :type 'natnum
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-wheel-fallback-lines 3
  "Screen-line heights used when a PDF wheel event has no pixel delta."
  :type 'natnum
  :group 'yunge-reader)

(defcustom yunge-reader-pdf-center-pages t
  "Whether to center PDF pages in their Reader window."
  :type 'boolean
  :group 'yunge-reader)

(defconst yunge-reader-pdf--points-to-pixels (/ 96.0 72.0)
  "Nominal conversion from PDF points to screen pixels at scale one.")

(defvar-local yunge-reader-pdf-page 0
  "Zero-based page currently displayed in the PDF adapter.")


(defvar-local yunge-reader-pdf--page-infos nil
  "Vector of canonical geometry for every PDF page.")

(defvar-local yunge-reader-pdf--page-positions nil
  "Vector mapping zero-based PDF pages to buffer positions.")


(defvar-local yunge-reader-pdf--displayed-pages nil
  "Pages currently painted as images in any live view window.")

(defvar-local yunge-reader-pdf--updating-visible nil
  "Non-nil while PDF roll virtualization is updating display slots.")

(defvar-local yunge-reader-pdf--pending-location nil
  "Stable PDF position waiting for a live viewport window.")

(defvar-local yunge-reader-pdf--pending-viewport-anchor nil
  "Continuous-roll viewport anchor waiting for resized page geometry.")

(defvar-local yunge-reader-pdf--resize-timer nil
  "Idle timer coalescing changes to the current PDF viewport size.")

(defvar-local yunge-reader-pdf--pending-resize nil
  "Latest PDF viewport resize waiting for redisplay to settle.")

(defvar-local yunge-reader-pdf--programmatic-scroll nil
  "Non-nil while Reader itself is changing the PDF viewport.")

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

(defun yunge-reader-pdf--display-width (page)
  "Return the pixel width currently painted for PAGE, or nil."
  (when-let* ((position (yunge-reader-pdf--page-position page))
              (_ (< position (point-max)))
              (width
               (get-text-property
                position 'yunge-reader-pdf-display-width)))
    (and (natnump width) (> width 0) width)))


(defun yunge-reader-pdf--location (_document window)
  "Return the stable PDF position visible at the top left of WINDOW."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (> (yunge-reader-pdf--page-count) 0)
             yunge-reader-pdf--page-positions)
    (let* ((page
            (or (yunge-reader-pdf--page-at-position
                 (window-start window))
                yunge-reader-pdf-page))
           (page
            (max 0 (min (1- (yunge-reader-pdf--page-count)) page)))
           (page-info (yunge-reader-pdf--page-info page))
           (width
            (or (yunge-reader-pdf--display-width page)
                (yunge-reader-pdf--page-width page window)))
           (size (yunge-reader-pdf--pixel-size page-info width))
           (pixel-width (car size))
           (pixel-height (cdr size))
           (page-width (alist-get 'width page-info))
           (page-height (alist-get 'height page-info))
           (vertical
            (max 0 (min pixel-height
                        (or (window-vscroll window t) 0))))
           (column-width
            (max 1 (frame-char-width (window-frame window))))
           (horizontal
            (max 0
                 (min pixel-width
                      (* (window-hscroll window) column-width))))
           (x (* page-width (/ (float horizontal) pixel-width)))
           (y (* page-height
                 (- 1.0 (/ (float vertical) pixel-height)))))
      (make-yunge-reader-position
       :unit page
       :x x
       :y y))))

(defun yunge-reader-pdf--viewport-anchor (window)
  "Return the continuous-roll pixel anchor currently shown in WINDOW."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             yunge-reader-pdf--page-positions)
    (when-let* ((page
                 (yunge-reader-pdf--page-at-position
                  (window-start window)))
                (page-info (yunge-reader-pdf--page-info page))
                (width
                 (or (yunge-reader-pdf--display-width page)
                     (yunge-reader-pdf--page-width page window))))
      (list :page page
            :page-height
            (cdr (yunge-reader-pdf--pixel-size page-info width))
            :vscroll (max 0 (or (window-vscroll window t) 0))))))

(defun yunge-reader-pdf--rescaled-vscroll
    (anchor page pixel-height fallback)
  "Return ANCHOR's vscroll rescaled for PAGE at PIXEL-HEIGHT.
Use FALLBACK when ANCHOR does not describe PAGE.  Preserve the fixed pixel
gap below a page separately from the portion which scales with the page."
  (let ((anchor-page (plist-get anchor :page))
        (old-height (plist-get anchor :page-height))
        (old-vscroll (plist-get anchor :vscroll)))
    (if (and (eql anchor-page page)
             (numberp old-height)
             (> old-height 0)
             (numberp old-vscroll)
             (>= old-vscroll 0))
        (let* ((page-offset (min old-vscroll old-height))
               (gap-offset
                (min yunge-reader-pdf-page-gap
                     (max 0 (- old-vscroll old-height))))
               (limit
                (+ pixel-height
                   (if (< page (1- (yunge-reader-pdf--page-count)))
                       yunge-reader-pdf-page-gap
                     0))))
          (min limit
               (+ (round (* pixel-height
                            (/ (float page-offset) old-height)))
                  gap-offset)))
      fallback)))

(defun yunge-reader-pdf--apply-pending-location (window)
  "Apply a pending stable PDF location to live WINDOW."
  (when (and yunge-reader-pdf--pending-location
             (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (> (yunge-reader-pdf--page-count) 0)
             yunge-reader-pdf--page-positions)
    (let* ((location yunge-reader-pdf--pending-location)
           (page
            (max
             0
             (min (1- (yunge-reader-pdf--page-count))
                  (yunge-reader-position-unit location))))
           (page-info (yunge-reader-pdf--page-info page))
           (page-width (alist-get 'width page-info))
           (page-height (alist-get 'height page-info))
           (width (yunge-reader-pdf--page-width page window))
           (size (yunge-reader-pdf--pixel-size page-info width))
           (pixel-width (car size))
           (pixel-height (cdr size))
           (x
            (max 0.0
                 (min page-width
                      (or (yunge-reader-position-x location) 0.0))))
           (y
            (max 0.0
                 (min page-height
                      (or (yunge-reader-position-y location)
                          page-height))))
           (body-width (window-body-width window t))
           (location-vertical
            (max
             0
             (min pixel-height
                  (round
                   (* pixel-height (- 1.0 (/ y page-height)))))))
           (vertical
            (yunge-reader-pdf--rescaled-vscroll
             yunge-reader-pdf--pending-viewport-anchor
             page pixel-height location-vertical))
           (horizontal-pixels
            (max
             0
             (min (max 0 (- pixel-width body-width))
                  (round (* pixel-width (/ x page-width))))))
           (column-width
            (max 1 (frame-char-width (window-frame window))))
           (position (yunge-reader-pdf--page-position page)))
      (setq yunge-reader-pdf--pending-location nil
            yunge-reader-pdf--pending-viewport-anchor nil
            yunge-reader-pdf-page page)
      (goto-char position)
      (set-window-start window position t)
      (set-window-vscroll window vertical t t)
      (set-window-hscroll
       window (floor (/ (float horizontal-pixels) column-width)))
      t)))

(defun yunge-reader-pdf--restore-location
    (_document location window)
  "Accept stable PDF LOCATION for restoration in WINDOW."
  (when (and (yunge-reader-position-p location)
             (natnump (yunge-reader-position-unit location))
             (> (yunge-reader-pdf--page-count) 0)
             yunge-reader-pdf--page-positions)
    (let* ((page
            (max
             0
             (min (1- (yunge-reader-pdf--page-count))
                  (yunge-reader-position-unit location))))
           (position (yunge-reader-pdf--page-position page))
           (restored (copy-yunge-reader-position location))
           (live-window (yunge-reader--place-window window)))
      (setf (yunge-reader-position-unit restored) page)
      (setq yunge-reader-pdf--pending-location restored
            yunge-reader-pdf--pending-viewport-anchor nil
            yunge-reader-pdf-page page)
      (goto-char position)
      (when live-window
        (set-window-start live-window position t))
      (yunge-reader-pdf--update-visible-pages live-window)
      t)))


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


(defun yunge-reader-pdf--queue-pages (pages &optional window)
  "Replace low-priority PDF work for PAGES viewed in WINDOW."
  (let* ((pages (cl-remove-duplicates (copy-sequence pages)))
         (tasks (yunge-reader-pdf--prefetch-tasks pages window)))
    (setq yunge-reader-pdf--working-pages pages)
    (yunge-reader-pdf--prune-working-set pages tasks)
    (setq yunge-reader-pdf--prefetch-queue
          (if yunge-reader-pdf--prefetch-active
              (seq-remove
               (lambda (task)
                 (yunge-reader-pdf--prefetch-task-same-p
                  task yunge-reader-pdf--prefetch-active))
               tasks)
            tasks))
    (yunge-reader-pdf--run-prefetch)))

(defun yunge-reader-pdf--sync-current-page (&optional window)
  "Update the current page from WINDOW's topmost roll slot."
  (let* ((window
          (or window (yunge-reader--place-window)))
         (page
          (and window
               (yunge-reader-pdf--page-at-position
                (window-start window)))))
    (when (natnump page)
      (setq yunge-reader-pdf-page page))))

(defun yunge-reader-pdf--update-visible-pages (&optional window)
  "Virtualize the PDF roll around the active presentation.
WINDOW identifies the presentation which triggered this update."
  (when (and yunge-reader-pdf-view-mode
             yunge-reader-document
             yunge-reader-pdf--page-positions
             (not yunge-reader-pdf--updating-visible))
    (let* ((yunge-reader-pdf--updating-visible t)
           (source-window window)
           (window (or (yunge-reader--presentation-window)
                       (yunge-reader--place-window source-window))))
      (unless (yunge-reader-pdf--apply-pending-location window)
        (yunge-reader-pdf--sync-current-page window))
      (yunge-reader-pdf--target-width
       (yunge-reader-pdf--page-info yunge-reader-pdf-page)
       window)
      (let ((visible (yunge-reader-pdf--visible-pages)))
        (yunge-reader-pdf--paint-pages visible window)
        (yunge-reader-pdf--queue-pages
         (yunge-reader-pdf--prefetch-range visible) window))
      (yunge-reader-pdf--update-header)
      (yunge-reader-record-place source-window))))

(defun yunge-reader-pdf--refresh (&optional window location viewport-anchor)
  "Refresh the PDF roll in WINDOW, then restore LOCATION and VIEWPORT-ANCHOR."
  (when (and yunge-reader-pdf-view-mode yunge-reader-document)
    (setq window
          (or (and (window-live-p window)
                   (eq (window-buffer window) (current-buffer))
                   window)
              (yunge-reader--place-window)))
    (cl-incf yunge-reader-pdf--generation)
    (unless yunge-reader-pdf--page-infos
      (yunge-reader-pdf--load-page-infos))
    (if (zerop (yunge-reader-pdf--page-count))
        (yunge-reader--display-status "This PDF contains no pages")
      (unless yunge-reader-pdf--page-positions
        (yunge-reader-pdf--build-roll))
      (when (and (yunge-reader-position-p location)
                 (not yunge-reader-pdf--pending-location))
        (setq yunge-reader-pdf--pending-location
              (copy-yunge-reader-position location)
              yunge-reader-pdf--pending-viewport-anchor
              (copy-tree viewport-anchor t)))
      (setq yunge-reader-pdf--displayed-pages nil)
      (dotimes (page (yunge-reader-pdf--page-count))
        (yunge-reader-pdf--paint-page
         page
         (and window (yunge-reader-pdf--page-width page window))))
      (yunge-reader-pdf--update-visible-pages window)
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


(defun yunge-reader-pdf--cancel-resize ()
  "Cancel a pending PDF viewport resize for the current buffer."
  (when (timerp yunge-reader-pdf--resize-timer)
    (cancel-timer yunge-reader-pdf--resize-timer))
  (setq yunge-reader-pdf--resize-timer nil
        yunge-reader-pdf--pending-resize nil
        yunge-reader-pdf--pending-viewport-anchor nil))

(defun yunge-reader-pdf--finish-resize (buffer)
  "Refresh BUFFER after its latest PDF viewport resize settles."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((state yunge-reader-pdf--pending-resize)
             (document (plist-get state :document))
             (window (plist-get state :window))
             (location (plist-get state :location))
             (viewport-anchor (plist-get state :viewport-anchor)))
        (setq yunge-reader-pdf--resize-timer nil
              yunge-reader-pdf--pending-resize nil)
        (when (and yunge-reader-pdf-view-mode
                   (eq document yunge-reader-document)
                   (window-live-p window)
                   (eq (window-buffer window) buffer)
                   (yunge-reader--active-presentation-p window))
          (if (memq yunge-reader-zoom-mode '(fit-width fit-page))
              (yunge-reader-pdf--refresh
               window location viewport-anchor)
            (yunge-reader-pdf--update-visible-pages window)))))))

(defun yunge-reader-pdf--window-size-change (window)
  "Schedule viewport work after WINDOW changes its body size."
  (when (and yunge-reader-document
             (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (yunge-reader--active-presentation-p window))
    (let* ((former yunge-reader-pdf--pending-resize)
           (same-view
            (and (eq (plist-get former :document)
                     yunge-reader-document)
                 (eq (plist-get former :window) window)))
           (location
            (or (and same-view (plist-get former :location))
                (and (not yunge-reader-pdf--pending-location)
                     (ignore-errors
                       (yunge-reader-pdf--location
                        yunge-reader-document window)))))
           (viewport-anchor
            (or (and same-view
                     (plist-get former :viewport-anchor))
                (and (not yunge-reader-pdf--pending-location)
                     (ignore-errors
                       (yunge-reader-pdf--viewport-anchor window))))))
      (setq yunge-reader-pdf--pending-resize
            (list :document yunge-reader-document
                  :window window
                  :width (window-body-width window t)
                  :height (window-body-height window t)
                  :location location
                  :viewport-anchor viewport-anchor))
      (unless (timerp yunge-reader-pdf--resize-timer)
        (setq yunge-reader-pdf--resize-timer
              (run-with-idle-timer
               0 nil #'yunge-reader-pdf--finish-resize
               (current-buffer)))))))

(defun yunge-reader-pdf--window-scrolled (window _start)
  "Update PDF virtualization after WINDOW scrolls."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (unless (or yunge-reader-pdf--programmatic-scroll
                (not (yunge-reader--active-presentation-p window)))
      (yunge-reader--detach-search-navigation))
    (yunge-reader-pdf--update-visible-pages window)))

(defun yunge-reader-pdf--set-page (page)
  "Display zero-based PDF PAGE."
  (unless yunge-reader-pdf--programmatic-scroll
    (yunge-reader--detach-search-navigation))
  (let ((count (yunge-reader-pdf--page-count)))
    (unless (> count 0)
      (user-error "This PDF has no pages"))
    (setq yunge-reader-pdf--pending-location nil
          yunge-reader-pdf-page
          (max 0 (min (1- count) page)))
    (let ((position
           (yunge-reader-pdf--page-position yunge-reader-pdf-page))
          (window (yunge-reader--place-window)))
      (when position
        (goto-char position)
        (when (window-live-p window)
          (set-window-start window position t)
          (set-window-vscroll window 0 t))))
    (yunge-reader-pdf--update-visible-pages)
    yunge-reader-pdf-page))

(defun yunge-reader-pdf--scroll-half-window (direction count)
  "Scroll in DIRECTION by half a window COUNT times."
  (yunge-reader--detach-search-navigation)
  (let ((function
         (if (eq direction 'up)
             #'pixel-scroll-precision-scroll-up-page
           #'pixel-scroll-precision-scroll-down-page)))
    (dotimes (_ (abs count))
      (funcall
       function
       (max 1 (/ (window-text-height nil t) 2))))
    (yunge-reader-pdf--update-visible-pages (selected-window))))

(defun yunge-reader-pdf-scroll-up (&optional count)
  "Scroll backward by half a PDF window COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (yunge-reader-pdf--scroll-half-window
   (if (< count 0) 'down 'up) count))

(defun yunge-reader-pdf-scroll-down (&optional count)
  "Scroll forward by half a PDF window COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (yunge-reader-pdf--scroll-half-window
   (if (< count 0) 'up 'down) count))

(defun yunge-reader-pdf--scroll-line (direction count)
  "Scroll in DIRECTION by one screen line COUNT times."
  (yunge-reader--detach-search-navigation)
  (let ((function
         (if (eq direction 'up)
             #'pixel-scroll-precision-scroll-up-page
           #'pixel-scroll-precision-scroll-down-page))
        (pixels
         (max 1 (frame-char-height (window-frame)))))
    (dotimes (_ (abs count))
      (funcall function pixels))
    (yunge-reader-pdf--update-visible-pages (selected-window))))

(defun yunge-reader-pdf--wheel-pixel-delta (event window)
  "Return the bounded signed pixel delta for wheel EVENT in WINDOW."
  (let* ((pixel-data (nth 4 event))
         (raw-delta (and (consp pixel-data) (cdr pixel-data)))
         (line-height
          (max 1 (frame-char-height (window-frame window))))
         (fallback
          (* line-height
             (max 1 yunge-reader-pdf-wheel-fallback-lines)))
         (event-type (event-basic-type event))
         (delta
          (cond
           ((numberp raw-delta) (round raw-delta))
           ((memq event-type '(wheel-down mouse-5)) (- fallback))
           ((memq event-type '(wheel-up mouse-4)) fallback)
           (t 0)))
         (limit
          (max line-height
               (/ (window-text-height window t) 2))))
    (max (- limit) (min limit delta))))

(defun yunge-reader-pdf-scroll-wheel (event)
  "Scroll the PDF under wheel EVENT by its bounded pixel delta."
  (interactive "e")
  (let ((window (mwheel-event-window event)))
    (when (framep window)
      (setq window (frame-selected-window window)))
    (when (window-live-p window)
      (with-selected-window window
        (when yunge-reader-pdf-view-mode
          (let ((delta
                 (yunge-reader-pdf--wheel-pixel-delta event window)))
            (unless (zerop delta)
              (yunge-reader--detach-search-navigation)
              (condition-case nil
                  (if (< delta 0)
                      (pixel-scroll-precision-scroll-down-page (- delta))
                    (pixel-scroll-precision-scroll-up-page delta))
                (beginning-of-buffer nil)
                (end-of-buffer nil))
              (yunge-reader-pdf--update-visible-pages window))))))))

(put 'yunge-reader-pdf-scroll-wheel 'scroll-command t)

(defun yunge-reader-pdf-scroll-up-line (&optional count)
  "Scroll backward by one PDF screen line COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (yunge-reader-pdf--scroll-line
   (if (< count 0) 'down 'up) count))

(defun yunge-reader-pdf-scroll-down-line (&optional count)
  "Scroll forward by one PDF screen line COUNT times."
  (interactive "p")
  (setq count (or count 1))
  (yunge-reader-pdf--scroll-line
   (if (< count 0) 'up 'down) count))

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

(dolist (command
         '(yunge-reader-pdf-first-page
           yunge-reader-pdf-last-page
           yunge-reader-pdf-goto-page
           yunge-reader-pdf--follow-location-link))
  (yunge-jump-history-track-command command))

(defun yunge-reader-pdf--force-redisplay (&optional window)
  "Immediately redisplay WINDOW or every window showing this buffer."
  (let (updated)
    (dolist (target
             (if window
                 (list window)
               (get-buffer-window-list (current-buffer) nil t)))
      (when (window-live-p target)
        (force-window-update target)
        (setq updated t)))
    (when (and updated (display-graphic-p))
      (redisplay t))))


(provide 'yunge-reader-pdf-viewport)

;;; yunge-reader-pdf-viewport.el ends here
