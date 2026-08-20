;;; yunge-reader-pdf-smoke.el --- PDF smoke -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(setq native-comp-jit-compilation nil)

(require 'cl-lib)
(require 'subr-x)

(defconst yunge-reader-pdf-smoke--script-directory
  (file-name-directory
   (or load-file-name
       (error "PDF smoke must be loaded from a file"))))

(add-to-list 'load-path yunge-reader-pdf-smoke--script-directory)
(require 'yunge-reader-graphical-smoke)

(defconst yunge-reader-pdf-smoke--context
  (yunge-reader-graphical-smoke-create
   yunge-reader-pdf-smoke--script-directory
   "PDF graphical smoke"
   "yunge-reader-pdf-"
   "YUNGE_READER_PDF_LOG"))

(defun yunge-reader-pdf-smoke--log (format-string &rest arguments)
  "Write FORMAT-STRING with ARGUMENTS to stdout and the optional log."
  (apply
   #'yunge-reader-graphical-smoke-log
   yunge-reader-pdf-smoke--context
   format-string arguments))

(condition-case error-data
    (progn
      (yunge-reader-graphical-smoke-initialize
       yunge-reader-pdf-smoke--context)
      (require 'yunge-reader-pdf))
  (error
   (yunge-reader-pdf-smoke--log
    "PDF smoke load failed: %S\n" error-data)
   (yunge-reader-graphical-smoke-cleanup
    yunge-reader-pdf-smoke--context)
   (signal (car error-data) (cdr error-data))))

(defvar yunge-reader-pdf--page-positions)
(defvar yunge-reader-pdf--pending-resize)
(defvar yunge-reader-pdf--render-pending)
(defvar yunge-reader-pdf--render-results)
(defvar yunge-reader-pdf-view-mode)

(declare-function yunge-reader-pdf--display-width "yunge-reader-pdf")
(declare-function yunge-reader-pdf--location "yunge-reader-pdf")
(declare-function yunge-reader-pdf--page-count "yunge-reader-pdf")
(declare-function yunge-reader-pdf--page-position "yunge-reader-pdf")
(declare-function yunge-reader-pdf--render-appearance "yunge-reader-pdf")
(declare-function yunge-reader-pdf--render-key "yunge-reader-pdf")
(declare-function yunge-reader-pdf--restore-location "yunge-reader-pdf")

(defun yunge-reader-native-program ()
  "Use the release helper built by this isolated smoke."
  (yunge-reader-graphical-smoke-context-helper
   yunge-reader-pdf-smoke--context))

(defun yunge-reader-native-cache-directory ()
  "Use the disposable render cache owned by this smoke."
  (expand-file-name
   "render-cache"
   (yunge-reader-graphical-smoke-context-temporary-root
    yunge-reader-pdf-smoke--context)))

(defvar yunge-reader-pdf-smoke--file nil)
(defvar yunge-reader-pdf-smoke--buffer nil)
(defvar yunge-reader-pdf-smoke--deadline nil)
(defvar yunge-reader-pdf-smoke--phase 'opening)
(defvar yunge-reader-pdf-smoke--not-before nil)
(defvar yunge-reader-pdf-smoke--warnings nil)
(defvar yunge-reader-pdf-smoke--observations nil)
(defvar yunge-reader-pdf-smoke--initial nil)
(defvar yunge-reader-pdf-smoke--fit-page nil)
(defvar yunge-reader-pdf-smoke--manual-location nil)
(defvar yunge-reader-pdf-smoke--original-render-path nil)
(defvar yunge-reader-pdf-smoke--exit-status 0)

(defun yunge-reader-pdf-smoke--warning
    (original type message &rest arguments)
  "Record Reader MESSAGE, then call ORIGINAL with TYPE and ARGUMENTS."
  (when (eq type 'yunge-reader)
    (push message yunge-reader-pdf-smoke--warnings))
  (apply original type message arguments))

(defun yunge-reader-pdf-smoke--continue ()
  "Continue polling the current PDF smoke phase."
  (yunge-reader-graphical-smoke-schedule
   #'yunge-reader-pdf-smoke--poll))

(defun yunge-reader-pdf-smoke--page-stream (page)
  "Return the PDF content stream for one-based fixture PAGE."
  (let ((red (+ 0.72 (* 0.06 page)))
        (green (- 0.94 (* 0.05 page))))
    (format
     (concat
      "q %.2f %.2f 1.00 rg 0 0 595 842 re f Q\n"
      "q 0.10 0.28 0.62 rg 48 552 499 116 re f Q\n"
      "BT /F1 30 Tf 1 1 1 rg 72 610 Td "
      "(Yunge Reader PDF Smoke - Page %d) Tj ET\n"
      "BT /F1 15 Tf 0.08 0.12 0.20 rg 72 500 Td "
      "(Centered image and live resize fixture) Tj ET\n")
     red green page)))

(defun yunge-reader-pdf-smoke--stream-object (stream)
  "Return a PDF stream object body containing ASCII STREAM."
  (format
   "<< /Length %d >>\nstream\n%sendstream"
   (string-bytes stream) stream))

(defun yunge-reader-pdf-smoke--write-fixture (file)
  "Write the deterministic three-page PDF fixture to FILE."
  (let ((objects (make-vector 10 nil)))
    (aset objects 1 "<< /Type /Catalog /Pages 2 0 R >>")
    (aset objects 2
          "<< /Type /Pages /Kids [3 0 R 5 0 R 7 0 R] /Count 3 >>")
    (dolist (page '((1 3 4) (2 5 6) (3 7 8)))
      (pcase-let ((`(,number ,page-object ,stream-object) page))
        (aset
         objects page-object
         (format
          (concat
           "<< /Type /Page /Parent 2 0 R "
           "/MediaBox [0 0 595 842] "
           "/Resources << /Font << /F1 9 0 R >> >> "
           "/Contents %d 0 R >>")
          stream-object))
        (aset
         objects stream-object
         (yunge-reader-pdf-smoke--stream-object
          (yunge-reader-pdf-smoke--page-stream number)))))
    (aset
     objects 9
     "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
    (let ((data
           (encode-coding-string
            "%PDF-1.4\n%Yunge Reader smoke\n" 'us-ascii))
          (offsets (make-vector 10 0)))
      (dotimes (index 9)
        (let* ((number (1+ index))
               (object
                (encode-coding-string
                 (format "%d 0 obj\n%s\nendobj\n"
                         number (aref objects number))
                 'us-ascii)))
          (aset offsets number (string-bytes data))
          (setq data (concat data object))))
      (let ((xref-offset (string-bytes data)))
        (setq data (concat data "xref\n0 10\n0000000000 65535 f \n"))
        (dotimes (index 9)
          (setq data
                (concat
                 data
                 (format "%010d 00000 n \n"
                         (aref offsets (1+ index))))))
        (setq data
              (concat
               data
               "trailer\n<< /Size 10 /Root 1 0 R >>\n"
               (format "startxref\n%d\n%%%%EOF\n" xref-offset))))
      (let ((coding-system-for-write 'no-conversion))
        (write-region data nil file nil 'silent)))))

(defun yunge-reader-pdf-smoke--cleanup-temporary-root ()
  "Remove the temporary directory owned by this smoke."
  (yunge-reader-graphical-smoke-cleanup
   yunge-reader-pdf-smoke--context))

(defun yunge-reader-pdf-smoke--window ()
  "Return the live fixture window, or nil."
  (and (buffer-live-p yunge-reader-pdf-smoke--buffer)
       (get-buffer-window yunge-reader-pdf-smoke--buffer t)))

(defun yunge-reader-pdf-smoke--width ()
  "Return the displayed width of fixture page one, or nil."
  (and (vectorp yunge-reader-pdf--page-positions)
       (> (length yunge-reader-pdf--page-positions) 0)
       (yunge-reader-pdf--display-width 0)))

(defun yunge-reader-pdf-smoke--exact-render ()
  "Return page one's current exact render result, or nil."
  (when-let* ((window (yunge-reader-pdf-smoke--window))
              (width (yunge-reader-pdf-smoke--width))
              (appearance
               (yunge-reader-pdf--render-appearance window))
              (entry
               (and (hash-table-p yunge-reader-pdf--render-results)
                    (gethash
                     (yunge-reader-pdf--render-key
                      0 width appearance)
                     yunge-reader-pdf--render-results))))
    entry))

(defun yunge-reader-pdf-smoke--exact-image-p ()
  "Return whether page one displays its exact completed render."
  (when-let* ((position (yunge-reader-pdf--page-position 0))
              (display (get-text-property position 'display))
              (entry (yunge-reader-pdf-smoke--exact-render))
              (path (alist-get 'path entry)))
    (and (imagep display)
         (file-regular-p path)
         (equal (image-property display :file) path))))

(defun yunge-reader-pdf-smoke--render-path ()
  "Return page one's current exact render path, or nil."
  (alist-get 'path (yunge-reader-pdf-smoke--exact-render)))

(defun yunge-reader-pdf-smoke--location ()
  "Return a copy of the current stable PDF location, or nil."
  (when-let* ((window (yunge-reader-pdf-smoke--window))
              (location
               (yunge-reader-pdf--location
                yunge-reader-document window)))
    (copy-yunge-reader-position location)))

(defun yunge-reader-pdf-smoke--same-location-p (left right)
  "Return whether PDF locations LEFT and RIGHT are materially equal."
  (and (yunge-reader-position-p left)
       (yunge-reader-position-p right)
       (= (yunge-reader-position-unit left)
          (yunge-reader-position-unit right))
       (< (abs (- (or (yunge-reader-position-x left) 0.0)
                  (or (yunge-reader-position-x right) 0.0)))
          8.0)
       (< (abs (- (or (yunge-reader-position-y left) 0.0)
                  (or (yunge-reader-position-y right) 0.0)))
          2.0)))

(defun yunge-reader-pdf-smoke--different-number-p (left right)
  "Return whether numeric values LEFT and RIGHT differ materially."
  (and (numberp left)
       (numberp right)
       (> (abs (- left right)) 0.01)))

(defun yunge-reader-pdf-smoke--centered-p ()
  "Return whether page one carries valid centered graphical geometry."
  (when-let* ((window (yunge-reader-pdf-smoke--window))
              (width (yunge-reader-pdf-smoke--width))
              (position (yunge-reader-pdf--page-position 0))
              (display (get-text-property position 'display))
              (prefix (get-text-property position 'line-prefix))
              (size (image-size display t)))
    (and (imagep display)
         (= (car size) width)
         (<= width (window-body-width window t))
         (equal prefix
                `(space :align-to (- center (,(floor (/ width 2)))))))))

(defun yunge-reader-pdf-smoke--observation (name)
  "Record the current graphical PDF state under NAME."
  (push
   (list
    :name name
    :frame (cons (frame-pixel-width) (frame-pixel-height))
     :body
    (when-let* ((window (yunge-reader-pdf-smoke--window)))
      (cons (window-body-width window t)
            (window-body-height window t)))
     :zoom yunge-reader-zoom-mode
     :scale yunge-reader-effective-scale
    :manual-scale yunge-reader-scale
    :width (yunge-reader-pdf-smoke--width)
    :appearance (yunge-reader-effective-appearance)
    :render (yunge-reader-pdf-smoke--render-path)
    :location (yunge-reader-pdf-smoke--location))
   yunge-reader-pdf-smoke--observations))

(defun yunge-reader-pdf-smoke--diagnostic ()
  "Return bounded state for a failed graphical PDF phase."
  (let* ((window (yunge-reader-pdf-smoke--window))
         (position
          (and (vectorp yunge-reader-pdf--page-positions)
               (> (length yunge-reader-pdf--page-positions) 0)
               (yunge-reader-pdf--page-position 0)))
         (display (and position
                       (get-text-property position 'display))))
    (list
     :phase yunge-reader-pdf-smoke--phase
     :mode major-mode
     :view yunge-reader-pdf-view-mode
     :document (and yunge-reader-document t)
     :pages (and yunge-reader-document
                 (yunge-reader-pdf--page-count))
     :window (and window t)
     :width (yunge-reader-pdf-smoke--width)
     :image (and display (imagep display))
     :exact (yunge-reader-pdf-smoke--exact-image-p)
     :prefix
     (and position (get-text-property position 'line-prefix))
     :posn (and position window (posn-at-point position window))
     :centered (yunge-reader-pdf-smoke--centered-p)
     :pending-resize (and yunge-reader-pdf--pending-resize t)
     :pending-renders
     (and (hash-table-p yunge-reader-pdf--render-pending)
          (hash-table-count yunge-reader-pdf--render-pending))
     :renders
     (and (hash-table-p yunge-reader-pdf--render-results)
          (hash-table-count yunge-reader-pdf--render-results)))))

(defun yunge-reader-pdf-smoke--resize (width height phase)
  "Resize the selected frame to WIDTH and HEIGHT, then enter PHASE."
  (setq yunge-reader-pdf-smoke--phase phase
        yunge-reader-pdf-smoke--not-before (+ (float-time) 0.5))
  (set-frame-size nil width height t)
  (yunge-reader-pdf-smoke--continue))

(defun yunge-reader-pdf-smoke--settled-p ()
  "Return whether the current PDF resize and exact render settled."
  (and (or (null yunge-reader-pdf-smoke--not-before)
           (> (float-time) yunge-reader-pdf-smoke--not-before))
       (null yunge-reader-pdf--pending-resize)
       (yunge-reader-pdf-smoke--exact-image-p)))

(defun yunge-reader-pdf-smoke--complete-run ()
  "Clean smoke resources and exit with the recorded status."
  (advice-remove 'display-warning #'yunge-reader-pdf-smoke--warning)
  (condition-case error-data
      (yunge-reader-pdf-smoke--cleanup-temporary-root)
    (error
     (setq yunge-reader-pdf-smoke--exit-status 1)
     (yunge-reader-pdf-smoke--log
      "PDF smoke cleanup failed: %S\n" error-data)))
  (when (zerop yunge-reader-pdf-smoke--exit-status)
    (yunge-reader-pdf-smoke--log
     "PDF graphical smoke passed: %S\n"
     (nreverse yunge-reader-pdf-smoke--observations)))
  (yunge-reader-graphical-smoke-exit
   yunge-reader-pdf-smoke--exit-status))

(defun yunge-reader-pdf-smoke--finish (&optional error-data)
  "Finish the graphical PDF smoke, reporting optional ERROR-DATA."
  (when error-data
    (setq yunge-reader-pdf-smoke--exit-status 1)
    (yunge-reader-pdf-smoke--log
     "PDF graphical smoke failed: %S\n" error-data))
  (when yunge-reader-pdf-smoke--warnings
    (setq yunge-reader-pdf-smoke--exit-status 1)
    (yunge-reader-pdf-smoke--log
     "PDF graphical smoke warnings: %S\n"
     (nreverse yunge-reader-pdf-smoke--warnings)))
  (when (buffer-live-p yunge-reader-pdf-smoke--buffer)
    (with-current-buffer yunge-reader-pdf-smoke--buffer
      (when (buffer-modified-p)
        (setq yunge-reader-pdf-smoke--exit-status 1)
        (yunge-reader-pdf-smoke--log
         "PDF graphical smoke left the Reader buffer modified\n")))
    (condition-case cleanup-error
        (unless (kill-buffer yunge-reader-pdf-smoke--buffer)
          (error "Killing the unmodified PDF buffer was rejected"))
      (error
       (setq yunge-reader-pdf-smoke--exit-status 1)
       (yunge-reader-pdf-smoke--log
        "PDF buffer cleanup failed: %S\n" cleanup-error))))
  (yunge-reader-native-stop t)
  (yunge-reader-graphical-smoke-schedule
   #'yunge-reader-pdf-smoke--complete-run))

(defun yunge-reader-pdf-smoke--poll ()
  "Advance the graphical PDF smoke state machine."
  (condition-case error-data
      (cond
       ((> (float-time) yunge-reader-pdf-smoke--deadline)
        (yunge-reader-pdf-smoke--log
         "PDF graphical smoke state: %S\n"
         (with-current-buffer yunge-reader-pdf-smoke--buffer
           (yunge-reader-pdf-smoke--diagnostic)))
        (error "PDF graphical smoke timed out in %s"
               yunge-reader-pdf-smoke--phase))
       ((not (and (buffer-live-p yunge-reader-pdf-smoke--buffer)
                  (yunge-reader-pdf-smoke--window)))
        (yunge-reader-pdf-smoke--continue))
       (t
        (with-current-buffer yunge-reader-pdf-smoke--buffer
          (pcase yunge-reader-pdf-smoke--phase
            ('opening
             (if (and (eq major-mode 'yunge-reader-mode)
                      yunge-reader-pdf-view-mode
                      yunge-reader-document
                      (= (yunge-reader-pdf--page-count) 3)
                      (yunge-reader-pdf-smoke--settled-p)
                      (yunge-reader-pdf-smoke--centered-p))
                 (let ((location (yunge-reader-pdf-smoke--location)))
                   (setq yunge-reader-pdf-smoke--initial
                         (list
                          :width (yunge-reader-pdf-smoke--width)
                          :scale yunge-reader-effective-scale
                          :location location))
                   (yunge-reader-pdf-smoke--observation 'fit-width)
                   (yunge-reader-pdf-smoke--resize
                    760 780 'fit-width-resized))
               (yunge-reader-pdf-smoke--continue)))
            ('fit-width-resized
             (if (and (yunge-reader-pdf-smoke--settled-p)
                      (yunge-reader-pdf-smoke--centered-p)
                      (/= (yunge-reader-pdf-smoke--width)
                          (plist-get
                           yunge-reader-pdf-smoke--initial :width))
                      (yunge-reader-pdf-smoke--different-number-p
                       yunge-reader-effective-scale
                       (plist-get
                        yunge-reader-pdf-smoke--initial :scale))
                      (yunge-reader-pdf-smoke--same-location-p
                       (yunge-reader-pdf-smoke--location)
                       (plist-get
                        yunge-reader-pdf-smoke--initial :location)))
                 (progn
                   (yunge-reader-pdf-smoke--observation
                    'fit-width-resized)
                   (yunge-reader-fit-page)
                   (setq yunge-reader-pdf-smoke--phase 'fit-page
                         yunge-reader-pdf-smoke--not-before
                         (+ (float-time) 0.5))
                   (yunge-reader-pdf-smoke--continue))
               (yunge-reader-pdf-smoke--continue)))
            ('fit-page
             (if (and (eq yunge-reader-zoom-mode 'fit-page)
                      (yunge-reader-pdf-smoke--settled-p)
                      (yunge-reader-pdf-smoke--centered-p))
                 (progn
                   (setq yunge-reader-pdf-smoke--fit-page
                         (list
                          :width (yunge-reader-pdf-smoke--width)
                          :scale yunge-reader-effective-scale
                          :location
                          (yunge-reader-pdf-smoke--location)))
                   (yunge-reader-pdf-smoke--observation 'fit-page)
                   (yunge-reader-pdf-smoke--resize
                    760 560 'fit-page-resized))
               (yunge-reader-pdf-smoke--continue)))
            ('fit-page-resized
             (if (and (yunge-reader-pdf-smoke--settled-p)
                      (yunge-reader-pdf-smoke--centered-p)
                      (/= (yunge-reader-pdf-smoke--width)
                          (plist-get
                           yunge-reader-pdf-smoke--fit-page :width))
                      (yunge-reader-pdf-smoke--different-number-p
                       yunge-reader-effective-scale
                       (plist-get
                        yunge-reader-pdf-smoke--fit-page :scale))
                      (yunge-reader-pdf-smoke--same-location-p
                       (yunge-reader-pdf-smoke--location)
                       (plist-get
                        yunge-reader-pdf-smoke--fit-page :location)))
                 (progn
                   (yunge-reader-pdf-smoke--observation
                    'fit-page-resized)
                   (yunge-reader--set-manual-scale 1.0)
                   (setq yunge-reader-pdf-smoke--phase 'manual
                         yunge-reader-pdf-smoke--not-before
                         (+ (float-time) 0.5))
                   (yunge-reader-pdf-smoke--continue))
               (yunge-reader-pdf-smoke--continue)))
            ('manual
             (if (and (eq yunge-reader-zoom-mode 'manual)
                      (= yunge-reader-scale 1.0)
                      (< (abs (- yunge-reader-effective-scale 1.0))
                         0.01)
                      (yunge-reader-pdf-smoke--settled-p))
                 (let ((target
                        (make-yunge-reader-position
                         :unit 0 :x 0.0 :y 600.0)))
                   (yunge-reader-pdf--restore-location
                    yunge-reader-document target
                    (yunge-reader-pdf-smoke--window))
                   (setq yunge-reader-pdf-smoke--phase 'manual-anchor
                         yunge-reader-pdf-smoke--not-before
                         (+ (float-time) 0.2))
                   (yunge-reader-pdf-smoke--continue))
               (yunge-reader-pdf-smoke--continue)))
            ('manual-anchor
             (let ((location (yunge-reader-pdf-smoke--location)))
               (if (and (yunge-reader-pdf-smoke--settled-p)
                        (yunge-reader-pdf-smoke--same-location-p
                         location
                         (make-yunge-reader-position
                          :unit 0 :x 0.0 :y 600.0)))
                   (progn
                     (setq yunge-reader-pdf-smoke--manual-location
                           location)
                     (yunge-reader-pdf-smoke--observation 'manual)
                     (yunge-reader-pdf-smoke--resize
                      920 700 'manual-resized))
                 (yunge-reader-pdf-smoke--continue))))
            ('manual-resized
             (if (and (yunge-reader-pdf-smoke--settled-p)
                      (yunge-reader-pdf-smoke--centered-p)
                      (eq yunge-reader-zoom-mode 'manual)
                      (= yunge-reader-scale 1.0)
                      (< (abs (- yunge-reader-effective-scale 1.0))
                         0.01)
                      (yunge-reader-pdf-smoke--same-location-p
                       (yunge-reader-pdf-smoke--location)
                       yunge-reader-pdf-smoke--manual-location))
                 (progn
                   (yunge-reader-pdf-smoke--observation
                    'manual-resized)
                   (unless (eq major-mode 'yunge-reader-mode)
                     (error "PDF did not remain in Reader mode"))
                   (when (buffer-modified-p)
                     (error "PDF Reader buffer became modified"))
                   (setq yunge-reader-pdf-smoke--original-render-path
                         (yunge-reader-pdf-smoke--render-path)
                         yunge-reader-pdf-smoke--phase 'themed
                         yunge-reader-pdf-smoke--not-before
                         (+ (float-time) 0.2))
                   (yunge-reader-set-document-appearance 'follow-emacs)
                   (yunge-reader-pdf-smoke--continue))
               (yunge-reader-pdf-smoke--continue)))
            ('themed
             (if (and (yunge-reader-pdf-smoke--settled-p)
                      (eq (yunge-reader-effective-appearance)
                          'follow-emacs)
                      (not
                       (equal (yunge-reader-pdf-smoke--render-path)
                              yunge-reader-pdf-smoke--original-render-path))
                      (yunge-reader-pdf-smoke--same-location-p
                       (yunge-reader-pdf-smoke--location)
                       yunge-reader-pdf-smoke--manual-location))
                 (progn
                   (yunge-reader-pdf-smoke--observation 'themed)
                   (setq yunge-reader-pdf-smoke--phase
                         'original-restored
                         yunge-reader-pdf-smoke--not-before
                         (+ (float-time) 0.2))
                   (yunge-reader-set-document-appearance 'original)
                   (yunge-reader-pdf-smoke--continue))
               (yunge-reader-pdf-smoke--continue)))
            ('original-restored
             (if (and (yunge-reader-pdf-smoke--settled-p)
                      (eq (yunge-reader-effective-appearance) 'original)
                      (equal (yunge-reader-pdf-smoke--render-path)
                             yunge-reader-pdf-smoke--original-render-path)
                      (yunge-reader-pdf-smoke--same-location-p
                       (yunge-reader-pdf-smoke--location)
                       yunge-reader-pdf-smoke--manual-location))
                 (progn
                   (yunge-reader-pdf-smoke--observation
                    'original-restored)
                   (yunge-reader-pdf-smoke--finish))
               (yunge-reader-pdf-smoke--continue)))))))
    (error
     (yunge-reader-pdf-smoke--finish error-data))))

(defun yunge-reader-pdf-smoke--launch ()
  "Build the helper, generate the fixture, and start the smoke."
  (condition-case error-data
      (progn
        (unless (display-graphic-p)
          (error "PDF smoke requires a graphical Emacs"))
        (yunge-reader-graphical-smoke-build-helper
         yunge-reader-pdf-smoke--context)
        (unless (file-regular-p (yunge-reader-native-pdfium-library))
          (error "Run M-x yunge-reader-native-setup before the PDF smoke"))
        (setq yunge-reader-pdf-smoke--file
              (expand-file-name
               "graphical-fixture.pdf"
               (yunge-reader-graphical-smoke-context-temporary-root
                yunge-reader-pdf-smoke--context))
              yunge-reader-pdf-smoke--deadline (+ (float-time) 45))
        (yunge-reader-pdf-smoke--write-fixture
         yunge-reader-pdf-smoke--file)
        (advice-add 'display-warning :around
                    #'yunge-reader-pdf-smoke--warning)
        (set-frame-size nil 1000 800 t)
        (set-frame-parameter nil 'visibility nil)
        (setq yunge-reader-pdf-smoke--buffer
              (find-file-noselect yunge-reader-pdf-smoke--file))
        (switch-to-buffer yunge-reader-pdf-smoke--buffer)
        (yunge-reader-pdf-smoke--continue))
    (error
     (yunge-reader-pdf-smoke--finish error-data))))

(unless noninteractive
  (yunge-reader-pdf-smoke--launch))

;;; yunge-reader-pdf-smoke.el ends here
