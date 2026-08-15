;;; yunge-reader-pdf.el --- PDF reader -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-reader)
(require 'yunge-reader-native)

(defcustom yunge-reader-pdf-page-margin 24
  "Pixel margin reserved around a rendered PDF page."
  :type 'natnum
  :group 'yunge-reader)

(defconst yunge-reader-pdf--points-to-pixels (/ 96.0 72.0)
  "Nominal conversion from PDF points to screen pixels at scale one.")

(defvar-local yunge-reader-pdf-page 0
  "Zero-based page currently displayed in the PDF adapter.")

(defvar-local yunge-reader-pdf--generation 0
  "Generation used to reject late PDF rendering completions.")

(defvar-local yunge-reader-pdf--page-info nil
  "Native geometry for the page currently being rendered.")

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
        (add-hook 'yunge-reader-refresh-hook
                  #'yunge-reader-pdf--refresh nil t)
        (add-hook 'window-size-change-functions
                  #'yunge-reader-pdf--window-size-change nil t))
    (remove-hook 'yunge-reader-refresh-hook
                 #'yunge-reader-pdf--refresh t)
    (remove-hook 'window-size-change-functions
                 #'yunge-reader-pdf--window-size-change t)))

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
       (funcall
        complete nil
        (yunge-reader-pdf--native-error
         "PDF text selection is not implemented yet")))
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

(defun yunge-reader-pdf--display-image
    (generation page result error-data)
  "Display page RESULT for GENERATION and PAGE, or show ERROR-DATA."
  (when (= generation yunge-reader-pdf--generation)
    (if error-data
        (yunge-reader--display-status
         "Could not render page %d:\n\n%s"
         (1+ page) (error-message-string error-data))
      (condition-case image-error
          (let* ((path (alist-get 'path result))
                 (image (create-image path nil nil)))
            (unless image
              (error "Emacs cannot display the rendered PNG"))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-image image "[rendered PDF page]")
              (goto-char (point-min)))
            (setq header-line-format
                  (format " Page %d/%d  %.0f%% "
                          (1+ page)
                          (yunge-reader-pdf--page-count)
                          (* 100 yunge-reader-effective-scale))))
        (error
         (yunge-reader--display-status
          "Could not display page %d:\n\n%s"
          (1+ page) (error-message-string image-error)))))))

(defun yunge-reader-pdf--render-with-info
    (generation page page-info error-data)
  "Render PAGE for GENERATION using PAGE-INFO, or show ERROR-DATA."
  (when (= generation yunge-reader-pdf--generation)
    (if error-data
        (yunge-reader--display-status
         "Could not inspect page %d:\n\n%s"
         (1+ page) (error-message-string error-data))
      (setq yunge-reader-pdf--page-info page-info)
      (let ((width (yunge-reader-pdf--target-width page-info)))
        (let ((buffer (current-buffer)))
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
                  generation page result render-error))))))))))

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
