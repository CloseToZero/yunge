;;; yunge-reader-pdf-protocol.el --- PDF protocol models -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-reader)

(defconst yunge-reader-pdf-link-maximum-items 4096
  "Maximum number of links accepted from one PDF page response.")

(defun yunge-reader-pdf--native-appearance (appearance)
  "Return APPEARANCE encoded for the native JSON boundary."
  (pcase (alist-get 'mode appearance)
    ('original '((mode . "original")))
    ('follow-emacs
     `((mode . "follow-emacs")
       (foreground . ,(alist-get 'foreground appearance))
       (background . ,(alist-get 'background appearance))))
    (mode (error "Invalid PDF appearance mode: %S" mode))))


(cl-defstruct yunge-reader-pdf-link
  "One PDF page link with disposable hit geometry."
  page
  index
  bounds
  label
  action)

(cl-defstruct yunge-reader-pdf-link-data
  "One bounded page of PDF links."
  page
  links
  truncated)


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

(defun yunge-reader-pdf--native-search-position-p (value)
  "Return non-nil when VALUE is a native PDF search position."
  (and (proper-list-p value)
       (= (length value) 2)
       (cl-every (lambda (entry)
                   (memq (car-safe entry) '(page offset)))
                 value)
       (assq 'page value)
       (assq 'offset value)
       (natnump (alist-get 'page value))
       (let ((offset (alist-get 'offset value)))
         (or (null offset) (natnump offset)))))

(defun yunge-reader-pdf--native-search-batch (value)
  "Return a generic search batch represented by native VALUE."
  (let ((results
         (mapcar #'yunge-reader-pdf--native-search-result
                 (alist-get 'matches value)))
        (cursor-value (alist-get 'cursor value)))
    (when (and (cl-every #'identity results)
               (or (null cursor-value)
                   (yunge-reader-pdf--native-search-position-p
                    cursor-value)))
      (make-yunge-reader-search-batch
       :results results
       :cursor (and cursor-value
                    (make-yunge-reader-search-cursor
                     :value (copy-tree cursor-value)))
       :done (eq (alist-get 'done value) t)))))

(defun yunge-reader-pdf--search-cursor-value (cursor)
  "Return native PDF value from generic search CURSOR, or nil."
  (when cursor
    (and (yunge-reader-search-cursor-p cursor)
         (let ((value (yunge-reader-search-cursor-value cursor)))
           (and (yunge-reader-pdf--native-search-position-p value)
                (copy-tree value))))))


(defun yunge-reader-pdf--native-selection-batch (value)
  "Return a generic selection batch represented by native VALUE."
  (let ((cursor-value (alist-get 'cursor value)))
    (make-yunge-reader-selection-batch
     :text (alist-get 'text value)
     :cursor (and cursor-value
                  (yunge-reader-pdf--native-position cursor-value))
     :done (eq (alist-get 'done value) t))))

(defun yunge-reader-pdf--indexed-position-parameters (position)
  "Return native indexed parameters for reader POSITION, or nil."
  (let ((page (and (yunge-reader-position-p position)
                   (yunge-reader-position-unit position)))
        (offset (and (yunge-reader-position-p position)
                     (yunge-reader-position-offset position))))
    (when (and (natnump page) (natnump offset))
      (list (cons 'page page) (cons 'offset offset)))))

(defun yunge-reader-pdf--outline-page-info (document page)
  "Return metadata for zero-based PAGE in PDF DOCUMENT."
  (let* ((metadata (yunge-reader-document-metadata document))
         (pages (plist-get metadata :pages)))
    (and (natnump page)
         (listp pages)
         (< page (length pages))
         (nth page pages))))

(defun yunge-reader-pdf--outline-view-state (destination)
  "Return generic zoom mode and scale for native DESTINATION."
  (let ((view (alist-get 'view destination))
        (zoom (alist-get 'zoom destination)))
    (pcase view
      ("xyz"
       (if (and (numberp zoom) (> zoom 0))
           (cons 'manual zoom)
         '(nil)))
      ((or "fit" "fit-bounds") '(fit-page))
      ((or "fit-horizontal" "fit-bounds-horizontal")
       '(fit-width))
      (_ '(nil)))))

(defun yunge-reader-pdf--native-location-action
    (document destination)
  "Return a generic location action for PDF DESTINATION in DOCUMENT."
  (let* ((page (alist-get 'page destination))
         (page-info
          (yunge-reader-pdf--outline-page-info document page))
         (height (and page-info (alist-get 'height page-info)))
         (x (alist-get 'x destination))
         (y (alist-get 'y destination))
         (view-state
          (yunge-reader-pdf--outline-view-state destination)))
    (when (and page-info
               (numberp height)
               (> height 0)
               (or (null x) (numberp x))
               (or (null y) (numberp y)))
      (make-yunge-reader-action
       :type 'location
       :position
       (make-yunge-reader-position
        :unit page
        :x (or x 0.0)
        :y (or y height))
       :zoom-mode (car view-state)
       :scale (cdr view-state)))))

(defun yunge-reader-pdf--native-outline-item (document value)
  "Return a generic outline item represented by native VALUE."
  (let ((title (alist-get 'title value))
        (depth (alist-get 'depth value))
        (destination (alist-get 'destination value)))
    (when (and (stringp title)
               (not (string-empty-p (string-trim title)))
               (natnump depth)
               (or (null destination)
                   (listp destination)))
      (let ((action
             (and destination
                  (yunge-reader-pdf--native-location-action
                   document destination))))
        (when (or (null destination) action)
          (make-yunge-reader-outline-item
           :title title
           :depth depth
           :action action))))))

(defun yunge-reader-pdf--native-outline (document value)
  "Return a generic PDF outline represented by native VALUE."
  (let* ((items-entry (assq 'items value))
         (native-items (cdr items-entry))
         (items
          (and (listp native-items)
               (mapcar
                (lambda (item)
                  (yunge-reader-pdf--native-outline-item
                   document item))
                native-items))))
    (when (and items-entry
               (listp native-items)
               (cl-every #'identity items))
      (make-yunge-reader-outline-data
       :items items
       :truncated (eq (alist-get 'truncated value) t)))))

(defun yunge-reader-pdf--native-link-bounds (value)
  "Return validated canonical PDF link bounds represented by VALUE."
  (let ((left (alist-get 'left value))
        (bottom (alist-get 'bottom value))
        (right (alist-get 'right value))
        (top (alist-get 'top value)))
    (when (and (numberp left)
               (numberp bottom)
               (numberp right)
               (numberp top)
               (< left right)
               (< bottom top))
      (list
       (cons 'left left)
       (cons 'bottom bottom)
       (cons 'right right)
       (cons 'top top)))))

(defun yunge-reader-pdf--native-link-action (document value)
  "Return a generic Reader action represented by native link VALUE."
  (let ((action
         (pcase (alist-get 'type value)
           ("location"
            (when-let* ((destination
                         (alist-get 'destination value)))
              (yunge-reader-pdf--native-location-action
               document destination)))
           ("uri"
            (make-yunge-reader-action
             :type 'uri
             :uri (alist-get 'uri value))))))
    (and (yunge-reader--action-valid-p action) action)))

(defun yunge-reader-pdf--native-page-link
    (document page index value)
  "Return PAGE link INDEX represented by native VALUE in DOCUMENT."
  (let ((bounds
         (yunge-reader-pdf--native-link-bounds
          (alist-get 'bounds value)))
        (native-action (alist-get 'action value))
        (label (alist-get 'label value)))
    (when (and bounds
               (listp native-action)
               (or (null label)
                   (and (stringp label)
                        (not (string-empty-p label)))))
      (when-let* ((action
                   (yunge-reader-pdf--native-link-action
                    document native-action)))
        (make-yunge-reader-pdf-link
         :page page
         :index index
         :bounds bounds
         :label label
         :action action)))))

(defun yunge-reader-pdf--native-page-links
    (document expected-page value)
  "Return PDF links for EXPECTED-PAGE represented by native VALUE."
  (let* ((page (alist-get 'page value))
         (links-entry (assq 'links value))
         (native-links (cdr links-entry))
         (links
          (and (listp native-links)
               (cl-loop
                for item in native-links
                for index from 0
                collect
                (yunge-reader-pdf--native-page-link
                 document page index item)))))
    (when (and (eql page expected-page)
               links-entry
               (listp native-links)
               (<= (length native-links)
                   yunge-reader-pdf-link-maximum-items)
               (cl-every #'identity links))
      (make-yunge-reader-pdf-link-data
       :page page
       :links links
       :truncated (eq (alist-get 'truncated value) t)))))


(provide 'yunge-reader-pdf-protocol)

;;; yunge-reader-pdf-protocol.el ends here
