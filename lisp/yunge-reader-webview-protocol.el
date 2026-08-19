;;; yunge-reader-webview-protocol.el --- Protocol -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-reader-native)

(define-error 'yunge-reader-webview-native-error
  "The Yunge Reader WebView helper reported an error")

(defconst yunge-reader-webview-protocol-version 1
  "WebView protocol version understood by this client.")

(defconst yunge-reader-webview--max-location-text-bytes 3072
  "Maximum byte length of one EPUB locator text field.")

(defconst yunge-reader-webview--max-selection-characters 1048576
  "Maximum character length of one EPUB text selection.")

(defconst yunge-reader-webview--max-selection-character-limit 65536
  "Maximum characters requested in one EPUB selection batch.")

(defconst yunge-reader-webview--max-search-query-characters 256
  "Maximum characters accepted in one EPUB search query.")

(defconst yunge-reader-webview--max-search-match-limit 200
  "Maximum matches accepted in one EPUB search batch.")

(defconst yunge-reader-webview--max-search-section-limit 64
  "Maximum spine items searched in one EPUB search batch.")

(defconst yunge-reader-webview--max-search-cursor-offset 1048576
  "Maximum transient match ordinal in one EPUB spine item.")

(defconst yunge-reader-webview--max-search-match-text-bytes 16384
  "Maximum byte length of one EPUB search match text.")

(defconst yunge-reader-webview--max-search-context-bytes 4096
  "Maximum byte length of one EPUB search context field.")

(defconst yunge-reader-webview--max-outline-depth 256
  "Maximum nesting depth accepted from one EPUB outline.")

(defconst yunge-reader-webview--max-outline-items 4096
  "Maximum number of items accepted from one EPUB outline.")

(defconst yunge-reader-webview--max-outline-title-bytes 1024
  "Maximum byte length of one EPUB outline title.")

(defconst yunge-reader-webview--epub-style-keys
  '(font-scale line-height content-width side-padding)
  "Semantic fields in one EPUB reading style.")

(defconst yunge-reader-webview--epub-appearance-color-keys
  '(foreground background link selection-foreground
    selection-background search-background)
  "Color fields required by one follow-Emacs EPUB appearance.")

(defconst yunge-reader-webview--epub-fixed-scale-min 0.25
  "Minimum manual scale accepted for a fixed-layout EPUB.")

(defconst yunge-reader-webview--epub-fixed-scale-max 8.0
  "Maximum manual scale accepted for a fixed-layout EPUB.")

(defconst yunge-reader-webview--epub-viewport-coordinate-max 1000000.0
  "Maximum unscaled EPUB viewport coordinate accepted from the renderer.")

(defconst yunge-reader-webview--epub-fixed-fit-modes
  '(fit-page fit-width)
  "Automatic fit modes accepted for a fixed-layout EPUB.")

(defconst yunge-reader-webview--scroll-bar-modes '(hidden visible)
  "Resolved display modes for an EPUB spine-item scroll bar.")

(defconst yunge-reader-webview--accelerators
  '("+" "-" "=" "<escape>" "<next>" "<prior>" "C-d" "C-g" "C-u"
    "G" "J" "K" "M-m" "SPC" "g" "j" "k" "y")
  "Normalized keys accepted from the WebView helper.")

(defun yunge-reader-webview--validate-ready (message)
  "Validate WebView helper ready MESSAGE."
  (let ((expected (yunge-reader-native--build-id))
        (platform
         (pcase system-type
           ('windows-nt "windows")
           ('darwin "macos")))
        (engine
         (pcase system-type
           ('windows-nt "webview2")
           ('darwin "wkwebview"))))
    (unless expected
      (error "Yunge Reader native source hash is unavailable"))
    (unless
        (and
         (equal (alist-get 'kind message) "webview-ready")
         (= (or (alist-get 'protocol message) -1)
            yunge-reader-webview-protocol-version)
         (equal (alist-get 'build-id message) expected)
         platform engine
         (equal (alist-get 'platform message) platform)
         (equal (alist-get 'engine message) engine)
         (equal (alist-get 'accelerators message)
                yunge-reader-webview--accelerators)
         (cl-every
          (lambda (capability)
            (member capability (alist-get 'capabilities message)))
          '("publication-close" "publication-info" "publication-open"
            "publication-resources" "view-appearance" "view-bounds"
            "view-clear-selection" "view-create"
            "view-destroy" "view-events" "view-focus"
            "view-focus-parent" "view-info"
            "view-navigate" "view-open-publication"
            "view-search"
            "view-search-result"
            "view-selection-text"
            "view-set-selection"
            "view-scroll-bars" "view-status" "view-style"
            "view-visible" "view-zoom")))
      (error
       "Incompatible Yunge Reader WebView helper: %S"
       message))))

(defun yunge-reader-webview--response-error (message)
  "Return an Emacs error value represented by response MESSAGE."
  (let ((object (alist-get 'error message)))
    (list
     'yunge-reader-webview-native-error
     (or (alist-get 'code object) "webview-error")
     (or (alist-get 'message object)
         "The Yunge Reader WebView helper failed"))))

(defun yunge-reader-webview--valid-location-p (location)
  "Return non-nil when LOCATION is a bounded EPUB locator."
  (and
   (listp location)
   (let ((cfi (alist-get 'cfi location))
         (href (alist-get 'href location))
         (fraction (alist-get 'fraction location))
         (x (alist-get 'x location))
         (y (alist-get 'y location)))
     (and
      (cl-every
       (lambda (entry)
         (memq (car-safe entry) '(cfi href fraction x y)))
       location)
      (cl-every
       (lambda (value)
         (and (stringp value)
              (not (string-empty-p value))
              (<= (string-bytes value)
                  yunge-reader-webview--max-location-text-bytes)
              (not (string-match-p "[[:cntrl:]]" value))))
       (list cfi href))
      (string-prefix-p "epubcfi(" cfi)
      (string-suffix-p ")" cfi)
      (yunge-reader-webview--valid-target-href-p href)
      (not (string-match-p "#" href))
      (or (null fraction)
          (and (numberp fraction)
               (= fraction fraction)
               (<= 0 fraction 1)))
      (or
       (and (null x) (null y))
       (and
        (numberp x)
        (numberp y)
        (= x x)
        (= y y)
        (<= 0 x yunge-reader-webview--epub-viewport-coordinate-max)
        (<= 0 y
            yunge-reader-webview--epub-viewport-coordinate-max)))))))

(defun yunge-reader-webview--valid-target-href-p (href)
  "Return non-nil when HREF is a bounded internal EPUB target."
  (and
   (stringp href)
   (not (string-empty-p href))
   (<= (string-bytes href)
       yunge-reader-webview--max-location-text-bytes)
   (not (string-match-p "[[:cntrl:]\\\\?]" href))
   (<= (cl-count ?# href) 1)
   (let ((path (car (split-string href "#"))))
     (and
      (not (string-empty-p path))
      (not (string-prefix-p "/" path))
      (not (string-match-p ":" path))
      (cl-every
       (lambda (part)
         (not (member part '("" "." ".."))))
       (split-string path "/"))))))

(defun yunge-reader-webview--valid-target-p (target)
  "Return non-nil when TARGET is a bounded EPUB navigation target."
  (or
   (yunge-reader-webview--valid-location-p target)
   (and
    (listp target)
    (equal (mapcar #'car-safe target) '(href))
    (yunge-reader-webview--valid-target-href-p
     (alist-get 'href target)))))

(defun yunge-reader-webview--valid-outline-item-p (item)
  "Return non-nil when ITEM is a bounded EPUB outline item."
  (and
   (listp item)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(title depth href)))
    item)
   (let ((title (alist-get 'title item))
         (depth (alist-get 'depth item))
         (href (alist-get 'href item)))
     (and
      (stringp title)
      (not (string-empty-p (string-trim title)))
      (<= (string-bytes title)
          yunge-reader-webview--max-outline-title-bytes)
      (not (string-match-p "[[:cntrl:]]" title))
      (natnump depth)
      (<= depth yunge-reader-webview--max-outline-depth)
      (or (null href)
          (yunge-reader-webview--valid-target-href-p href))))))

(defun yunge-reader-webview--valid-outline-p (outline)
  "Return non-nil when OUTLINE is bounded renderer outline data."
  (and
   (listp outline)
   (assq 'items outline)
   (assq 'truncated outline)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(items truncated)))
    outline)
   (let ((items (alist-get 'items outline))
         (truncated (alist-get 'truncated outline)))
     (and
      (listp items)
      (<= (length items)
          yunge-reader-webview--max-outline-items)
      (cl-every #'yunge-reader-webview--valid-outline-item-p items)
      (memq truncated '(nil t))))))

(defun yunge-reader-webview--check-location (location)
  "Return LOCATION or signal when it is not a valid EPUB locator."
  (unless (yunge-reader-webview--valid-location-p location)
    (error "Invalid EPUB location: %S" location))
  location)

(defun yunge-reader-webview--check-target (target)
  "Return TARGET or signal when it is not a valid EPUB target."
  (unless (yunge-reader-webview--valid-target-p target)
    (error "Invalid EPUB navigation target: %S" target))
  target)

(defun yunge-reader-webview--valid-selection-p (selection)
  "Return non-nil when SELECTION is one bounded EPUB spine range."
  (and
   (listp selection)
   (= (length selection) 3)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(href start end)))
    selection)
   (cl-every (lambda (key) (assq key selection))
             '(href start end))
   (let ((href (alist-get 'href selection))
         (start (alist-get 'start selection))
         (end (alist-get 'end selection)))
     (and
      (yunge-reader-webview--valid-target-href-p href)
      (not (string-match-p "#" href))
      (cl-every
       (lambda (cfi)
         (and
          (stringp cfi)
          (not (string-empty-p cfi))
          (<= (string-bytes cfi)
              yunge-reader-webview--max-location-text-bytes)
          (not (string-match-p "[[:cntrl:]]" cfi))
          (string-prefix-p "epubcfi(" cfi)
          (string-suffix-p ")" cfi)))
       (list start end))
      (not (equal start end))))))

(defun yunge-reader-webview--valid-selection-text-result-p
    (result offset character-limit)
  "Return non-nil when RESULT is a consistent EPUB text batch.
OFFSET and CHARACTER-LIMIT describe the request that produced RESULT."
  (and
   (listp result)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(text total next-offset done)))
    result)
   (assq 'text result)
   (assq 'total result)
   (assq 'done result)
   (= (length result)
      (if (assq 'next-offset result) 4 3))
   (let* ((text (alist-get 'text result))
          (total (alist-get 'total result))
          (done (alist-get 'done result))
          (next-entry (assq 'next-offset result))
          (next (cdr-safe next-entry))
          (expected (and (stringp text) (+ offset (length text)))))
     (and
      (stringp text)
      (natnump total)
      (<= total yunge-reader-webview--max-selection-characters)
      (<= (length text) character-limit)
      (<= offset total)
      (memq done '(nil t))
      (if done
          (and (null next-entry) (= expected total))
        (and
         (consp next-entry)
         (natnump next)
         (> (length text) 0)
         (= next expected)
         (< next total)))))))

(defun yunge-reader-webview--valid-search-cursor-p (cursor)
  "Return non-nil when CURSOR is a bounded transient EPUB search cursor."
  (and
   (listp cursor)
   (= (length cursor) 2)
   (cl-every (lambda (entry) (memq (car-safe entry) '(href offset)))
             cursor)
   (assq 'href cursor)
   (assq 'offset cursor)
   (let ((href (alist-get 'href cursor))
         (offset (alist-get 'offset cursor)))
     (and
      (yunge-reader-webview--valid-target-href-p href)
      (not (string-match-p "#" href))
      (or (null offset)
          (and (natnump offset)
               (<= offset
                   yunge-reader-webview--max-search-cursor-offset)))))))

(defun yunge-reader-webview--valid-search-match-p (match)
  "Return non-nil when MATCH is one bounded native EPUB search match."
  (and
   (listp match)
   (= (length match) 6)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(href start end text before after)))
    match)
   (cl-every (lambda (key) (assq key match))
             '(href start end text before after))
   (yunge-reader-webview--valid-selection-p
    `((href . ,(alist-get 'href match))
      (start . ,(alist-get 'start match))
      (end . ,(alist-get 'end match))))
   (let ((text (alist-get 'text match))
         (before (alist-get 'before match))
         (after (alist-get 'after match)))
     (and
      (stringp text)
      (not (string-empty-p text))
      (<= (string-bytes text)
          yunge-reader-webview--max-search-match-text-bytes)
      (stringp before)
      (<= (string-bytes before)
          yunge-reader-webview--max-search-context-bytes)
      (stringp after)
      (<= (string-bytes after)
          yunge-reader-webview--max-search-context-bytes)))))

(defun yunge-reader-webview--valid-search-result-p (result match-limit)
  "Return non-nil when RESULT is a bounded EPUB batch for MATCH-LIMIT."
  (and
   (listp result)
   (cl-every
    (lambda (entry)
      (memq (car-safe entry) '(matches cursor done)))
    result)
   (assq 'matches result)
   (assq 'done result)
   (= (length result) (if (assq 'cursor result) 3 2))
   (let ((matches (alist-get 'matches result))
         (cursor-entry (assq 'cursor result))
         (done (alist-get 'done result)))
     (and
      (proper-list-p matches)
      (<= (length matches) match-limit)
      (cl-every #'yunge-reader-webview--valid-search-match-p matches)
      (memq done '(nil t))
      (if done
          (null cursor-entry)
        (and
         cursor-entry
         (yunge-reader-webview--valid-search-cursor-p
          (cdr cursor-entry))))))))

(defun yunge-reader-webview--valid-style-p (style)
  "Return non-nil when STYLE is a bounded EPUB reading style."
  (and
   (listp style)
   (= (length style)
      (length yunge-reader-webview--epub-style-keys))
   (cl-every
    (lambda (entry)
      (memq (car-safe entry)
            yunge-reader-webview--epub-style-keys))
    style)
   (cl-every (lambda (key) (assq key style))
             yunge-reader-webview--epub-style-keys)
   (let ((font-scale (alist-get 'font-scale style))
         (line-height (alist-get 'line-height style))
         (content-width (alist-get 'content-width style))
         (side-padding (alist-get 'side-padding style)))
     (and
      (numberp font-scale)
      (= font-scale font-scale)
      (<= 0.5 font-scale 3.0)
      (numberp line-height)
      (= line-height line-height)
      (<= 1.0 line-height 3.0)
      (integerp content-width)
      (<= 320 content-width 1600)
      (numberp side-padding)
      (= side-padding side-padding)
      (<= 0 side-padding 20)))))

(defun yunge-reader-webview--check-style (style)
  "Return STYLE or signal when it is not a valid EPUB reading style."
  (unless (yunge-reader-webview--valid-style-p style)
    (error "Invalid EPUB reading style: %S" style))
  style)

(defun yunge-reader-webview--valid-color-p (color)
  "Return non-nil when COLOR is one normalized CSS RGB color."
  (let ((case-fold-search nil))
    (and (stringp color)
         (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" color))))

(defun yunge-reader-webview--valid-appearance-p (appearance)
  "Return non-nil when APPEARANCE is bounded EPUB surface data."
  (and
   (listp appearance)
   (assq 'mode appearance)
   (let ((mode (alist-get 'mode appearance)))
     (pcase mode
       ('original
        (equal (mapcar #'car-safe appearance) '(mode)))
       ('follow-emacs
        (and
         (= (length appearance)
            (1+ (length
                 yunge-reader-webview--epub-appearance-color-keys)))
         (cl-every
          (lambda (entry)
            (memq (car-safe entry)
                  (cons
                   'mode
                   yunge-reader-webview--epub-appearance-color-keys)))
          appearance)
         (cl-every
          (lambda (key)
            (and (assq key appearance)
                 (yunge-reader-webview--valid-color-p
                  (alist-get key appearance))))
          yunge-reader-webview--epub-appearance-color-keys)))
       (_ nil)))))

(defun yunge-reader-webview--check-appearance (appearance)
  "Return APPEARANCE or signal when it is not valid for EPUB."
  (unless (yunge-reader-webview--valid-appearance-p appearance)
    (error "Invalid EPUB appearance: %S" appearance))
  appearance)

(defun yunge-reader-webview--valid-fixed-zoom-p (zoom)
  "Return non-nil when ZOOM is a bounded fixed-layout EPUB zoom."
  (or
   (memq zoom yunge-reader-webview--epub-fixed-fit-modes)
   (and
    (numberp zoom)
    (= zoom zoom)
    (<= yunge-reader-webview--epub-fixed-scale-min
        zoom
        yunge-reader-webview--epub-fixed-scale-max))))

(defun yunge-reader-webview--check-fixed-zoom (zoom)
  "Return ZOOM or signal when it is not a fixed-layout EPUB zoom."
  (unless (yunge-reader-webview--valid-fixed-zoom-p zoom)
    (error "Invalid EPUB fixed-layout zoom: %S" zoom))
  zoom)

(defun yunge-reader-webview--check-scroll-bar-mode (mode)
  "Return resolved EPUB scroll bar MODE or signal."
  (unless (memq mode yunge-reader-webview--scroll-bar-modes)
    (error "Invalid EPUB scroll bar mode: %S" mode))
  mode)

(provide 'yunge-reader-webview-protocol)

;;; yunge-reader-webview-protocol.el ends here
