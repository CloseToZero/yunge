;;; yunge-reader-webview-renderer.el --- Renderer -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-reader-webview-protocol)
(require 'yunge-reader-webview-view)

(declare-function yunge-reader-webview--request
                  "yunge-reader-webview-service"
                  (operation parameters complete))
(defun yunge-reader-webview--open-publication (path callback)
  "Open the local EPUB at PATH and invoke CALLBACK with its result."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (not (file-remote-p path)))
    (error "EPUB publication path must be absolute and local"))
  (yunge-reader-webview--request
   "publication-open" `((path . ,(expand-file-name path))) callback))

(defun yunge-reader-webview--publication-info (publication callback)
  "Query PUBLICATION and invoke CALLBACK with its result."
  (unless (and (integerp publication) (> publication 0))
    (error "Invalid EPUB publication ID: %S" publication))
  (yunge-reader-webview--request
   "publication-info" `((publication . ,publication)) callback))

(defun yunge-reader-webview--close-publication (publication callback)
  "Close PUBLICATION and invoke CALLBACK with its result."
  (unless (and (integerp publication) (> publication 0))
    (error "Invalid EPUB publication ID: %S" publication))
  (yunge-reader-webview--request
   "publication-close" `((publication . ,publication)) callback))

(defun yunge-reader-webview--selection-text-complete
    (offset character-limit complete result error-data)
  "Validate one EPUB text RESULT before invoking COMPLETE.
OFFSET and CHARACTER-LIMIT are the corresponding request bounds."
  (cond
   (error-data
    (funcall complete nil error-data))
   ((yunge-reader-webview--valid-selection-text-result-p
     result offset character-limit)
    (funcall complete result nil))
   (t
    (funcall
     complete nil
     (list 'error
           (format "Malformed EPUB selection text result: %S" result))))))

(defun yunge-reader-webview--request-selection-text
    (view selection offset character-limit complete)
  "Request one text batch for SELECTION in native VIEW.
OFFSET is a transient Unicode character cursor.  CHARACTER-LIMIT bounds the
returned batch, and COMPLETE receives the validated native result."
  (unless (and (integerp offset)
               (<= 0 offset
                   yunge-reader-webview--max-selection-characters))
    (error "Invalid EPUB selection offset: %S" offset))
  (unless
      (and
       (integerp character-limit)
       (<= 1 character-limit
           yunge-reader-webview--max-selection-character-limit))
    (error "Invalid EPUB selection character limit: %S" character-limit))
  (unless (functionp complete)
    (error "Invalid EPUB selection text completion: %S" complete))
  (yunge-reader-webview--request
   "view-selection-text"
   `((view . ,(yunge-reader-webview--view-id view))
     (selection . ,(copy-tree
                    (if (yunge-reader-webview--valid-selection-p selection)
                        selection
                      (error "Invalid EPUB selection: %S" selection))))
     (offset . ,offset)
     (character-limit . ,character-limit))
   (apply-partially
    #'yunge-reader-webview--selection-text-complete
    offset character-limit complete)))

(defun yunge-reader-webview--search-complete
    (match-limit complete result error-data)
  "Validate one native search RESULT before invoking COMPLETE."
  (cond
   (error-data
    (funcall complete nil error-data))
   ((yunge-reader-webview--valid-search-result-p result match-limit)
    (funcall complete result nil))
   (t
    (funcall
     complete nil
     (list 'error
           (format "Malformed EPUB search result: %S" result))))))

(defun yunge-reader-webview--request-search
    (view query case-sensitive direction origin cursor
          match-limit section-limit complete)
  "Request one bounded native EPUB search batch from VIEW."
  (unless
      (and
       (stringp query)
       (<= 1 (length query)
           yunge-reader-webview--max-search-query-characters)
       (not (string-match-p "[[:cntrl:]]" query)))
    (error "Invalid EPUB search query: %S" query))
  (unless (memq case-sensitive '(nil t))
    (error "Invalid EPUB search case flag: %S" case-sensitive))
  (unless (memq direction '(forward backward))
    (error "Invalid EPUB search direction: %S" direction))
  (unless (or (null origin)
              (yunge-reader-webview--valid-location-p origin))
    (error "Invalid EPUB search origin: %S" origin))
  (unless (or (null cursor)
              (yunge-reader-webview--valid-search-cursor-p cursor))
    (error "Invalid EPUB search cursor: %S" cursor))
  (when (and origin cursor)
    (error "EPUB search origin and cursor are mutually exclusive"))
  (unless
      (and
       (integerp match-limit)
       (<= 1 match-limit
           yunge-reader-webview--max-search-match-limit))
    (error "Invalid EPUB search match limit: %S" match-limit))
  (unless
      (and
       (integerp section-limit)
       (<= 1 section-limit
           yunge-reader-webview--max-search-section-limit))
    (error "Invalid EPUB search section limit: %S" section-limit))
  (unless (functionp complete)
    (error "Invalid EPUB search completion: %S" complete))
  (yunge-reader-webview--request
   "view-search"
   `((view . ,(yunge-reader-webview--view-id view))
     (query . ,query)
     (case-sensitive . ,(if case-sensitive t :false))
     (direction . ,(symbol-name direction))
     (origin . ,(copy-tree origin))
     (cursor . ,(copy-tree cursor))
     (match-limit . ,match-limit)
     (section-limit . ,section-limit))
   (apply-partially
    #'yunge-reader-webview--search-complete match-limit complete)))

(defun yunge-reader-webview--fixed-zoom-value (zoom)
  "Return protocol data for validated fixed-layout ZOOM."
  (let ((value (yunge-reader-webview--check-fixed-zoom zoom)))
    (if (symbolp value) (symbol-name value) value)))

(defun yunge-reader-webview--open-view-publication
    (view publication callback location style zoom bar-mode)
  "Open PUBLICATION in native VIEW with LOCATION and presentation state.
STYLE and ZOOM are mutually exclusive.  Invoke CALLBACK when complete."
  (yunge-reader-webview--request
   "view-open-publication"
   (append
    `((view . ,(yunge-reader-webview--view-id view))
      (publication . ,publication))
    (when location
      `((location . ,(yunge-reader-webview--check-location location))))
    (when style
      `((style . ,(yunge-reader-webview--check-style style))))
    (when zoom
      `((zoom . ,(yunge-reader-webview--fixed-zoom-value zoom))))
    `((scroll-bars
       . ,(if (eq (yunge-reader-webview--check-scroll-bar-mode
                   bar-mode)
                  'visible)
              t
            :false))))
   callback))

(defun yunge-reader-webview--navigate-view
    (view command callback &optional location)
  "Ask native VIEW to run semantic COMMAND and invoke CALLBACK.
LOCATION is required only for the go-to command."
  (unless (member command '("previous-page" "next-page"
                            "previous-screen" "next-screen"
                            "previous-line" "next-line"
                            "first" "last" "go-to"))
    (error "Unsupported EPUB navigation command: %S" command))
  (when (and (equal command "go-to") (null location))
    (error "EPUB go-to navigation requires a location"))
  (when (and location (not (equal command "go-to")))
    (error "EPUB non-go-to navigation does not accept a location"))
  (yunge-reader-webview--request
   "view-navigate"
   (append
    `((view . ,(yunge-reader-webview--view-id view))
      (command . ,command))
    (when location
      `((location . ,(yunge-reader-webview--check-target location)))))
   callback))

(defun yunge-reader-webview--set-native-view-style
    (view style callback)
  "Apply STYLE to native VIEW, then invoke CALLBACK."
  (yunge-reader-webview--request
   "view-style"
   `((view . ,(yunge-reader-webview--view-id view))
     (style . ,(yunge-reader-webview--check-style style)))
   callback))

(defun yunge-reader-webview--set-native-view-zoom
    (view zoom callback)
  "Apply fixed-layout ZOOM to native VIEW, then invoke CALLBACK."
  (yunge-reader-webview--request
   "view-zoom"
   `((view . ,(yunge-reader-webview--view-id view))
     (zoom . ,(yunge-reader-webview--fixed-zoom-value zoom)))
   callback))

(defun yunge-reader-webview--set-native-scroll-bar-mode
    (view mode callback)
  "Apply resolved scroll bar MODE to native VIEW, then invoke CALLBACK."
  (yunge-reader-webview--request
   "view-scroll-bars"
   `((view . ,(yunge-reader-webview--view-id view))
     (visible
      . ,(if (eq (yunge-reader-webview--check-scroll-bar-mode mode)
                 'visible)
             t
           :false)))
   callback))

(provide 'yunge-reader-webview-renderer)

;;; yunge-reader-webview-renderer.el ends here
