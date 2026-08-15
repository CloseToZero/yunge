;;; yunge-reader-pdf-test.el --- PDF reader tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-pdf)

(defun yunge-reader-pdf-test--document (&rest pages)
  "Return a fixed-layout reader document containing PAGES."
  (make-yunge-reader-document
   :metadata (list :page-count (length pages) :pages pages)))

(ert-deftest yunge-reader-pdf-uses-viewer-page-bindings ()
  (yunge-test-keymap-keys
   yunge-reader-pdf-view-mode-map
   '(("G" . yunge-reader-pdf-last-page)
     ("J" . yunge-reader-pdf-next-page)
     ("K" . yunge-reader-pdf-previous-page)
     ("gg" . yunge-reader-pdf-first-page)
     ("gp" . yunge-reader-pdf-goto-page)
     ("gr" . yunge-reader-refresh)))
  (should-not
   (eq (lookup-key yunge-reader-pdf-view-mode-map (kbd "n"))
       #'yunge-reader-pdf-next-page))
  (should-not
   (eq (lookup-key yunge-reader-pdf-view-mode-map (kbd "b"))
       #'yunge-reader-pdf-previous-page)))

(ert-deftest yunge-reader-pdf-integrates-page-bindings-with-evil ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (yunge-test-evil-keys
     'normal
     '(("/" . yunge-reader-search)
       ("G" . yunge-reader-pdf-last-page)
       ("J" . yunge-reader-pdf-next-page)
       ("K" . yunge-reader-pdf-previous-page)
       ("gg" . yunge-reader-pdf-first-page)
       ("gp" . yunge-reader-pdf-goto-page)
       ("gr" . yunge-reader-refresh)
       ("n" . yunge-reader-search-next)
       ("y" . yunge-reader-copy-selection)
       ("b" . evil-backward-word-begin)))
    (yunge-test-which-key-prefix
     "g" '(("g" nil "first page")
           ("p" nil "go to page")
           ("r" nil "refresh")))))

(ert-deftest yunge-reader-pdf-registers-only-when-requested ()
  (let ((yunge-reader-drivers nil)
        (modes auto-mode-alist))
    (should-not (yunge-reader-driver-for-file "book.pdf"))
    (yunge-reader-pdf-register)
    (should
     (eq (yunge-reader-driver-name
          (yunge-reader-driver-for-file "book.PDF"))
         'pdf))
    (should (equal auto-mode-alist modes))))

(ert-deftest yunge-reader-pdf-open-and-close-balance-native-lease ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let (opened
          properties
          error-data
          request
          (leases 0))
      (cl-letf (((symbol-function 'yunge-reader-native-acquire)
                 (lambda () (cl-incf leases)))
                ((symbol-function 'yunge-reader-native-release)
                 (lambda () (cl-decf leases)))
                ((symbol-function 'yunge-reader-native-live-p)
                 (lambda () t))
                ((symbol-function 'yunge-reader-native-request)
                 (lambda (operation parameters complete)
                   (setq request (list operation parameters))
                   (pcase operation
                     ("open"
                     (funcall complete
                               '((document . 7)
                                 (page-count . 3)
                                 (pages
                                  . (((page . 0)
                                      (width . 612.0)
                                      (height . 792.0))
                                     ((page . 1)
                                      (width . 612.0)
                                      (height . 792.0))
                                     ((page . 2)
                                      (width . 612.0)
                                      (height . 792.0))))
                                 (layout . "fixed"))
                               nil))
                     ("close"
                      (funcall complete '((closed . t)) nil))))))
        (yunge-reader-pdf--open
         "C:/books/test.pdf"
         (lambda (handle value error)
           (setq opened handle
                 properties value
                 error-data error)))
        (should (= leases 1))
        (should (= opened 7))
        (should-not error-data)
        (should (eq (plist-get properties :layout) 'fixed))
        (should (= (plist-get (plist-get properties :metadata)
                              :page-count)
                   3))
        (should (= (length
                    (plist-get (plist-get properties :metadata)
                               :pages))
                   3))
        (should (equal (car request) "open"))
        (yunge-reader-pdf--close
         (make-yunge-reader-document :handle opened))
        (should (zerop leases))
        (should (equal (car request) "close"))))))

(ert-deftest yunge-reader-pdf-open-failure-releases-native-lease ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((leases 0)
          completion-error)
      (cl-letf (((symbol-function 'yunge-reader-native-acquire)
                 (lambda () (cl-incf leases)))
                ((symbol-function 'yunge-reader-native-release)
                 (lambda () (cl-decf leases)))
                ((symbol-function 'yunge-reader-native-request)
                 (lambda (_operation _parameters complete)
                   (funcall complete nil '(error "cannot open")))))
        (yunge-reader-pdf--open
         "C:/books/broken.pdf"
         (lambda (_handle _properties error-data)
           (setq completion-error error-data))))
      (should (zerop leases))
      (should (equal completion-error '(error "cannot open"))))))

(ert-deftest yunge-reader-pdf-close-does-not-restart-a-stopped-helper ()
  (let ((leases 1)
        requested)
    (cl-letf (((symbol-function 'yunge-reader-native-live-p)
               (lambda () nil))
              ((symbol-function 'yunge-reader-native-release)
               (lambda () (cl-decf leases)))
              ((symbol-function 'yunge-reader-native-request)
               (lambda (&rest _arguments) (setq requested t))))
      (yunge-reader-pdf--close
       (make-yunge-reader-document :handle 7)))
    (should (zerop leases))
    (should-not requested)))

(ert-deftest yunge-reader-pdf-maps-reader-requests-to-native-operations ()
  (let ((document (make-yunge-reader-document :handle 11))
        calls)
    (cl-letf (((symbol-function 'yunge-reader-native-request)
               (lambda (operation parameters _complete)
                 (push (cons operation parameters) calls))))
      (yunge-reader-pdf--request
       document 'page-info '(:page 2) #'ignore)
      (yunge-reader-pdf--request
       document 'page-text '(:page 2) #'ignore)
      (yunge-reader-pdf--request
       document 'render-page
       '(:page 2 :width 900 :cache-key "key") #'ignore)
      (yunge-reader-pdf--request
       document 'search
       (list
        :query "Needle"
        :case-sensitive t
        :cursor (make-yunge-reader-position :unit 3 :offset 4)
        :match-limit 25
        :page-limit 6)
       #'ignore))
    (setq calls (nreverse calls))
    (should (equal (caar calls) "page-info"))
    (should (equal (cdr (assq 'document (cdar calls))) 11))
    (should (equal (cdr (assq 'page (cdar calls))) 2))
    (should (equal (caadr calls) "page-text"))
    (should (equal (caaddr calls) "render-page"))
    (should (equal (cdr (assq 'width (cdaddr calls))) 900))
    (should (equal (cdr (assq 'cache-key (cdaddr calls))) "key"))
    (let ((search (nth 3 calls)))
      (should (equal (car search) "search"))
      (should (eq (cdr (assq 'case-sensitive (cdr search))) t))
      (should (equal (cdr (assq 'cursor (cdr search)))
                     '((page . 3) (offset . 4))))
      (should (= (cdr (assq 'match-limit (cdr search))) 25))
      (should (= (cdr (assq 'page-limit (cdr search))) 6)))))

(ert-deftest yunge-reader-pdf-converts-native-search-batches ()
  (let* ((value
          '((matches
             . (((start . ((page . 2) (offset . 4)))
                 (end . ((page . 2) (offset . 9)))
                 (text . "Needle")
                 (before . "before ")
                 (after . " after"))))
            (cursor . ((page . 3) (offset . 0)))
            (done . nil)))
         (batch (yunge-reader-pdf--native-search-batch value))
         (result (car (yunge-reader-search-batch-results batch))))
    (should (yunge-reader-search-batch-p batch))
    (should-not (yunge-reader-search-batch-done batch))
    (should (= (yunge-reader-position-unit
                (yunge-reader-search-result-start result))
               2))
    (should (= (yunge-reader-position-offset
                (yunge-reader-search-result-end result))
               9))
    (should (equal (yunge-reader-search-result-text result) "Needle"))
    (should (= (yunge-reader-position-unit
                (yunge-reader-search-batch-cursor batch))
               3))))

(ert-deftest yunge-reader-pdf-resolves-cross-page-selection-natively ()
  (let* ((document (make-yunge-reader-document :handle 11))
         (start (make-yunge-reader-position :unit 3 :offset 9))
         (end (make-yunge-reader-position :unit 2 :offset 4))
         request
         result)
    (cl-letf (((symbol-function 'yunge-reader-native-request)
               (lambda (operation parameters complete)
                 (setq request (cons operation parameters))
                 (funcall complete '((text . "exact text")) nil))))
      (yunge-reader-pdf--request
       document 'selection-text
       (list :start start :end end)
       (lambda (value error-data)
         (should-not error-data)
         (setq result value))))
    (should (equal (car request) "selection-text"))
    (should (equal (cdr (assq 'start (cdr request)))
                   '((page . 3) (offset . 9))))
    (should (equal (cdr (assq 'end (cdr request)))
                   '((page . 2) (offset . 4))))
    (should (equal result "exact text"))))

(ert-deftest yunge-reader-pdf-resolves-fit-and-manual-widths ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((yunge-reader-pdf-page-margin 24)
          (page-info '((width . 612.0) (height . 792.0))))
      (cl-letf (((symbol-function 'window-body-width)
                 (lambda (_window _pixelwise) 1000))
                ((symbol-function 'window-body-height)
                 (lambda (_window _pixelwise) 800)))
        (setq yunge-reader-zoom-mode 'fit-width)
        (should (= (yunge-reader-pdf--target-width page-info 'window)
                   952))
        (setq yunge-reader-zoom-mode 'fit-page)
        (should (= (yunge-reader-pdf--target-width page-info 'window)
                   581))
        (setq yunge-reader-zoom-mode 'manual
              yunge-reader-scale 2.0)
        (should (= (yunge-reader-pdf--target-width page-info 'window)
                   1632))
        (should (= yunge-reader-effective-scale 2.0))))))

(ert-deftest yunge-reader-pdf-rejects-obsolete-width-completions ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--generation 2
          yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))]
          yunge-reader-pdf--displayed-pages '(0))
    (let (painted)
      (cl-letf (((symbol-function 'yunge-reader-pdf--page-width)
                 (lambda (_page &optional _window) 901))
                ((symbol-function 'yunge-reader-pdf--paint-page)
                 (lambda (_page) (setq painted t))))
        (yunge-reader-pdf--render-complete
         (current-buffer) 1 0 900
         '((path . "stale.png")) nil))
      (should-not painted)
      (should
       (gethash '(0 . 900) yunge-reader-pdf--render-results)))))

(ert-deftest yunge-reader-pdf-converts-display-to-page-coordinates ()
  (with-temp-buffer
    (setq yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))])
    (let ((point
           (yunge-reader-pdf--pixel-to-page-point
            0 500 250 1000 1000)))
      (should (= (car point) 50.0))
      (should (= (cdr point) 150.0)))))

(ert-deftest yunge-reader-pdf-captures-canonical-viewport-location ()
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (setq yunge-reader-document
            (yunge-reader-pdf-test--document
             '((width . 100.0) (height . 200.0))
             '((width . 100.0) (height . 200.0)))
            yunge-reader-pdf--page-infos
            [((width . 100.0) (height . 200.0))
             ((width . 100.0) (height . 200.0))]
            yunge-reader-pdf--page-positions [1 3])
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (_window) t))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function 'window-start)
                 (lambda (_window) 3))
                ((symbol-function 'window-vscroll)
                 (lambda (_window &optional _pixels) 500))
                ((symbol-function 'window-hscroll)
                 (lambda (_window) 20))
                ((symbol-function 'window-frame)
                 (lambda (_window) 'frame))
                ((symbol-function 'frame-char-width)
                 (lambda (_frame) 10))
                ((symbol-function 'yunge-reader-pdf--page-at-position)
                 (lambda (_position) 1))
                ((symbol-function 'yunge-reader-pdf--page-width)
                 (lambda (_page &optional _window) 1000)))
        (let ((location
               (yunge-reader-pdf--location nil 'window)))
          (should (= (yunge-reader-position-unit location) 1))
          (should (= (yunge-reader-position-x location) 20.0))
          (should (= (yunge-reader-position-y location) 150.0)))))))

(ert-deftest yunge-reader-pdf-defers-and-rescales-restored-location ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          window-start
          vertical
          horizontal)
      (insert " \n ")
      (setq yunge-reader-document
            (yunge-reader-pdf-test--document
             '((width . 100.0) (height . 200.0))
             '((width . 100.0) (height . 200.0)))
            yunge-reader-pdf--page-infos
            [((width . 100.0) (height . 200.0))
             ((width . 100.0) (height . 200.0))]
            yunge-reader-pdf--page-positions [1 3])
      (cl-letf (((symbol-function 'yunge-reader--place-window)
                 (lambda (&optional _window) nil))
                ((symbol-function 'yunge-reader-pdf--update-visible-pages)
                 #'ignore))
        (should
         (yunge-reader-pdf--restore-location
          nil
          (make-yunge-reader-position
           :unit 9 :x 20.0 :y 150.0)
          nil)))
      (should yunge-reader-pdf--pending-location)
      (should (= yunge-reader-pdf-page 1))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (_window) t))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function 'window-body-width)
                 (lambda (_window pixelwise) (and pixelwise 800)))
                ((symbol-function 'window-body-height)
                 (lambda (_window pixelwise) (and pixelwise 600)))
                ((symbol-function 'window-frame)
                 (lambda (_window) 'frame))
                ((symbol-function 'frame-char-width)
                 (lambda (_frame) 10))
                ((symbol-function 'yunge-reader-pdf--page-width)
                 (lambda (_page &optional _window) 1000))
                ((symbol-function 'set-window-start)
                 (lambda (_window position &optional _noforce)
                   (setq window-start position)))
                ((symbol-function 'set-window-vscroll)
                 (lambda (_window value &optional _pixels)
                   (setq vertical value)))
                ((symbol-function 'set-window-hscroll)
                 (lambda (_window value)
                   (setq horizontal value))))
        (should
         (yunge-reader-pdf--apply-pending-location 'window)))
      (should-not yunge-reader-pdf--pending-location)
      (should (= yunge-reader-pdf-page 1))
      (should (= (point) 3))
      (should (= window-start 3))
      (should (= vertical 500))
      (should (= horizontal 20)))))

(ert-deftest yunge-reader-pdf-hit-testing-creates-cross-page-selection ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf-page 0
          yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))
           ((width . 100.0) (height . 200.0))])
    (puthash
     0
     '((characters
        . (((index . 4)
            (text . "A")
            (bounds . ((left . 10.0) (bottom . 10.0)
                       (right . 20.0) (top . 20.0)))))))
     yunge-reader-pdf--text-cache)
    (puthash
     1
     '((characters
        . (((index . 5)
            (text . "B")
            (bounds . ((left . 22.0) (bottom . 10.0)
                       (right . 32.0) (top . 20.0)))))))
     yunge-reader-pdf--text-cache)
    (cl-letf (((symbol-function 'yunge-reader-pdf--paint-pages)
               #'ignore))
      (yunge-reader-pdf--select-points
       '(:page 0 :point (15.0 . 15.0))
       '(:page 1 :point (27.0 . 15.0))))
    (should (= (yunge-reader-position-unit
                (yunge-reader-selection-start
                 yunge-reader-selection))
               0))
    (should (= (yunge-reader-position-offset
                (yunge-reader-selection-start
                 yunge-reader-selection))
               4))
    (should (= (yunge-reader-position-unit
                (yunge-reader-selection-end
                 yunge-reader-selection))
               1))
    (should (= (yunge-reader-position-offset
                (yunge-reader-selection-end
                 yunge-reader-selection))
               5))))

(ert-deftest yunge-reader-pdf-caches-page-text-once ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (let ((requests 0)
          (result '((page . 0) (characters . ()))))
      (cl-letf (((symbol-function 'yunge-reader-request)
                 (lambda (operation arguments complete)
                   (cl-incf requests)
                   (should (eq operation 'page-text))
                   (should (= (plist-get arguments :page) 0))
                   (funcall complete result nil))))
        (yunge-reader-pdf--request-text 0)
        (yunge-reader-pdf--request-text 0))
      (should (= requests 1))
      (should (eq (gethash 0 yunge-reader-pdf--text-cache)
                  result)))))

(ert-deftest yunge-reader-pdf-prioritizes-image-before-text-layer ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--generation 1)
    (let (operations)
      (cl-letf (((symbol-function 'yunge-reader-pdf--request-render)
                 (lambda (_generation page)
                   (push (list 'render page) operations)))
                ((symbol-function 'yunge-reader-pdf--request-text)
                 (lambda (page)
                   (push (list 'text page) operations))))
        (yunge-reader-pdf--queue-pages '(0 1)))
      (should (equal (nreverse operations)
                     '((render 0) (render 1)
                       (text 0) (text 1)))))))

(ert-deftest yunge-reader-pdf-refresh-builds-and-prefetches-the-roll ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document
          (yunge-reader-pdf-test--document
           '((page . 0) (width . 100.0) (height . 200.0))
           '((page . 1) (width . 100.0) (height . 200.0))))
    (let (operations)
      (cl-letf (((symbol-function 'yunge-reader-pdf--cache-key)
                 (lambda (_page _width) (make-string 64 ?a)))
                ((symbol-function 'yunge-reader-request)
                 (lambda (operation arguments _complete)
                   (push (list operation (plist-get arguments :page))
                         operations))))
        (yunge-reader-pdf--refresh))
      (should (equal yunge-reader-pdf--page-positions [1 3]))
      (should (equal (nreverse operations)
                     '((render-page 0) (render-page 1)
                       (page-text 0) (page-text 1)))))))

(ert-deftest yunge-reader-pdf-paints-selection-in-svg-coordinates ()
  (with-temp-buffer
    (setq yunge-reader-pdf-page 0
          yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))]
          yunge-reader-selection
          (make-yunge-reader-selection
           :start (make-yunge-reader-position
                   :unit 0 :offset 5)
           :end (make-yunge-reader-position
                 :unit 0 :offset 4)))
    (let ((svg (svg-create 1000 1000)))
      (yunge-reader-pdf--paint-selection
       svg 0 '((width . 100.0) (height . 200.0))
       '((characters
          . (((index . 4)
              (bounds . ((left . 10.0) (bottom . 10.0)
                         (right . 20.0) (top . 20.0))))
             ((index . 5)
              (bounds . ((left . 22.0) (bottom . 10.0)
                         (right . 32.0) (top . 20.0)))))))
       1000 1000)
      (let ((rectangles (dom-by-tag svg 'rect)))
        (should (= (length rectangles) 2))
        (should (= (dom-attr (car rectangles) 'x) 100.0))
        (should (= (dom-attr (car rectangles) 'y) 900.0))
        (should (= (dom-attr (car rectangles) 'width) 100.0))
         (should (= (dom-attr (car rectangles) 'height) 50.0))))))

(ert-deftest yunge-reader-pdf-paints-search-independently-from-selection ()
  (with-temp-buffer
    (setq yunge-reader-selection
          (make-yunge-reader-selection
           :start (make-yunge-reader-position :unit 0 :offset 1)
           :end (make-yunge-reader-position :unit 0 :offset 1))
          yunge-reader-search-result
          (make-yunge-reader-search-result
           :start (make-yunge-reader-position :unit 0 :offset 4)
           :end (make-yunge-reader-position :unit 0 :offset 4)))
    (let* ((page-info '((width . 100.0) (height . 100.0)))
           (text-layer
            '((characters
               . (((index . 1)
                   (bounds . ((left . 10.0) (bottom . 10.0)
                              (right . 20.0) (top . 20.0))))
                  ((index . 4)
                   (bounds . ((left . 40.0) (bottom . 40.0)
                              (right . 50.0) (top . 50.0))))))))
           (svg (svg-create 1000 1000)))
      (yunge-reader-pdf--paint-selection
       svg 0 page-info text-layer 1000 1000)
      (yunge-reader-pdf--paint-search
       svg 0 page-info text-layer 1000 1000)
      (let ((rectangles (dom-by-tag svg 'rect)))
        (should (= (length rectangles) 2))
        (should
         (member yunge-reader-pdf-selection-color
                 (mapcar (lambda (node) (dom-attr node 'fill))
                         rectangles)))
        (should
         (member yunge-reader-pdf-search-color
                 (mapcar (lambda (node) (dom-attr node 'fill))
                         rectangles)))))))

(ert-deftest yunge-reader-pdf-scrolls-to-search-character-geometry ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))]
          yunge-reader-pdf--page-positions [1]
          yunge-reader-search-result
          (make-yunge-reader-search-result
           :start (make-yunge-reader-position :unit 0 :offset 7)
           :end (make-yunge-reader-position :unit 0 :offset 7)))
    (puthash
     0
     '((characters
        . (((index . 7)
            (bounds . ((left . 85.0) (bottom . 20.0)
                       (right . 95.0) (top . 30.0)))))))
     yunge-reader-pdf--text-cache)
    (let (window-start vertical horizontal)
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _arguments) 'window))
                ((symbol-function 'window-live-p) (lambda (_window) t))
                ((symbol-function 'window-body-width)
                 (lambda (_window pixelwise) (and pixelwise 800)))
                ((symbol-function 'window-body-height)
                 (lambda (_window pixelwise) (and pixelwise 600)))
                ((symbol-function 'window-frame)
                 (lambda (_window) 'frame))
                ((symbol-function 'frame-char-width)
                 (lambda (_frame) 10))
                ((symbol-function 'yunge-reader-pdf--page-width)
                 (lambda (_page &optional _window) 1000))
                ((symbol-function 'set-window-start)
                 (lambda (_window position &optional _noforce)
                   (setq window-start position)))
                ((symbol-function 'set-window-vscroll)
                 (lambda (_window value &optional _pixels)
                   (setq vertical value)))
                ((symbol-function 'set-window-hscroll)
                 (lambda (_window value)
                   (setq horizontal value))))
        (yunge-reader-pdf--scroll-to-search-result))
      (should (= window-start 1))
      (should (= vertical 1400))
      (should (= horizontal 20))
      (setq yunge-reader-pdf-page 1
            window-start nil
            vertical nil
            horizontal nil)
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _arguments) 'window))
                ((symbol-function 'window-live-p) (lambda (_window) t)))
        (yunge-reader-pdf--scroll-to-search-result))
      (should-not window-start)
      (should-not vertical)
      (should-not horizontal))))

(ert-deftest yunge-reader-pdf-paints-rotated-selection-as-polygon ()
  (with-temp-buffer
    (setq yunge-reader-selection
          (make-yunge-reader-selection
           :start (make-yunge-reader-position :unit 0 :offset 4)
           :end (make-yunge-reader-position :unit 0 :offset 5)))
    (let* ((character
            '((index . 4)
              (bounds . ((left . 0.0) (bottom . 0.0)
                         (right . 100.0) (top . 100.0)))
              (quad . (((x . 0.0) (y . 50.0))
                       ((x . 50.0) (y . 100.0))
                       ((x . 100.0) (y . 50.0))
                       ((x . 50.0) (y . 0.0))))))
           (svg (svg-create 1000 1000)))
      (yunge-reader-pdf--paint-selection
       svg 0 '((width . 100.0) (height . 100.0))
       `((characters
          . (,character
             ((index . 5) (generated . t)
              (bounds . ((left . 10.0) (bottom . 10.0)
                         (right . 20.0) (top . 20.0)))))))
       1000 1000)
      (should-not (dom-by-tag svg 'rect))
      (let ((polygons (dom-by-tag svg 'polygon)))
        (should (= (length polygons) 1))
        (should
         (equal
          (dom-attr (car polygons) 'points)
          "0.0 500.0, 500.0 0.0, 1000.0 500.0, 500.0 1000.0"))))))

(ert-deftest yunge-reader-pdf-hits-rotated-character-quads ()
  (with-temp-buffer
    (setq yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 100.0))])
    (let* ((character
            '((index . 7)
              (text . "R")
              (bounds . ((left . 0.0) (bottom . 0.0)
                         (right . 100.0) (top . 100.0)))
              (quad . (((x . 0.0) (y . 50.0))
                       ((x . 50.0) (y . 100.0))
                       ((x . 100.0) (y . 50.0))
                       ((x . 50.0) (y . 0.0))))))
           (layer `((characters . (,character)))))
      (should
       (= (alist-get
           'index
           (yunge-reader-pdf--hit-character
            0 '(50.0 . 50.0) layer))
          7))
      (should-not
       (yunge-reader-pdf--hit-character
        0 '(0.0 . 0.0) layer)))))

(ert-deftest yunge-reader-pdf-falls-back-from-invalid-quads ()
  (with-temp-buffer
    (setq yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 100.0))])
    (let* ((character
            '((index . 9)
              (text . "F")
              (bounds . ((left . 10.0) (bottom . 10.0)
                         (right . 20.0) (top . 20.0)))
              (quad . (((x . 10.0) (y . 10.0))
                       ((x . 12.0) (y . 12.0))
                       ((x . 14.0) (y . 14.0))
                       ((x . 16.0) (y . 16.0))))))
           (layer `((characters . (,character)))))
      (should
       (= (alist-get
           'index
           (yunge-reader-pdf--hit-character
            0 '(15.0 . 15.0) layer))
          9)))))

(ert-deftest yunge-reader-pdf-builds-one-stable-slot-per-page ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document
          (yunge-reader-pdf-test--document
           '((page . 0) (width . 100.0) (height . 200.0))
           '((page . 1) (width . 200.0) (height . 100.0))
           '((page . 2) (width . 100.0) (height . 100.0))))
    (yunge-reader-pdf--load-page-infos)
    (yunge-reader-pdf--build-roll)
    (should (equal (buffer-string) " \n \n "))
    (should (equal yunge-reader-pdf--page-positions [1 3 5]))
    (should (= (get-text-property 1 'yunge-reader-pdf-page) 0))
    (should (= (get-text-property 3 'yunge-reader-pdf-page) 1))
    (should (= (get-text-property 5 'yunge-reader-pdf-page) 2))))

(ert-deftest yunge-reader-pdf-reports-an-empty-document ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document
          (yunge-reader-pdf-test--document))
    (yunge-reader-pdf--refresh)
    (should (string-match-p "contains no pages" (buffer-string)))
    (should (equal yunge-reader-pdf--page-infos []))
    (should-not yunge-reader-pdf--page-positions)))

(ert-deftest yunge-reader-pdf-prefetches-only-near-visible-pages ()
  (with-temp-buffer
    (setq yunge-reader-document
          (apply #'yunge-reader-pdf-test--document
                 (make-list 6 '((width . 100.0)
                                (height . 200.0)))))
    (let ((yunge-reader-pdf-prefetch-pages 1))
      (should (equal (yunge-reader-pdf--prefetch-range '(2 3))
                     '(1 2 3 4)))
      (should (equal (yunge-reader-pdf--prefetch-range '(0))
                     '(0 1)))
      (should (equal (yunge-reader-pdf--prefetch-range '(5))
                     '(4 5))))))

(ert-deftest yunge-reader-pdf-computes-selection-ranges-across-pages ()
  (with-temp-buffer
    (setq yunge-reader-selection
          (make-yunge-reader-selection
           :start (make-yunge-reader-position :unit 2 :offset 1)
           :end (make-yunge-reader-position :unit 0 :offset 1)))
    (let ((layer
           '((characters
              . (((index . 0)) ((index . 1)) ((index . 2)))))))
      (should (equal
               (yunge-reader-pdf--selection-offsets 0 layer)
               '(1 . 2)))
      (should (equal
               (yunge-reader-pdf--selection-offsets 1 layer)
               '(0 . 2)))
      (should (equal
               (yunge-reader-pdf--selection-offsets 2 layer)
               '(0 . 1)))
      (should-not
       (yunge-reader-pdf--selection-offsets 3 layer)))))

(ert-deftest yunge-reader-pdf-virtualizes-pages-outside-the-viewport ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document
          (yunge-reader-pdf-test--document
           '((page . 0) (width . 100.0) (height . 200.0))
           '((page . 1) (width . 100.0) (height . 200.0))))
    (yunge-reader-pdf--load-page-infos)
    (yunge-reader-pdf--build-roll)
    (puthash '(0 . 900)
             '((path . "zero.png")
               (pixel-width . 900) (pixel-height . 1800))
             yunge-reader-pdf--render-results)
    (puthash '(1 . 900)
             '((path . "one.png")
               (pixel-width . 900) (pixel-height . 1800))
             yunge-reader-pdf--render-results)
    (cl-letf (((symbol-function 'yunge-reader-pdf--page-width)
               (lambda (_page &optional _window) 900))
              ((symbol-function 'create-image)
               (lambda (path &rest _arguments) (list 'image path))))
      (dotimes (page 2)
        (yunge-reader-pdf--paint-page page))
      (yunge-reader-pdf--paint-pages '(0))
      (should (equal (get-text-property 1 'display)
                     '(image "zero.png")))
      (should (eq (car (get-text-property 3 'display)) 'space))
      (yunge-reader-pdf--paint-pages '(1))
      (should (eq (car (get-text-property 1 'display)) 'space))
      (should (equal (get-text-property 3 'display)
                     '(image "one.png"))))))

(ert-deftest yunge-reader-pdf-page-jumps-preserve-logical-selection ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document
          (yunge-reader-pdf-test--document
           '((page . 0) (width . 100.0) (height . 200.0))
           '((page . 1) (width . 100.0) (height . 200.0))
           '((page . 2) (width . 100.0) (height . 200.0))))
    (yunge-reader-pdf--load-page-infos)
    (yunge-reader-pdf--build-roll)
    (let ((selection
           (make-yunge-reader-selection
            :start (make-yunge-reader-position :unit 0 :offset 4)
            :end (make-yunge-reader-position :unit 2 :offset 8))))
      (setq yunge-reader-selection selection)
      (cl-letf (((symbol-function
                  'yunge-reader-pdf--update-visible-pages)
                 #'ignore))
        (yunge-reader-pdf--set-page 2))
      (should (= yunge-reader-pdf-page 2))
      (should (= (point) 5))
      (should (eq yunge-reader-selection selection)))))

(ert-deftest yunge-reader-pdf-jumps-to-first-and-last-pages ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document
          (yunge-reader-pdf-test--document
           '((page . 0) (width . 100.0) (height . 200.0))
           '((page . 1) (width . 100.0) (height . 200.0))
           '((page . 2) (width . 100.0) (height . 200.0))))
    (yunge-reader-pdf--load-page-infos)
    (yunge-reader-pdf--build-roll)
    (cl-letf (((symbol-function
                'yunge-reader-pdf--update-visible-pages)
               #'ignore))
      (yunge-reader-pdf-last-page)
      (should (= yunge-reader-pdf-page 2))
      (yunge-reader-pdf-first-page)
      (should (zerop yunge-reader-pdf-page)))))

(ert-deftest yunge-reader-pdf-coalesces-identical-render-requests ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--page-infos
          [((page . 0) (width . 100.0) (height . 200.0))])
    (let ((requests 0))
      (cl-letf (((symbol-function 'yunge-reader-pdf--page-width)
                 (lambda (_page &optional _window) 900))
                ((symbol-function 'yunge-reader-pdf--cache-key)
                 (lambda (_page _width) (make-string 64 ?a)))
                ((symbol-function 'yunge-reader-request)
                 (lambda (_operation _arguments _complete)
                   (cl-incf requests))))
        (yunge-reader-pdf--request-render 4 0)
        (yunge-reader-pdf--request-render 5 0))
      (should (= requests 1)))))

;;; yunge-reader-pdf-test.el ends here
