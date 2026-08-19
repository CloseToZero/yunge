;;; yunge-reader-epub-test.el --- EPUB reader tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-epub)

(defun yunge-reader-epub-test--location (&optional fraction x y)
  "Return one bounded EPUB locator with optional FRACTION, X, and Y."
  (append
   '((cfi . "epubcfi(/6/4!/4/2)"))
   '((href . "OPS/chapter.xhtml"))
   (when fraction `((fraction . ,fraction)))
   (when (and x y) `((x . ,x) (y . ,y)))))

(defun yunge-reader-epub-test--handle (&optional publication)
  "Return a live EPUB handle for PUBLICATION."
  (make-yunge-reader-epub-handle
   :publication (or publication 7)
   :metadata '(:title "Test Book")
   :pending-detaches 0))

(defun yunge-reader-epub-test--document (&optional handle layout)
  "Return an EPUB document backed by HANDLE with optional LAYOUT."
  (make-yunge-reader-document
   :file "test.epub"
   :handle (or handle (yunge-reader-epub-test--handle))
   :layout (or layout 'reflow)
   :metadata '(:title "Test Book")))

(defun yunge-reader-epub-test--selection ()
  "Return one stable same-spine EPUB selection."
  '((href . "OPS/chapter.xhtml")
    (start . "epubcfi(/6/4!/4/2/1:0)")
    (end . "epubcfi(/6/4!/4/2/1:8)")))

(defun yunge-reader-epub-test--surface
    (id state &rest properties)
  "Return an EPUB test surface with ID, STATE, and PROPERTIES."
  (apply #'yunge-reader-webview--make-surface
         :id id :state state properties))

(ert-deftest yunge-reader-epub-uses-reflowable-screen-bindings ()
  (yunge-test-keymap-keys
   yunge-reader-epub-view-mode-map
   '(("j" . yunge-reader-epub-next-line)
     ("k" . yunge-reader-epub-previous-line)
     ("C-d" . yunge-reader-epub-next-screen)
     ("C-u" . yunge-reader-epub-previous-screen)
     ("G" . yunge-reader-epub-last-location)
     ("J" . yunge-reader-epub-next-page)
     ("K" . yunge-reader-epub-previous-page)
     ("y" . yunge-reader-epub-copy-selection)
     ("gg" . yunge-reader-epub-first-location))))

(ert-deftest yunge-reader-epub-keeps-layout-under-local-leader ()
  (should
   (eq (lookup-key yunge-reader-epub-command-map (kbd "l"))
       yunge-reader-epub-layout-map))
  (should
   (eq (lookup-key yunge-reader-epub-command-map (kbd "p"))
       #'yunge-reader-make-primary))
  (yunge-test-keymap-keys
   yunge-reader-epub-layout-map
   '(("+" . yunge-reader-epub-increase-line-height)
     ("-" . yunge-reader-epub-decrease-line-height)
     (">" . yunge-reader-epub-widen-content)
     ("<" . yunge-reader-epub-narrow-content)
     ("=" . yunge-reader-epub-reset-text-layout))))

(ert-deftest yunge-reader-epub-integrates-screen-bindings-with-evil ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-epub-view-mode 1)
    (yunge-reader-epub-reflow-view-mode 1)
    (let ((local-map (key-binding [localleader])))
      (should
       (eq (lookup-key local-map (kbd "l"))
           yunge-reader-epub-layout-map))
      (should
       (eq (lookup-key local-map (kbd "p"))
           #'yunge-reader-make-primary)))
    (yunge-test-evil-keys
     'normal
     '(("j" . yunge-reader-epub-next-line)
       ("k" . yunge-reader-epub-previous-line)
       ("C-d" . yunge-reader-epub-next-screen)
       ("C-u" . yunge-reader-epub-previous-screen)
       ("G" . yunge-reader-epub-last-location)
       ("J" . yunge-reader-epub-next-page)
       ("K" . yunge-reader-epub-previous-page)
       ("y" . yunge-reader-epub-copy-selection)
       ("gg" . yunge-reader-epub-first-location)
       ("+" . yunge-reader-zoom-in)
       ("-" . yunge-reader-zoom-out)
       ("=" . yunge-reader-zoom-reset)
       ("n" . yunge-reader-search-next)
       ("q" . evil-record-macro)))))

(ert-deftest yunge-reader-epub-hides-reflow-menu-in-fixed-views ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-epub-view-mode 1)
    (let ((local-map (key-binding [localleader])))
      (should-not (lookup-key local-map (kbd "l")))
      (should
       (eq (lookup-key local-map (kbd "p"))
           #'yunge-reader-make-primary)))))

(ert-deftest yunge-reader-epub-registers-only-when-requested ()
  (let ((yunge-reader-drivers nil)
        (modes auto-mode-alist))
    (should-not (yunge-reader-driver-for-file "book.epub"))
    (yunge-reader-epub-register)
    (should
     (eq (yunge-reader-driver-name
          (yunge-reader-driver-for-file "book.EPUB"))
         'epub))
    (should (equal auto-mode-alist modes))))

(ert-deftest yunge-reader-epub-binds-epub-file-visits ()
  (should
   (eq (cdr (assoc "\\.epub\\'" auto-mode-alist))
       'yunge-reader-epub-mode)))

(ert-deftest yunge-reader-epub-file-visits-reopen-cleanly ()
  (let ((file (expand-file-name "visited.epub"))
        (yunge-reader-drivers nil)
        (yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-saved-places nil)
        (opens 0)
        (closes 0)
        (attaches 0)
        (detaches 0))
    (cl-letf
        (((symbol-function 'yunge-reader-epub--open)
          (lambda (_file complete)
            (cl-incf opens)
            (funcall complete 'handle
                     '(:layout reflow :metadata (:title "Test"))
                     nil)))
         ((symbol-function 'yunge-reader-epub--close)
          (lambda (_document) (cl-incf closes)))
         ((symbol-function 'yunge-reader-epub--attach)
          (lambda (_document) (cl-incf attaches)))
         ((symbol-function 'yunge-reader-epub--detach)
          (lambda (_document) (cl-incf detaches))))
      (let ((buffer (generate-new-buffer "yunge-reader-epub-file-test")))
        (unwind-protect
            (with-current-buffer buffer
              (setq buffer-file-name file)
              (set-buffer-multibyte nil)
              (insert "binary EPUB fixture")
              (set-buffer-modified-p nil)
              (set-auto-mode)
              (should (eq major-mode 'yunge-reader-mode))
              (should yunge-reader-document)
              (should-not (buffer-modified-p))
              (should (= opens 1))
              (should (= attaches 1))
              (revert-buffer nil t)
              (should (eq major-mode 'yunge-reader-mode))
              (should yunge-reader-document)
              (should-not (buffer-modified-p))
              (should (= opens 2))
              (should (= closes 1))
              (should (= attaches 2))
              (should (= detaches 1))
              (should (kill-buffer buffer)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))
    (should (= closes 2))
    (should (= detaches 2))))

(ert-deftest yunge-reader-epub-validates-native-open-results ()
  (let (handle properties error-data)
    (yunge-reader-epub--open-complete
     (lambda (value props error)
       (setq handle value
             properties props
             error-data error))
     '((publication . 9)
       (metadata
        . ((title . "Protocol Book")
           (language . "en")
           (identifier . "urn:test")
           (version . "3.0")
           (layout . "reflowable")))
       (entry-count . 12))
     nil)
    (should-not error-data)
    (should (= (yunge-reader-epub-handle-publication handle) 9))
    (should (eq (plist-get properties :layout) 'reflow))
    (should
     (equal (plist-get (plist-get properties :metadata) :title)
            "Protocol Book"))
    (should
     (= (plist-get (plist-get properties :metadata) :entry-count)
        12))))

(ert-deftest yunge-reader-epub-maps-pre-paginated-package-layout ()
  (let (handle properties error-data)
    (yunge-reader-epub--open-complete
     (lambda (value props error)
       (setq handle value
             properties props
             error-data error))
     '((publication . 10)
       (metadata
        . ((title . "Fixed Book")
           (layout . "pre-paginated")))
       (entry-count . 4))
     nil)
    (should-not error-data)
    (should (= (yunge-reader-epub-handle-publication handle) 10))
    (should (eq (plist-get properties :layout) 'fixed))))

(ert-deftest yunge-reader-epub-rejects-invalid-package-layouts ()
  (dolist (layout '(nil "scrolling" 7))
    (let (closed error-data)
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--close-owned-publication)
            (lambda (publication) (setq closed publication))))
        (yunge-reader-epub--open-complete
         (lambda (_value _properties error)
           (setq error-data error))
         `((publication . 11)
           (metadata . ((layout . ,layout))))
         nil))
      (should error-data)
      (should (= closed 11)))))

(ert-deftest yunge-reader-epub-builds-independent-default-styles ()
  (let ((first (yunge-reader-epub--default-style))
        (second (yunge-reader-epub--default-style)))
    (should (equal first second))
    (should-not (eq first second))
    (setcdr (assq 'font-scale first) 2.0)
    (should (= (alist-get 'font-scale second) 1.0))))

(ert-deftest yunge-reader-epub-resolves-scroll-bar-policy ()
  (let ((window (selected-window)))
    (dolist (entry '((hidden . hidden) (visible . visible)))
      (let ((yunge-reader-epub-scroll-bar-mode (car entry)))
        (should
         (eq (yunge-reader-epub--scroll-bar-mode window)
             (cdr entry)))))
    (let ((yunge-reader-epub-scroll-bar-mode 'follow-emacs))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (_frame _parameter) 'right)))
        (should
         (eq (yunge-reader-epub--scroll-bar-mode window) 'visible)))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (_frame _parameter) nil)))
        (should
         (eq (yunge-reader-epub--scroll-bar-mode window) 'hidden))))))

(ert-deftest yunge-reader-epub-opens-with-a-pending-manual-scale ()
  (let ((yunge-reader-epub-default-font-scale 1.25)
        (yunge-reader-default-appearances '((epub . follow-emacs)))
        (yunge-reader-saved-appearance-overrides nil)
        (document (yunge-reader-epub-test--document))
        attached-appearance-function
        attached-style)
    (with-temp-buffer
      (yunge-reader-mode)
      (setf (yunge-reader-document-driver document) 'epub)
      (setq yunge-reader-document document
            yunge-reader--pending-place
            '(:zoom-mode manual :scale 1.8))
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--attach-shared-publication)
             (lambda (_publication layout &rest options)
               (should (eq layout 'reflow))
               (setq attached-appearance-function
                     (plist-get options :appearance-function)
                     attached-style (plist-get options :style)))))
        (yunge-reader-epub--attach document))
      (should yunge-reader-epub-view-mode)
      (should (= yunge-reader-default-scale 1.25))
      (should (= yunge-reader-minimum-scale 0.5))
      (should (= yunge-reader-maximum-scale 3.0))
      (should (eq yunge-reader-zoom-mode 'manual))
      (should (= yunge-reader-scale 1.8))
      (should (= yunge-reader-effective-scale 1.8))
      (should (functionp attached-appearance-function))
      (should
       (eq (alist-get
            'mode
            (funcall attached-appearance-function (selected-window)))
           'follow-emacs))
      (should (= (alist-get 'font-scale attached-style) 1.8))
      (should
       (memq #'yunge-reader-epub--appearance-changed
             yunge-reader-appearance-change-hook))
      (should (string-match-p "Font 180%" header-line-format)))))

(ert-deftest yunge-reader-epub-normalizes-frame-face-colors ()
  (let (seen)
    (cl-letf (((symbol-function 'facep) (lambda (_face) t))
              ((symbol-function 'face-attribute)
               (lambda (face attribute frame inherit)
                 (setq seen (list face attribute frame inherit))
                 "#123456"))
              ((symbol-function 'color-values)
               (lambda (color frame)
                 (should (equal color "#123456"))
                 (should (eq frame 'test-frame))
                 (mapcar (lambda (value) (* value 257))
                         '(18 52 86)))))
      (should
       (equal
        (yunge-reader--face-color
         'default :foreground 'test-frame "#000000")
        "#123456"))
      (should
       (equal seen '(default :foreground test-frame default))))))

(ert-deftest yunge-reader-epub-resolves-appearance-for-surface-frame ()
  (let ((yunge-reader-default-appearances
         '((epub . follow-emacs)))
        (document (yunge-reader-epub-test--document))
        calls)
    (setf (yunge-reader-document-driver document) 'epub)
    (with-temp-buffer
      (setq yunge-reader-document document)
      (cl-letf
          (((symbol-function 'window-frame)
            (lambda (_window) 'surface-frame))
           ((symbol-function 'yunge-reader--face-color)
            (lambda (face attribute frame fallback)
              (push (list face attribute frame fallback) calls)
              (pcase (cons face attribute)
                (`(default . :foreground) "#112233")
                (`(default . :background) "#f4f5f6")
                (`(link . :foreground) "#2244aa")
                (`(region . :foreground) "#ffffff")
                (`(region . :background) "#335577")
                (`(isearch . :background) "#aa5500")))))
        (should
         (equal
          (yunge-reader-epub--resolved-appearance 'surface-window)
          '((mode . follow-emacs)
            (foreground . "#112233")
            (background . "#f4f5f6")
            (link . "#2244aa")
            (selection-foreground . "#ffffff")
            (selection-background . "#335577")
            (search-background . "#aa5500"))))
        (should (= (length calls) 6))
        (should
         (cl-every (lambda (call) (eq (nth 2 call) 'surface-frame))
                   calls))))))

(ert-deftest yunge-reader-epub-keeps-reflow-controls-from-fixed-views ()
  (let ((document
         (yunge-reader-epub-test--document nil 'fixed))
        attached-layout
        attached-style
        attached-zoom)
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader-document document)
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--attach-shared-publication)
            (lambda (_publication layout &rest options)
              (setq attached-layout layout
                    attached-style (plist-get options :style)
                    attached-zoom (plist-get options :zoom)))))
        (yunge-reader-epub--attach document))
      (should yunge-reader-epub-view-mode)
      (should-not yunge-reader-epub-reflow-view-mode)
      (should (eq attached-layout 'fixed))
      (should-not attached-style)
      (should (eq attached-zoom 'fit-page))
      (should (eq yunge-reader-zoom-mode 'fit-page))
      (should (= yunge-reader-scale 1.0))
      (should-not yunge-reader-effective-scale)
      (should (string-match-p "EPUB  Fit Page" header-line-format))
      (should-not (string-match-p "Font" header-line-format))
      (should-error
       (yunge-reader-epub-increase-line-height)
       :type 'user-error))))

(ert-deftest yunge-reader-epub-restores-fixed-manual-zoom ()
  (let ((document
         (yunge-reader-epub-test--document nil 'fixed))
        attached-zoom)
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader-document document
            yunge-reader--pending-place
            '(:zoom-mode manual :scale 1.8))
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--attach-shared-publication)
            (lambda (_publication _layout &rest options)
              (setq attached-zoom (plist-get options :zoom)))))
        (yunge-reader-epub--attach document))
      (should (= attached-zoom 1.8))
      (should (eq yunge-reader-zoom-mode 'manual))
      (should (= yunge-reader-scale 1.8))
      (should (= yunge-reader-effective-scale 1.8))
      (should (string-match-p "Zoom 180%" header-line-format)))))

(ert-deftest yunge-reader-epub-restores-fixed-fit-zoom ()
  (let ((document
         (yunge-reader-epub-test--document nil 'fixed))
        attached-zoom)
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader-document document
            yunge-reader--pending-place
            '(:zoom-mode fit-width :scale 1.4))
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--attach-shared-publication)
            (lambda (_publication _layout &rest options)
              (setq attached-zoom (plist-get options :zoom)))))
        (yunge-reader-epub--attach document))
      (should (eq attached-zoom 'fit-width))
      (should (eq yunge-reader-zoom-mode 'fit-width))
      (should (= yunge-reader-scale 1.4))
      (should-not yunge-reader-effective-scale)
      (should (string-match-p "Fit Width" header-line-format)))))

(ert-deftest yunge-reader-epub-header-shows-view-progress ()
  (with-temp-buffer
    (yunge-reader-mode)
    (setq yunge-reader-document (yunge-reader-epub-test--document)
          yunge-reader-webview--buffer-view
          (yunge-reader-webview--make-view
           :location (yunge-reader-epub-test--location 0.359)))
    (yunge-reader-epub-view-mode 1)
    (yunge-reader-epub--update-header)
    (should (string-match-p "EPUB  35%  Font" header-line-format))))

(ert-deftest yunge-reader-epub-rejects-a-nonmanual-pending-place ()
  (with-temp-buffer
    (yunge-reader-mode)
    (setq yunge-reader--pending-place
          '(:zoom-mode fit-width :scale 1.0))
    (should-error
     (yunge-reader-epub--initial-font-scale))))

(ert-deftest yunge-reader-epub-maps-reader-zoom-to-font-scale ()
  (let ((yunge-reader-epub-default-font-scale 1.25)
        applied-style
        (syncs 0)
        (records 0))
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader-document
            (yunge-reader-epub-test--document))
      (let ((view
             (yunge-reader-webview--make-view
              :style (yunge-reader-epub--default-style))))
        (setq yunge-reader-webview--buffer-view view)
        (yunge-reader-epub--configure-zoom 'reflow)
        (yunge-reader-epub-view-mode 1)
        (cl-letf
            (((symbol-function 'yunge-reader-webview--set-view-style)
              (lambda (value style)
                (setq applied-style (copy-tree style))
                (setf (yunge-reader-webview--view-style value)
                      (copy-tree style))))
             ((symbol-function 'yunge-reader-webview--sync-view)
              (lambda (_view) (cl-incf syncs)))
             ((symbol-function 'yunge-reader-record-place)
              (lambda (&optional _window) (cl-incf records))))
          (should (= (yunge-reader-zoom-in) 1.5))
          (should (= (alist-get 'font-scale applied-style) 1.5))
          (should (= yunge-reader-effective-scale 1.5))
          (yunge-reader-zoom-in 10)
          (should (= yunge-reader-scale 3.0))
          (yunge-reader-zoom-out 20)
          (should (= yunge-reader-scale 0.5))
          (should (= (yunge-reader-zoom-reset) 1.25))
          (should (= (alist-get 'font-scale applied-style) 1.25))
          (should (string-match-p "Font 125%" header-line-format))
          (should (= syncs 4))
          (should (= records 4)))))))

(ert-deftest yunge-reader-epub-maps-reader-zoom-to-fixed-layout ()
  (let ((yunge-reader-zoom-factor 1.2)
        applied
        (syncs 0)
        (records 0))
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader-document
            (yunge-reader-epub-test--document nil 'fixed))
      (let ((view
             (yunge-reader-webview--make-view :layout 'fixed)))
        (setq yunge-reader-webview--buffer-view view)
        (yunge-reader-epub--configure-zoom 'fixed)
        (yunge-reader-epub-view-mode 1)
        (cl-letf
            (((symbol-function 'yunge-reader-webview--set-view-zoom)
              (lambda (value zoom)
                (setq applied zoom)
                (setf (yunge-reader-webview--view-zoom value) zoom)))
             ((symbol-function 'yunge-reader-webview--sync-view)
              (lambda (_view) (cl-incf syncs)))
             ((symbol-function 'yunge-reader-record-place)
              (lambda (&optional _window) (cl-incf records))))
          (setq yunge-reader-effective-scale 0.8)
          (should (= (yunge-reader-zoom-in) 0.96))
          (should (= applied 0.96))
          (should (eq yunge-reader-zoom-mode 'manual))
          (should (eq (yunge-reader-fit-width) 'fit-width))
          (should (eq applied 'fit-width))
          (should (eq (yunge-reader-fit-page) 'fit-page))
          (should (eq applied 'fit-page))
          (should (= (yunge-reader-zoom-reset) 1.0))
          (should (= applied 1.0))
          (should (string-match-p "Zoom 100%" header-line-format))
          (should (= syncs 4))
          (should (= records 4)))))))

(ert-deftest yunge-reader-epub-keeps-effective-zoom-view-local ()
  (let ((first (generate-new-buffer " *fixed EPUB first*"))
        (second (generate-new-buffer " *fixed EPUB second*")))
    (unwind-protect
        (let (first-view second-view)
          (with-current-buffer first
            (yunge-reader-mode)
            (setq yunge-reader-document
                  (yunge-reader-epub-test--document nil 'fixed)
                  first-view
                  (yunge-reader-webview--make-view
                   :buffer first :layout 'fixed))
            (setq yunge-reader-webview--buffer-view first-view)
            (yunge-reader-epub--configure-zoom 'fixed)
            (yunge-reader-epub-view-mode 1)
            (yunge-reader-epub--update-header))
          (with-current-buffer second
            (yunge-reader-mode)
            (setq yunge-reader-document
                  (yunge-reader-epub-test--document nil 'fixed)
                  second-view
                  (yunge-reader-webview--make-view
                   :buffer second :layout 'fixed))
            (setq yunge-reader-webview--buffer-view second-view)
            (yunge-reader-epub--configure-zoom 'fixed)
            (yunge-reader-epub-view-mode 1)
            (yunge-reader-epub--update-header))
          (yunge-reader-epub--zoom-changed first-view 0.75)
          (with-current-buffer first
            (should (= yunge-reader-effective-scale 0.75))
            (should
             (string-match-p "Fit Page 75%" header-line-format)))
          (with-current-buffer second
            (should-not yunge-reader-effective-scale)
            (should (string-match-p "Fit Page" header-line-format))))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq yunge-reader-document nil))
          (kill-buffer buffer))))))

(ert-deftest yunge-reader-epub-adjusts-view-local-text-layout ()
  (let ((yunge-reader-epub-default-line-height 1.6)
        (yunge-reader-epub-default-content-width 720)
        (yunge-reader-epub-line-height-step 0.1)
        (yunge-reader-epub-content-width-step 40)
        (other-view
         (yunge-reader-webview--make-view
          :style (yunge-reader-epub--default-style)))
        (syncs 0)
        (records 0))
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader-document
            (yunge-reader-epub-test--document))
      (let ((view
             (yunge-reader-webview--make-view
              :style (yunge-reader-epub--default-style))))
        (setq yunge-reader-webview--buffer-view view)
        (yunge-reader-epub--configure-zoom 'reflow)
        (yunge-reader-epub-view-mode 1)
        (cl-letf
            (((symbol-function 'yunge-reader-webview--sync-view)
              (lambda (_view) (cl-incf syncs)))
             ((symbol-function 'yunge-reader-record-place)
              (lambda (&optional _window) (cl-incf records))))
          (should
           (= (yunge-reader-epub-increase-line-height 2) 1.8))
          (should
           (= (yunge-reader-epub-decrease-line-height 20) 1.0))
          (should (= (yunge-reader-epub-widen-content 2) 800))
          (should (= (yunge-reader-epub-narrow-content 20) 320))
          (should
           (equal (yunge-reader-epub-reset-text-layout)
                  '(1.6 720)))
          (let ((style (yunge-reader-webview--view-style view)))
            (should (= (alist-get 'font-scale style) 1.0))
            (should (= (alist-get 'line-height style) 1.6))
            (should (= (alist-get 'content-width style) 720))
            (should (= (alist-get 'side-padding style) 7.0)))
          (should (= syncs 5))
          (should (= records 5))
          (should
           (equal (yunge-reader-webview--view-style other-view)
                  (yunge-reader-epub--default-style))))))))

(ert-deftest yunge-reader-epub-rejects-unsupported-hosts-before-open ()
  (let (completed opened)
    (cl-letf
        (((symbol-function 'yunge-reader-epub--supported-p)
          (lambda () nil))
         ((symbol-function 'yunge-reader-webview--open-publication)
          (lambda (&rest _arguments) (setq opened t))))
      (yunge-reader-epub--open
       "book.epub"
       (lambda (_handle _properties error-data)
         (setq completed error-data))))
    (should completed)
    (should-not opened)))

(ert-deftest yunge-reader-epub-balances-shared-publication-ownership ()
  (let* ((handle (yunge-reader-epub-test--handle 11))
         (document (yunge-reader-epub-test--document handle))
         (yunge-reader-saved-appearance-overrides nil)
         detach-complete
         close-complete
         closed-publication)
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader-document document)
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--attach-shared-publication)
            (lambda (publication layout &rest options)
              (should (eq layout 'reflow))
              (setq yunge-reader-webview--buffer-view
                    (yunge-reader-webview--make-view
                     :buffer (current-buffer)
                     :publication publication
                     :layout layout
                     :persistent t
                     :appearance-function
                     (plist-get options :appearance-function)
                     :style
                     (copy-tree (plist-get options :style))
                     :location-changed-function
                     (plist-get options :location-changed-function)
                     :selection-changed-function
                     (plist-get options :selection-changed-function)
                     :accelerator-function
                     (plist-get options :accelerator-function)
                     :scroll-bar-function
                     (plist-get options :scroll-bar-function)
                     :external-link-function
                     (plist-get options :external-link-function)))))
           ((symbol-function
             'yunge-reader-webview--detach-shared-publication)
            (lambda (complete)
              (setq yunge-reader-webview--buffer-view nil
                    detach-complete complete)))
           ((symbol-function 'process-live-p)
            (lambda (_process) t))
           ((symbol-function
             'yunge-reader-webview--close-publication)
            (lambda (publication complete)
              (setq closed-publication publication
                    close-complete complete))))
        (yunge-reader-epub--attach document)
        (should yunge-reader-epub-view-mode)
        (should (= (yunge-reader-webview--view-publication
                    yunge-reader-webview--buffer-view)
                   11))
        (should
         (eq (yunge-reader-webview--view-selection-changed-function
              yunge-reader-webview--buffer-view)
             #'yunge-reader-epub--selection-changed))
        (should
         (eq (yunge-reader-webview--view-accelerator-function
              yunge-reader-webview--buffer-view)
             #'yunge-reader-epub--accelerator))
        (should
         (eq (yunge-reader-webview--view-scroll-bar-function
              yunge-reader-webview--buffer-view)
             #'yunge-reader-epub--scroll-bar-mode))
        (should
         (eq (yunge-reader-webview--view-external-link-function
              yunge-reader-webview--buffer-view)
             #'yunge-reader-epub--external-link))
        (should
         (eq (yunge-reader-webview--view-appearance-function
              yunge-reader-webview--buffer-view)
             #'yunge-reader-epub--resolved-appearance))
        (should
         (equal
          (yunge-reader-webview--view-style
           yunge-reader-webview--buffer-view)
          '((font-scale . 1.0)
            (line-height . 1.6)
            (content-width . 720)
            (side-padding . 7.0))))
        (yunge-reader--store-appearance-override
         (yunge-reader-document-file document) 'follow-emacs)
        (run-hooks 'yunge-reader-appearance-change-hook)
        (should-not
         (yunge-reader-webview--view-appearance
          yunge-reader-webview--buffer-view))
        (yunge-reader-epub--detach document)
        (should-not yunge-reader-epub-view-mode)
        (should-not
         (memq #'yunge-reader-epub--appearance-changed
               yunge-reader-appearance-change-hook))
        (should (= (yunge-reader-epub-handle-pending-detaches
                    handle)
                   1))
        (yunge-reader-epub--close document)
        (should-not closed-publication)
        (funcall detach-complete)
        (should (= closed-publication 11))
        (should-not
         (yunge-reader-epub-handle-closed handle))
        (funcall close-complete '((closed . t)) nil)
        (should (yunge-reader-epub-handle-closed handle))))))

(ert-deftest yunge-reader-epub-opens-only-allowlisted-external-links ()
  (let ((yunge-reader-uri-schemes '("https"))
        opened)
    (require 'browse-url)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (uri &rest _arguments)
                 (setq opened uri))))
      (let ((inhibit-message t))
        (should
         (yunge-reader-epub--external-link
          (yunge-reader-webview--make-view)
          "https://example.com/reference")))
      (should (equal opened "https://example.com/reference"))
      (setq opened nil)
      (should-error
       (yunge-reader-epub--external-link
        (yunge-reader-webview--make-view)
        "javascript:alert(1)")
       :type 'user-error)
      (should-not opened))))

(ert-deftest yunge-reader-epub-resolves-forwarded-keys-in-emacs ()
  (let ((view (yunge-reader-webview--make-view))
        called)
    (with-temp-buffer
      (yunge-reader-mode)
      (yunge-reader-epub-view-mode 1)
      (setq yunge-reader-webview--buffer-view view)
      (cl-letf
          (((symbol-function 'yunge-reader-epub-next-page)
            (lambda (&optional count)
              (interactive "p")
              (setq called (list count (current-buffer))))))
        (yunge-reader-epub--accelerator view "J"))
      (should (equal called (list 1 (current-buffer))))
      (cl-letf
          (((symbol-function 'yunge-reader-epub-next-line)
            (lambda (&optional count)
              (interactive "p")
              (setq called (list 'line count (current-buffer))))))
        (yunge-reader-epub--accelerator view "j"))
      (should
       (equal called (list 'line 1 (current-buffer))))
      (let ((command
             (lambda ()
               (interactive)
               (setq called
                     (list 'remapped this-command
                           (current-buffer))))))
        (cl-letf (((symbol-function 'key-binding)
                   (lambda (key &rest _arguments)
                     (should (equal key (kbd "+")))
                     command)))
          (yunge-reader-epub--accelerator view "+"))
        (should (eq (car called) 'remapped))
        (should (eq (cadr called) command))
        (should (eq (caddr called) (current-buffer))))
      (cl-letf
          (((symbol-function 'yunge-reader-epub-copy-selection)
            (lambda ()
              (interactive)
              (setq called (list 'copied (current-buffer))))))
        (yunge-reader-epub--accelerator view "y"))
      (should (equal called (list 'copied (current-buffer))))
      (cl-letf
          (((symbol-function 'yunge-reader-epub-last-location)
            (lambda ()
              (interactive)
              (setq called (list 'last (current-buffer))))))
        (yunge-reader-epub--accelerator view "G"))
      (should (equal called (list 'last (current-buffer)))))))

(ert-deftest yunge-reader-epub-navigation-cancels-a-delayed-search-jump ()
  (let ((view (yunge-reader-webview--make-view))
        (yunge-reader-search-query "needle")
        (yunge-reader--search-navigation-intent 'forward)
        navigated)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--current-ready-view)
          (lambda () view))
         ((symbol-function 'yunge-reader-webview--navigate-view)
          (lambda (actual command complete &optional target)
            (setq navigated (list actual command complete target)))))
      (yunge-reader-epub--navigate "next-screen"))
    (should-not yunge-reader--search-navigation-intent)
    (should yunge-reader--search-detached)
    (should (eq (car navigated) view))
    (should (equal (cadr navigated) "next-screen"))))

(ert-deftest yunge-reader-epub-maps-counted-page-and-line-movement ()
  (let (navigations)
    (cl-letf (((symbol-function 'yunge-reader-epub--navigate)
               (lambda (command) (push command navigations))))
      (yunge-reader-epub-next-page 2)
      (yunge-reader-epub-previous-page -1)
      (yunge-reader-epub-next-line 3)
      (yunge-reader-epub-previous-line -2))
    (should
     (equal navigations
             '("next-line" "next-line" "next-line"
               "next-line" "next-line"
               "next-page" "next-page" "next-page")))))

(ert-deftest yunge-reader-epub-maps-semantic-boundary-navigation ()
  (let (navigations)
    (cl-letf (((symbol-function 'yunge-reader-epub--navigate)
               (lambda (command) (push command navigations))))
      (should (eq (yunge-reader-epub-first-location) :deferred))
      (should (eq (yunge-reader-epub-last-location) :deferred)))
    (should (equal navigations '("last" "first")))))

(ert-deftest yunge-reader-epub-tracks-only-semantic-boundaries ()
  (dolist (command
           '(yunge-reader-epub-first-location
             yunge-reader-epub-last-location))
    (should
     (advice-member-p
      #'yunge-jump-history--track-navigation command)))
  (dolist (command
           '(yunge-reader-epub-next-page
             yunge-reader-epub-previous-page
             yunge-reader-epub-next-screen
             yunge-reader-epub-previous-screen
             yunge-reader-epub-next-line
             yunge-reader-epub-previous-line))
    (should-not
     (advice-member-p
      #'yunge-jump-history--track-navigation command))))

(ert-deftest yunge-reader-epub-maps-native-selection-to-reader-state ()
  (let* ((buffer (generate-new-buffer " *EPUB selection owner*"))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer
           :selection-changed-function
           #'yunge-reader-epub--selection-changed)))
    (unwind-protect
        (with-current-buffer buffer
          (yunge-reader-mode)
          (yunge-reader-epub-view-mode 1)
          (setq yunge-reader-webview--buffer-view view)
          (yunge-reader-webview--set-view-selection
           view (yunge-reader-epub-test--selection))
          (setq yunge-reader-search-highlight-visible t)
          (should
           (equal
            (yunge-reader-selection-start yunge-reader-selection)
            (make-yunge-reader-position
             :unit "OPS/chapter.xhtml"
             :offset "epubcfi(/6/4!/4/2/1:0)")))
          (should
           (equal
            (yunge-reader-selection-end yunge-reader-selection)
            (make-yunge-reader-position
             :unit "OPS/chapter.xhtml"
             :offset "epubcfi(/6/4!/4/2/1:8)")))
          (let (echoed-clear)
            (cl-letf
                (((symbol-function
                   'yunge-reader-webview--clear-view-selection)
                  (lambda (_view) (setq echoed-clear t))))
              (yunge-reader-webview--set-view-selection view nil))
            (should-not echoed-clear))
          (should-not yunge-reader-selection)
          (should yunge-reader-search-highlight-visible))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-epub-copy-reads-the-live-native-selection ()
  (let* ((buffer (generate-new-buffer " *EPUB live copy*"))
         (view
          (yunge-reader-webview--make-view
           :surface (yunge-reader-epub-test--surface 7 'ready)
           :buffer buffer
           :selection-changed-function
           #'yunge-reader-epub--selection-changed))
         requested
         completion
         copied)
    (unwind-protect
        (with-current-buffer buffer
          (yunge-reader-mode)
          (yunge-reader-epub-view-mode 1)
          (setq yunge-reader-webview--buffer-view view)
          (cl-letf
              (((symbol-function
                 'yunge-reader-webview--current-ready-view)
                (lambda () view))
               ((symbol-function
                 'yunge-reader-webview--request-current-selection)
                (lambda (actual complete)
                  (setq requested actual
                        completion complete)))
               ((symbol-function 'yunge-reader-copy-selection)
                (lambda ()
                  (setq copied (copy-tree yunge-reader-selection)))))
            (yunge-reader-epub-copy-selection)
            (should (eq requested view))
            (should yunge-reader--copy-pending)
            (funcall completion
                     (yunge-reader-epub-test--selection) nil)
            (should-not yunge-reader--copy-pending)
            (should copied)
            (should
             (equal (yunge-reader-webview--view-selection view)
                    (yunge-reader-epub-test--selection)))))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-epub-copy-rejects-a-stale-live-selection ()
  (let* ((buffer (generate-new-buffer " *EPUB stale live copy*"))
         (view
          (yunge-reader-webview--make-view
           :surface (yunge-reader-epub-test--surface 7 'ready)
           :buffer buffer
           :selection-changed-function
           #'yunge-reader-epub--selection-changed))
         completion
         copied)
    (unwind-protect
        (with-current-buffer buffer
          (yunge-reader-mode)
          (yunge-reader-epub-view-mode 1)
          (setq yunge-reader-webview--buffer-view view)
          (cl-letf
              (((symbol-function
                 'yunge-reader-webview--current-ready-view)
                (lambda () view))
               ((symbol-function
                 'yunge-reader-webview--request-current-selection)
                (lambda (_view complete) (setq completion complete)))
               ((symbol-function 'yunge-reader-copy-selection)
                (lambda () (setq copied t))))
            (yunge-reader-epub-copy-selection)
            (cl-incf yunge-reader--copy-generation)
            (funcall completion
                     (yunge-reader-epub-test--selection) nil)
            (should-not copied)
            (should yunge-reader--copy-pending)))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-epub-clears-native-selection-from-reader ()
  (let ((view (yunge-reader-webview--make-view))
        cleared)
    (with-temp-buffer
      (yunge-reader-mode)
      (yunge-reader-epub-view-mode 1)
      (setq yunge-reader-webview--buffer-view view
            yunge-reader-selection
            (make-yunge-reader-selection
             :start (make-yunge-reader-position :unit "chapter" :offset 1)
             :end (make-yunge-reader-position :unit "chapter" :offset 2)))
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--clear-view-selection)
            (lambda (value) (setq cleared value))))
        (yunge-reader-clear-selection t))
      (should (eq cleared view)))))

(ert-deftest yunge-reader-epub-syncs-only-the-current-search-result ()
  (let ((view (yunge-reader-webview--make-view))
        updates)
    (with-temp-buffer
      (yunge-reader-mode)
      (yunge-reader-epub-view-mode 1)
      (setq yunge-reader-webview--buffer-view view
            yunge-reader-search-highlight-visible t
            yunge-reader-search-result
            (make-yunge-reader-search-result
             :start
             (make-yunge-reader-position
              :unit "OPS/chapter.xhtml"
              :offset "epubcfi(/6/4!/4/2/1:0)")
             :end
             (make-yunge-reader-position
              :unit "OPS/chapter.xhtml"
              :offset "epubcfi(/6/4!/4/2/1:8)")))
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--set-view-search-result)
            (lambda (_view selection) (push selection updates))))
        (run-hooks 'yunge-reader-search-result-hook)
        (setq yunge-reader-search-highlight-visible nil)
        (run-hooks 'yunge-reader-search-result-hook))
      (should (= (length updates) 2))
      (should-not (car updates))
      (should
       (equal (cadr updates)
              (yunge-reader-epub-test--selection))))))

(ert-deftest yunge-reader-epub-dismiss-keys-use-active-evil-maps ()
  (yunge-test-enable-evil)
  (let ((view (yunge-reader-webview--make-view))
        (clears 0))
    (with-temp-buffer
      (yunge-reader-mode)
      (yunge-reader-epub-view-mode 1)
      (setq yunge-reader-webview--buffer-view view)
      (should (eq (key-binding (kbd "<escape>"))
                  'evil-force-normal-state))
      (evil-insert-state 1)
      (should (eq evil-state 'insert))
      (yunge-reader-epub--accelerator view "<escape>")
      (should (eq evil-state 'normal))
      (dolist (key '("<escape>" "C-g"))
        (setq yunge-reader-selection
              (make-yunge-reader-selection
               :start
               (make-yunge-reader-position :unit "chapter" :offset 1)
               :end
               (make-yunge-reader-position :unit "chapter" :offset 2))
              yunge-reader-search-highlight-visible t)
        (cl-letf
            (((symbol-function
               'yunge-reader-webview--clear-view-selection)
              (lambda (_view) (cl-incf clears))))
          (yunge-reader-epub--accelerator view key))
        (should-not yunge-reader-selection)
        (should-not yunge-reader-search-highlight-visible))
      (should (= clears 2)))))

(ert-deftest yunge-reader-epub-maps-reader-selection-text-batches ()
  (let* ((document (yunge-reader-epub-test--document))
         (selection (yunge-reader-epub-test--selection))
         (start
          (make-yunge-reader-position
           :unit (alist-get 'href selection)
           :offset (alist-get 'start selection)))
         (end
          (make-yunge-reader-position
           :unit (alist-get 'href selection)
           :offset (alist-get 'end selection)))
         (cursor
          (make-yunge-reader-position
           :unit (alist-get 'href selection) :offset 4))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-epub-test--surface 9 'ready)
           :publication 7
           :selection (copy-tree selection)))
         request
         result
         error-data)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view view)
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--request-selection-text)
            (lambda (requested-view requested-selection offset limit
                                    complete)
              (setq request
                    (list requested-view requested-selection offset limit))
              (funcall
               complete
               '((text . "text")
                 (total . 12)
                 (next-offset . 8)
                 (done))
               nil))))
        (yunge-reader-epub--request
         document 'selection-text
         (list :start start :end end :cursor cursor
               :unit-limit 1 :character-limit 16)
         (lambda (value error)
           (setq result value
                 error-data error)))))
    (should-not error-data)
    (should (equal request (list view selection 4 16)))
    (should (equal (yunge-reader-selection-batch-text result) "text"))
    (should-not (yunge-reader-selection-batch-done result))
    (should
     (equal
      (yunge-reader-selection-batch-cursor result)
      (make-yunge-reader-position
       :unit "OPS/chapter.xhtml" :offset 8)))))

(ert-deftest yunge-reader-epub-maps-bounded-search-batches ()
  (let* ((document (yunge-reader-epub-test--document))
         (cursor
          (make-yunge-reader-search-cursor
           :value '((href . "OPS/chapter.xhtml") (offset . 2))))
         (view
          (yunge-reader-webview--make-view
           :surface
           (yunge-reader-epub-test--surface 9 'ready)
           :publication 7))
         request
         result
         error-data)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view view)
      (cl-letf
          (((symbol-function 'yunge-reader-webview--request-search)
            (lambda (requested-view query case-sensitive direction
                                    origin native-cursor match-limit
                                    section-limit complete)
              (setq request
                    (list requested-view query case-sensitive direction
                          origin native-cursor match-limit section-limit))
              (funcall
               complete
               '((matches
                  . (((href . "OPS/chapter.xhtml")
                      (start . "epubcfi(/6/4!/4/2/1:0)")
                      (end . "epubcfi(/6/4!/4/2/1:7)")
                      (text . "Chapter")
                      (before . "A ")
                      (after . " title"))))
                 (cursor
                  . ((href . "OPS/chapter.xhtml") (offset . 3)))
                 (done))
               nil))))
        (yunge-reader-epub--request
         document 'search
         (list :query "Chapter" :case-sensitive t :direction 'forward
               :origin nil :cursor cursor :match-limit 32 :page-limit 8)
         (lambda (value error)
           (setq result value
                 error-data error)))))
    (should-not error-data)
    (should
     (equal
      request
       (list view "Chapter" t 'forward nil
             '((href . "OPS/chapter.xhtml") (offset . 2))
            32 8)))
    (let ((match (car (yunge-reader-search-batch-results result))))
      (should (equal (yunge-reader-search-result-text match) "Chapter"))
      (should
       (equal
        (yunge-reader-search-result-start match)
        (make-yunge-reader-position
         :unit "OPS/chapter.xhtml"
         :offset "epubcfi(/6/4!/4/2/1:0)"))))
    (should-not (yunge-reader-search-batch-done result))
    (should
       (equal
        (yunge-reader-search-batch-cursor result)
        (make-yunge-reader-search-cursor
         :value '((href . "OPS/chapter.xhtml") (offset . 3)))))))

(ert-deftest yunge-reader-epub-rejects-invalid-search-cursors ()
  (let ((document (yunge-reader-epub-test--document))
        (cursor
         (make-yunge-reader-search-cursor
          :value '((href . "../chapter.xhtml") (offset . 0))))
        requested
        error-data)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view
            (yunge-reader-webview--make-view
             :surface
             (yunge-reader-epub-test--surface 9 'ready)
             :publication 7))
      (cl-letf
          (((symbol-function 'yunge-reader-webview--request-search)
            (lambda (&rest _arguments) (setq requested t))))
        (yunge-reader-epub--request
         document 'search
         (list :query "Chapter" :case-sensitive nil :direction 'forward
               :origin nil :cursor cursor :match-limit 32 :page-limit 8)
         (lambda (_value error) (setq error-data error)))))
    (should error-data)
    (should-not requested)))

(ert-deftest yunge-reader-epub-rejects-stale-selection-copy ()
  (let* ((document (yunge-reader-epub-test--document))
         (selection (yunge-reader-epub-test--selection))
         (start
          (make-yunge-reader-position
           :unit (alist-get 'href selection)
           :offset (alist-get 'start selection)))
         (end
          (make-yunge-reader-position
           :unit (alist-get 'href selection)
           :offset (alist-get 'end selection)))
         error-data
         requested)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view
            (yunge-reader-webview--make-view
             :surface
             (yunge-reader-epub-test--surface 9 'ready)
             :publication 7 :selection nil))
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--request-selection-text)
            (lambda (&rest _arguments) (setq requested t))))
        (yunge-reader-epub--request
         document 'selection-text
         (list :start start :end end :cursor nil
               :unit-limit 1 :character-limit 16)
         (lambda (_value error) (setq error-data error)))))
    (should error-data)
    (should-not requested)))

(ert-deftest yunge-reader-epub-does-not-hide-close-failures ()
  (let ((handle (yunge-reader-epub-test--handle))
        warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest value) (setq warning value))))
      (yunge-reader-epub--close-complete
       handle nil '(error "publication remains in use")))
    (should-not (yunge-reader-epub-handle-closed handle))
    (should (equal (cadr warning) "publication remains in use"))))

(ert-deftest yunge-reader-epub-keeps-locator-data-printable ()
  (let* ((location
          (yunge-reader-epub-test--location 0.35 12.5 30.0))
         (position
           (yunge-reader-epub--locator-position location)))
    (should (yunge-reader-position-p position))
    (should
     (equal
      (yunge-reader-epub--position-locator position)
      '((cfi . "epubcfi(/6/4!/4/2)")
        (href . "OPS/chapter.xhtml")
        (x . 12.5)
        (y . 30.0))))
    (should
     (equal (yunge-reader--position-data position)
            '(:unit "OPS/chapter.xhtml"
              :offset "epubcfi(/6/4!/4/2)"
               :x 12.5
               :y 30.0)))))

(ert-deftest yunge-reader-epub-does-not-store-reflow-fraction-as-x ()
  (let ((position
         (yunge-reader-epub--locator-position
          (yunge-reader-epub-test--location 0.35))))
    (should-not (yunge-reader-position-x position))
    (should-not (yunge-reader-position-y position))
    (should
     (equal
      (yunge-reader-epub--position-locator position)
      '((cfi . "epubcfi(/6/4!/4/2)")
        (href . "OPS/chapter.xhtml"))))))

(ert-deftest yunge-reader-epub-maps-bounded-renderer-outlines ()
  (let* ((view
          (yunge-reader-webview--make-view
           :publication 7
           :outline-ready t
           :outline
           '((items
              . (((title . "Part One") (depth . 0))
                 ((title . "Chapter")
                  (depth . 1)
                  (href . "OPS/chapter.xhtml#start"))))
             (truncated))))
         (document (yunge-reader-epub-test--document))
         result
         request-error)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view view)
      (yunge-reader-epub--request
       document 'outline nil
       (lambda (value error-data)
         (setq result value
               request-error error-data))))
    (should-not request-error)
    (should (yunge-reader-outline-data-p result))
    (should-not (yunge-reader-outline-data-truncated result))
    (let ((items (yunge-reader-outline-data-items result)))
      (should (= (length items) 2))
      (should-not (yunge-reader-outline-item-action (car items)))
      (let* ((action
              (yunge-reader-outline-item-action (cadr items)))
             (position (yunge-reader-action-position action)))
        (should (eq (yunge-reader-action-type action) 'location))
        (should
         (equal (yunge-reader-position-unit position)
                "OPS/chapter.xhtml#start"))
        (should-not (yunge-reader-position-offset position))))))

(ert-deftest yunge-reader-epub-restores-before-or-after-surface-ready ()
  (let* ((location
          (yunge-reader-epub-test--location nil 18.0 24.0))
         (position
          (yunge-reader-epub--locator-position location))
         (view
          (yunge-reader-webview--make-view
           :publication 3
           :persistent t))
         navigations)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view view)
      (cl-letf
          (((symbol-function 'yunge-reader-webview--navigate-view)
            (lambda (_view command _complete &optional target)
              (push (list command target) navigations))))
        (should
         (yunge-reader-epub--restore-location nil position nil))
        (should-not navigations)
        (should
         (equal (yunge-reader-webview--view-location view)
                location))
        (setf (yunge-reader-webview--view-surface view)
              (yunge-reader-epub-test--surface 9 'ready))
        (should
         (yunge-reader-epub--restore-location nil position nil))
        (should
         (equal navigations (list (list "go-to" location))))))))

(ert-deftest yunge-reader-epub-keeps-outline-targets-transient ()
  (let* ((stable (yunge-reader-epub-test--location 0.4))
         (position
          (make-yunge-reader-position
           :unit "OPS/chapter.xhtml#section"))
         (view
          (yunge-reader-webview--make-view
           :publication 3
           :persistent t
           :location (copy-tree stable)))
         navigation)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view view)
      (cl-letf
          (((symbol-function 'yunge-reader-webview--navigate-view)
            (lambda (_view command _complete &optional target)
              (setq navigation (list command target)))))
        (should
         (eq (yunge-reader-epub--restore-location nil position nil)
             :deferred))
        (should-not navigation)
        (should
         (equal (yunge-reader-webview--view-pending-target view)
                '((href . "OPS/chapter.xhtml#section"))))
        (setf (yunge-reader-webview--view-surface view)
              (yunge-reader-epub-test--surface 9 'ready))
        (yunge-reader-webview--dispatch-pending-target view)))
    (should
     (equal navigation
            '("go-to" ((href . "OPS/chapter.xhtml#section")))))
    (should-not
     (yunge-reader-webview--view-pending-target view))
    (should
     (equal (yunge-reader-webview--view-location view) stable))))

(ert-deftest yunge-reader-epub-records-only-renderer-locations ()
  (let* ((buffer (generate-new-buffer " *EPUB location owner*"))
         (window (selected-window))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer
           :surface
           (yunge-reader-epub-test--surface
            9 'opening :window window)
           :persistent t))
         recorded)
    (unwind-protect
        (with-current-buffer buffer
          (yunge-reader-mode)
          (yunge-reader-epub-view-mode 1)
          (setq yunge-reader-webview--buffer-view view)
          (cl-letf (((symbol-function 'yunge-reader-record-place)
                     (lambda (&optional value)
                       (setq recorded value))))
            (yunge-reader-epub--location-changed view nil)
            (should-not recorded)
            (yunge-reader-webview--set-surface-state
             (yunge-reader-webview--view-surface view) 'ready)
            (yunge-reader-epub--location-changed view nil))
          (should (eq recorded window)))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-epub-user-location-cancels-search-navigation ()
  (let* ((buffer (generate-new-buffer " *EPUB user location owner*"))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer
           :surface
           (yunge-reader-epub-test--surface
            9 'ready :window (selected-window))
           :persistent t)))
    (unwind-protect
        (with-current-buffer buffer
          (yunge-reader-mode)
          (yunge-reader-epub-view-mode 1)
          (setq yunge-reader-webview--buffer-view view
                yunge-reader-search-query "needle"
                yunge-reader--search-navigation-intent 'forward)
          (cl-letf (((symbol-function 'yunge-reader-record-place)
                     #'ignore))
            (yunge-reader-epub--location-changed view t))
          (should-not yunge-reader--search-navigation-intent)
          (should yunge-reader--search-detached))
      (kill-buffer buffer))))

;;; yunge-reader-epub-test.el ends here
