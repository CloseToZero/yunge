;;; yunge-reader-epub-test.el --- EPUB reader tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-epub)

(defun yunge-reader-epub-test--location (&optional fraction)
  "Return one bounded EPUB locator with optional FRACTION."
  (append
   '((cfi . "epubcfi(/6/4!/4/2)"))
   '((href . "OPS/chapter.xhtml"))
   (when fraction `((fraction . ,fraction)))))

(defun yunge-reader-epub-test--handle (&optional publication)
  "Return a live EPUB handle for PUBLICATION."
  (make-yunge-reader-epub-handle
   :publication (or publication 7)
   :metadata '(:title "Test Book")
   :pending-detaches 0))

(defun yunge-reader-epub-test--document (&optional handle)
  "Return a reflowable document backed by HANDLE."
  (make-yunge-reader-document
   :file "test.epub"
   :handle (or handle (yunge-reader-epub-test--handle))
   :layout 'reflow
   :metadata '(:title "Test Book")))

(defun yunge-reader-epub-test--selection ()
  "Return one stable same-spine EPUB selection."
  '((href . "OPS/chapter.xhtml")
    (start . "epubcfi(/6/4!/4/2/1:0)")
    (end . "epubcfi(/6/4!/4/2/1:8)")))

(ert-deftest yunge-reader-epub-uses-reflowable-screen-bindings ()
  (yunge-test-keymap-keys
   yunge-reader-epub-view-mode-map
   '(("C-d" . yunge-reader-epub-next-screen)
     ("C-u" . yunge-reader-epub-previous-screen)
     ("J" . yunge-reader-epub-next-screen)
     ("K" . yunge-reader-epub-previous-screen)))
  (should-not
   (lookup-key yunge-reader-epub-view-mode-map (kbd "j")))
  (should-not
   (lookup-key yunge-reader-epub-view-mode-map (kbd "k"))))

(ert-deftest yunge-reader-epub-integrates-screen-bindings-with-evil ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-epub-view-mode 1)
    (yunge-test-evil-keys
     'normal
     '(("C-d" . yunge-reader-epub-next-screen)
       ("C-u" . yunge-reader-epub-previous-screen)
       ("J" . yunge-reader-epub-next-screen)
       ("K" . yunge-reader-epub-previous-screen)
       ("+" . yunge-reader-zoom-in)
       ("-" . yunge-reader-zoom-out)
       ("=" . yunge-reader-zoom-reset)
       ("j" . evil-next-line)
       ("k" . evil-previous-line)
       ("n" . yunge-reader-search-next)
       ("q" . evil-record-macro)))))

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
           (version . "3.0")))
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
        (document (yunge-reader-epub-test--document))
        attached-style)
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader--pending-place
            '(:zoom-mode manual :scale 1.8))
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--attach-shared-publication)
            (lambda (_publication _location _location-function
                     _selection-function _accelerator-function style
                     _scroll-bar-function)
              (setq attached-style style))))
        (yunge-reader-epub--attach document))
      (should yunge-reader-epub-view-mode)
      (should (= yunge-reader-default-scale 1.25))
      (should (= yunge-reader-minimum-scale 0.5))
      (should (= yunge-reader-maximum-scale 3.0))
      (should (eq yunge-reader-zoom-mode 'manual))
      (should (= yunge-reader-scale 1.8))
      (should (= yunge-reader-effective-scale 1.8))
      (should (= (alist-get 'font-scale attached-style) 1.8))
      (should (string-match-p "Font 180%" header-line-format)))))

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
      (let ((view
             (yunge-reader-webview--make-view
              :style (yunge-reader-epub--default-style))))
        (setq yunge-reader-webview--buffer-view view)
        (yunge-reader-epub--configure-zoom)
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
         detach-complete
         close-complete
         closed-publication)
    (with-temp-buffer
      (yunge-reader-mode)
      (setq yunge-reader-document document)
      (cl-letf
          (((symbol-function
             'yunge-reader-webview--attach-shared-publication)
            (lambda (publication _location location-function
                                 selection-function
                                 accelerator-function style
                                 scroll-bar-function)
              (setq yunge-reader-webview--buffer-view
                    (yunge-reader-webview--make-view
                     :buffer (current-buffer)
                     :publication publication
                     :persistent t
                     :style (copy-tree style)
                     :location-changed-function location-function
                     :selection-changed-function selection-function
                     :accelerator-function accelerator-function
                     :scroll-bar-function scroll-bar-function))))
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
         (equal
          (yunge-reader-webview--view-style
           yunge-reader-webview--buffer-view)
          '((font-scale . 1.0)
            (line-height . 1.6)
            (content-width . 720)
            (side-padding . 7.0))))
        (yunge-reader-epub--detach document)
        (should-not yunge-reader-epub-view-mode)
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

(ert-deftest yunge-reader-epub-resolves-forwarded-keys-in-emacs ()
  (let ((view (yunge-reader-webview--make-view))
        called)
    (with-temp-buffer
      (yunge-reader-mode)
      (yunge-reader-epub-view-mode 1)
      (setq yunge-reader-webview--buffer-view view)
      (cl-letf
          (((symbol-function 'yunge-reader-epub-next-screen)
            (lambda (&optional count)
              (interactive "p")
              (setq called (list count (current-buffer))))))
        (yunge-reader-epub--accelerator view "J"))
      (should (equal called (list 1 (current-buffer))))
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
          (((symbol-function 'yunge-reader-copy-selection)
            (lambda ()
              (interactive)
              (setq called (list 'copied (current-buffer))))))
        (yunge-reader-epub--accelerator view "y"))
      (should (equal called (list 'copied (current-buffer)))))))

(ert-deftest yunge-reader-epub-navigation-cancels-a-delayed-search-jump ()
  (let ((view (yunge-reader-webview--make-view))
        (yunge-reader--search-navigation-intent 'next)
        navigated)
    (cl-letf
        (((symbol-function 'yunge-reader-webview--current-ready-view)
          (lambda () view))
         ((symbol-function 'yunge-reader-webview--navigate-view)
          (lambda (actual command complete &optional target)
            (setq navigated (list actual command complete target)))))
      (yunge-reader-epub--navigate "next-screen"))
    (should-not yunge-reader--search-navigation-intent)
    (should (eq (car navigated) view))
    (should (equal (cadr navigated) "next-screen"))))

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
           :id 9
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
          (make-yunge-reader-position
           :unit "OPS/chapter.xhtml" :offset 2))
         (view
          (yunge-reader-webview--make-view
           :id 9 :publication 7))
         request
         result
         error-data)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view view)
      (cl-letf
          (((symbol-function 'yunge-reader-webview--request-search)
            (lambda (requested-view query case-sensitive native-cursor
                                    match-limit section-limit complete)
              (setq request
                    (list requested-view query case-sensitive native-cursor
                          match-limit section-limit))
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
         (list :query "Chapter" :case-sensitive t :cursor cursor
               :match-limit 32 :page-limit 8)
         (lambda (value error)
           (setq result value
                 error-data error)))))
    (should-not error-data)
    (should
     (equal
      request
      (list view "Chapter" t
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
      (make-yunge-reader-position
       :unit "OPS/chapter.xhtml" :offset 3)))))

(ert-deftest yunge-reader-epub-rejects-invalid-search-cursors ()
  (let ((document (yunge-reader-epub-test--document))
        (cursor
         (make-yunge-reader-position
          :unit "../chapter.xhtml" :offset 0))
        requested
        error-data)
    (with-temp-buffer
      (setq yunge-reader-webview--buffer-view
            (yunge-reader-webview--make-view
             :id 9 :publication 7))
      (cl-letf
          (((symbol-function 'yunge-reader-webview--request-search)
            (lambda (&rest _arguments) (setq requested t))))
        (yunge-reader-epub--request
         document 'search
         (list :query "Chapter" :case-sensitive nil :cursor cursor
               :match-limit 32 :page-limit 8)
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
             :id 9 :publication 7 :selection nil))
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
  (let* ((location (yunge-reader-epub-test--location 0.35))
         (position
          (yunge-reader-epub--locator-position location)))
    (should (yunge-reader-position-p position))
    (should
     (equal (yunge-reader-epub--position-locator position)
            location))
    (should
     (equal (yunge-reader--position-data position)
            '(:unit "OPS/chapter.xhtml"
              :offset "epubcfi(/6/4!/4/2)"
              :x 0.35
              :y nil)))))

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
  (let* ((location (yunge-reader-epub-test--location 0.6))
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
        (setf (yunge-reader-webview--view-publication-ready view) t)
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
        (setf (yunge-reader-webview--view-publication-ready view) t)
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
           :window window
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
            (yunge-reader-epub--location-changed view nil))
          (should (eq recorded window)))
      (kill-buffer buffer))))

(ert-deftest yunge-reader-epub-user-location-cancels-search-navigation ()
  (let* ((buffer (generate-new-buffer " *EPUB user location owner*"))
         (view
          (yunge-reader-webview--make-view
           :buffer buffer
           :window (selected-window)
           :persistent t)))
    (unwind-protect
        (with-current-buffer buffer
          (yunge-reader-mode)
          (yunge-reader-epub-view-mode 1)
          (setq yunge-reader-webview--buffer-view view
                yunge-reader--search-navigation-intent 'next)
          (cl-letf (((symbol-function 'yunge-reader-record-place)
                     #'ignore))
            (yunge-reader-epub--location-changed view t))
          (should-not yunge-reader--search-navigation-intent))
      (kill-buffer buffer))))

;;; yunge-reader-epub-test.el ends here
