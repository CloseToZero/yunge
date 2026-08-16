;;; yunge-reader-pdf-test.el --- PDF reader tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-pdf)

(defun yunge-reader-pdf-test--document (&rest pages)
  "Return a fixed-layout reader document containing PAGES."
  (make-yunge-reader-document
   :metadata (list :page-count (length pages) :pages pages)))

(defun yunge-reader-pdf-test--handle (id &optional session)
  "Return a native PDF handle for ID in SESSION."
  (make-yunge-reader-pdf-handle
   :session (or session 17)
   :id id
   :identity 'test-identity))

(defun yunge-reader-pdf-test--link
    (page index bounds target &optional label)
  "Return one internal PDF link fixture from PAGE to TARGET."
  (make-yunge-reader-pdf-link
   :page page
   :index index
   :bounds bounds
   :label label
   :action
   (make-yunge-reader-action
    :type 'location
    :position (make-yunge-reader-position :unit target))))

(defun yunge-reader-pdf-test--uri-link
    (page index bounds uri &optional label)
  "Return one external PDF URI link fixture from PAGE."
  (make-yunge-reader-pdf-link
   :page page
   :index index
   :bounds bounds
   :label label
   :action (make-yunge-reader-action :type 'uri :uri uri)))

(ert-deftest yunge-reader-pdf-uses-viewer-page-bindings ()
  (yunge-test-keymap-keys
   yunge-reader-pdf-view-mode-map
   '(("RET" . yunge-reader-pdf-follow-link)
     ("C-d" . yunge-reader-pdf-scroll-down)
     ("C-u" . yunge-reader-pdf-scroll-up)
     ("G" . yunge-reader-pdf-last-page)
     ("J" . yunge-reader-pdf-next-page)
     ("K" . yunge-reader-pdf-previous-page)
     ("gg" . yunge-reader-pdf-first-page)
     ("gp" . yunge-reader-pdf-goto-page)
     ("gr" . yunge-reader-refresh)
     ("j" . yunge-reader-pdf-scroll-down-line)
     ("k" . yunge-reader-pdf-scroll-up-line)))
  (should-not
   (eq (lookup-key yunge-reader-pdf-view-mode-map (kbd "n"))
       #'yunge-reader-pdf-next-page))
  (should-not
   (eq (lookup-key yunge-reader-pdf-view-mode-map (kbd "b"))
       #'yunge-reader-pdf-previous-page))
  (should
   (eq (lookup-key yunge-reader-pdf--image-map
                   (kbd "<down-mouse-1>"))
       #'yunge-reader-pdf-select-with-mouse))
  (should
   (eq (lookup-key yunge-reader-pdf--image-map (kbd "<mouse-1>"))
       #'ignore))
  (should
   (eq (lookup-key yunge-reader-pdf--image-map
                   (kbd "C-<mouse-1>"))
       #'yunge-reader-pdf-activate-at-mouse))
  (should
   (eq (lookup-key yunge-reader-pdf--image-map
                   (kbd "<drag-mouse-1>"))
       #'ignore)))

(ert-deftest yunge-reader-pdf-tracks-only-semantic-page-jumps ()
  (dolist (command
           '(yunge-reader-pdf-first-page
             yunge-reader-pdf-last-page
             yunge-reader-pdf-goto-page
             yunge-reader-pdf--follow-location-link))
    (should
     (advice-member-p
      #'yunge-jump-history--track-navigation command)))
  (dolist (command
           '(yunge-reader-pdf-next-page
             yunge-reader-pdf-previous-page
             yunge-reader-pdf--follow-link
             scroll-up-command
             scroll-down-command))
    (should-not
     (advice-member-p
      #'yunge-jump-history--track-navigation command))))

(ert-deftest yunge-reader-pdf-integrates-page-bindings-with-evil ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (yunge-test-evil-keys
     'normal
     '(("RET" . yunge-reader-pdf-follow-link)
       ("/" . yunge-reader-search)
       ("C-d" . yunge-reader-pdf-scroll-down)
       ("C-u" . yunge-reader-pdf-scroll-up)
       ("G" . yunge-reader-pdf-last-page)
       ("J" . yunge-reader-pdf-next-page)
       ("K" . yunge-reader-pdf-previous-page)
       ("gg" . yunge-reader-pdf-first-page)
       ("gp" . yunge-reader-pdf-goto-page)
       ("gr" . yunge-reader-refresh)
       ("j" . yunge-reader-pdf-scroll-down-line)
       ("k" . yunge-reader-pdf-scroll-up-line)
       ("n" . yunge-reader-search-next)
       ("y" . yunge-reader-copy-selection)
       ("b" . evil-backward-word-begin)))
    (yunge-test-which-key-prefix
     "g" '(("g" nil "first page")
           ("p" nil "go to page")
           ("r" nil "refresh")))))

(ert-deftest yunge-reader-pdf-scrolls-half-windows-by-pixels ()
  (let (scrolls updates)
    (cl-letf (((symbol-function 'window-text-height)
               (lambda (&rest _arguments) 601))
              ((symbol-function 'selected-window)
               (lambda () 'window))
              ((symbol-function
                'pixel-scroll-precision-scroll-up-page)
               (lambda (pixels)
                 (push (list 'up pixels) scrolls)))
              ((symbol-function
                'pixel-scroll-precision-scroll-down-page)
               (lambda (pixels)
                 (push (list 'down pixels) scrolls)))
              ((symbol-function
                'yunge-reader-pdf--update-visible-pages)
               (lambda (&optional window)
                 (push window updates))))
      (yunge-reader-pdf-scroll-up 2)
      (yunge-reader-pdf-scroll-down 1))
    (should
     (equal (nreverse scrolls)
            '((up 300) (up 300) (down 300))))
    (should (equal updates '(window window)))))

(ert-deftest yunge-reader-pdf-scrolls-screen-lines-by-pixels ()
  (let (scrolls updates)
    (cl-letf (((symbol-function 'window-frame)
               (lambda (&optional _window) 'frame))
              ((symbol-function 'frame-char-height)
               (lambda (&optional _frame) 19))
              ((symbol-function 'selected-window)
               (lambda () 'window))
              ((symbol-function
                'pixel-scroll-precision-scroll-up-page)
               (lambda (pixels)
                 (push (list 'up pixels) scrolls)))
              ((symbol-function
                'pixel-scroll-precision-scroll-down-page)
               (lambda (pixels)
                 (push (list 'down pixels) scrolls)))
              ((symbol-function
                'yunge-reader-pdf--update-visible-pages)
               (lambda (&optional window)
                 (push window updates))))
      (yunge-reader-pdf-scroll-up-line 2)
      (yunge-reader-pdf-scroll-down-line 1))
    (should
     (equal (nreverse scrolls)
            '((up 19) (up 19) (down 19))))
    (should (equal updates '(window window)))))

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

(ert-deftest yunge-reader-pdf-binds-pdf-file-visits ()
  (should
   (eq (cdr (assoc "\\.pdf\\'" auto-mode-alist))
       'yunge-reader-pdf-mode)))

(ert-deftest yunge-reader-pdf-centers-pages-by-default ()
  (should (default-value 'yunge-reader-pdf-center-pages)))

(ert-deftest yunge-reader-pdf-file-visits-reopen-cleanly ()
  (let ((file (expand-file-name "visited.pdf"))
        (yunge-reader-drivers nil)
        (yunge-reader--document-registry (make-hash-table :test #'equal))
        (yunge-reader-saved-places nil)
        (opens 0)
        (closes 0)
        (attaches 0)
        (detaches 0))
    (cl-letf (((symbol-function 'yunge-reader-pdf--open)
               (lambda (_file complete)
                 (cl-incf opens)
                 (funcall complete 'handle
                          '(:layout fixed
                            :metadata (:page-count 1 :pages nil))
                          nil)))
              ((symbol-function 'yunge-reader-pdf--close)
               (lambda (_document) (cl-incf closes)))
              ((symbol-function 'yunge-reader-pdf--attach)
               (lambda (_document) (cl-incf attaches)))
              ((symbol-function 'yunge-reader-pdf--detach)
               (lambda (_document) (cl-incf detaches))))
      (let ((buffer
             (generate-new-buffer "yunge-reader-pdf-file-test")))
        (unwind-protect
            (with-current-buffer buffer
              (setq buffer-file-name file)
              (set-buffer-multibyte nil)
              (insert "%PDF binary fixture")
              (set-buffer-modified-p nil)
              (set-auto-mode)
              (should (eq major-mode 'yunge-reader-mode))
              (should (equal buffer-file-name file))
              (should enable-multibyte-characters)
              (should (eq revert-buffer-function
                          #'yunge-reader--revert-file-buffer))
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
              (should (memq #'yunge-reader--close-document
                            kill-buffer-hook))
              (should (kill-buffer buffer)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))
    (should (= closes 2))
    (should (= detaches 2))))

(ert-deftest yunge-reader-pdf-attaches-and-detaches-its-view ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((document (yunge-reader-pdf-test--document)))
      (yunge-reader-pdf--attach document)
      (should yunge-reader-pdf-view-mode)
      (should
       (memq #'yunge-reader-pdf--refresh
             yunge-reader-refresh-hook))
      (should
       (memq #'yunge-reader-pdf--update-header
             yunge-reader-view-role-change-hook))
      (yunge-reader-pdf--detach document)
      (should-not yunge-reader-pdf-view-mode)
      (should-not
       (memq #'yunge-reader-pdf--refresh
             yunge-reader-refresh-hook))
      (should-not
       (memq #'yunge-reader-pdf--update-header
             yunge-reader-view-role-change-hook)))))

(ert-deftest yunge-reader-pdf-header-shows-the-view-role ()
  (with-temp-buffer
    (yunge-reader-mode)
    (setq yunge-reader-pdf-page 1
          yunge-reader-document
          (yunge-reader-pdf-test--document nil nil nil)
          yunge-reader-effective-scale 1.5)
    (dolist (spec '((primary . "Primary")
                    (additional . "Additional")
                    (nil . "Reader")))
      (cl-letf (((symbol-function 'yunge-reader-view-role)
                 (lambda () (car spec))))
        (yunge-reader-pdf--update-header)
        (should
         (equal
          header-line-format
          (format " %s  Page 2/3  150%%  Continuous "
                  (cdr spec))))))
    (setq yunge-reader-document nil)))

(ert-deftest yunge-reader-pdf-open-and-close-balance-native-lease ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let (opened
          properties
          error-data
          request
          (leases 0))
      (cl-letf (((symbol-function 'yunge-reader-native-acquire)
                 (lambda ()
                   (cl-incf leases)
                   17))
                ((symbol-function 'yunge-reader-native-release)
                 (lambda () (cl-decf leases)))
                ((symbol-function 'yunge-reader-native-live-p)
                 (lambda () t))
                ((symbol-function
                  'yunge-reader-native-session-live-p)
                 (lambda (session) (= session 17)))
                ((symbol-function
                  'yunge-reader-native-request-in-session)
                 (lambda (session operation parameters complete)
                   (should (= session 17))
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
        (should-not yunge-reader-pdf-view-mode)
        (should (= leases 1))
        (should (yunge-reader-pdf-handle-p opened))
        (should (= (yunge-reader-pdf-handle-session opened) 17))
        (should (= (yunge-reader-pdf-handle-id opened) 7))
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
                 (lambda ()
                   (cl-incf leases)
                   17))
                ((symbol-function 'yunge-reader-native-release)
                 (lambda () (cl-decf leases)))
                ((symbol-function
                  'yunge-reader-native-request-in-session)
                 (lambda (session _operation _parameters complete)
                   (should (= session 17))
                   (funcall complete nil '(error "cannot open")))))
        (yunge-reader-pdf--open
         "C:/books/broken.pdf"
         (lambda (_handle _properties error-data)
           (setq completion-error error-data))))
      (should (zerop leases))
      (should (equal completion-error '(error "cannot open"))))))

(ert-deftest yunge-reader-pdf-retries-and-caches-a-valid-password ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((password-cache t)
          (leases 0)
          (answers (list (copy-sequence "wrong")
                         (copy-sequence "secret")))
          requests
          prompts
          removed
          cached
          opened
          properties
          completion-error)
      (cl-letf
          (((symbol-function 'yunge-reader-native-acquire)
            (lambda ()
              (cl-incf leases)
              17))
           ((symbol-function 'yunge-reader-native-release)
            (lambda () (cl-decf leases)))
           ((symbol-function 'password-read-from-cache)
            (lambda (_key) (copy-sequence "stale")))
           ((symbol-function 'password-cache-remove)
            (lambda (key) (setq removed key)))
           ((symbol-function 'password-cache-add)
            (lambda (key password)
              (setq cached (list key (copy-sequence password)))))
           ((symbol-function
             'yunge-reader-pdf--password-prompt-current-p)
            (lambda (&rest _arguments) t))
           ((symbol-function 'read-passwd)
            (lambda (prompt &rest _arguments)
              (push prompt prompts)
              (pop answers)))
           ((symbol-function
             'yunge-reader-native-request-in-session)
            (lambda (session _operation parameters complete)
              (should (= session 17))
              (let ((password (alist-get 'password parameters)))
                (push (and password (copy-sequence password)) requests)
                (if (equal password "secret")
                    (funcall
                     complete
                     '((document . 9)
                       (page-count . 1)
                       (pages
                        . (((page . 0)
                            (width . 612.0)
                            (height . 792.0)))))
                     nil)
                  (funcall
                   complete nil
                   '(yunge-reader-native-pdf-password-error)))))))
        (yunge-reader-pdf--open
         "C:/books/locked.pdf"
         (lambda (handle value error-data)
           (setq opened handle
                 properties value
                 completion-error error-data))))
      (setq requests (nreverse requests)
            prompts (nreverse prompts))
      (should (= leases 1))
      (should (yunge-reader-pdf-handle-p opened))
      (should (= (yunge-reader-pdf-handle-session opened) 17))
      (should (= (yunge-reader-pdf-handle-id opened) 9))
      (should-not completion-error)
      (should (equal requests '("stale" "wrong" "secret")))
      (should (= (length prompts) 2))
      (should (string-prefix-p "Password for" (car prompts)))
      (should (string-prefix-p "Incorrect password" (cadr prompts)))
      (should (equal removed (car cached)))
      (should (equal (cadr cached) "secret"))
      (should-not (plist-member properties :password)))))

(ert-deftest yunge-reader-pdf-password-cancel-releases-native-lease ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((leases 0)
          completion-error)
      (cl-letf
          (((symbol-function 'yunge-reader-native-acquire)
            (lambda ()
              (cl-incf leases)
              17))
           ((symbol-function 'yunge-reader-native-release)
            (lambda () (cl-decf leases)))
           ((symbol-function 'password-read-from-cache)
            (lambda (_key) nil))
           ((symbol-function
             'yunge-reader-pdf--password-prompt-current-p)
            (lambda (&rest _arguments) t))
           ((symbol-function 'read-passwd)
            (lambda (&rest _arguments) (signal 'quit nil)))
           ((symbol-function
             'yunge-reader-native-request-in-session)
            (lambda (session _operation _parameters complete)
              (should (= session 17))
              (funcall
               complete nil
               '(yunge-reader-native-pdf-password-error)))))
        (yunge-reader-pdf--open
         "C:/books/locked.pdf"
         (lambda (_handle _properties error-data)
           (setq completion-error error-data))))
      (should (zerop leases))
      (should
       (equal completion-error
              '(error "PDF password entry cancelled"))))))

(ert-deftest yunge-reader-pdf-late-password-error-does-not-prompt ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((leases 0)
          prompted
          completion-error)
      (cl-letf
          (((symbol-function 'yunge-reader-native-acquire)
            (lambda ()
              (cl-incf leases)
              17))
           ((symbol-function 'yunge-reader-native-release)
            (lambda () (cl-decf leases)))
           ((symbol-function 'password-read-from-cache)
            (lambda (_key) nil))
           ((symbol-function
             'yunge-reader-pdf--password-prompt-current-p)
            (lambda (&rest _arguments) nil))
           ((symbol-function 'read-passwd)
            (lambda (&rest _arguments) (setq prompted t)))
           ((symbol-function
             'yunge-reader-native-request-in-session)
            (lambda (session _operation _parameters complete)
              (should (= session 17))
              (funcall
               complete nil
               '(yunge-reader-native-pdf-password-error)))))
        (yunge-reader-pdf--open
         "C:/books/locked.pdf"
         (lambda (_handle _properties error-data)
           (setq completion-error error-data))))
      (should (zerop leases))
      (should-not prompted)
      (should
       (eq (car completion-error)
           'yunge-reader-native-pdf-password-error)))))

(ert-deftest yunge-reader-pdf-bounds-password-attempts ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((yunge-reader-pdf-password-attempts 2)
          (leases 0)
          (requests 0)
          (prompts 0)
          completion-error)
      (cl-letf
          (((symbol-function 'yunge-reader-native-acquire)
            (lambda ()
              (cl-incf leases)
              17))
           ((symbol-function 'yunge-reader-native-release)
            (lambda () (cl-decf leases)))
           ((symbol-function 'password-read-from-cache)
            (lambda (_key) nil))
           ((symbol-function
             'yunge-reader-pdf--password-prompt-current-p)
            (lambda (&rest _arguments) t))
           ((symbol-function 'read-passwd)
            (lambda (&rest _arguments)
              (cl-incf prompts)
              (copy-sequence "wrong")))
           ((symbol-function
             'yunge-reader-native-request-in-session)
            (lambda (session _operation _parameters complete)
              (should (= session 17))
              (cl-incf requests)
              (funcall
               complete nil
               '(yunge-reader-native-pdf-password-error)))))
        (yunge-reader-pdf--open
         "C:/books/locked.pdf"
         (lambda (_handle _properties error-data)
           (setq completion-error error-data))))
      (should (zerop leases))
      (should (= requests 3))
      (should (= prompts 2))
      (should
       (eq (car completion-error)
           'yunge-reader-native-pdf-password-error)))))

(ert-deftest yunge-reader-pdf-close-does-not-restart-a-stopped-helper ()
  (let ((leases 1)
        requested)
    (cl-letf (((symbol-function 'yunge-reader-native-live-p)
               (lambda () nil))
              ((symbol-function 'yunge-reader-native-release)
               (lambda () (cl-decf leases)))
              ((symbol-function
                'yunge-reader-native-request-in-session)
               (lambda (&rest _arguments) (setq requested t))))
      (yunge-reader-pdf--close
       (make-yunge-reader-document
        :handle (yunge-reader-pdf-test--handle 7))))
    (should (zerop leases))
    (should-not requested)))

(ert-deftest yunge-reader-pdf-close-is-idempotent ()
  (let* ((handle (yunge-reader-pdf-test--handle 7))
         (document (make-yunge-reader-document :handle handle))
         complete
         (releases 0)
         (requests 0))
    (cl-letf
        (((symbol-function 'yunge-reader-native-session-live-p)
          (lambda (session) (= session 17)))
         ((symbol-function 'yunge-reader-native-release)
          (lambda () (cl-incf releases)))
         ((symbol-function 'yunge-reader-native-request-in-session)
          (lambda (_session operation _parameters callback)
            (should (equal operation "close"))
            (cl-incf requests)
            (setq complete callback))))
      (yunge-reader-pdf--close document)
      (yunge-reader-pdf--close document)
      (should (= requests 1))
      (should (zerop releases))
      (funcall complete '((closed . t)) nil)
      (should (= releases 1))
      (yunge-reader-pdf--close document))
    (should (= requests 1))
    (should (= releases 1))))

(ert-deftest yunge-reader-pdf-does-not-close-a-colliding-new-session-handle ()
  (let ((leases 1)
        requested)
    (cl-letf
        (((symbol-function 'yunge-reader-native-live-p)
          (lambda () t))
         ((symbol-function 'yunge-reader-native-session-live-p)
          (lambda (session)
            (should (= session 17))
            nil))
         ((symbol-function 'yunge-reader-native-release)
          (lambda () (cl-decf leases)))
         ((symbol-function 'yunge-reader-native-request-in-session)
          (lambda (&rest _arguments) (setq requested t))))
      (yunge-reader-pdf--close
       (make-yunge-reader-document
        :handle (yunge-reader-pdf-test--handle 1))))
    (should (zerop leases))
    (should-not requested)))

(ert-deftest yunge-reader-pdf-coalesces-stale-handle-recovery ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((file "C:/books/test.pdf")
           (handle (yunge-reader-pdf-test--handle 7))
           (document
            (make-yunge-reader-document :file file :handle handle))
           (yunge-reader-document document)
           (yunge-reader-pdf-page 4)
           (yunge-reader-zoom-mode 'manual)
           (yunge-reader-scale 1.5)
           (saved-places '(("saved.pdf" :version 1)))
           (yunge-reader-saved-places (copy-tree saved-places))
           open-complete
           (open-count 0)
           requests
           results)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--file-identity)
            (lambda (_file) 'test-identity))
           ((symbol-function 'yunge-reader-native-session-live-p)
            (lambda (session) (= session 22)))
           ((symbol-function 'yunge-reader-native-start) #'ignore)
           ((symbol-function 'yunge-reader-native-current-session)
            (lambda () 22))
           ((symbol-function 'yunge-reader-pdf--open-in-session)
            (lambda (_file session _buffer _generation _window _state
                     complete)
              (should (= session 22))
              (cl-incf open-count)
              (setq open-complete complete)))
           ((symbol-function 'yunge-reader-native-request-in-session)
            (lambda (session operation parameters complete)
              (should (= session 22))
              (should (equal operation "page-info"))
              (should (= (alist-get 'document parameters) 9))
              (push parameters requests)
              (funcall complete '((page . 4)) nil))))
        (dotimes (index 2)
          (yunge-reader-pdf--request
           document 'page-info '(:page 4)
           (lambda (value error-data)
             (push (list index value error-data) results))))
        (should (= open-count 1))
        (should-not requests)
        (should (= (yunge-reader-pdf-handle-session handle) 17))
        (should (= (yunge-reader-pdf-handle-id handle) 7))
        (funcall open-complete '((document . 9)) nil))
      (should (= (length requests) 2))
      (should (= (length results) 2))
      (should-not (seq-some #'caddr results))
      (should (= (yunge-reader-pdf-handle-session handle) 22))
      (should (= (yunge-reader-pdf-handle-id handle) 9))
      (should (= yunge-reader-pdf-page 4))
      (should (eq yunge-reader-zoom-mode 'manual))
      (should (= yunge-reader-scale 1.5))
      (should (equal yunge-reader-saved-places saved-places)))))

(ert-deftest yunge-reader-pdf-recovers-for-the-requesting-shared-view ()
  (let* ((yunge-reader--document-registry
          (make-hash-table :test #'equal))
         (file (expand-file-name "shared-recovery.pdf"))
         (key (list 'pdf (yunge-reader--place-file-key file)))
         (driver
          (yunge-reader--make-driver
           :name 'pdf
           :close-function #'ignore
           :detach-function #'ignore))
         (handle (yunge-reader-pdf-test--handle 7))
         (document
          (make-yunge-reader-document
           :key key :file file :driver driver :handle handle))
         (entry
          (yunge-reader--make-document-entry
           :key key :file file :driver driver :state 'ready
           :document document))
         recovery-buffer
         result
         completion-error
         first
         second)
    (unwind-protect
        (progn
          (setq first (generate-new-buffer " *pdf-shared-primary*")
                second (generate-new-buffer " *pdf-shared-requester*"))
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (yunge-reader-mode)
              (setq yunge-reader-document document
                    yunge-reader--document-entry entry)))
          (setf (yunge-reader--document-entry-views entry)
                (list first second)
                (yunge-reader--document-entry-primary-view entry) first
                (yunge-reader--document-entry-active-view entry) first)
          (puthash key entry yunge-reader--document-registry)
          (cl-letf
              (((symbol-function 'yunge-reader-pdf--file-identity)
                (lambda (_file) 'test-identity))
               ((symbol-function 'yunge-reader-native-session-live-p)
                (lambda (session) (= session 22)))
               ((symbol-function 'yunge-reader-native-start) #'ignore)
               ((symbol-function 'yunge-reader-native-current-session)
                (lambda () 22))
               ((symbol-function 'yunge-reader-pdf--open-in-session)
                (lambda (_file session buffer _generation _window _state
                         complete)
                  (should (= session 22))
                  (setq recovery-buffer buffer)
                  (funcall complete '((document . 9)) nil)))
               ((symbol-function
                 'yunge-reader-native-request-in-session)
                (lambda (session operation parameters complete)
                  (should (= session 22))
                  (should (equal operation "page-info"))
                  (should (= (alist-get 'document parameters) 9))
                  (funcall complete '((page . 0)) nil))))
            (with-current-buffer second
              (yunge-reader-pdf--request
               document 'page-info '(:page 0)
               (lambda (value error-data)
                 (setq result value
                       completion-error error-data)))))
          (should (eq recovery-buffer second))
          (should (equal result '((page . 0))))
          (should-not completion-error)
          (should (= (yunge-reader-pdf-handle-session handle) 22))
          (should (= (yunge-reader-pdf-handle-id handle) 9)))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-pdf-does-not-prompt-for-background-recovery ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((file "C:/books/locked.pdf")
           (handle (yunge-reader-pdf-test--handle 7))
           (document
            (make-yunge-reader-document :file file :handle handle))
           (yunge-reader-document document)
           prompted
           completion-error)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--file-identity)
            (lambda (_file) 'test-identity))
           ((symbol-function 'yunge-reader-native-session-live-p)
            (lambda (session) (= session 22)))
           ((symbol-function 'yunge-reader-native-start) #'ignore)
           ((symbol-function 'yunge-reader-native-current-session)
            (lambda () 22))
           ((symbol-function 'password-read-from-cache)
            (lambda (_key) "wrong"))
           ((symbol-function 'password-cache-remove) #'ignore)
           ((symbol-function 'read-passwd)
            (lambda (&rest _arguments)
              (setq prompted t)
              "secret"))
           ((symbol-function 'yunge-reader-native-request-in-session)
            (lambda (session operation _parameters complete)
              (should (= session 22))
              (should (equal operation "open"))
              (funcall
               complete nil
               '(yunge-reader-native-pdf-password-error)))))
        (yunge-reader-pdf--request
         document 'page-info '(:page 0)
         (lambda (_value error-data)
           (setq completion-error error-data))))
      (should-not prompted)
      (should
       (eq (car completion-error)
           'yunge-reader-native-pdf-password-error))
      (should (= (yunge-reader-pdf-handle-session handle) 17))
      (should (= (yunge-reader-pdf-handle-id handle) 7)))))

(ert-deftest yunge-reader-pdf-does-not-retry-an-intentional-stop ()
  (let ((document
         (make-yunge-reader-document
          :handle (yunge-reader-pdf-test--handle 7)))
        (dispatches 0)
        calls
        completion-error)
    (cl-letf
        (((symbol-function 'yunge-reader-pdf--ensure-handle)
          (lambda (_document _view complete)
            (funcall complete nil)))
         ((symbol-function 'yunge-reader-pdf--dispatch)
          (lambda (_document _operation _arguments complete)
            (cl-incf dispatches)
            (funcall
             complete nil
             '(yunge-reader-native-session-stopped "stopped")))))
      (yunge-reader-pdf--request
       document 'page-info '(:page 3)
       (lambda (_value error-data)
         (setq calls (1+ (or calls 0))
               completion-error error-data))))
    (should (= dispatches 1))
    (should (= calls 1))
    (should
     (eq (car completion-error)
         'yunge-reader-native-session-stopped))))

(ert-deftest yunge-reader-pdf-retries-one-interrupted-request ()
  (let ((document
         (make-yunge-reader-document
          :handle (yunge-reader-pdf-test--handle 7)))
        (ensures 0)
        (dispatches 0)
        (calls 0)
        result
        completion-error)
    (cl-letf
        (((symbol-function 'yunge-reader-pdf--ensure-handle)
          (lambda (_document _view complete)
            (cl-incf ensures)
            (funcall complete nil)))
         ((symbol-function 'yunge-reader-pdf--dispatch)
          (lambda (_document _operation _arguments complete)
            (cl-incf dispatches)
            (if (= dispatches 1)
                (funcall
                 complete nil
                 '(yunge-reader-native-session-lost "crashed"))
              (funcall complete '((page . 3)) nil)))))
      (yunge-reader-pdf--request
       document 'page-info '(:page 3)
       (lambda (value error-data)
         (cl-incf calls)
         (setq result value
               completion-error error-data))))
    (should (= ensures 2))
    (should (= dispatches 2))
    (should (= calls 1))
    (should (equal result '((page . 3))))
    (should-not completion-error)))

(ert-deftest yunge-reader-pdf-does-not-retry-session-loss-twice ()
  (let ((document
        (make-yunge-reader-document
          :handle (yunge-reader-pdf-test--handle 7)))
        (dispatches 0)
        (calls 0)
        completion-error)
    (cl-letf
        (((symbol-function 'yunge-reader-pdf--ensure-handle)
          (lambda (_document _view complete)
            (funcall complete nil)))
         ((symbol-function 'yunge-reader-pdf--dispatch)
          (lambda (_document _operation _arguments complete)
            (cl-incf dispatches)
            (funcall
             complete nil
             '(yunge-reader-native-session-lost "crashed")))))
      (yunge-reader-pdf--request
       document 'page-info '(:page 3)
       (lambda (_value error-data)
         (cl-incf calls)
         (setq completion-error error-data))))
    (should (= dispatches 2))
    (should (= calls 1))
    (should
     (eq (car completion-error)
         'yunge-reader-native-session-lost))))

(ert-deftest yunge-reader-pdf-refuses-to-recover-a-changed-file ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((file "C:/books/test.pdf")
           (handle (yunge-reader-pdf-test--handle 7))
           (document
            (make-yunge-reader-document :file file :handle handle))
           (yunge-reader-document document)
           (yunge-reader-pdf-page 4)
           (saved-places '(("saved.pdf" :version 1)))
           (yunge-reader-saved-places (copy-tree saved-places))
           started
           completion-error)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--file-identity)
            (lambda (_file) 'changed-identity))
           ((symbol-function 'yunge-reader-native-session-live-p)
            (lambda (_session) nil))
           ((symbol-function 'yunge-reader-native-start)
            (lambda () (setq started t))))
        (yunge-reader-pdf--request
         document 'page-info '(:page 4)
         (lambda (_value error-data)
           (setq completion-error error-data))))
      (should-not started)
      (should (string-match-p "changed on disk"
                              (error-message-string completion-error)))
      (should (= (yunge-reader-pdf-handle-session handle) 17))
      (should (= (yunge-reader-pdf-handle-id handle) 7))
      (should (= yunge-reader-pdf-page 4))
      (should (equal yunge-reader-saved-places saved-places)))))

(ert-deftest yunge-reader-pdf-rejects-a-file-changed-during-recovery ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((file "C:/books/test.pdf")
           (handle (yunge-reader-pdf-test--handle 7))
           (document
            (make-yunge-reader-document :file file :handle handle))
           (yunge-reader-document document)
           (identities '(test-identity changed-identity))
           open-complete
           closed
           completion-error)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--file-identity)
            (lambda (_file) (pop identities)))
           ((symbol-function 'yunge-reader-native-session-live-p)
            (lambda (session) (= session 22)))
           ((symbol-function 'yunge-reader-native-start) #'ignore)
           ((symbol-function 'yunge-reader-native-current-session)
            (lambda () 22))
           ((symbol-function 'yunge-reader-pdf--open-in-session)
            (lambda (_file _session _buffer _generation _window _state
                     complete)
              (setq open-complete complete)))
           ((symbol-function 'yunge-reader-native-request-in-session)
            (lambda (session operation parameters _complete)
              (setq closed (list session operation parameters)))))
        (yunge-reader-pdf--request
         document 'page-info '(:page 4)
         (lambda (_value error-data)
           (setq completion-error error-data)))
        (funcall open-complete '((document . 9)) nil))
      (should (string-match-p "changed on disk"
                              (error-message-string completion-error)))
      (should (equal (butlast closed) '(22 "close")))
      (should (= (alist-get 'document (car (last closed))) 9))
      (should (= (yunge-reader-pdf-handle-session handle) 17))
      (should (= (yunge-reader-pdf-handle-id handle) 7)))))

(ert-deftest yunge-reader-pdf-cleans-up-a-late-recovery-after-close ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((file "C:/books/test.pdf")
           (handle (yunge-reader-pdf-test--handle 7))
           (document
            (make-yunge-reader-document :file file :handle handle))
           (yunge-reader-document document)
           open-complete
           closed
           (releases 0)
           (calls 0)
           completion-error)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--file-identity)
            (lambda (_file) 'test-identity))
           ((symbol-function 'yunge-reader-native-session-live-p)
            (lambda (session) (= session 22)))
           ((symbol-function 'yunge-reader-native-start) #'ignore)
           ((symbol-function 'yunge-reader-native-current-session)
            (lambda () 22))
           ((symbol-function 'yunge-reader-native-release)
            (lambda () (cl-incf releases)))
           ((symbol-function 'yunge-reader-pdf--open-in-session)
            (lambda (_file _session _buffer _generation _window _state
                     complete)
              (setq open-complete complete)))
           ((symbol-function 'yunge-reader-native-request-in-session)
            (lambda (session operation parameters _complete)
              (setq closed (list session operation parameters)))))
        (yunge-reader-pdf--request
         document 'page-info '(:page 4)
         (lambda (_value error-data)
           (cl-incf calls)
           (setq completion-error error-data)))
        (yunge-reader-pdf--close document)
        (should (= calls 1))
        (should (= releases 1))
        (should
         (eq (car completion-error)
             'yunge-reader-native-session-lost))
        (funcall open-complete '((document . 9)) nil))
      (should (= calls 1))
      (should (= releases 1))
      (should (equal (butlast closed) '(22 "close")))
      (should (= (alist-get 'document (car (last closed))) 9))
      (should (yunge-reader-pdf-handle-closed handle))
      (should (= (yunge-reader-pdf-handle-session handle) 17))
      (should (= (yunge-reader-pdf-handle-id handle) 7)))))

(ert-deftest yunge-reader-pdf-maps-reader-requests-to-native-operations ()
  (let ((document
         (make-yunge-reader-document
          :handle (yunge-reader-pdf-test--handle 11)))
        calls)
    (cl-letf
        (((symbol-function 'yunge-reader-native-session-live-p)
          (lambda (session) (= session 17)))
         ((symbol-function 'yunge-reader-native-request-in-session)
          (lambda (session operation parameters _complete)
            (should (= session 17))
            (push (cons operation parameters) calls))))
      (yunge-reader-pdf--request
       document 'page-info '(:page 2) #'ignore)
      (yunge-reader-pdf--request
       document 'page-text '(:page 2) #'ignore)
      (yunge-reader-pdf--request
       document 'page-links '(:page 2) #'ignore)
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
       #'ignore)
      (yunge-reader-pdf--request
       document 'outline nil #'ignore))
    (setq calls (nreverse calls))
    (should (equal (caar calls) "page-info"))
    (should (equal (cdr (assq 'document (cdar calls))) 11))
    (should (equal (cdr (assq 'page (cdar calls))) 2))
    (should (equal (caadr calls) "page-text"))
    (should (equal (caaddr calls) "page-links"))
    (should (= (cdr (assq 'page (cdaddr calls))) 2))
    (let ((render (nth 3 calls)))
      (should (equal (car render) "render-page"))
      (should (= (cdr (assq 'width (cdr render))) 900))
      (should (equal (cdr (assq 'cache-key (cdr render))) "key")))
    (let ((search (nth 4 calls)))
      (should (equal (car search) "search"))
      (should (eq (cdr (assq 'case-sensitive (cdr search))) t))
      (should (equal (cdr (assq 'cursor (cdr search)))
                     '((page . 3) (offset . 4))))
      (should (= (cdr (assq 'match-limit (cdr search))) 25))
      (should (= (cdr (assq 'page-limit (cdr search))) 6)))
    (let ((outline (nth 5 calls)))
      (should (equal (car outline) "outline"))
      (should (= (cdr (assq 'document (cdr outline))) 11)))))

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

(ert-deftest yunge-reader-pdf-converts-native-outlines ()
  (let* ((document
          (make-yunge-reader-document
           :metadata
           '(:page-count 2
             :pages
             (((page . 0) (width . 100.0) (height . 200.0))
              ((page . 1) (width . 300.0) (height . 400.0))))))
         (value
          '((items
             . (((title . "Part")
                 (depth . 0)
                 (destination))
                ((title . "Exact")
                 (depth . 1)
                 (destination
                  . ((page . 1) (x . 12.0) (y . 34.0)
                     (zoom . 1.5) (view . "xyz"))))
                ((title . "Width")
                 (depth . 1)
                 (destination
                  . ((page . 0) (y . 150.0)
                     (view . "fit-horizontal"))))))
            (truncated . t)))
         (outline
          (yunge-reader-pdf--native-outline document value))
         (items (yunge-reader-outline-data-items outline))
         (exact
          (yunge-reader-outline-item-action (nth 1 items)))
         (width
          (yunge-reader-outline-item-action (nth 2 items))))
    (should (yunge-reader-outline-data-p outline))
    (should (yunge-reader-outline-data-truncated outline))
    (should-not (yunge-reader-outline-item-action (car items)))
    (should (= (yunge-reader-outline-item-depth (nth 1 items)) 1))
    (should (= (yunge-reader-position-unit
                (yunge-reader-action-position exact))
               1))
    (should (= (yunge-reader-position-x
                (yunge-reader-action-position exact))
               12.0))
    (should (= (yunge-reader-position-y
                (yunge-reader-action-position exact))
               34.0))
    (should (eq (yunge-reader-action-zoom-mode exact) 'manual))
    (should (= (yunge-reader-action-scale exact) 1.5))
    (should (eq (yunge-reader-action-zoom-mode width) 'fit-width))
    (should (= (yunge-reader-position-y
                (yunge-reader-action-position width))
               150.0))))

(ert-deftest yunge-reader-pdf-accepts-an-empty-native-outline ()
  (let* ((document
          (make-yunge-reader-document
           :metadata '(:page-count 0 :pages nil)))
         (outline
          (yunge-reader-pdf--native-outline
           document '((items) (truncated)))))
    (should (yunge-reader-outline-data-p outline))
    (should-not (yunge-reader-outline-data-items outline))))

(ert-deftest yunge-reader-pdf-converts-native-internal-links ()
  (let* ((document
          (make-yunge-reader-document
           :metadata
           '(:page-count 2
             :pages
             (((page . 0) (width . 100.0) (height . 200.0))
              ((page . 1) (width . 300.0) (height . 400.0))))))
         (data
          (yunge-reader-pdf--native-page-links
           document 0
           '((page . 0)
             (links
              . (((bounds
                   . ((left . 10.0) (bottom . 20.0)
                      (right . 30.0) (top . 40.0)))
                  (action
                   . ((type . "location")
                      (destination
                       . ((page . 1) (x . 12.0) (y . 34.0)
                          (zoom . 1.5) (view . "xyz")))))
                  (label . "Details"))))
             (truncated . t))))
         (link (car (yunge-reader-pdf-link-data-links data)))
         (action (yunge-reader-pdf-link-action link)))
    (should (yunge-reader-pdf-link-data-p data))
    (should (yunge-reader-pdf-link-data-truncated data))
    (should (equal (yunge-reader-pdf-link-label link) "Details"))
    (should (= (alist-get 'left
                          (yunge-reader-pdf-link-bounds link))
               10.0))
    (should (= (yunge-reader-position-unit
                (yunge-reader-action-position action))
               1))
    (should (= (yunge-reader-position-y
                (yunge-reader-action-position action))
               34.0))
    (should (eq (yunge-reader-action-zoom-mode action) 'manual))
    (should (= (yunge-reader-action-scale action) 1.5))))

(ert-deftest yunge-reader-pdf-converts-native-uri-links ()
  (let* ((document
          (make-yunge-reader-document
           :metadata
           '(:page-count 1
             :pages
             (((page . 0) (width . 100.0) (height . 200.0))))))
         (data
          (yunge-reader-pdf--native-page-links
           document 0
           '((page . 0)
             (links
              . (((bounds
                   . ((left . 10.0) (bottom . 20.0)
                      (right . 30.0) (top . 40.0)))
                  (action
                   . ((type . "uri")
                      (uri . "https://example.com/book")))
                  (label . "Website"))))
             (truncated))))
         (link (car (yunge-reader-pdf-link-data-links data)))
         (action (yunge-reader-pdf-link-action link)))
    (should (eq (yunge-reader-action-type action) 'uri))
    (should
     (equal (yunge-reader-action-uri action)
            "https://example.com/book"))
    (should (equal (yunge-reader-pdf-link-label link) "Website"))))

(ert-deftest yunge-reader-pdf-rejects-malformed-native-link-pages ()
  (let ((document
         (make-yunge-reader-document
          :metadata
          '(:page-count 2
            :pages
            (((page . 0) (width . 100.0) (height . 200.0))
             ((page . 1) (width . 100.0) (height . 200.0)))))))
    (should-not
     (yunge-reader-pdf--native-page-links
      document 0
      '((page . 1) (links) (truncated))))
    (should-not
     (yunge-reader-pdf--native-page-links
      document 0
      '((page . 0)
        (links
         . (((bounds
              . ((left . 30.0) (bottom . 20.0)
                 (right . 10.0) (top . 40.0)))
             (action
              . ((type . "location")
                 (destination
                  . ((page . 1) (view . "xyz")))))))))))
    (should-not
     (yunge-reader-pdf--native-page-links
      document 0
      '((page . 0)
        (links
         . (((bounds
              . ((left . 10.0) (bottom . 20.0)
                 (right . 30.0) (top . 40.0)))
             (action
              . ((type . "uri")
                 (uri . "https://example.com/a b")))))))))))

(ert-deftest yunge-reader-pdf-opens-uri-links-without-tracking-jumps ()
  (let* ((yunge-reader-uri-schemes '("https"))
         (bounds
          '((left . 0.0) (bottom . 0.0)
            (right . 10.0) (top . 10.0)))
         (link
          (yunge-reader-pdf-test--uri-link
           0 0 bounds "https://example.com/book" "Website"))
         opened)
    (require 'browse-url)
    (with-temp-buffer
      (setq yunge-reader-pdf--page-infos [((label . "1"))])
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (uri &rest _arguments)
                   (setq opened uri))))
        (let ((inhibit-message t))
          (should (yunge-reader-pdf--follow-link link))))
      (should (equal opened "https://example.com/book")))))

(ert-deftest yunge-reader-pdf-builds-unique-link-candidates ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--page-infos
          [((label . "i"))
           ((label . "ii"))
           ((label . "ii [1]"))])
    (let* ((bounds
            '((left . 0.0) (bottom . 0.0)
              (right . 10.0) (top . 10.0)))
           (first
            (yunge-reader-pdf-test--link
             0 0 bounds 1 "Section"))
           (second
            (yunge-reader-pdf-test--link
             0 1 bounds 1 "Section"))
           (colliding
            (yunge-reader-pdf-test--link
             0 2 bounds 2 "Section")))
      (puthash
       0
       (make-yunge-reader-pdf-link-data
        :page 0 :links (list first second colliding))
       yunge-reader-pdf--link-cache)
      (let ((candidates (yunge-reader-pdf--link-candidates '(0))))
        (should
         (equal
          (mapcar #'car candidates)
          '("Page i: Section -> page ii [1]"
            "Page i: Section -> page ii [2]"
            "Page i: Section -> page ii [1] [2]")))
        (should (eq (cdar candidates) first))
        (should (eq (cdadr candidates) second))))))

(ert-deftest yunge-reader-pdf-maps-bounded-selection-text-batches ()
  (let* ((document
          (make-yunge-reader-document
           :handle (yunge-reader-pdf-test--handle 11)))
         (start (make-yunge-reader-position :unit 3 :offset 9))
         (end (make-yunge-reader-position :unit 2 :offset 4))
         (cursor (make-yunge-reader-position :unit 2 :offset 7))
         request
         result)
    (cl-letf
        (((symbol-function 'yunge-reader-native-session-live-p)
          (lambda (session) (= session 17)))
         ((symbol-function 'yunge-reader-native-request-in-session)
          (lambda (session operation parameters complete)
            (should (= session 17))
            (setq request (cons operation parameters))
            (funcall
             complete
             '((text . "exact text")
               (cursor . ((page . 3) (offset . 1)))
               (done . :false))
             nil))))
      (yunge-reader-pdf--request
       document 'selection-text
       (list :start start :end end :cursor cursor
             :unit-limit 4 :character-limit 123)
       (lambda (value error-data)
         (should-not error-data)
         (setq result value))))
    (should (equal (car request) "selection-text"))
    (should (equal (cdr (assq 'start (cdr request)))
                   '((page . 3) (offset . 9))))
    (should (equal (cdr (assq 'end (cdr request)))
                   '((page . 2) (offset . 4))))
    (should (equal (cdr (assq 'cursor (cdr request)))
                   '((page . 2) (offset . 7))))
    (should (= (cdr (assq 'page-limit (cdr request))) 4))
    (should (= (cdr (assq 'character-limit (cdr request))) 123))
    (should (yunge-reader-selection-batch-p result))
    (should (equal (yunge-reader-selection-batch-text result)
                   "exact text"))
    (should-not (yunge-reader-selection-batch-done result))
    (should
     (equal
      (yunge-reader-selection-batch-cursor result)
      (make-yunge-reader-position :unit 3 :offset 1)))))

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

(ert-deftest yunge-reader-pdf-discards-obsolete-width-completions ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--generation 2
          yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))]
          yunge-reader-pdf--working-pages '(0)
          yunge-reader-pdf--displayed-pages '(0))
    (puthash '(0 . 901)
             '((path . "current.png"))
             yunge-reader-pdf--render-results)
    (let (painted)
      (cl-letf (((symbol-function 'yunge-reader-pdf--page-width)
                 (lambda (_page &optional _window) 901))
                ((symbol-function 'yunge-reader-pdf--paint-page)
                 (lambda (_page) (setq painted t))))
        (yunge-reader-pdf--render-complete
         (current-buffer) nil 1 0 900
         '((path . "stale.png")) nil))
      (should-not painted)
      (should-not
       (gethash '(0 . 900) yunge-reader-pdf--render-results))
      (should
       (gethash '(0 . 901) yunge-reader-pdf--render-results)))))

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

(ert-deftest yunge-reader-pdf-captures-location-at-painted-width ()
  (with-temp-buffer
    (insert " \n ")
    (let ((buffer (current-buffer)))
      (setq yunge-reader-document
            (yunge-reader-pdf-test--document
             '((width . 100.0) (height . 200.0))
             '((width . 100.0) (height . 200.0)))
            yunge-reader-pdf--page-infos
            [((width . 100.0) (height . 200.0))
             ((width . 100.0) (height . 200.0))]
            yunge-reader-pdf--page-positions [1 3])
      (put-text-property
       3 4 'yunge-reader-pdf-display-width 500)
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
          (should (= (yunge-reader-position-x location) 40.0))
          (should (= (yunge-reader-position-y location) 100.0)))))))

(ert-deftest yunge-reader-pdf-coalesces-window-resizes ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (let* ((buffer (current-buffer))
           (document (yunge-reader-pdf-test--document))
           (anchor (make-yunge-reader-position :unit 2 :y 60.0))
           (width 800)
           (height 600)
           (timer 'resize-timer)
           callback
           (scheduled 0)
           (locations 0)
           refresh)
      (setq yunge-reader-document document
            yunge-reader-zoom-mode 'fit-width)
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (_window) t))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function 'window-body-width)
                 (lambda (_window pixelwise)
                   (and pixelwise width)))
                ((symbol-function 'window-body-height)
                 (lambda (_window pixelwise)
                   (and pixelwise height)))
                ((symbol-function 'window-end)
                 (lambda (&rest _arguments)
                   (ert-fail "Resize must not depend on window-end")))
                ((symbol-function 'timerp)
                 (lambda (value) (eq value timer)))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (seconds repeat function &rest arguments)
                   (should (zerop seconds))
                   (should-not repeat)
                   (cl-incf scheduled)
                   (setq callback (cons function arguments))
                   timer))
                ((symbol-function 'yunge-reader-pdf--location)
                 (lambda (_document _window)
                   (cl-incf locations)
                   anchor))
                ((symbol-function 'yunge-reader-pdf--refresh)
                 (lambda (&optional window location)
                   (setq refresh (list window location)))))
        (yunge-reader-pdf--window-size-change 'window)
        (setq width 700
              height 500)
        (yunge-reader-pdf--window-size-change 'window)
        (should (= scheduled 1))
        (should (= locations 1))
        (should (= (plist-get yunge-reader-pdf--pending-resize
                              :width)
                   700))
        (should (= (plist-get yunge-reader-pdf--pending-resize
                              :height)
                   500))
        (apply (car callback) (cdr callback))
        (should (equal refresh (list 'window anchor)))
        (should-not yunge-reader-pdf--resize-timer)
        (should-not yunge-reader-pdf--pending-resize)))))

(ert-deftest yunge-reader-pdf-refresh-targets-the-resized-window ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (let* ((buffer (current-buffer))
           (anchor
            (make-yunge-reader-position :unit 0 :x 12.0 :y 80.0))
           painted
           updated)
      (setq yunge-reader-document
            (yunge-reader-pdf-test--document
             '((width . 100.0) (height . 200.0))))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (_window) t))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function 'yunge-reader-pdf--page-width)
                 (lambda (_page &optional window)
                   (should (eq window 'window))
                   700))
                ((symbol-function 'yunge-reader-pdf--paint-page)
                 (lambda (page &optional width)
                   (setq painted (list page width))))
                ((symbol-function
                  'yunge-reader-pdf--update-visible-pages)
                 (lambda (&optional window)
                   (setq updated window))))
        (yunge-reader-pdf--refresh 'window anchor)
        (should (equal painted '(0 700)))
        (should (eq updated 'window))
        (should (equal yunge-reader-pdf--pending-location anchor))
        (should-not (eq yunge-reader-pdf--pending-location anchor))))))

(ert-deftest yunge-reader-pdf-resize-updates-a-manual-viewport ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (let ((buffer (current-buffer))
          (document (yunge-reader-pdf-test--document))
          updated)
      (setq yunge-reader-document document
            yunge-reader-zoom-mode 'manual
            yunge-reader-pdf--pending-resize
            (list :document document :window 'window))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (_window) t))
                ((symbol-function 'window-buffer)
                 (lambda (_window) buffer))
                ((symbol-function
                  'yunge-reader-pdf--update-visible-pages)
                 (lambda (&optional window)
                   (setq updated window))))
        (yunge-reader-pdf--finish-resize buffer)
        (should (eq updated 'window))))))

(ert-deftest yunge-reader-pdf-ignores-a-stale-resize-window ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (let ((buffer (current-buffer))
          (document (yunge-reader-pdf-test--document))
          refreshed)
      (setq yunge-reader-document document
            yunge-reader-zoom-mode 'fit-page
            yunge-reader-pdf--pending-resize
            (list :document document :window 'dead-window))
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (_window) nil))
                ((symbol-function 'yunge-reader-pdf--refresh)
                 (lambda (&rest _arguments)
                   (setq refreshed t))))
        (yunge-reader-pdf--finish-resize buffer)
        (should-not refreshed)
        (should-not yunge-reader-pdf--pending-resize)))))

(ert-deftest yunge-reader-pdf-cancels-resize-when-view-stops ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (let ((timer 'resize-timer)
          cancelled)
      (setq yunge-reader-pdf--resize-timer timer
            yunge-reader-pdf--pending-resize 'pending)
      (cl-letf (((symbol-function 'timerp)
                 (lambda (value) (eq value timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (value) (setq cancelled value))))
        (yunge-reader-pdf-view-mode -1))
      (should (eq cancelled timer))
      (should-not yunge-reader-pdf--resize-timer)
      (should-not yunge-reader-pdf--pending-resize)
      (should-not
       (memq #'yunge-reader-pdf--window-size-change
             window-size-change-functions)))))

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

(ert-deftest yunge-reader-pdf-rejects-mouse-points-outside-images ()
  (with-temp-buffer
    (setq yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))])
    (cl-letf (((symbol-function 'posn-point) (lambda (_position) 1))
              ((symbol-function 'yunge-reader-pdf--page-at-position)
               (lambda (_position) 0))
              ((symbol-function 'posn-image) (lambda (_position) t))
              ((symbol-function 'posn-object-x-y)
               (lambda (_position) '(100 . 200)))
              ((symbol-function 'posn-object-width-height)
               (lambda (_position) '(200 . 400))))
      (should
       (equal (yunge-reader-pdf--event-page-point 'position)
              '(:page 0 :point (50.0 . 100.0))))
      (cl-letf (((symbol-function 'posn-image)
                 (lambda (_position) nil)))
        (should-not
         (yunge-reader-pdf--event-page-point 'position t))
        (should-error
         (yunge-reader-pdf--event-page-point 'position)
         :type 'user-error)))))

(ert-deftest yunge-reader-pdf-tracks-selection-before-mouse-release ()
  (with-temp-buffer
    (let ((events '(motion release))
          calls
          (messages 0))
      (setq mark-active t)
      (cl-letf (((symbol-function 'event-start)
                 (lambda (event)
                   (pcase event
                     ('motion 'motion-position)
                     ('release 'start-position))))
                ((symbol-function 'event-end)
                 (lambda (event)
                   (and (eq event 'release) 'release-position)))
                ((symbol-function 'event-basic-type)
                 (lambda (event)
                   (if (eq event 'release) 'mouse-1 event)))
                ((symbol-function 'mouse-movement-p)
                 (lambda (event) (eq event 'motion)))
                ((symbol-function 'read-event)
                 (lambda (&rest _arguments) (pop events)))
                ((symbol-function 'posn-window)
                 (lambda (_position) 'window))
                ((symbol-function 'posn-x-y)
                 (lambda (position)
                   (pcase position
                     ('start-position '(0 . 0))
                     ('motion-position '(8 . 0))
                     ('release-position '(12 . 0)))))
                ((symbol-function 'yunge-reader-pdf--event-page-point)
                 (lambda (position &optional _noerror)
                   (list :page 0 :point position)))
                ((symbol-function 'yunge-reader-pdf--select-points)
                 (lambda (start end &rest arguments)
                   (push (list start end arguments) calls)
                   t))
                ((symbol-function 'yunge-reader-pdf--message-selection)
                 (lambda () (cl-incf messages))))
        (yunge-reader-pdf--track-selection-events
         '(:page 0 :point start-position) 'start-position 'window))
      (should-not mark-active)
      (should
       (equal
        (nreverse calls)
        '(((:page 0 :point start-position)
           (:page 0 :point motion-position) (t t window))
          ((:page 0 :point start-position)
           (:page 0 :point release-position) (t t window)))))
      (should (= messages 1)))))

(ert-deftest yunge-reader-pdf-click-clears-selection-without-selecting ()
  (with-temp-buffer
    (let ((events '(motion release))
          (repaints 0)
          (selects 0))
      (setq yunge-reader-selection
            (make-yunge-reader-selection
             :start (make-yunge-reader-position :unit 0 :offset 4)
             :end (make-yunge-reader-position :unit 0 :offset 11)))
      (cl-letf (((symbol-function 'event-start)
                 (lambda (event)
                   (if (eq event 'motion)
                       'motion-position
                     'start-position)))
                ((symbol-function 'event-end)
                 (lambda (_event) 'release-position))
                ((symbol-function 'event-basic-type)
                 (lambda (_event) 'mouse-1))
                ((symbol-function 'mouse-movement-p)
                 (lambda (event) (eq event 'motion)))
                ((symbol-function 'read-event)
                 (lambda (&rest _arguments) (pop events)))
                ((symbol-function 'posn-window)
                 (lambda (_position) 'window))
                ((symbol-function 'posn-x-y)
                 (lambda (position)
                   (pcase position
                     ('start-position '(20 . 20))
                     ('motion-position '(22 . 21))
                     ('release-position '(21 . 20)))))
                ((symbol-function 'yunge-reader-pdf--select-points)
                 (lambda (&rest _arguments) (cl-incf selects)))
                ((symbol-function 'yunge-reader-pdf--repaint-selection)
                 (lambda (_window) (cl-incf repaints))))
        (yunge-reader-pdf--track-selection-events
         '(:page 0 :point start) 'start-position 'window))
      (should-not yunge-reader-selection)
      (should (= selects 0))
      (should (= repaints 1)))))

(ert-deftest yunge-reader-pdf-forces-selection-redisplay ()
  (let (painted forced redisplayed)
    (cl-letf (((symbol-function 'yunge-reader-pdf--paint-pages)
               (lambda (pages &optional _window) (setq painted pages)))
              ((symbol-function 'window-live-p)
               (lambda (window) (eq window 'window)))
              ((symbol-function 'force-window-update)
               (lambda (window) (setq forced window)))
              ((symbol-function 'display-graphic-p) (lambda () t))
              ((symbol-function 'redisplay)
               (lambda (&optional force) (setq redisplayed force))))
      (let ((yunge-reader-pdf--displayed-pages '(2 3)))
        (yunge-reader-pdf--repaint-selection 'window)))
    (should (equal painted '(2 3)))
    (should (eq forced 'window))
    (should redisplayed)))

(ert-deftest yunge-reader-pdf-redisplays-every-visible-view ()
  (let (forced redisplayed)
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (buffer _minibuffer all-frames)
                 (should (eq buffer (current-buffer)))
                 (should all-frames)
                 '(left dead right)))
              ((symbol-function 'window-live-p)
               (lambda (window) (not (eq window 'dead))))
              ((symbol-function 'force-window-update)
               (lambda (window) (push window forced)))
              ((symbol-function 'display-graphic-p) (lambda () t))
              ((symbol-function 'redisplay)
               (lambda (&optional force) (setq redisplayed force))))
      (yunge-reader-pdf--force-redisplay))
    (should (equal (nreverse forced) '(left right)))
    (should redisplayed)))

(ert-deftest yunge-reader-pdf-reuses-render-files-in-svg-highlights ()
  (with-temp-buffer
    (setq yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))]
          yunge-reader-pdf--render-results
          (make-hash-table :test #'equal)
          yunge-reader-pdf--text-cache
          (make-hash-table :test #'eql)
          yunge-reader-selection
          (make-yunge-reader-selection
           :start (make-yunge-reader-position :unit 0 :offset 4)
           :end (make-yunge-reader-position :unit 0 :offset 4)))
    (puthash
     '(0 . 100)
     '((path . "C:/cache/page.png")
       (pixel-width . 100) (pixel-height . 200))
     yunge-reader-pdf--render-results)
    (puthash
     0
     '((characters
        . (((index . 4)
            (text . "A")
            (bounds . ((left . 10.0) (bottom . 10.0)
                       (right . 20.0) (top . 20.0)))))))
     yunge-reader-pdf--text-cache)
    (let (captured-svg captured-properties)
      (cl-letf (((symbol-function 'svg-image)
                 (lambda (svg &rest properties)
                   (setq captured-svg svg
                         captured-properties properties)
                   'image)))
        (should
         (eq (yunge-reader-pdf--display-image-object 0 100)
             'image)))
      (should
       (equal (dom-attr (car (dom-by-tag captured-svg 'image))
                        'xlink:href)
              "page.png"))
      (should
       (equal captured-properties
              '(:base-uri "C:/cache/page.png"))))))

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

(ert-deftest yunge-reader-pdf-redisplays-an-asynchronous-search-overlay ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document 'document
          yunge-reader-pdf--displayed-pages '(0)
          yunge-reader-search-highlight-visible t
          yunge-reader-search-result
          (make-yunge-reader-search-result
           :start (make-yunge-reader-position :unit 0 :offset 4)
           :end (make-yunge-reader-position :unit 0 :offset 11)))
    (puthash 0 '(document) yunge-reader-pdf--text-pending)
    (let ((result '((page . 0) (characters . ())))
          events)
      (cl-letf (((symbol-function 'yunge-reader-pdf--retain-page-p)
                 (lambda (_page) t))
                ((symbol-function 'yunge-reader-pdf--paint-page)
                 (lambda (_page &optional _width) (push 'paint events)))
                ((symbol-function
                  'yunge-reader-pdf--scroll-to-search-result)
                 (lambda () (push 'scroll events)))
                ((symbol-function 'yunge-reader-pdf--force-redisplay)
                 (lambda (&optional _window) (push 'redisplay events)))
                ((symbol-function 'yunge-reader-pdf--finish-prefetch)
                 #'ignore))
        (yunge-reader-pdf--text-complete
         (current-buffer) 'document 0 result nil))
      (should (equal (nreverse events) '(paint scroll redisplay)))
      (should (eq (gethash 0 yunge-reader-pdf--text-cache) result))
      (should-not (gethash 0 yunge-reader-pdf--text-pending)))))

(ert-deftest yunge-reader-pdf-coalesces-and-caches-page-links ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document 'document)
    (let ((requests 0)
          (callbacks 0)
          completion
          (result
           (make-yunge-reader-pdf-link-data
            :page 0 :links nil)))
      (cl-letf (((symbol-function 'yunge-reader-request)
                 (lambda (operation arguments complete)
                   (should (eq operation 'page-links))
                   (should (= (plist-get arguments :page) 0))
                   (cl-incf requests)
                   (setq completion complete))))
        (yunge-reader-pdf--request-links
         0 (lambda (&rest _arguments) (cl-incf callbacks)))
        (yunge-reader-pdf--request-links
         0 (lambda (&rest _arguments) (cl-incf callbacks)))
        (should (= requests 1))
        (funcall completion result nil)
        (should (= callbacks 2))
        (should (eq (gethash 0 yunge-reader-pdf--link-cache)
                    result))
        (yunge-reader-pdf--request-links
         0 (lambda (&rest _arguments) (cl-incf callbacks)))
        (should (= requests 1))
        (should (= callbacks 3))))))

(ert-deftest yunge-reader-pdf-modified-click-follows-only-links ()
  (let* ((bounds
          '((left . 10.0) (bottom . 20.0)
            (right . 30.0) (top . 40.0)))
         (link
          (yunge-reader-pdf-test--link 0 0 bounds 1 "Target"))
         (data
          (make-yunge-reader-pdf-link-data
           :page 0 :links (list link)))
         followed)
    (cl-letf (((symbol-function 'yunge-reader-pdf--follow-link)
               (lambda (value) (setq followed value))))
      (yunge-reader-pdf--activate-page-point
       '(:page 0 :point (15.0 . 25.0)) data)
      (should (eq followed link))
      (setq followed nil)
      (let ((inhibit-message t))
        (should-not
         (yunge-reader-pdf--activate-page-point
          '(:page 0 :point (5.0 . 5.0)) data)))
      (should-not followed))))

(ert-deftest yunge-reader-pdf-caches-late-links-without-prompting ()
  (let (completion reader other
        (requests 0)
        (shown 0))
    (unwind-protect
        (save-window-excursion
          (setq reader (generate-new-buffer " *reader-links-late*"))
          (setq other (generate-new-buffer " *reader-links-other*"))
          (switch-to-buffer reader)
          (yunge-reader-mode)
          (yunge-reader-pdf-view-mode 1)
          (setq yunge-reader-document 'document)
          (cl-letf
              (((symbol-function 'yunge-reader-pdf--window-pages)
                (lambda (_window) '(0)))
               ((symbol-function 'yunge-reader--window-state)
                (lambda (_window) 'state))
               ((symbol-function 'yunge-reader--window-state-current-p)
                (lambda (window _state)
                  (eq (window-buffer window) reader)))
               ((symbol-function 'yunge-reader-request)
                (lambda (_operation _arguments complete)
                  (cl-incf requests)
                  (setq completion complete)))
               ((symbol-function 'yunge-reader-pdf--select-link)
                (lambda (_pages) (cl-incf shown))))
            (yunge-reader-pdf-follow-link)
            (switch-to-buffer other)
            (funcall
             completion
             (make-yunge-reader-pdf-link-data
              :page 0 :links nil)
             nil)
            (should (zerop shown))
            (with-current-buffer reader
              (should
               (yunge-reader-pdf-link-data-p
                (gethash 0 yunge-reader-pdf--link-cache))))
            (switch-to-buffer reader)
            (yunge-reader-pdf-follow-link)
            (should (= shown 1))
            (should (= requests 1))))
      (when (buffer-live-p reader)
        (with-current-buffer reader
          (setq yunge-reader-document nil))
        (kill-buffer reader))
      (when (buffer-live-p other)
        (kill-buffer other)))))

(ert-deftest yunge-reader-pdf-prioritizes-image-before-text-layer ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--generation 1
          yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))
           ((width . 100.0) (height . 200.0))])
    (let (operations)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--page-width)
            (lambda (_page &optional _window) 900))
           ((symbol-function 'yunge-reader-pdf--dispatch-prefetch-task)
            (lambda (task)
              (let ((kind
                     (yunge-reader-pdf--prefetch-task-kind task))
                    (page
                     (yunge-reader-pdf--prefetch-task-page task)))
                (push (list kind page) operations)
                (yunge-reader-pdf--finish-prefetch
                 (yunge-reader-pdf--prefetch-task-document task)
                 kind page
                 (yunge-reader-pdf--prefetch-task-width task)))
              'started)))
        (yunge-reader-pdf--queue-pages '(0 1)))
      (should (equal (nreverse operations)
                     '((render 0) (render 1)
                       (text 0) (text 1)
                       (links 0) (links 1)))))))

(ert-deftest yunge-reader-pdf-prefetch-continues-after-an-error ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))
           ((width . 100.0) (height . 200.0))])
    (let (operations
          warnings)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--page-width)
            (lambda (_page &optional _window) 900))
           ((symbol-function 'display-warning)
            (lambda (&rest arguments) (push arguments warnings)))
           ((symbol-function 'yunge-reader-pdf--dispatch-prefetch-task)
            (lambda (task)
              (let ((kind
                     (yunge-reader-pdf--prefetch-task-kind task))
                    (page
                     (yunge-reader-pdf--prefetch-task-page task)))
                (push (list kind page) operations)
                (if (= (length operations) 1)
                    (error "prefetch failed")
                  (yunge-reader-pdf--finish-prefetch
                   (yunge-reader-pdf--prefetch-task-document task)
                   kind page
                   (yunge-reader-pdf--prefetch-task-width task))))
              'started)))
        (yunge-reader-pdf--queue-pages '(0 1)))
      (should (= (length operations) 6))
      (should (= (length warnings) 1))
      (should-not yunge-reader-pdf--prefetch-active)
      (should-not yunge-reader-pdf--prefetch-queue))))

(ert-deftest yunge-reader-pdf-prefetch-stops-after-an-explicit-stop ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document 'document
          yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))
           ((width . 100.0) (height . 200.0))])
    (let (complete
          (requests 0))
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--page-width)
            (lambda (_page &optional _window) 900))
           ((symbol-function 'yunge-reader-pdf--cache-key)
            (lambda (_page _width) (make-string 64 ?a)))
           ((symbol-function 'yunge-reader-request)
            (lambda (_operation _arguments callback)
              (cl-incf requests)
              (setq complete callback))))
        (yunge-reader-pdf--queue-pages '(0 1))
        (funcall
         complete nil
         '(yunge-reader-native-session-stopped "stopped")))
      (should (= requests 1))
      (should-not yunge-reader-pdf--prefetch-active)
      (should-not yunge-reader-pdf--prefetch-queue))))

(ert-deftest yunge-reader-pdf-replaces-obsolete-prefetch-work ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document 'document
          yunge-reader-pdf--page-infos
          (make-vector
           1001 '((width . 100.0) (height . 200.0))))
    (let (requests
          first-complete)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--page-width)
            (lambda (_page &optional _window) 900))
           ((symbol-function 'yunge-reader-pdf--cache-key)
            (lambda (_page _width) (make-string 64 ?a)))
           ((symbol-function 'yunge-reader-request)
            (lambda (operation arguments complete)
              (push
               (list operation (plist-get arguments :page) complete)
               requests))))
        (yunge-reader-pdf--queue-pages '(0))
        (setq first-complete (caddr (car requests)))
        (dotimes (index 1000)
          (yunge-reader-pdf--queue-pages (list (1+ index))))
        (should (= (length requests) 1))
        (should (= (length yunge-reader-pdf--prefetch-queue) 3))
        (should
         (zerop
          (yunge-reader-pdf--prefetch-task-page
           yunge-reader-pdf--prefetch-active)))
        (funcall
         first-complete
         '((path . "old.png")
           (pixel-width . 900)
           (pixel-height . 1800))
         nil))
      (should (= (length requests) 2))
      (should (eq (caar requests) 'render-page))
      (should (= (cadar requests) 1000))
      (should (= (length yunge-reader-pdf--prefetch-queue) 2))
      (should
       (= (yunge-reader-pdf--prefetch-task-page
           yunge-reader-pdf--prefetch-active)
          1000))
      (should (= (hash-table-count
                  yunge-reader-pdf--render-pending)
                 1))
      (should-not
       (gethash '(0 . 900) yunge-reader-pdf--render-results)))))

(ert-deftest yunge-reader-pdf-keeps-interactive-work-ahead-of-prefetch ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document 'document
          yunge-reader-pdf--page-infos
          (make-vector 10 '((width . 100.0) (height . 200.0))))
    (let (requests)
      (cl-letf
          (((symbol-function 'yunge-reader-pdf--page-width)
            (lambda (_page &optional _window) 900))
           ((symbol-function 'yunge-reader-pdf--cache-key)
            (lambda (_page _width) (make-string 64 ?a)))
           ((symbol-function 'yunge-reader-request)
            (lambda (operation arguments complete)
              (push
               (list operation (plist-get arguments :page) complete)
               requests))))
        (yunge-reader-pdf--queue-pages '(0 1))
        (yunge-reader-pdf--request-links 9 #'ignore))
      (should
       (equal
        (mapcar (lambda (request) (seq-take request 2))
                (nreverse requests))
        '((render-page 0) (page-links 9))))
      (should (= (length yunge-reader-pdf--prefetch-queue) 5)))))

(ert-deftest yunge-reader-pdf-bounds-in-memory-caches-to-working-pages ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document 'document
          yunge-reader-pdf--page-infos
          (make-vector
           1000 '((width . 100.0) (height . 200.0))))
    (dotimes (page 1000)
      (puthash
       (cons page 800) '((path . "old.png"))
       yunge-reader-pdf--render-results)
      (puthash
       page '((characters . ())) yunge-reader-pdf--text-cache)
      (puthash
       page
       (make-yunge-reader-pdf-link-data :page page :links nil)
       yunge-reader-pdf--link-cache))
    (dolist (page '(500 501))
      (puthash
       (cons page 900) '((path . "current.png"))
       yunge-reader-pdf--render-results))
    (cl-letf
        (((symbol-function 'yunge-reader-pdf--page-width)
          (lambda (_page &optional _window) 900))
         ((symbol-function 'yunge-reader-request)
          (lambda (&rest _arguments)
            (ert-fail "Cached working pages requested native work"))))
      (yunge-reader-pdf--queue-pages '(500 501)))
    (should (= (hash-table-count
                yunge-reader-pdf--render-results)
               2))
    (should (= (hash-table-count yunge-reader-pdf--text-cache) 2))
    (should (= (hash-table-count yunge-reader-pdf--link-cache) 2))
    (should (gethash '(500 . 900)
                     yunge-reader-pdf--render-results))
    (should-not (gethash '(500 . 800)
                         yunge-reader-pdf--render-results))
    (should-not yunge-reader-pdf--prefetch-active)
    (should-not yunge-reader-pdf--prefetch-queue)))

(ert-deftest yunge-reader-pdf-retains-one-nearest-render-while-replacing ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (dolist (width '(700 800 1100))
      (puthash
       (cons 0 width)
       `((path . ,(format "%d.png" width)))
       yunge-reader-pdf--render-results))
    (puthash '(1 . 800)
             '((path . "other.png"))
             yunge-reader-pdf--render-results)
    (let ((task
           (make-yunge-reader-pdf--prefetch-task
            :kind 'render :page 0 :width 900)))
      (yunge-reader-pdf--prune-working-set '(0) (list task)))
    (should (= (hash-table-count
                yunge-reader-pdf--render-results)
               1))
    (should
     (equal
      (gethash '(0 . 800) yunge-reader-pdf--render-results)
      '((path . "800.png"))))
    (puthash '(0 . 900)
             '((path . "900.png"))
             yunge-reader-pdf--render-results)
    (yunge-reader-pdf--prune-page-renders 0 900)
    (should (= (hash-table-count
                yunge-reader-pdf--render-results)
               1))
    (should
     (gethash '(0 . 900) yunge-reader-pdf--render-results))))

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
                ((symbol-function 'yunge-reader-pdf--paint-page)
                 #'ignore)
                ((symbol-function 'yunge-reader-request)
                 (lambda (operation arguments complete)
                   (push (list operation (plist-get arguments :page))
                         operations)
                   (pcase operation
                     ('render-page
                      (funcall
                       complete
                       '((path . "page.png")
                         (pixel-width . 900)
                         (pixel-height . 1800))
                       nil))
                     ('page-text
                      (funcall complete '((characters . ())) nil))
                     ('page-links
                      (funcall
                       complete
                       (make-yunge-reader-pdf-link-data
                        :page (plist-get arguments :page)
                        :links nil)
                       nil))))))
        (yunge-reader-pdf--refresh))
      (should (equal yunge-reader-pdf--page-positions [1 3]))
      (should (equal (nreverse operations)
                     '((render-page 0) (render-page 1)
                       (page-text 0) (page-text 1)
                       (page-links 0) (page-links 1)))))))

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
          yunge-reader-search-highlight-visible t
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
                         rectangles))))
      (setq yunge-reader-search-highlight-visible nil)
      (let ((hidden-svg (svg-create 1000 1000)))
        (yunge-reader-pdf--paint-selection
         hidden-svg 0 page-info text-layer 1000 1000)
        (yunge-reader-pdf--paint-search
         hidden-svg 0 page-info text-layer 1000 1000)
        (let ((rectangles (dom-by-tag hidden-svg 'rect)))
          (should (= (length rectangles) 1))
          (should (equal (dom-attr (car rectangles) 'fill)
                         yunge-reader-pdf-selection-color)))))))

(ert-deftest yunge-reader-pdf-scrolls-to-search-character-geometry ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-pdf--page-infos
          [((width . 100.0) (height . 200.0))]
          yunge-reader-pdf--page-positions [1]
          yunge-reader-search-highlight-visible t
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

(ert-deftest yunge-reader-pdf-redisplays-search-result-and-hide ()
  (with-temp-buffer
    (setq yunge-reader-pdf--displayed-pages '(0)
          yunge-reader-search-highlight-visible t
          yunge-reader-search-result
          (make-yunge-reader-search-result
           :start (make-yunge-reader-position :unit 0 :offset 7)
           :end (make-yunge-reader-position :unit 0 :offset 14)))
    (let (pages requests (scrolls 0) (redisplays 0))
      (cl-letf (((symbol-function 'yunge-reader-pdf--page-count)
                 (lambda () 1))
                ((symbol-function 'yunge-reader-pdf--set-page)
                 (lambda (page) (push page pages)))
                ((symbol-function 'yunge-reader-pdf--request-text)
                 (lambda (page) (push page requests)))
                ((symbol-function 'yunge-reader-pdf--paint-pages)
                 (lambda (_pages &optional _window) nil))
                ((symbol-function
                  'yunge-reader-pdf--scroll-to-search-result)
                 (lambda () (cl-incf scrolls)))
                ((symbol-function 'yunge-reader-pdf--force-redisplay)
                 (lambda (&optional _window) (cl-incf redisplays))))
        (yunge-reader-pdf--search-result-changed)
        (setq yunge-reader-search-highlight-visible nil)
        (yunge-reader-pdf--search-result-changed))
      (should (equal pages '(0)))
      (should (equal requests '(0)))
      (should (= scrolls 1))
      (should (= redisplays 2)))))

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

(ert-deftest yunge-reader-pdf-builds-a-large-stable-roll ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (let* ((count 50000)
           (page-info
            '((width . 100.0) (height . 200.0)))
           (metadata
            (list :page-count count
                  :pages (make-list count page-info))))
      (setq yunge-reader-document
            (make-yunge-reader-document :metadata metadata))
      (yunge-reader-pdf--load-page-infos)
      (yunge-reader-pdf--build-roll)
      (should (= (length yunge-reader-pdf--page-infos) count))
      (should (= (length yunge-reader-pdf--page-positions) count))
      (should (= (buffer-size) (1- (* 2 count))))
      (should (= (aref yunge-reader-pdf--page-positions 0) 1))
      (should (= (aref yunge-reader-pdf--page-positions 25000)
                 50001))
      (should (= (aref yunge-reader-pdf--page-positions 49999)
                 99999))
      (should (= (yunge-reader-pdf--page-at-position 99999)
                 49999)))))

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
      (let ((yunge-reader-pdf-center-pages t))
        (dotimes (page 2)
          (yunge-reader-pdf--paint-page page))
        (yunge-reader-pdf--paint-pages '(0))
        (should (equal (get-text-property 1 'display)
                       '(image "zero.png")))
        (should
         (equal (get-text-property 1 'line-prefix)
                '(space :align-to (- center (450)))))
        (should (eq (car (get-text-property 3 'display)) 'space))
        (yunge-reader-pdf--paint-pages '(1))
        (should (eq (car (get-text-property 1 'display)) 'space))
        (should (equal (get-text-property 3 'display)
                       '(image "one.png"))))
      (let ((yunge-reader-pdf-center-pages nil))
        (yunge-reader-pdf--paint-page 1))
      (should-not (get-text-property 3 'line-prefix))
      (should-not (buffer-modified-p)))))

(ert-deftest yunge-reader-pdf-scales-a-cached-page-during-zoom ()
  (with-temp-buffer
    (yunge-reader-mode)
    (yunge-reader-pdf-view-mode 1)
    (setq yunge-reader-document
          (yunge-reader-pdf-test--document
           '((page . 0) (width . 100.0) (height . 200.0))))
    (yunge-reader-pdf--load-page-infos)
    (yunge-reader-pdf--build-roll)
    (setq yunge-reader-pdf--displayed-pages '(0))
    (puthash '(0 . 800)
             '((path . "old.png")
               (pixel-width . 800) (pixel-height . 1600))
             yunge-reader-pdf--render-results)
    (cl-letf (((symbol-function 'create-image)
               (lambda (path &rest properties)
                 (cons 'image (cons path properties)))))
      (yunge-reader-pdf--paint-page 0 1000)
      (should
       (equal
        (get-text-property 1 'display)
        '(image "old.png" nil nil
                :width 1000 :height 2000
                :transform-smoothing t)))
      (should
       (equal
        (get-text-property 1 'line-prefix)
        '(space :align-to (- center (500)))))
      (should (= (get-text-property
                  1 'yunge-reader-pdf-display-width)
                 1000))
      (puthash '(0 . 1000)
               '((path . "new.png")
                 (pixel-width . 1000) (pixel-height . 2000))
               yunge-reader-pdf--render-results)
      (yunge-reader-pdf--paint-page 0 1000)
      (should
       (equal (get-text-property 1 'display)
              '(image "new.png" nil nil)))
      (should-not (buffer-modified-p)))))

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
