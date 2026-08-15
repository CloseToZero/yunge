;;; yunge-reader-test.el --- Document reader tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader)

(yunge-test-deftest-lazy-load yunge-reader
  (evil which-key))

(defun yunge-reader-test--place
    (driver unit &optional x y zoom-mode scale)
  "Return printable place data for DRIVER at UNIT."
  (list
   :version yunge-reader-place-version
   :driver driver
   :position (list :unit unit :offset nil :x x :y y)
   :zoom-mode (or zoom-mode 'fit-width)
   :scale (or scale 1.0)))

(ert-deftest yunge-reader-exposes-one-viewer-key-vocabulary ()
  (yunge-test-keymap-keys
   yunge-reader-mode-map
   '(("+" . yunge-reader-zoom-in)
     ("-" . yunge-reader-zoom-out)
     ("=" . yunge-reader-zoom-reset)
     ("/" . yunge-reader-search)
     ("N" . yunge-reader-search-previous)
     ("P" . yunge-reader-fit-page)
     ("W" . yunge-reader-fit-width)
     ("gr" . yunge-reader-refresh)
     ("n" . yunge-reader-search-next)
     ("o" . yunge-reader-outline)
     ("q" . quit-window)
     ("y" . yunge-reader-copy-selection)))
  (should-not
   (eq (lookup-key yunge-reader-mode-map (kbd "M-w"))
       #'yunge-reader-copy-selection))
  (should-not
   (eq (lookup-key yunge-reader-mode-map (kbd "s"))
       #'yunge-reader-search-next)))

(ert-deftest yunge-reader-integrates-with-evil-states ()
  (yunge-test-enable-evil)
  (require 'which-key)
  (yunge-test-evil-normal-keys
   'yunge-reader-mode
   '(("/" . yunge-reader-search)
     ("N" . yunge-reader-search-previous)
     ("P" . yunge-reader-fit-page)
     ("W" . yunge-reader-fit-width)
     ("gr" . yunge-reader-refresh)
     ("n" . yunge-reader-search-next)
     ("o" . yunge-reader-outline)
     ("q" . quit-window)
     ("y" . yunge-reader-copy-selection)
     ("0" . evil-beginning-of-line)
     ("b" . evil-backward-word-begin)
     ("p" . evil-paste-after)
     ("w" . evil-forward-word-begin)))
  (yunge-test-which-key-prefix-bindings
   'yunge-reader-mode "g" '(("r" nil "refresh")))
  (yunge-test-evil-visual-keys
   'yunge-reader-mode
   '(("y" . evil-yank))))

(ert-deftest yunge-reader-builds-unique-outline-candidates ()
  (let* ((first
          (make-yunge-reader-outline-item
           :title "Chapter"
           :depth 1
           :action
           (make-yunge-reader-action
            :type 'location
            :position (make-yunge-reader-position :unit 1))))
         (second (copy-yunge-reader-outline-item first))
         (colliding
          (make-yunge-reader-outline-item
           :title "Chapter [1]"
           :depth 1
           :action
           (make-yunge-reader-action
            :type 'location
            :position (make-yunge-reader-position :unit 2))))
         (heading
          (make-yunge-reader-outline-item
           :title "Part" :depth 0))
         (outline
          (make-yunge-reader-outline-data
           :items (list heading first second colliding)))
         (candidates
          (yunge-reader--outline-candidates outline)))
    (should
     (equal (mapcar #'car candidates)
            '("  Chapter [1]"
              "  Chapter [2]"
              "  Chapter [1] [2]")))
    (should (eq (cdar candidates) first))
    (should (eq (cdadr candidates) second))
    (should (eq (cdr (nth 2 candidates)) colliding))))

(ert-deftest yunge-reader-caches-a-late-outline-without-opening-it ()
  (let ((yunge-reader-drivers nil)
        (requests 0)
        completion
        (shown 0)
        reader
        other)
    (unwind-protect
        (save-window-excursion
          (setq reader (generate-new-buffer " *reader-outline-late*"))
          (setq other (generate-new-buffer " *reader-outline-other*"))
          (switch-to-buffer reader)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match #'ignore :open #'ignore :close #'ignore
                  :request
                  (lambda (_document operation arguments complete)
                    (should (eq operation 'outline))
                    (should-not arguments)
                    (cl-incf requests)
                    (setq completion complete))
                  :location
                  (lambda (_document _window)
                    (make-yunge-reader-position :unit 0))
                  :restore (lambda (&rest _arguments) t))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file "outline.pdf" :driver driver :layout 'fixed)))
          (cl-letf
              (((symbol-function 'yunge-reader--select-outline-item)
                (lambda (_outline) (cl-incf shown))))
            (yunge-reader-outline)
            (should yunge-reader--outline-pending)
            (switch-to-buffer other)
            (funcall
             completion
             (make-yunge-reader-outline-data
              :items
              (list
               (make-yunge-reader-outline-item
                :title "Chapter" :depth 0)))
             nil)
            (should (zerop shown))
            (with-current-buffer reader
              (should yunge-reader--outline-loaded)
              (should-not yunge-reader--outline-pending))
            (switch-to-buffer reader)
            (yunge-reader-outline)
            (should (= shown 1))
            (should (= requests 1))))
      (when (buffer-live-p reader)
        (kill-buffer reader))
      (when (buffer-live-p other)
        (kill-buffer other)))))

(ert-deftest yunge-reader-outline-jumps-restore-and-record-places ()
  (let* ((file (expand-file-name "outline-jump.pdf"))
         (key (yunge-reader--place-file-key file))
         (yunge-reader-saved-places nil)
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit 1))
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-outline-jump*"))
          (switch-to-buffer buffer)
          (set-window-parameter
           (selected-window) 'yunge-jump-history nil)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match #'ignore :open #'ignore :close #'ignore
                  :request #'ignore
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (_document position _window)
                    (setq current position)
                    t))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t)
            (let ((item
                   (make-yunge-reader-outline-item
                    :title "Target"
                    :depth 0
                    :action
                    (make-yunge-reader-action
                     :type 'location
                     :position
                     (make-yunge-reader-position :unit 9)
                     :zoom-mode 'manual
                     :scale 2.0))))
              (let ((inhibit-message t))
                (should
                 (yunge-reader--follow-outline-item item)))))
          (should (= (yunge-reader-position-unit current) 9))
          (should (eq yunge-reader-zoom-mode 'manual))
          (should (= yunge-reader-scale 2.0))
          (should
           (= (plist-get
               (plist-get
                (cdr (assoc key yunge-reader-saved-places))
                :position)
               :unit)
              9))
          (let* ((history
                  (window-parameter
                   (selected-window) 'yunge-jump-history))
                 (entry
                  (car
                   (yunge-jump-history--history-entries history)))
                 (place
                  (plist-get
                   (yunge-jump-history--entry-value entry)
                   :place)))
            (should
             (= (plist-get (plist-get place :position) :unit)
                1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-rejected-outline-jump-rolls-back ()
  (let* ((file (expand-file-name "outline-rejected.pdf"))
         (key (yunge-reader--place-file-key file))
         (old (yunge-reader-test--place 'test 4))
         (yunge-reader-saved-places
          (list (cons key (copy-tree old t))))
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit 4))
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-outline-reject*"))
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match #'ignore :open #'ignore :close #'ignore
                  :request #'ignore
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (_document position _window)
                    (setq current position)
                    (/= (yunge-reader-position-unit position) 9)))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t)
            (let ((item
                   (make-yunge-reader-outline-item
                    :title "Rejected"
                    :depth 0
                    :action
                    (make-yunge-reader-action
                     :type 'location
                     :position
                     (make-yunge-reader-position :unit 9)
                     :zoom-mode 'manual
                     :scale 2.0))))
              (should-error
               (yunge-reader--follow-outline-item item)
               :type 'user-error)))
          (should (= (yunge-reader-position-unit current) 4))
          (should (eq yunge-reader-zoom-mode 'fit-width))
          (should (= yunge-reader-scale 1.0))
          (should
           (equal (cdr (assoc key yunge-reader-saved-places))
                  old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-registers-replaces-and-resolves-drivers ()
  (let ((yunge-reader-drivers nil))
    (yunge-reader-register-driver
     'fallback
     :match (lambda (_file) t)
     :open #'ignore :close #'ignore :request #'ignore)
    (yunge-reader-register-driver
     'pdf
     :match (lambda (file) (string-suffix-p ".pdf" file t))
     :open #'ignore :close #'ignore :request #'ignore)
    (should
     (eq (yunge-reader-driver-name
          (yunge-reader-driver-for-file "book.pdf"))
         'pdf))
    (let ((replacement
           (yunge-reader-register-driver
            'pdf
            :match (lambda (file) (string-suffix-p ".xps" file t))
            :open #'ignore :close #'ignore :request #'ignore)))
      (should (= (length yunge-reader-drivers) 2))
      (should (eq (car yunge-reader-drivers) replacement))
      (should
       (eq (yunge-reader-driver-name
            (yunge-reader-driver-for-file "book.pdf"))
           'fallback))
      (should
       (eq (yunge-reader-driver-name
            (yunge-reader-driver-for-file "book.xps"))
           'pdf)))))

(ert-deftest yunge-reader-requires-both-driver-place-functions ()
  (let ((yunge-reader-drivers nil))
    (should-error
     (yunge-reader-register-driver
      'location-only
      :match #'ignore :open #'ignore :close #'ignore :request #'ignore
      :location #'ignore))
    (should-error
     (yunge-reader-register-driver
      'restore-only
      :match #'ignore :open #'ignore :close #'ignore :request #'ignore
      :restore #'ignore))))

(ert-deftest yunge-reader-open-failure-preserves-the-saved-place ()
  (let* ((file (expand-file-name "unbuilt.pdf"))
         (key (yunge-reader--place-file-key file))
         (old (yunge-reader-test--place 'test 23))
         (yunge-reader-saved-places
          (list (cons key (copy-tree old t))))
         (yunge-reader-drivers nil)
         complete
         locations)
    (with-temp-buffer
      (yunge-reader-mode)
      (let ((driver
             (yunge-reader-register-driver
              'test
              :match (lambda (_file) t)
              :open (lambda (_file callback) (setq complete callback))
              :close #'ignore :request #'ignore
              :location
              (lambda (_document _window)
                (cl-incf locations)
                (make-yunge-reader-position :unit 0))
              :restore #'ignore)))
        (yunge-reader--begin-open (current-buffer) driver file)
        (should (equal yunge-reader--pending-place old))
        (funcall complete nil nil '(error "Native helper unavailable"))
        (should-not yunge-reader--place-recording-enabled)
        (should-not yunge-reader--pending-place)
        (should-not locations)
        (should (equal (cdr (assoc key yunge-reader-saved-places))
                       old))))))

(ert-deftest yunge-reader-restores-before-committing-a-place ()
  (let* ((file (expand-file-name "remembered.pdf"))
         (key (yunge-reader--place-file-key file))
         (old (yunge-reader-test--place
               'test 23 11.0 17.0 'manual 2.0))
         (yunge-reader-saved-places
          (list (cons key (copy-tree old t))))
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit 0))
         restored
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-place-test*"))
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (add-hook
           'yunge-reader-refresh-hook
           (lambda ()
             (yunge-reader-record-place)
             (should
              (equal (cdr (assoc key yunge-reader-saved-places))
                     old)))
           nil t)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore :request #'ignore
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (_document location _window)
                    (should (eq yunge-reader-zoom-mode 'manual))
                    (should (= yunge-reader-scale 2.0))
                    (setq current location
                          restored t)))))
            (yunge-reader--begin-open buffer driver file))
          (should restored)
          (should yunge-reader--place-recording-enabled)
          (should-not yunge-reader--pending-place)
          (should
           (equal (cdr (assoc key yunge-reader-saved-places)) old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-rejected-restore-preserves-the-saved-place ()
  (let* ((file (expand-file-name "rejected.pdf"))
         (key (yunge-reader--place-file-key file))
         (old (yunge-reader-test--place 'test 42))
         (yunge-reader-saved-places
          (list (cons key (copy-tree old t))))
         (yunge-reader-drivers nil)
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-reject-test*"))
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (add-hook 'yunge-reader-refresh-hook
                    #'yunge-reader-record-place nil t)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore :request #'ignore
                  :location
                  (lambda (_document _window)
                    (make-yunge-reader-position :unit 0))
                  :restore (lambda (&rest _arguments) nil))))
            (yunge-reader--begin-open buffer driver file))
          (should-not yunge-reader--place-recording-enabled)
          (should
           (equal (cdr (assoc key yunge-reader-saved-places)) old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-captures-immutable-jump-targets ()
  (let ((file (expand-file-name "jump-capture.pdf"))
        (yunge-reader-drivers nil)
        (current (make-yunge-reader-position :unit 3))
        buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-jump-capture*"))
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open #'ignore :close #'ignore :request #'ignore
                  :location (lambda (_document _window) current)
                  :restore (lambda (&rest _arguments) t))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t)
            (let* ((entry
                    (yunge-jump-history--window-entry
                     (selected-window)))
                   (target
                    (yunge-jump-history--entry-value entry)))
              (should
               (eq (yunge-jump-history--entry-type entry) 'reader))
              (should (equal (plist-get target :file) file))
              (setf (yunge-reader-position-unit current) 9)
              (should
               (= (plist-get
                   (plist-get (plist-get target :place) :position)
                   :unit)
                  3)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-rejected-live-jump-rolls-back ()
  (let* ((file (expand-file-name "jump-rejected.pdf"))
         (old (yunge-reader-test--place 'test 4))
         (target (yunge-reader-test--place 'test 9))
         (key (yunge-reader--place-file-key file))
         (yunge-reader-saved-places
          (list (cons key (copy-tree old t))))
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit 4))
         completion
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-jump-reject*"))
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open #'ignore :close #'ignore :request #'ignore
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (_document position _window)
                    (setq current position)
                    (/= (yunge-reader-position-unit position) 9)))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t)
            (yunge-reader--visit-jump-target
             (list :file file :place target)
             (selected-window)
             (lambda (success) (setq completion success))))
          (should-not completion)
          (should (= (yunge-reader-position-unit current) 4))
          (should (eq (window-buffer) buffer))
          (should (equal (cdr (assoc key yunge-reader-saved-places))
                         old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-reopens-jump-target-asynchronously ()
  (let* ((file (expand-file-name "jump-reopen.pdf"))
         (old (yunge-reader-test--place 'test 17))
         (target (yunge-reader-test--place 'test 6))
         (key (yunge-reader--place-file-key file))
         (yunge-reader-saved-places
          (list (cons key (copy-tree old t))))
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit 0))
         complete-open
         completion
         (restored 0)
         reader-buffer)
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (yunge-reader-register-driver
             'test
             :match (lambda (_file) t)
             :open
             (lambda (_file complete) (setq complete-open complete))
             :close #'ignore :request #'ignore
             :location (lambda (_document _window) current)
             :restore
             (lambda (_document position _window)
               (setq current position)
               (cl-incf restored)
               t))
            (yunge-reader--visit-jump-target
             (list :file file :place target)
             (selected-window)
             (lambda (success) (setq completion success)))
            (should complete-open)
            (should-not completion)
            (should (eq (window-buffer) (current-buffer)))
            (funcall complete-open 'handle '(:layout fixed) nil)
            (setq reader-buffer (window-buffer))
            (should completion)
            (with-current-buffer reader-buffer
              (should (derived-mode-p 'yunge-reader-mode)))
            (should (= restored 2))
            (should (= (yunge-reader-position-unit current) 6))
            (should
             (equal (cdr (assoc key yunge-reader-saved-places))
                    target))))
      (when (buffer-live-p reader-buffer)
        (kill-buffer reader-buffer)))))

(ert-deftest yunge-reader-abandons-reopen-after-window-change ()
  (let* ((file (expand-file-name "jump-abandoned.pdf"))
         (old (yunge-reader-test--place 'test 21))
         (target (yunge-reader-test--place 'test 8))
         (key (yunge-reader--place-file-key file))
         (yunge-reader-saved-places
          (list (cons key (copy-tree old t))))
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit 0))
         complete-open
         completion)
    (save-window-excursion
      (with-temp-buffer
        (insert "before\nafter\n")
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))
        (yunge-reader-register-driver
         'test
         :match (lambda (_file) t)
         :open (lambda (_file complete) (setq complete-open complete))
         :close #'ignore :request #'ignore
         :location (lambda (_document _window) current)
         :restore
         (lambda (_document position _window)
           (setq current position)
           t))
        (yunge-reader--visit-jump-target
         (list :file file :place target)
         (selected-window)
         (lambda (success) (setq completion success)))
        (goto-char (point-max))
        (funcall complete-open 'handle '(:layout fixed) nil)
        (should (eq completion :cancel))
        (should (eq (window-buffer) (current-buffer)))
        (should (= (point) (point-max)))
        (should-not (yunge-reader--existing-buffer file))
        (should
         (equal (cdr (assoc key yunge-reader-saved-places)) old))))))

(ert-deftest yunge-reader-saved-places-are-bounded-and-most-recent-first ()
  (let ((yunge-reader-place-limit 2)
        (yunge-reader-saved-places nil))
    (dolist (name '("one.pdf" "two.pdf" "three.pdf"))
      (yunge-reader--store-place
       name (yunge-reader-test--place 'test name)))
    (should
     (equal (mapcar (lambda (entry)
                      (file-name-nondirectory (car entry)))
                    yunge-reader-saved-places)
            '("three.pdf" "two.pdf")))))

(ert-deftest yunge-reader-opens-reuses-and-closes-one-document ()
  (let ((yunge-reader-drivers nil)
        opened
        closed
        buffer)
    (unwind-protect
        (save-window-excursion
          (yunge-reader-register-driver
           'test
           :match (lambda (_file) t)
           :open
           (lambda (file complete)
             (setq opened file)
             (funcall complete 'handle
                      '(:layout fixed :metadata (:pages 3)) nil))
           :close (lambda (document) (push document closed))
           :request #'ignore)
          (setq buffer (yunge-reader-open "book.pdf"))
          (with-current-buffer buffer
            (should (eq major-mode 'yunge-reader-mode))
            (should (equal opened
                           (expand-file-name "book.pdf")))
            (should
             (eq (yunge-reader-document-handle yunge-reader-document)
                 'handle))
            (should
             (eq (yunge-reader-document-layout yunge-reader-document)
                 'fixed)))
          (should (eq (yunge-reader-open "book.pdf") buffer))
          (kill-buffer buffer)
          (setq buffer nil)
          (should (= (length closed) 1))
          (should
           (eq (yunge-reader-document-handle (car closed)) 'handle)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-closes-a-handle-returned-after-buffer-death ()
  (let ((yunge-reader-drivers nil)
        complete
        closed
        buffer)
    (unwind-protect
        (save-window-excursion
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open (lambda (_file callback)
                          (setq complete callback))
                  :close (lambda (document) (push document closed))
                  :request #'ignore)))
            (setq buffer (generate-new-buffer " *reader-late-test*"))
            (with-current-buffer buffer
              (yunge-reader-mode))
            (yunge-reader--begin-open buffer driver "late.pdf")
            (kill-buffer buffer)
            (setq buffer nil)
            (funcall complete 'late-handle '(:layout fixed) nil)
            (should (= (length closed) 1))
            (should
             (eq (yunge-reader-document-handle (car closed))
                 'late-handle))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-rejects-invalid-driver-layouts ()
  (let ((yunge-reader-drivers nil)
        closed
        buffer)
    (unwind-protect
        (save-window-excursion
          (yunge-reader-register-driver
           'test
           :match (lambda (_file) t)
           :open (lambda (_file complete)
                   (funcall complete 'handle '(:layout pages) nil))
           :close (lambda (document) (push document closed))
           :request #'ignore)
          (setq buffer (yunge-reader-open "invalid.pdf"))
          (with-current-buffer buffer
            (should-not yunge-reader-document)
            (should (string-match-p "invalid layout" (buffer-string))))
          (should (= (length closed) 1)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-zoom-state-is-bounded-and-refreshes ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((yunge-reader-minimum-scale 0.5)
          (yunge-reader-maximum-scale 2.0)
          (yunge-reader-zoom-factor 2.0)
          (refreshes 0))
      (add-hook 'yunge-reader-refresh-hook
                (lambda () (cl-incf refreshes)) nil t)
      (yunge-reader-set-effective-scale 1.5)
      (should (= (yunge-reader-zoom-in) 2.0))
      (should (eq yunge-reader-zoom-mode 'manual))
      (should (= (yunge-reader-zoom-out 3) 0.5))
      (should (eq (yunge-reader-fit-width) 'fit-width))
      (should-not yunge-reader-effective-scale)
      (should (eq (yunge-reader-fit-page) 'fit-page))
      (should (= refreshes 4)))))

(ert-deftest yunge-reader-copies-cached-and-driver-resolved-selections ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (kill-ring nil)
           (start (make-yunge-reader-position :unit 1 :offset 2))
           (end (make-yunge-reader-position :unit 1 :offset 5))
           (requests 0)
           completion
           (driver
            (yunge-reader-register-driver
             'selection-test
             :match #'ignore
             :open #'ignore
             :close #'ignore
             :request
             (lambda (_document operation arguments complete)
               (cl-incf requests)
               (should (eq operation 'selection-text))
               (should (eq (plist-get arguments :start) start))
               (should (eq (plist-get arguments :end) end))
               (setq completion complete)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "selection.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-set-selection start end "cached")
      (let ((inhibit-message t))
        (should (equal (yunge-reader-copy-selection) "cached")))
      (should (equal (car kill-ring) "cached"))
      (yunge-reader-set-selection start end)
      (let ((inhibit-message t))
        (yunge-reader-copy-selection))
      (should (= requests 1))
      (with-temp-buffer
        (let ((inhibit-message t))
          (funcall completion '(:text "resolved") nil)))
      (should (equal (car kill-ring) "resolved"))
      (should
       (equal (yunge-reader-selection-text yunge-reader-selection)
              "resolved")))))

(ert-deftest yunge-reader-request-completes-only-once ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (calls 0)
           (driver
            (yunge-reader-register-driver
             'request-test
             :match #'ignore :open #'ignore :close #'ignore
             :request
             (lambda (_document _operation _arguments complete)
               (funcall complete 'first nil)
               (funcall complete 'second nil)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "request.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-request
       'test nil
       (lambda (value error-data)
         (should-not error-data)
         (should (eq value 'first))
         (cl-incf calls)))
      (should (= calls 1)))))

(ert-deftest yunge-reader-search-records-before-visiting-a-result ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (yunge-reader-mode)
      (let ((yunge-reader-search-results
             (list
              (make-yunge-reader-search-result
               :start (make-yunge-reader-position :unit 2)
               :end (make-yunge-reader-position :unit 2)
               :text "match")))
            events)
        (add-hook
         'yunge-reader-search-result-hook
         (lambda () (push 'visit events)) nil t)
        (cl-letf (((symbol-function 'yunge-jump-history-record)
                   (lambda (window)
                     (should (eq window (selected-window)))
                     (should-not yunge-reader-search-result)
                     (push 'record events))))
          (yunge-reader--set-search-index 0))
        (should (equal (nreverse events) '(record visit)))))))

(ert-deftest yunge-reader-search-loads-bounded-batches-on-demand ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completions
           (hook-calls 0)
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 8 :offset 3)
             :end (make-yunge-reader-position :unit 8 :offset 8)
             :text "needle" :before "a " :after " b"))
           (second
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 9 :offset 4)
             :end (make-yunge-reader-position :unit 9 :offset 9)
             :text "needle"))
           (driver
            (yunge-reader-register-driver
             'search-test
             :match #'ignore :open #'ignore :close #'ignore
             :request
             (lambda (_document operation arguments complete)
               (should (eq operation 'search))
               (setq requests (append requests (list arguments))
                     completions (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (add-hook 'yunge-reader-search-result-hook
                (lambda () (cl-incf hook-calls)) nil t)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest arguments)
                   (apply function arguments))))
        (let ((inhibit-message t))
          (yunge-reader-search "needle"))
        (should-not (plist-get (car requests) :case-sensitive))
        (should (= (plist-get (car requests) :page-limit)
                   yunge-reader-search-page-limit))
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results nil
          :cursor (make-yunge-reader-position :unit 8 :offset 0))
         nil)
        (should (= (length requests) 2))
        (should
         (equal (plist-get (cadr requests) :cursor)
                (make-yunge-reader-position :unit 8 :offset 0)))
        (let ((inhibit-message t))
          (funcall
           (pop completions)
           (make-yunge-reader-search-batch
            :results (list first second)
            :cursor (make-yunge-reader-position :unit 10 :offset 0))
           nil))
        (should (eq yunge-reader-search-result first))
        (should (= yunge-reader--search-index 0))
        (let ((inhibit-message t))
          (yunge-reader-search-next))
        (should (eq yunge-reader-search-result second))
        (let ((inhibit-message t))
          (yunge-reader-search-next))
        (should (= (length requests) 3))
        (let ((inhibit-message t))
          (funcall
           (pop completions)
           (make-yunge-reader-search-batch :results nil :done t)
           nil))
        (should (eq yunge-reader-search-result first))
        (should (= hook-calls 4))))))

(ert-deftest yunge-reader-search-uses-smart-case-and-rejects-late-results ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completions
           (late
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 0 :offset 0)
             :end (make-yunge-reader-position :unit 0 :offset 2)
             :text "old"))
           (current
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 1 :offset 1)
             :end (make-yunge-reader-position :unit 1 :offset 3)
             :text "New"))
           (driver
            (yunge-reader-register-driver
             'stale-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :request
             (lambda (_document _operation arguments complete)
               (push arguments requests)
               (push complete completions)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((inhibit-message t))
        (yunge-reader-search "old")
        (yunge-reader-search "New"))
      (should (plist-get (car requests) :case-sensitive))
      (should-not (plist-get (cadr requests) :case-sensitive))
      (let ((new-completion (car completions))
            (old-completion (cadr completions))
            (inhibit-message t))
        (funcall
         old-completion
         (make-yunge-reader-search-batch
          :results (list late) :done t)
         nil)
        (should-not yunge-reader-search-result)
        (funcall
         new-completion
         (make-yunge-reader-search-batch
          :results (list current) :done t)
         nil))
      (should (eq yunge-reader-search-result current)))))

(ert-deftest yunge-reader-search-previous-finishes-before-wrapping ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           completion
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 0 :offset 0)
             :end (make-yunge-reader-position :unit 0 :offset 0)))
           (last
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 2 :offset 0)
             :end (make-yunge-reader-position :unit 2 :offset 0)))
           (driver
            (yunge-reader-register-driver
             'backward-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :request
             (lambda (_document _operation _arguments complete)
               (setq completion complete)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((inhibit-message t))
        (yunge-reader-search "x")
        (funcall
         completion
         (make-yunge-reader-search-batch
          :results (list first)
          :cursor (make-yunge-reader-position :unit 1 :offset 0))
         nil)
        (yunge-reader-search-previous)
        (funcall
         completion
         (make-yunge-reader-search-batch
          :results (list last) :done t)
         nil))
      (should (eq yunge-reader-search-result last)))))

;;; yunge-reader-test.el ends here
