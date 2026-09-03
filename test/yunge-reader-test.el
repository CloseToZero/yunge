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
  (ignore driver)
  (list
   :position (list :unit unit :offset nil :x x :y y)
   :zoom-mode (or zoom-mode 'fit-width)
   :scale (or scale 1.0)))

(defun yunge-reader-test--mark (driver unit &optional offset)
  "Return printable Reader mark data for DRIVER at UNIT and OFFSET."
  (ignore driver)
  (list
   :position (list :unit unit :offset offset :x nil :y nil)))

(defun yunge-reader-test--document-state (file driver &rest properties)
  "Return one saved document-state list for FILE and DRIVER."
  (let ((format (yunge-reader--driver-format driver))
        (path (yunge-reader-state-canonical-path file)))
    (list
     (cons
      (yunge-reader-state-key file format)
      (append
       (list :version yunge-reader-document-state-version
             :aliases (list path))
       (copy-tree properties t))))))

(defun yunge-reader-test--saved-state-value (file driver property)
  "Return saved FILE DRIVER PROPERTY without exposing record layout."
  (yunge-reader-state-value file (yunge-reader--driver-format driver)
                            property))

(defun yunge-reader-test--saved-place (file driver)
  "Return FILE's saved place for DRIVER."
  (yunge-reader-test--saved-state-value file driver :place))

(defun yunge-reader-test--saved-marks (file driver)
  "Return FILE's saved marks for DRIVER."
  (yunge-reader-test--saved-state-value file driver :marks))

(defun yunge-reader-test--buffer (name)
  "Return a new Reader buffer named NAME."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (yunge-reader-mode))
    buffer))

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
     ("C-g" . yunge-reader-keyboard-quit)
     ("<escape>" . yunge-reader-escape)
     ("'a" . yunge-reader-goto-mark)
     ("gr" . yunge-reader-refresh)
     ("ma" . yunge-reader-set-mark)
     ("n" . yunge-reader-search-next)
     ("o" . yunge-reader-outline)
     ("q" . undefined)
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
     ("'a" . yunge-reader-goto-mark)
     ("gr" . yunge-reader-refresh)
     ("ma" . yunge-reader-set-mark)
     ("n" . yunge-reader-search-next)
     ("o" . yunge-reader-outline)
     ("q" . evil-record-macro)
     ("SPC m a D" . yunge-reader-set-default-appearance)
     ("SPC m a d" . yunge-reader-set-document-appearance)
     ("SPC m a u" . yunge-reader-unset-document-appearance)
     ("SPC m p" . yunge-reader-make-primary)
     ("SPC m v" . yunge-reader-new-view)
     ("y" . yunge-reader-copy-selection)
     ("0" . evil-beginning-of-line)
     ("b" . evil-backward-word-begin)
     ("p" . evil-paste-after)
     ("w" . evil-forward-word-begin)))
  (yunge-test-evil-visual-keys
   'yunge-reader-mode
   '(("y" . evil-yank))))

(ert-deftest yunge-reader-keeps-the-cursor-hidden-with-evil ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (yunge-reader-mode)
    (evil-refresh-cursor 'normal)
    (should (equal evil-normal-state-cursor '(nil)))
    (should-not cursor-type)
    (evil-refresh-cursor 'visual)
    (should (equal evil-visual-state-cursor '(nil)))
    (should-not cursor-type)))

(ert-deftest yunge-reader-refuses-evil-editing-states ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (insert "reader slots")
    (yunge-reader-mode)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((contents (buffer-string)))
        (dolist (command '(evil-insert-state evil-replace-state))
          (funcall command 1)
          (should (eq evil-state 'normal))
          (should buffer-read-only)
          (should (equal (buffer-string) contents))))))
  (with-temp-buffer
    (evil-normal-state 1)
    (evil-insert-state 1)
    (should (eq evil-state 'insert))
    (evil-normal-state 1)
    (evil-replace-state 1)
    (should (eq evil-state 'replace))
    (evil-normal-state 1)))

(ert-deftest yunge-reader-disables-auto-save ()
  (with-temp-buffer
    (auto-save-mode 1)
    (should buffer-auto-save-file-name)
    (yunge-reader-mode)
    (should-not buffer-auto-save-file-name)))

(ert-deftest yunge-reader-registers-open-place-savehist-hook ()
  (require 'savehist)
  (should
   (memq #'yunge-reader--save-open-places savehist-save-hook)))

(ert-deftest yunge-reader-keeps-one-active-window-per-logical-view ()
  (let ((buffer (generate-new-buffer " *reader-presentations*"))
        (other (generate-new-buffer " *reader-other*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let* ((first (selected-window))
                 (second (split-window-right)))
            (set-window-buffer second buffer)
            (should (eq (yunge-reader--presentation-window) first))
            (select-window second)
            (yunge-reader--note-view-activity)
            (should (eq yunge-reader--active-presentation second))
            (let ((windows (yunge-reader--presentation-windows)))
              (should (= (length windows) 2))
              (should (memq first windows))
              (should (memq second windows)))
            (set-window-buffer second other)
            (should (eq (yunge-reader--presentation-window) first))
            (should (equal (yunge-reader--presentation-windows)
                           (list first)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p other)
        (kill-buffer other)))))

(ert-deftest yunge-reader-records-only-the-active-presentation ()
  (let ((buffer (generate-new-buffer " *reader-active-place*"))
        (yunge-reader-drivers nil)
        (yunge-reader-saved-document-state nil))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let* ((first (selected-window))
                 (second (split-window-right))
                 (driver
                  (yunge-reader-register-driver
                   'test
                   :match #'ignore :open #'ignore :close #'ignore
                   :location
                   (lambda (_document window)
                     (make-yunge-reader-position
                      :unit (if (eq window first) 1 2)))
                   :restore #'ignore)))
            (set-window-buffer second buffer)
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file "active.pdf" :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t)
            (yunge-reader-record-place second)
            (should-not
             (yunge-reader-test--saved-place "active.pdf" driver))
            (yunge-reader-record-place first)
            (should
             (= (plist-get
                 (plist-get
                  (yunge-reader-test--saved-place "active.pdf" driver)
                  :position)
                 :unit)
                1))
            (select-window second)
            (yunge-reader-record-place second)
            (should
             (= (plist-get
                 (plist-get
                  (yunge-reader-test--saved-place "active.pdf" driver)
                  :position)
                 :unit)
                2))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-savehist-captures-a-visible-primary-place ()
  (let ((buffer (generate-new-buffer " *reader-savehist-place*"))
        (yunge-reader-drivers nil)
        (yunge-reader-saved-document-state nil)
        (file (expand-file-name "savehist.pdf"))
        (unit 17))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match #'ignore :open #'ignore :close #'ignore
                  :location
                  (lambda (_document _window)
                    (make-yunge-reader-position :unit unit))
                  :restore #'ignore)))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t)
            (yunge-reader--save-open-places)
            (should
             (= (plist-get
                 (plist-get
                  (yunge-reader-test--saved-place file driver)
                  :position)
                 :unit)
                17))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-hidden-close-saves-last-visible-place ()
  (let ((buffer (generate-new-buffer " *reader-hidden-place*"))
        (other (generate-new-buffer " *reader-hidden-other*"))
        (yunge-reader-drivers nil)
        (yunge-reader-saved-document-state nil)
        (file (expand-file-name "hidden.pdf"))
        (unit 23))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match #'ignore :open #'ignore :close #'ignore
                  :location
                  (lambda (_document _window)
                    (make-yunge-reader-position :unit unit))
                  :restore #'ignore)))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t)
            (yunge-reader--note-view-activity)
            (should-not
             (yunge-reader-test--saved-place file driver))
            (setq unit 99)
            (switch-to-buffer other)
            (with-current-buffer buffer
              (yunge-reader--close-document))
            (should
             (= (plist-get
                 (plist-get
                  (yunge-reader-test--saved-place file driver)
                  :position)
                 :unit)
                23))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p other)
        (kill-buffer other)))))

(ert-deftest yunge-reader-evil-escape-clears-transient-highlights ()
  (yunge-test-enable-evil)
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((result
           (make-yunge-reader-search-result
            :start (make-yunge-reader-position :unit 0 :offset 3)
            :end (make-yunge-reader-position :unit 0 :offset 8)))
          (refreshes 0)
          (search-updates 0))
      (setq yunge-reader-selection
            (make-yunge-reader-selection
             :start (make-yunge-reader-position :unit 0 :offset 1)
             :end (make-yunge-reader-position :unit 0 :offset 2))
            yunge-reader-search-query "needle"
            yunge-reader-search-results (list result)
            yunge-reader-search-result result
            yunge-reader-search-highlight-visible t)
      (add-hook 'yunge-reader-refresh-hook
                (lambda () (cl-incf refreshes)) nil t)
      (add-hook 'yunge-reader-search-result-hook
                (lambda () (cl-incf search-updates)) nil t)
      (let ((this-command nil))
        (evil-force-normal-state))
      (should yunge-reader-selection)
      (should yunge-reader-search-query)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (should (eq (key-binding (kbd "<escape>"))
                    'evil-force-normal-state))
        (execute-kbd-macro (kbd "<escape>")))
      (should-not yunge-reader-selection)
      (should (equal yunge-reader-search-query "needle"))
      (should (eq yunge-reader-search-result result))
      (should-not yunge-reader-search-highlight-visible)
      (should (zerop refreshes))
      (should (= search-updates 1)))))

(ert-deftest yunge-reader-quit-commands-clear-or-use-ordinary-action ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((refreshes 0)
          escaped
          quit)
      (setq yunge-reader-selection
            (make-yunge-reader-selection
             :start (make-yunge-reader-position :unit 0 :offset 1)
             :end (make-yunge-reader-position :unit 0 :offset 2)))
      (add-hook 'yunge-reader-refresh-hook
                (lambda () (cl-incf refreshes)) nil t)
      (yunge-reader-escape)
      (should-not yunge-reader-selection)
      (should (= refreshes 1))
      (cl-letf (((symbol-function 'keyboard-escape-quit)
                 (lambda () (setq escaped t))))
        (yunge-reader-escape))
      (should escaped)
      (should (= refreshes 1))
      (cl-letf (((symbol-function 'keyboard-quit)
                 (lambda () (setq quit t))))
        (yunge-reader-keyboard-quit))
      (should quit)
      (should (= refreshes 1)))))

(ert-deftest yunge-reader-notifies-logical-selection-changes ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((start (make-yunge-reader-position :unit 0 :offset 1))
          (end (make-yunge-reader-position :unit 0 :offset 2))
          changes)
      (add-hook 'yunge-reader-selection-change-hook
                (lambda ()
                  (push yunge-reader-selection changes))
                nil t)
      (yunge-reader-set-selection start end)
      (yunge-reader-set-selection start end)
      (yunge-reader-clear-selection t)
      (yunge-reader-clear-selection t)
      (should (= (length changes) 2))
      (should-not (car changes))
      (should
       (equal
        (cadr changes)
        (make-yunge-reader-selection :start start :end end))))))

(ert-deftest yunge-reader-search-starts-empty-and-uses-history ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((origin (make-yunge-reader-position :unit 7 :offset 3)))
      (setq yunge-reader-document 'document
            yunge-reader-search-query "old query")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt initial history &rest _arguments)
                   (should (equal prompt "Search document: "))
                   (should-not initial)
                   (should (eq history 'yunge-reader-search-history))
                  "new query"))
               ((symbol-function 'yunge-reader--current-position)
                (lambda (&optional _window) origin))
               ((symbol-function 'yunge-reader--request-search-batch)
                #'ignore))
        (call-interactively #'yunge-reader-search))
      (should (equal yunge-reader-search-query "new query"))
      (should (eq yunge-reader--search-direction 'forward))
      (should (eq yunge-reader--search-origin origin))
      (should yunge-reader-search-highlight-visible))))

(ert-deftest yunge-reader-validates-search-batch-completion-state ()
  (let ((cursor (make-yunge-reader-search-cursor :value 'next)))
    (should
     (yunge-reader--search-batch-valid-p
      (make-yunge-reader-search-batch :results nil :done t)))
    (should
     (yunge-reader--search-batch-valid-p
      (make-yunge-reader-search-batch
       :results nil :cursor cursor :done nil)))
    (should-not
     (yunge-reader--search-batch-valid-p
      (make-yunge-reader-search-batch :results nil :done nil)))
    (should-not
     (yunge-reader--search-batch-valid-p
      (make-yunge-reader-search-batch
       :results nil :cursor cursor :done t)))))

(ert-deftest yunge-reader-search-navigation-restores-hidden-highlight ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((first
           (make-yunge-reader-search-result
            :start (make-yunge-reader-position :unit 0 :offset 1)
            :end (make-yunge-reader-position :unit 0 :offset 2)))
          (second
           (make-yunge-reader-search-result
            :start (make-yunge-reader-position :unit 1 :offset 3)
            :end (make-yunge-reader-position :unit 1 :offset 4))))
      (setq yunge-reader-search-query "needle"
            yunge-reader-search-results (list first second)
            yunge-reader-search-result first
            yunge-reader-search-highlight-visible nil
            yunge-reader--search-index 0
            yunge-reader--search-direction 'forward)
      (yunge-reader-search-next)
      (should yunge-reader-search-highlight-visible)
      (should (eq yunge-reader-search-result second)))))

(ert-deftest yunge-reader-opens-only-allowlisted-uri-actions ()
  (let* ((yunge-reader-uri-schemes '("https" "mailto"))
         (allowed
          (make-yunge-reader-action
           :type 'uri :uri "HTTPS://example.com/book"))
         (blocked
          (make-yunge-reader-action
           :type 'uri :uri "javascript:alert(1)"))
         opened)
    (should (yunge-reader--action-valid-p allowed))
    (should (yunge-reader--action-valid-p blocked))
    (should-not
     (yunge-reader--outline-item-valid-p
      (make-yunge-reader-outline-item
       :title "Website" :depth 0 :action allowed)))
    (should-not
     (yunge-reader--action-valid-p
      (make-yunge-reader-action
       :type 'uri :uri "relative/path")))
    (should-not
     (yunge-reader--action-valid-p
      (make-yunge-reader-action
       :type 'uri :uri "https://example.com/a b")))
    (should-not
     (yunge-reader--action-valid-p
      (make-yunge-reader-action
       :type 'uri
       :uri (concat "https:" (make-string 4096 ?a)))))
    (should-not
     (yunge-reader--action-valid-p
      (make-yunge-reader-action
       :type 'location
       :position (make-yunge-reader-position :unit 1)
       :uri "https://example.com")))
    (require 'browse-url)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (uri &rest _arguments)
                 (setq opened uri))))
      (let ((inhibit-message t))
        (should (yunge-reader--follow-action allowed)))
      (should (equal opened "HTTPS://example.com/book"))
      (setq opened nil)
      (should-error
       (yunge-reader--follow-action blocked)
       :type 'user-error)
      (should-not opened))))

(ert-deftest yunge-reader-updates-a-hidden-outline-without-opening-it ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        (requests 0)
        completion
        reader
        outline-buffer
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
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore
                  :outline
                  (lambda (_document arguments complete)
                    (should-not arguments)
                    (cl-incf requests)
                    (setq completion complete))
                  :location
                  (lambda (_document _window)
                    (make-yunge-reader-position :unit 0))
                  :restore (lambda (&rest _arguments) t))))
            (yunge-reader--begin-open reader driver "outline.pdf"))
          (yunge-reader-outline)
          (setq outline-buffer (current-buffer))
          (should (derived-mode-p 'yunge-reader-outline-mode))
          (should (string-match-p
                   "Loading document outline"
                   (buffer-string)))
          (should
           (with-current-buffer reader
             (yunge-reader--document-entry-outline-task
              yunge-reader--document-entry)))
          (quit-window)
          (switch-to-buffer other)
          (funcall
           completion
           (make-yunge-reader-outline-data
            :items
            (list
             (make-yunge-reader-outline-item
              :title "Chapter" :depth 0)))
           nil)
          (should (eq (current-buffer) other))
          (should-not (get-buffer-window outline-buffer t))
          (with-current-buffer outline-buffer
            (should (equal (buffer-string) "  Chapter\n")))
          (with-current-buffer reader
            (should
             (yunge-reader--document-entry-outline-loaded
              yunge-reader--document-entry))
            (should-not
             (yunge-reader--document-entry-outline-task
              yunge-reader--document-entry)))
          (switch-to-buffer reader)
          (yunge-reader-outline)
          (should (eq (current-buffer) outline-buffer))
          (should (= requests 1))
          (kill-buffer reader)
          (setq reader nil)
          (should-not (buffer-live-p outline-buffer)))
      (when (buffer-live-p reader)
        (kill-buffer reader))
      (when (buffer-live-p other)
        (kill-buffer other)))))

(ert-deftest yunge-reader-shares-one-outline-request-between-views ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        (requests 0)
        completion
        first
        second
        first-outline
        second-outline)
    (unwind-protect
        (save-window-excursion
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore
                  :attach #'ignore
                  :detach #'ignore
                  :outline
                  (lambda (_document _arguments complete)
                    (cl-incf requests)
                    (setq completion complete))
                  :location
                  (lambda (_document _window)
                    (make-yunge-reader-position :unit 0))
                  :restore (lambda (&rest _arguments) t))))
            (setq first
                  (yunge-reader-test--buffer
                   " *reader-outline-primary*")
                  second
                  (yunge-reader-test--buffer
                   " *reader-outline-additional*"))
            (yunge-reader--begin-open first driver "shared-outline.pdf")
            (yunge-reader--begin-open second driver "shared-outline.pdf"))
          (switch-to-buffer first)
          (let ((first-window (selected-window))
                (second-window (split-window-right))
                (outline
                 (make-yunge-reader-outline-data
                  :items
                  (list
                   (make-yunge-reader-outline-item
                    :title "Chapter" :depth 0)))))
            (set-window-buffer second-window second)
            (with-selected-window first-window
              (with-current-buffer first
                (yunge-reader-outline)
                (setq first-outline
                      (buffer-local-value
                       'yunge-reader--outline-buffer first))))
            (with-selected-window second-window
              (with-current-buffer second
                (yunge-reader-outline)
                (setq second-outline
                      (buffer-local-value
                       'yunge-reader--outline-buffer second))))
            (let ((entry
                   (buffer-local-value
                    'yunge-reader--document-entry first)))
              (should
               (eq entry
                   (buffer-local-value
                    'yunge-reader--document-entry second)))
              (should (= requests 1))
              (should-not (eq first-outline second-outline))
              (should
               (yunge-reader--document-entry-outline-task entry))
              (should
               (= (length
                   (yunge-reader--document-entry-outline-waiters entry))
                  2))
              (with-selected-window second-window
                (funcall completion outline nil))
              (should
               (eq (yunge-reader--document-entry-outline entry)
                   outline))
              (should
               (yunge-reader--document-entry-outline-loaded entry))
              (should-not
               (yunge-reader--document-entry-outline-waiters entry))
              (dolist (buffer (list first-outline second-outline))
                (with-current-buffer buffer
                  (should (equal (buffer-string)
                                 "  Chapter\n"))))
              (with-selected-window first-window
                (with-current-buffer first
                  (yunge-reader-outline)))
              (should (get-buffer-window first-outline t))
              (should (= requests 1)))))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-cancels-outline-with-its-last-view ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        cancelled
        reader)
    (unwind-protect
        (save-window-excursion
          (let* ((child
                  (yunge-reader-task--make
                   :operation "outline"
                   :state 'sent
                   :cancel-function
                   (lambda (task reason)
                     (setq cancelled reason)
                     (setf (yunge-reader-task-state task) 'cancelled)
                     t)))
                 (driver
                  (yunge-reader-register-driver
                   'test
                   :match (lambda (_file) t)
                   :open
                   (lambda (_file complete)
                     (funcall complete 'handle '(:layout fixed) nil))
                   :close #'ignore
                   :outline
                   (lambda (_document _arguments _complete)
                     child)
                   :location
                   (lambda (_document _window)
                     (make-yunge-reader-position :unit 0))
                   :restore (lambda (&rest _arguments) t))))
            (setq reader
                  (yunge-reader-test--buffer
                   " *reader-outline-cancel*"))
            (switch-to-buffer reader)
            (yunge-reader--begin-open reader driver "outline-cancel.pdf")
            (yunge-reader-outline)
            (should
             (yunge-reader-task-active-p
              (with-current-buffer reader
                (yunge-reader--document-entry-outline-task
                 yunge-reader--document-entry))))
            (kill-buffer reader)
            (setq reader nil)
            (should (stringp cancelled))
            (should
             (zerop
              (hash-table-count yunge-reader--document-registry)))))
      (when (buffer-live-p reader)
        (kill-buffer reader)))))

(ert-deftest yunge-reader-outline-jumps-restore-and-record-places ()
  (let* ((file (expand-file-name "outline-jump.pdf"))
         (yunge-reader-saved-document-state nil)
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
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (_document position _window)
                    (setq current position)
                    t))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t
                  yunge-reader-search-query "needle"
                  yunge-reader--search-navigation-intent 'forward)
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
          (should yunge-reader--search-detached)
          (should-not yunge-reader--search-navigation-intent)
          (should (eq yunge-reader-zoom-mode 'manual))
          (should (= yunge-reader-scale 2.0))
          (should
           (= (plist-get
               (plist-get
                (yunge-reader-test--saved-place file 'test)
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

(ert-deftest yunge-reader-deferred-outline-jumps-preserve-stable-places ()
  (let* ((file (expand-file-name "outline-deferred.epub"))
         (old (yunge-reader-test--place 'test 1))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit 1))
         requested
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer
                (generate-new-buffer " *reader-outline-deferred*"))
          (switch-to-buffer buffer)
          (set-window-parameter
           (selected-window) 'yunge-jump-history nil)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match #'ignore :open #'ignore :close #'ignore
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (_document position _window)
                    (setq requested position)
                    :deferred))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'reflow)
                  yunge-reader--place-recording-enabled t)
            (let ((item
                   (make-yunge-reader-outline-item
                    :title "Deferred"
                    :depth 0
                    :action
                    (make-yunge-reader-action
                     :type 'location
                     :position
                     (make-yunge-reader-position :unit 9)))))
              (let ((inhibit-message t))
                (should
                 (eq (yunge-reader--follow-outline-item item)
                     :deferred)))))
          (should (= (yunge-reader-position-unit requested) 9))
          (should (= (yunge-reader-position-unit current) 1))
          (should
           (equal (yunge-reader-test--saved-place file 'test) old))
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
         (old (yunge-reader-test--place 'test 4))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
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
           (equal (yunge-reader-test--saved-place file 'test)
                  old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-registers-replaces-and-resolves-drivers ()
  (let ((yunge-reader-drivers nil))
    (yunge-reader-register-driver
     'fallback
     :match (lambda (_file) t)
     :open #'ignore :close #'ignore)
    (yunge-reader-register-driver
     'pdf
     :match (lambda (file) (string-suffix-p ".pdf" file t))
     :open #'ignore :close #'ignore)
    (should
     (eq (yunge-reader-driver-name
          (yunge-reader-driver-for-file "book.pdf"))
         'pdf))
    (let ((replacement
           (yunge-reader-register-driver
            'pdf
            :match (lambda (file) (string-suffix-p ".xps" file t))
            :open #'ignore :close #'ignore)))
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
      :match #'ignore :open #'ignore :close #'ignore
      :location #'ignore))
    (should-error
     (yunge-reader-register-driver
      'restore-only
      :match #'ignore :open #'ignore :close #'ignore
      :restore #'ignore))))

(ert-deftest yunge-reader-requires-both-driver-view-functions ()
  (let ((yunge-reader-drivers nil))
    (should-error
     (yunge-reader-register-driver
      'attach-only
      :match #'ignore :open #'ignore :close #'ignore
      :attach #'ignore))
    (should-error
     (yunge-reader-register-driver
      'detach-only
      :match #'ignore :open #'ignore :close #'ignore
      :detach #'ignore))))

(ert-deftest yunge-reader-open-failure-preserves-the-saved-place ()
  (let* ((file (expand-file-name "unbuilt.pdf"))
         (old (yunge-reader-test--place 'test 23))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
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
              :close #'ignore
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
        (should (equal (yunge-reader-test--saved-place file 'test)
                       old))))))

(ert-deftest yunge-reader-restores-before-committing-a-place ()
  (let* ((file (expand-file-name "remembered.pdf"))
         (old (yunge-reader-test--place
               'test 23 11.0 17.0 'manual 2.0))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
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
              (equal (yunge-reader-test--saved-place file 'test)
                     old)))
           nil t)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (document location window)
                    (setq current location
                          restored (list document location window))))))
            (yunge-reader--begin-open buffer driver file))
          (should
           (eq (yunge-reader-document-handle (nth 0 restored)) 'handle))
          (should (= (yunge-reader-position-unit (nth 1 restored)) 23))
          (should (eq (nth 2 restored) (selected-window)))
          (should yunge-reader--place-recording-enabled)
          (should-not yunge-reader--pending-place)
          (should
           (equal (yunge-reader-test--saved-place file 'test) old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-rejected-restore-preserves-the-saved-place ()
  (let* ((file (expand-file-name "rejected.pdf"))
         (old (yunge-reader-test--place 'test 42))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
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
                  :close #'ignore
                  :location
                  (lambda (_document _window)
                    (make-yunge-reader-position :unit 0))
                  :restore (lambda (&rest _arguments) nil))))
            (yunge-reader--begin-open buffer driver file))
          (should-not yunge-reader--place-recording-enabled)
          (should
           (equal (yunge-reader-test--saved-place file 'test) old)))
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
                  :open #'ignore :close #'ignore
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
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
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
                  :open #'ignore :close #'ignore
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
          (should (equal (yunge-reader-test--saved-place file 'test)
                         old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-reopens-jump-target-asynchronously ()
  (let* ((file (expand-file-name "jump-reopen.pdf"))
         (old (yunge-reader-test--place 'test 17))
         (target (yunge-reader-test--place 'test 6))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
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
             :close #'ignore
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
             (equal (yunge-reader-test--saved-place file 'test)
                    target))))
      (when (buffer-live-p reader-buffer)
        (kill-buffer reader-buffer)))))

(ert-deftest yunge-reader-abandons-reopen-after-window-change ()
  (let* ((file (expand-file-name "jump-abandoned.pdf"))
         (old (yunge-reader-test--place 'test 21))
         (target (yunge-reader-test--place 'test 8))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
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
         :close #'ignore
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
         (equal (yunge-reader-test--saved-place file 'test) old))))))

(ert-deftest yunge-reader-document-state-is-bounded-and-most-recent-first ()
  (let ((yunge-reader-document-state-limit 2)
        (yunge-reader-saved-document-state nil))
    (dolist (name '("one.pdf" "two.pdf" "three.pdf"))
      (yunge-reader--store-place
       name 'test (yunge-reader-test--place 'test name)))
    (should
     (equal (mapcar (lambda (entry)
                      (file-name-nondirectory
                       (car (plist-get (cdr entry) :aliases))))
                    yunge-reader-saved-document-state)
            '("three.pdf" "two.pdf")))))

(ert-deftest yunge-reader-consolidates-persistent-document-fields ()
  (let* ((file (expand-file-name "consolidated.epub"))
         (driver
          (yunge-reader--make-driver
           :name 'epub :close-function #'ignore))
         (place (yunge-reader-test--place 'epub "chapter"))
         (mark (yunge-reader-test--mark 'epub "chapter" 12))
         (yunge-reader-saved-document-state nil))
    (yunge-reader--store-place file driver place)
    (yunge-reader--store-appearance-override
     file driver 'follow-emacs)
    (yunge-reader--store-mark file driver ?a mark)
    (should (= (length yunge-reader-saved-document-state) 1))
    (should (equal (yunge-reader--saved-place file driver) place))
    (should
     (= (plist-get (cdar yunge-reader-saved-document-state) :version)
        yunge-reader-document-state-version))
    (should
     (eq (yunge-reader--saved-appearance-override file driver)
         'follow-emacs))
    (should (equal (yunge-reader--saved-mark file driver ?a) mark))))

(ert-deftest yunge-reader-shares-state-through-document-fingerprints ()
  (let* ((directory (make-temp-file "yunge-reader-fingerprint-" t))
         (original (expand-file-name "original.pdf" directory))
         (copy (expand-file-name "copy.pdf" directory))
         (driver
          (yunge-reader--make-driver
           :name 'pdf :close-function #'ignore))
         (place (yunge-reader-test--place 'pdf 7))
         (yunge-reader-saved-document-state nil)
         (yunge-reader-state--fingerprint-cache
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (write-region "same document" nil original nil 'silent)
          (copy-file original copy)
          (yunge-reader--store-place original driver place)
          (should
           (equal (yunge-reader--saved-place copy driver) place))
          (should (= (length yunge-reader-saved-document-state) 1))
          (should
           (equal
            (sort
             (copy-sequence
              (plist-get
               (cdar yunge-reader-saved-document-state) :aliases))
             #'string<)
            (sort
             (list (file-truename original) (file-truename copy))
             #'string<))))
      (delete-directory directory t))))

(ert-deftest yunge-reader-truncates-aliases-without-losing-state ()
  (let* ((directory (make-temp-file "yunge-reader-aliases-" t))
         (original (expand-file-name "original.pdf" directory))
         (copy-one (expand-file-name "copy-one.pdf" directory))
         (copy-two (expand-file-name "copy-two.pdf" directory))
         (place (yunge-reader-test--place 'pdf 7))
         (yunge-reader-document-alias-limit 3)
         (yunge-reader-saved-document-state nil)
         (yunge-reader-state--fingerprint-cache
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (write-region "same document" nil original nil 'silent)
          (copy-file original copy-one)
          (copy-file original copy-two)
          (yunge-reader--store-place original 'pdf place)
          (should (equal (yunge-reader--saved-place copy-one 'pdf) place))
          (should (equal (yunge-reader--saved-place copy-two 'pdf) place))
          (let ((yunge-reader-document-alias-limit 1))
            (should (equal (yunge-reader--saved-place original 'pdf) place))
            (should
             (equal
              (plist-get
               (cdar yunge-reader-saved-document-state) :aliases)
              (list (file-truename original))))))
      (delete-directory directory t))))

(ert-deftest yunge-reader-separates-changed-document-content ()
  (let* ((file (make-temp-file "yunge-reader-content-"))
         (place (yunge-reader-test--place 'pdf 3))
         (yunge-reader-saved-document-state nil)
         (yunge-reader-state--fingerprint-cache
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (write-region "first" nil file nil 'silent)
          (yunge-reader--store-place file 'pdf place)
          (write-region "second and different" nil file nil 'silent)
          (should-not (yunge-reader--saved-place file 'pdf))
          (should-not yunge-reader-saved-document-state)
          (yunge-reader-state-put file 'pdf :appearance 'original)
          (should (= (length yunge-reader-saved-document-state) 1))
          (should-not
           (yunge-reader-state-value file 'pdf :place)))
      (delete-file file))))

(ert-deftest yunge-reader-keeps-document-state-separate-by-driver ()
  (let* ((file (make-temp-file "yunge-reader-drivers-"))
         (pdf-place (yunge-reader-test--place 'pdf 3))
         (epub-place (yunge-reader-test--place 'epub "chapter"))
         (yunge-reader-saved-document-state nil)
         (yunge-reader-state--fingerprint-cache
          (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (write-region "one document" nil file nil 'silent)
          (yunge-reader--store-place file 'pdf pdf-place)
          (yunge-reader--store-place file 'epub epub-place)
          (should (= (length yunge-reader-saved-document-state) 2))
          (should (equal (yunge-reader--saved-place file 'pdf) pdf-place))
          (should (equal (yunge-reader--saved-place file 'epub) epub-place)))
      (delete-file file))))

(ert-deftest yunge-reader-marks-are-document-local-copied-and-bounded ()
  (let* ((yunge-reader-document-state-limit 2)
         (yunge-reader-saved-document-state nil)
         (driver
          (yunge-reader--make-driver :name 'test :close-function #'ignore))
         (one (expand-file-name "one.pdf"))
         (two (expand-file-name "two.pdf"))
         (three (expand-file-name "three.pdf"))
         (mark (yunge-reader-test--mark 'test 1 "start")))
    (yunge-reader--store-mark one driver ?a mark)
    (setf (plist-get (plist-get mark :position) :unit) 'mutated)
    (yunge-reader--store-mark
     one driver ?b (yunge-reader-test--mark 'test 2))
    (should
     (equal
      (mapcar #'car
              (yunge-reader-test--saved-marks one driver))
      '(98 97)))
    (should
     (= (plist-get
         (plist-get (yunge-reader--saved-mark one driver ?a) :position)
         :unit)
        1))
    (yunge-reader--store-mark
     two driver ?a (yunge-reader-test--mark 'test 3))
    (yunge-reader--store-mark
     three driver ?a (yunge-reader-test--mark 'test 4))
    (should
     (equal
      (mapcar
       (lambda (entry)
         (file-name-nondirectory
          (car (plist-get (cdr entry) :aliases))))
       yunge-reader-saved-document-state)
      '("three.pdf" "two.pdf")))
    (should-not (yunge-reader--saved-mark one driver ?a))
    (should-error
     (yunge-reader--store-mark two driver ?A mark)
     :type 'error)))

(ert-deftest yunge-reader-mark-commands-use-stable-position-and-current-zoom ()
  (let* ((file (expand-file-name "marked.pdf"))
         (yunge-reader-saved-document-state nil)
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit 1 :offset "source"))
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-marks*"))
          (switch-to-buffer buffer)
          (set-window-parameter
           (selected-window) 'yunge-jump-history nil)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match #'ignore :open #'ignore :close #'ignore
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (_document position _window)
                    (setq current position)
                    t))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'fixed)
                  yunge-reader--place-recording-enabled t
                  yunge-reader-zoom-mode 'manual
                  yunge-reader-scale 2.0)
            (let ((inhibit-message t))
              (should (eq (yunge-reader-set-mark ?a) ?a)))
            (let ((mark (yunge-reader--saved-mark file driver ?a)))
              (should mark)
              (should-not (plist-member mark :zoom-mode))
              (should-not (plist-member mark :scale)))
            (setq current (make-yunge-reader-position :unit 5 :offset "later")
                  yunge-reader-zoom-mode 'fit-width
                  yunge-reader-scale 1.0)
            (should (yunge-reader-goto-mark ?a)))
          (should (= (yunge-reader-position-unit current) 1))
          (should (equal (yunge-reader-position-offset current) "source"))
          (should (eq yunge-reader-zoom-mode 'fit-width))
          (should (= yunge-reader-scale 1.0))
          (should
           (= (plist-get
               (plist-get
                (yunge-reader-test--saved-place file 'test) :position)
               :unit)
              1))
          (let* ((history
                  (window-parameter
                   (selected-window) 'yunge-jump-history))
                 (entry
                  (car (yunge-jump-history--history-entries history)))
                 (place
                  (plist-get
                   (yunge-jump-history--entry-value entry) :place)))
            (should
             (= (plist-get (plist-get place :position) :unit) 5)))
          (should-error (yunge-reader-goto-mark ?b) :type 'user-error)
          (should-error (yunge-reader-set-mark ?A) :type 'user-error))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-mark-jumps-preserve-deferred-source-history ()
  (let* ((file (expand-file-name "marked.epub"))
         (yunge-reader-saved-document-state nil)
         (yunge-reader-drivers nil)
         (current (make-yunge-reader-position :unit "chapter-one"))
         requested
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-deferred-mark*"))
          (switch-to-buffer buffer)
          (set-window-parameter
           (selected-window) 'yunge-jump-history nil)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match #'ignore :open #'ignore :close #'ignore
                  :location (lambda (_document _window) current)
                  :restore
                  (lambda (_document position _window)
                    (setq requested position)
                    :deferred))))
            (setq yunge-reader-document
                  (make-yunge-reader-document
                   :file file :driver driver :layout 'reflow)
                  yunge-reader--place-recording-enabled t)
            (yunge-reader--store-mark
             file driver ?a
             (yunge-reader-test--mark 'test "chapter-two"))
            (should (eq (yunge-reader-goto-mark ?a) :deferred)))
          (should
           (equal (yunge-reader-position-unit requested) "chapter-two"))
          (should
           (equal (yunge-reader-position-unit current) "chapter-one"))
          (should-not (yunge-reader-test--saved-place file 'test))
          (let* ((history
                  (window-parameter
                   (selected-window) 'yunge-jump-history))
                 (entry
                  (car (yunge-jump-history--history-entries history)))
                 (place
                  (plist-get
                   (yunge-jump-history--entry-value entry) :place)))
            (should
             (equal
              (plist-get (plist-get place :position) :unit)
              "chapter-one"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-resolves-persistent-appearance-overrides ()
  (let* ((file (expand-file-name "appearance.epub"))
         (driver
          (yunge-reader--make-driver
           :name 'epub :close-function #'ignore))
         (document
          (make-yunge-reader-document :file file :driver driver))
         (yunge-reader-default-appearances
          '((epub . follow-emacs)))
         (yunge-reader-saved-document-state nil))
    (should (eq (yunge-reader-effective-appearance document)
                'follow-emacs))
    (should-not
     (yunge-reader-document-appearance-override document))
    (yunge-reader--store-appearance-override file driver 'original)
    (should (eq (yunge-reader-effective-appearance document) 'original))
    (should
     (eq (yunge-reader-document-appearance-override document)
         'original))
    (yunge-reader--unset-appearance-override file driver)
    (should (eq (yunge-reader-effective-appearance document)
                'follow-emacs))))

(ert-deftest yunge-reader-theme-changes-refresh-following-documents ()
  (let* ((following
          (make-yunge-reader-document
           :file "following.epub" :driver 'epub))
         (original
          (make-yunge-reader-document
           :file "original.pdf" :driver 'pdf))
         (following-entry
          (yunge-reader--make-document-entry
           :state 'ready :document following))
         (original-entry
          (yunge-reader--make-document-entry
           :state 'ready :document original))
         (loading-entry
          (yunge-reader--make-document-entry
           :state 'opening :document following))
         (yunge-reader-default-appearances
          '((epub . follow-emacs) (pdf . original)))
         (yunge-reader-saved-document-state nil)
         (yunge-reader--document-registry
          (make-hash-table :test #'equal))
         notified)
    (puthash 'following following-entry
             yunge-reader--document-registry)
    (puthash 'original original-entry
             yunge-reader--document-registry)
    (puthash 'loading loading-entry
             yunge-reader--document-registry)
    (cl-letf (((symbol-function
                'yunge-reader--notify-appearance-change)
               (lambda (entry) (push entry notified))))
      (yunge-reader--theme-changed 'test-theme))
    (should (equal notified (list following-entry)))))

(ert-deftest yunge-reader-cleans-missing-persistent-document-state ()
  (let* ((existing (make-temp-file "yunge-reader-state-"))
         (missing (concat existing ".missing"))
         (yunge-reader-saved-document-state
          (append
           (yunge-reader-test--document-state
            existing 'test
            :place 'existing-place
            :appearance 'follow-emacs
            :marks '((97 . existing-mark)))
           (yunge-reader-test--document-state
            missing 'test
            :place 'missing-place
            :appearance 'original
            :marks '((98 . missing-mark))))))
    (unwind-protect
        (let ((inhibit-message t))
          (yunge-reader-cleanup-missing-document-state)
          (should (= (length yunge-reader-saved-document-state) 1))
          (should
           (equal (yunge-reader-test--saved-place existing 'test)
                  'existing-place))
          (should
           (eq (yunge-reader-test--saved-state-value
                existing 'test :appearance)
               'follow-emacs))
          (should
           (equal (yunge-reader-test--saved-marks existing 'test)
                  '((97 . existing-mark)))))
      (when (file-exists-p existing)
        (delete-file existing)))))

(ert-deftest yunge-reader-appearance-commands-synchronize-document-views ()
  (let* ((file (expand-file-name "appearance-views.epub"))
         (driver
          (yunge-reader--make-driver
           :name 'epub :close-function #'ignore))
         (key (list 'epub (yunge-reader--place-file-key file)))
         (document
          (make-yunge-reader-document
           :key key :file file :driver driver))
         (entry
          (yunge-reader--make-document-entry
           :key key :file file :driver driver :state 'ready
           :document document))
         (yunge-reader--document-registry
          (make-hash-table :test #'equal))
         (yunge-reader-default-appearances '((epub . original)))
         (yunge-reader-saved-document-state nil)
         first
         second
         events
         messages)
    (unwind-protect
        (progn
          (setq first (yunge-reader-test--buffer
                       " *reader-appearance-one*")
                second (yunge-reader-test--buffer
                        " *reader-appearance-two*"))
          (setf (yunge-reader--document-entry-views entry)
                (list first second)
                (yunge-reader--document-entry-primary-view entry) first
                (yunge-reader--document-entry-active-view entry) first)
          (puthash key entry yunge-reader--document-registry)
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (setq yunge-reader-document document
                    yunge-reader--document-entry entry)
              (add-hook
               'yunge-reader-appearance-change-hook
               (lambda ()
                 (push
                  (cons (current-buffer)
                        (yunge-reader-effective-appearance))
                  events))
               nil t)))
          (with-current-buffer first
            (yunge-reader-set-document-appearance 'original))
          (should-not events)
          (cl-letf
              (((symbol-function 'customize-save-variable)
                (lambda (symbol value) (set symbol value)))
               ((symbol-function 'message)
                (lambda (format-string &rest arguments)
                  (push (apply #'format format-string arguments)
                        messages))))
            (with-current-buffer first
              (yunge-reader-set-default-appearance
               'epub 'follow-emacs)))
          (should-not events)
          (should
           (string-match-p
            "this book remains Original"
            (car messages)))
          (with-current-buffer second
            (yunge-reader-unset-document-appearance))
          (should
           (equal
            (sort events
                  (lambda (left right)
                    (string< (buffer-name (car left))
                             (buffer-name (car right)))))
            (list (cons first 'follow-emacs)
                  (cons second 'follow-emacs))))
          (should-not
           (yunge-reader-document-appearance-override document)))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-orders-document-and-view-lifecycles ()
  (let ((yunge-reader-drivers nil)
        events
        attached-document
        buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-lifecycle*"))
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (push 'open events)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close
                  (lambda (document)
                    (should (eq document attached-document))
                    (push 'close events))
                  :attach
                  (lambda (document initial-place)
                    (should
                     (equal initial-place
                            (yunge-reader-test--place 'test 7)))
                    (setq attached-document document)
                    (push 'attach events)
                    (add-hook
                     'yunge-reader-refresh-hook
                     (lambda () (push 'refresh events)) nil t))
                  :detach
                  (lambda (document)
                    (should (eq document attached-document))
                    (push 'detach events))
                  :location
                  (lambda (_document _window)
                    (push 'location events)
                    (make-yunge-reader-position :unit 7))
                  :restore
                  (lambda (_document _position _window)
                    (push 'restore events)
                    t))))
            (yunge-reader--begin-open
             buffer driver "lifecycle.pdf"
             (yunge-reader-test--place 'test 7)))
          (should yunge-reader--view-attached)
          (should (eq yunge-reader-document attached-document))
          (yunge-reader--close-document)
          (should-not yunge-reader--view-attached)
          (should-not yunge-reader-document)
          (should
           (equal
            (nreverse events)
            '(open attach refresh restore location location
                   detach close))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-shares-one-resource-between-views ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        (file (expand-file-name "shared.pdf"))
        (opens 0)
        open-complete
        attached
        detached
        closed
        completions
        first
        second)
    (unwind-protect
        (let ((driver
               (yunge-reader-register-driver
                'test
                :match (lambda (_file) t)
                :open
                (lambda (_file complete)
                  (cl-incf opens)
                  (setq open-complete complete))
                :close (lambda (document) (push document closed))
                :attach
                (lambda (_document _initial-place)
                  (push (current-buffer) attached))
                :detach
                (lambda (_document) (push (current-buffer) detached)))))
          (setq first
                (yunge-reader-test--buffer " *reader-shared-one*")
                second
                (yunge-reader-test--buffer " *reader-shared-two*"))
          (yunge-reader--begin-open
           first driver file nil
           (lambda (success)
             (push (list first success) completions)))
          (yunge-reader--begin-open
           second driver file nil
           (lambda (success)
             (push (list second success) completions)))
          (should (= opens 1))
          (funcall open-complete 'handle '(:layout fixed) nil)
          (should
           (equal (reverse attached) (list first second)))
          (should (= (length completions) 2))
          (should (cl-every #'cadr completions))
          (let ((first-document
                 (buffer-local-value 'yunge-reader-document first))
                (second-document
                 (buffer-local-value 'yunge-reader-document second)))
            (should (eq first-document second-document))
            (kill-buffer second)
            (should-not closed)
            (should (equal detached (list second)))
            (kill-buffer first)
            (should (equal detached (list first second)))
            (should (= (length closed) 1))
            (should (eq (car closed) first-document))))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-new-view-clones-the-live-place ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-saved-document-state nil)
        (yunge-reader-drivers nil)
        (file (expand-file-name "new-view.pdf"))
        (positions (make-hash-table :test #'eq))
        (opens 0)
        attached
        first
        second
        primary-window
        additional-window)
    (unwind-protect
        (save-window-excursion
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (cl-incf opens)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore
                  :attach
                  (lambda (_document _initial-place)
                    (push (current-buffer) attached))
                  :detach #'ignore
                  :location
                  (lambda (_document window)
                    (should (eq (window-buffer window)
                                (current-buffer)))
                    (when-let* ((unit
                                 (gethash (current-buffer) positions)))
                      (make-yunge-reader-position :unit unit)))
                  :restore
                  (lambda (_document position window)
                    (should (eq (window-buffer window)
                                (current-buffer)))
                    (puthash
                     (current-buffer)
                     (yunge-reader-position-unit position)
                     positions)
                    t))))
            (setq first
                  (yunge-reader-test--buffer
                   " *reader-new-view-primary*"))
            (puthash first 3 positions)
            (switch-to-buffer first)
            (yunge-reader--begin-open first driver file)
            (setq primary-window (selected-window)
                  additional-window (split-window-right))
            (set-window-buffer additional-window first)
            (select-window additional-window)
            (let ((document yunge-reader-document))
              (setq yunge-reader-zoom-mode 'manual
                    yunge-reader-scale 1.75
                    yunge-reader-selection 'source-selection
                    yunge-reader-search-query "source search")
              (puthash first 23 positions)
              (setq second (yunge-reader-new-view))
              (should (eq (current-buffer) second))
              (should (eq (selected-window) additional-window))
              (should (eq (window-buffer primary-window) first))
              (should (eq (window-buffer additional-window) second))
              (should (= opens 1))
              (should (equal (reverse attached) (list first second)))
              (with-current-buffer first
                (should (eq (yunge-reader-view-role) 'primary)))
              (with-current-buffer second
                (should (eq yunge-reader-document document))
                (should (eq (yunge-reader-view-role) 'additional))
                (should (eq yunge-reader-zoom-mode 'manual))
                (should (= yunge-reader-scale 1.75))
                (should (= (gethash second positions) 23))
                (should-not yunge-reader-selection)
                (should-not yunge-reader-search-query))
              (let ((entry
                     (buffer-local-value
                      'yunge-reader--document-entry first)))
                (should
                 (eq (yunge-reader--document-entry-primary-view entry)
                     first))
                (should
                 (equal (yunge-reader--document-entry-views entry)
                        (list first second))))
              (should
               (= (plist-get
                   (plist-get
                    (yunge-reader-test--saved-place file 'test)
                    :position)
                   :unit)
                  3)))))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-new-view-requires-a-stable-place ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-saved-document-state nil)
        (yunge-reader-drivers nil)
        (file (expand-file-name "new-view-unstable.pdf"))
        (unit 8)
        first)
    (unwind-protect
        (save-window-excursion
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore
                  :location
                  (lambda (_document _window)
                    (when unit
                      (make-yunge-reader-position :unit unit)))
                  :restore (lambda (&rest _arguments) t))))
            (setq first
                  (yunge-reader-test--buffer
                   " *reader-new-view-unstable*"))
            (switch-to-buffer first)
            (yunge-reader--begin-open first driver file)
            (let ((entry yunge-reader--document-entry)
                  (saved (copy-tree yunge-reader-saved-document-state t)))
              (setq unit nil)
              (should-error (yunge-reader-new-view) :type 'user-error)
              (should
               (equal (yunge-reader--document-entry-views entry)
                      (list first)))
              (should (equal yunge-reader-saved-document-state saved)))))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-new-view-restores-a-synchronous-failure ()
  (let* ((source
          (yunge-reader-test--buffer " *reader-new-view-failure*"))
         (file (expand-file-name "new-view-failure.pdf"))
         (document
          (make-yunge-reader-document :file file :driver 'test-driver))
         (entry
          (yunge-reader--make-document-entry :document document))
         (make-buffer (symbol-function 'generate-new-buffer))
         created)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (cl-letf
              (((symbol-function 'yunge-reader--ready-view-entry)
                (lambda () entry))
               ((symbol-function 'yunge-reader--stable-place)
                (lambda () '(:position (:unit 4))))
               ((symbol-function 'generate-new-buffer)
                (lambda (name)
                  (setq created (funcall make-buffer name))))
               ((symbol-function 'yunge-reader--begin-open)
                (lambda (&rest _arguments) (error "attach failed"))))
            (should-error (yunge-reader-new-view) :type 'error)
            (should (eq (current-buffer) source))
            (should (eq (window-buffer) source))
            (should-not (buffer-live-p created))))
      (when (buffer-live-p created)
        (kill-buffer created))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest yunge-reader-make-primary-saves-the-current-view ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-saved-document-state nil)
        (yunge-reader-drivers nil)
        (file (expand-file-name "make-primary.pdf"))
        (positions (make-hash-table :test #'eq))
        (opens 0)
        role-changes
        first
        second)
    (unwind-protect
        (save-window-excursion
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (cl-incf opens)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore
                  :location
                  (lambda (_document _window)
                    (when-let* ((unit
                                 (gethash (current-buffer) positions)))
                      (make-yunge-reader-position :unit unit)))
                  :restore
                  (lambda (_document position _window)
                    (puthash
                     (current-buffer)
                     (yunge-reader-position-unit position)
                     positions)
                    t))))
            (setq first
                  (yunge-reader-test--buffer
                   " *reader-make-primary-one*"))
            (puthash first 4 positions)
            (switch-to-buffer first)
            (yunge-reader--begin-open first driver file)
            (setq second (yunge-reader-new-view))
            (puthash second 29 positions)
            (setq yunge-reader-zoom-mode 'manual
                  yunge-reader-scale 2.0)
            (dolist (buffer (list first second))
              (with-current-buffer buffer
                (add-hook
                 'yunge-reader-view-role-change-hook
                 (lambda ()
                   (push
                    (cons (current-buffer)
                          (yunge-reader-view-role))
                    role-changes))
                 nil t)))
            (should (eq (yunge-reader-make-primary) second))
            (let* ((entry yunge-reader--document-entry)
                   (place
                    (yunge-reader-test--saved-place file 'test)))
              (should
               (eq (yunge-reader--document-entry-primary-view entry)
                   second))
              (should
               (eq (yunge-reader--document-entry-active-view entry)
                   second))
              (should
               (= (plist-get (plist-get place :position) :unit) 29))
              (should (eq (plist-get place :zoom-mode) 'manual))
              (should (= (plist-get place :scale) 2.0))
              (should (eq (yunge-reader-open file) second))
              (should (= opens 1))
              (should
               (equal
                (nreverse role-changes)
                (list (cons first 'additional)
                      (cons second 'primary)))))))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-make-primary-rejects-an-unstable-view ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-saved-document-state nil)
        (yunge-reader-drivers nil)
        (file (expand-file-name "make-primary-unstable.pdf"))
        (positions (make-hash-table :test #'eq))
        first
        second)
    (unwind-protect
        (save-window-excursion
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore
                  :location
                  (lambda (_document _window)
                    (when-let* ((unit
                                 (gethash (current-buffer) positions)))
                      (make-yunge-reader-position :unit unit)))
                  :restore
                  (lambda (_document position _window)
                    (puthash
                     (current-buffer)
                     (yunge-reader-position-unit position)
                     positions)
                    t))))
            (setq first
                  (yunge-reader-test--buffer
                   " *reader-make-primary-stable*"))
            (puthash first 6 positions)
            (switch-to-buffer first)
            (yunge-reader--begin-open first driver file)
            (setq second (yunge-reader-new-view))
            (let ((entry yunge-reader--document-entry)
                  (saved (copy-tree yunge-reader-saved-document-state t)))
              (remhash second positions)
              (should-error
               (yunge-reader-make-primary) :type 'user-error)
              (should
               (eq (yunge-reader--document-entry-primary-view entry)
                   first))
              (should (equal yunge-reader-saved-document-state saved)))))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-primary-view-alone-records-persistent-place ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-saved-document-state nil)
        (yunge-reader-drivers nil)
        (file (expand-file-name "primary.pdf"))
        (positions (make-hash-table :test #'eq))
        promoted-roles
        warnings
        first
        second)
    (unwind-protect
        (save-window-excursion
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close #'ignore
                  :location
                  (lambda (_document _window)
                    (make-yunge-reader-position
                     :unit (gethash (current-buffer) positions)))
                  :restore (lambda (&rest _arguments) t))))
            (setq first
                  (yunge-reader-test--buffer " *reader-primary*")
                  second
                  (yunge-reader-test--buffer " *reader-additional*"))
            (puthash first 10 positions)
            (puthash second 20 positions)
            (switch-to-buffer first)
            (yunge-reader--begin-open first driver file)
            (yunge-reader--begin-open second driver file)
            (with-current-buffer second
              (add-hook
               'yunge-reader-view-role-change-hook
               (lambda ()
                 (push (yunge-reader-view-role) promoted-roles))
               nil t))
            (with-current-buffer second
              (add-hook
               'yunge-reader-view-role-change-hook
               (lambda () (error "header update failed"))
               t t))
            (let* ((entry
                    (buffer-local-value
                     'yunge-reader--document-entry first)))
              (with-current-buffer second
                (yunge-reader-record-place))
              (should
               (= (plist-get
                   (plist-get
                    (yunge-reader-test--saved-place file 'test)
                    :position)
                   :unit)
                  10))
              (switch-to-buffer second)
              (with-current-buffer second
                (yunge-reader--note-view-activity))
              (cl-letf (((symbol-function 'display-warning)
                         (lambda (&rest arguments)
                           (push arguments warnings))))
                (kill-buffer first))
              (setq first nil)
              (should
               (eq (yunge-reader--document-entry-primary-view entry)
                   second))
              (should (equal promoted-roles '(primary)))
              (should (= (length warnings) 1))
              (should (eq (yunge-reader--existing-buffer file) second))
              (should
               (= (plist-get
                   (plist-get
                    (yunge-reader-test--saved-place file 'test)
                    :position)
                   :unit)
                  10))
              (with-current-buffer second
                (yunge-reader-record-place))
              (should
               (= (plist-get
                   (plist-get
                    (yunge-reader-test--saved-place file 'test)
                    :position)
                   :unit)
                  20)))))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-keeps-resource-after-one-view-attach-fails ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        failing-buffer
        completion
        closed
        first
        second)
    (unwind-protect
        (let ((driver
               (yunge-reader-register-driver
                'test
                :match (lambda (_file) t)
                :open
                (lambda (_file complete)
                  (funcall complete 'handle '(:layout fixed) nil))
                :close (lambda (document) (push document closed))
                :attach
                (lambda (_document _initial-place)
                  (when (eq (current-buffer) failing-buffer)
                    (error "attach failed")))
                :detach #'ignore)))
          (setq first
                (yunge-reader-test--buffer " *reader-attach-ok*")
                second
                (yunge-reader-test--buffer " *reader-attach-bad*")
                failing-buffer second)
          (yunge-reader--begin-open first driver "partial-shared.pdf")
          (yunge-reader--begin-open
           second driver "partial-shared.pdf" nil
           (lambda (success) (setq completion success)))
          (should-not completion)
          (should-not closed)
          (should-not
           (buffer-local-value 'yunge-reader-document second))
          (should
           (buffer-local-value 'yunge-reader-document first))
          (kill-buffer second)
          (setq second nil)
          (should-not closed)
          (kill-buffer first)
          (setq first nil)
          (should (= (length closed) 1)))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-opening-survives-losing-one-pending-view ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        open-complete
        first-completion
        second-completion
        first
        second)
    (unwind-protect
        (let ((driver
               (yunge-reader-register-driver
                'test
                :match (lambda (_file) t)
                :open
                (lambda (_file complete)
                  (setq open-complete complete))
                :close #'ignore)))
          (setq first
                (yunge-reader-test--buffer " *reader-pending-one*")
                second
                (yunge-reader-test--buffer " *reader-pending-two*"))
          (yunge-reader--begin-open
           first driver "pending-shared.pdf" nil
           (lambda (success) (setq first-completion success)))
          (yunge-reader--begin-open
           second driver "pending-shared.pdf" nil
           (lambda (success) (setq second-completion success)))
          (kill-buffer first)
          (setq first nil)
          (should-not first-completion)
          (should (= (hash-table-count
                      yunge-reader--document-registry)
                     1))
          (funcall open-complete 'handle '(:layout fixed) nil)
          (should second-completion)
          (should
           (buffer-local-value 'yunge-reader-document second)))
      (when (buffer-live-p second)
        (kill-buffer second))
      (when (buffer-live-p first)
        (kill-buffer first)))))

(ert-deftest yunge-reader-cleans-up-a-failed-view-attach ()
  (let* ((file (expand-file-name "attach-failure.pdf"))
         (old (yunge-reader-test--place 'test 19))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
         (yunge-reader-drivers nil)
         (completion 'pending)
         events
         document
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer (generate-new-buffer " *reader-attach-failure*"))
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close
                  (lambda (value)
                    (should (eq value document))
                    (push 'close events))
                  :attach
                  (lambda (value _initial-place)
                    (setq document value)
                    (push 'attach events)
                    (error "attach failed"))
                  :detach
                  (lambda (value)
                    (should (eq value document))
                    (push 'detach events))
                  :location
                  (lambda (&rest _arguments)
                    (push 'location events)
                    (make-yunge-reader-position :unit 0))
                  :restore (lambda (&rest _arguments) t))))
            (yunge-reader--begin-open
             buffer driver file nil
             (lambda (success) (setq completion success))))
          (should-not completion)
          (should-not yunge-reader-document)
          (should-not yunge-reader--view-attached)
          (should-not yunge-reader--place-recording-enabled)
          (should (equal (nreverse events)
                         '(attach detach close)))
          (should (equal (yunge-reader-test--saved-place file 'test)
                         old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-cleans-up-a-failed-view-restore ()
  (let* ((file (expand-file-name "restore-failure.pdf"))
         (old (yunge-reader-test--place 'test 23))
         (yunge-reader-saved-document-state
          (yunge-reader-test--document-state file 'test :place old))
         (yunge-reader-drivers nil)
         (completion 'pending)
         events
         buffer)
    (unwind-protect
        (save-window-excursion
          (setq buffer
                (generate-new-buffer " *reader-restore-failure*"))
          (switch-to-buffer buffer)
          (yunge-reader-mode)
          (let ((driver
                 (yunge-reader-register-driver
                  'test
                  :match (lambda (_file) t)
                  :open
                  (lambda (_file complete)
                    (funcall complete 'handle '(:layout fixed) nil))
                  :close (lambda (_document) (push 'close events))
                  :attach
                  (lambda (_document _initial-place)
                    (push 'attach events))
                  :detach
                  (lambda (_document) (push 'detach events))
                  :location
                  (lambda (&rest _arguments)
                    (push 'location events)
                    (make-yunge-reader-position :unit 0))
                  :restore
                  (lambda (&rest _arguments)
                    (push 'restore events)
                    (error "restore failed")))))
            (yunge-reader--begin-open
             buffer driver file nil
             (lambda (success) (setq completion success))))
          (should-not completion)
          (should-not yunge-reader-document)
          (should-not yunge-reader--view-attached)
          (should-not yunge-reader--place-recording-enabled)
          (should
           (equal (nreverse events)
                  '(attach restore detach close)))
          (should (equal (yunge-reader-test--saved-place file 'test)
                         old)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest yunge-reader-closes-resource-returned-with-open-error ()
  (let ((yunge-reader-drivers nil)
        (completion 'pending)
        attached
        closed)
    (with-temp-buffer
      (yunge-reader-mode)
      (let ((driver
             (yunge-reader-register-driver
              'test
              :match (lambda (_file) t)
              :open
              (lambda (_file complete)
                (funcall
                 complete 'handle
                 '(:layout fixed :metadata (:pages 3))
                 '(error "open failed")))
              :close (lambda (document) (setq closed document))
              :attach (lambda (_document _initial-place)
                        (setq attached t))
              :detach #'ignore)))
        (yunge-reader--begin-open
         (current-buffer) driver "open-error.pdf" nil
         (lambda (success) (setq completion success))))
      (should-not completion)
      (should-not attached)
      (should-not yunge-reader-document)
      (should (yunge-reader-document-p closed))
      (should (eq (yunge-reader-document-handle closed) 'handle))
      (should (eq (yunge-reader-document-layout closed) 'fixed))
      (should
       (equal (yunge-reader-document-metadata closed) '(:pages 3))))))

(ert-deftest yunge-reader-closes-resource-after-view-detach-error ()
  (let ((yunge-reader-drivers nil)
        events
        warnings)
    (with-temp-buffer
      (yunge-reader-mode)
      (let* ((driver
              (yunge-reader-register-driver
               'test
               :match #'ignore
               :open #'ignore
               :close (lambda (_document) (push 'close events))
               :attach (lambda (_document _initial-place)
                         (push 'attach events))
               :detach
               (lambda (_document)
                 (push 'detach events)
                 (error "detach failed"))))
             (document
              (make-yunge-reader-document
               :file "detach-error.pdf"
               :driver driver
               :handle 'handle
               :layout 'fixed)))
        (setq yunge-reader-document document)
        (yunge-reader--attach-view document nil)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest arguments)
                     (push arguments warnings))))
          (yunge-reader--close-document)))
      (should (equal (nreverse events) '(attach detach close)))
      (should (= (length warnings) 1))
      (should-not yunge-reader-document)
      (should-not yunge-reader--view-attached))))

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
           :close (lambda (document) (push document closed)))
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
                  :close (lambda (document) (push document closed)))))
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

(ert-deftest yunge-reader-cancels-an-unobserved-resource-open ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        callback
        cancelled
        closed
        buffer)
    (unwind-protect
        (let* ((child
                (yunge-reader-task--make
                 :operation "open"
                 :state 'sent
                 :cancel-function
                 (lambda (task reason)
                   (setq cancelled reason)
                   (setf (yunge-reader-task-state task) 'cancelled)
                   t)))
               (driver
                (yunge-reader-register-driver
                 'test
                 :match (lambda (_file) t)
                 :open
                 (lambda (_file complete)
                   (setq callback complete)
                   child)
                 :close (lambda (document) (push document closed)))))
          (setq buffer (yunge-reader-test--buffer " *reader-cancel-open*"))
          (yunge-reader--begin-open buffer driver "cancel-open.pdf")
          (kill-buffer buffer)
          (setq buffer nil)
          (should (stringp cancelled))
          (should
           (zerop
            (hash-table-count yunge-reader--document-registry)))
          ;; A non-cooperative driver completion is still disposed safely.
          (funcall callback 'late-handle '(:layout fixed) nil)
          (should (= (length closed) 1))
          (should
           (eq (yunge-reader-document-handle (car closed))
               'late-handle)))
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
           :close (lambda (document) (push document closed)))
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

(ert-deftest yunge-reader-rejects-fit-modes-for-reflowable-documents ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let ((yunge-reader-document
           (make-yunge-reader-document :layout 'reflow))
          (refreshes 0))
      (add-hook 'yunge-reader-refresh-hook
                (lambda () (cl-incf refreshes)) nil t)
      (should-error (yunge-reader-fit-width) :type 'user-error)
      (should-error (yunge-reader-fit-page) :type 'user-error)
      (should (eq yunge-reader-zoom-mode 'fit-width))
      (should-not yunge-reader-effective-scale)
      (should (zerop refreshes)))))

(ert-deftest yunge-reader-copies-cached-and-driver-resolved-selections ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (yunge-reader-copy-unit-limit 2)
           (yunge-reader-copy-character-limit 3)
           (kill-ring nil)
           (start (make-yunge-reader-position :unit 1 :offset 2))
           (end (make-yunge-reader-position :unit 1 :offset 5))
           (cursor (make-yunge-reader-position :unit 1 :offset 5))
           (requests 0)
           arguments-seen
           completion
           (driver
            (yunge-reader-register-driver
             'selection-test
             :match #'ignore
             :open #'ignore
             :close #'ignore
             :selection-text
             (lambda (_document arguments complete)
               (cl-incf requests)
               (push arguments arguments-seen)
               (should
                (eq (yunge-reader-selection-text-request-start arguments)
                    start))
               (should
                (eq (yunge-reader-selection-text-request-end arguments)
                    end))
               (should
                (= (yunge-reader-selection-text-request-unit-limit arguments)
                   2))
               (should
                (= (yunge-reader-selection-text-request-character-limit
                    arguments)
                   3))
               (if (yunge-reader-selection-text-request-cursor arguments)
                   (funcall
                    complete
                    (make-yunge-reader-selection-batch
                     :text "olved" :done t)
                    nil)
                 (setq completion complete))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "selection.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-set-selection start end "cached")
      (let ((inhibit-message t))
        (should (equal (yunge-reader-copy-selection) "cached")))
      (should (equal (car kill-ring) "cached"))
      (yunge-reader-set-selection start end)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest arguments)
                   (apply function arguments))))
        (let ((inhibit-message t))
          (yunge-reader-copy-selection)
          (yunge-reader-copy-selection))
        (should yunge-reader--copy-pending)
        (should (= requests 1))
        (with-temp-buffer
          (let ((inhibit-message t))
            (funcall
             completion
             (make-yunge-reader-selection-batch
              :text "res" :cursor cursor :done nil)
             nil))))
      (should (= requests 2))
      (should-not yunge-reader--copy-pending)
      (should
       (eq (yunge-reader-selection-text-request-cursor
            (car arguments-seen))
           cursor))
      (should (equal (car kill-ring) "resolved"))
      (should
       (equal (yunge-reader-selection-text yunge-reader-selection)
              "resolved")))))

(ert-deftest yunge-reader-discards-a-late-copy-after-selection-changes ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (kill-ring nil)
           (old-start
            (make-yunge-reader-position :unit 0 :offset 1))
           (old-end
            (make-yunge-reader-position :unit 0 :offset 2))
           (new-start
            (make-yunge-reader-position :unit 1 :offset 3))
           (new-end
            (make-yunge-reader-position :unit 1 :offset 4))
           completion
           (driver
            (yunge-reader-register-driver
             'selection-test
             :match #'ignore
             :open #'ignore
             :close #'ignore
             :selection-text
             (lambda (_document _arguments complete)
               (setq completion complete)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "selection.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-set-selection old-start old-end)
      (let ((inhibit-message t))
        (yunge-reader-copy-selection))
      (should yunge-reader--copy-pending)
      (yunge-reader-set-selection new-start new-end)
      (funcall
       completion
       (make-yunge-reader-selection-batch :text "obsolete" :done t)
       nil)
      (should-not kill-ring)
      (should-not yunge-reader--copy-pending)
      (should-not
       (yunge-reader-selection-text yunge-reader-selection)))))

(ert-deftest yunge-reader-stops-a-copy-when-its-cursor-does-not-advance ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (start (make-yunge-reader-position :unit 0 :offset 0))
           (end (make-yunge-reader-position :unit 0 :offset 4))
           (cursor (make-yunge-reader-position :unit 0 :offset 2))
           (requests 0)
           warnings
           (driver
            (yunge-reader-register-driver
             'selection-test
             :match #'ignore
             :open #'ignore
             :close #'ignore
             :selection-text
             (lambda (_document _arguments complete)
               (cl-incf requests)
               (funcall
                complete
                (make-yunge-reader-selection-batch
                 :text "part"
                 :cursor cursor
                 :done nil)
                nil)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "selection.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-set-selection start end)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest arguments)
                   (apply function arguments)))
                ((symbol-function 'display-warning)
                 (lambda (&rest arguments)
                   (push arguments warnings))))
        (let ((inhibit-message t))
          (yunge-reader-copy-selection)))
      (should (= requests 2))
      (should (= (length warnings) 1))
      (should-not yunge-reader--copy-pending)
      (should-not
       (yunge-reader-selection-text yunge-reader-selection)))))

(ert-deftest yunge-reader-discards-partial-text-after-a-copy-error ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (kill-ring nil)
           (start (make-yunge-reader-position :unit 0 :offset 0))
           (end (make-yunge-reader-position :unit 1 :offset 4))
           (cursor (make-yunge-reader-position :unit 1 :offset 0))
           (requests 0)
           warnings
           (driver
            (yunge-reader-register-driver
             'selection-test
             :match #'ignore
             :open #'ignore
             :close #'ignore
             :selection-text
             (lambda (_document arguments complete)
               (cl-incf requests)
               (if (yunge-reader-selection-text-request-cursor arguments)
                   (funcall complete nil '(error "copy stopped"))
                 (funcall
                  complete
                  (make-yunge-reader-selection-batch
                   :text "partial" :cursor cursor :done nil)
                  nil))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "selection.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-set-selection start end)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest arguments)
                   (apply function arguments)))
                ((symbol-function 'display-warning)
                 (lambda (&rest arguments)
                   (push arguments warnings))))
        (let ((inhibit-message t))
          (yunge-reader-copy-selection)))
      (should (= requests 2))
      (should (= (length warnings) 1))
      (should-not kill-ring)
      (should-not yunge-reader--copy-pending)
      (should-not
       (yunge-reader-selection-text yunge-reader-selection)))))

(ert-deftest yunge-reader-request-completes-only-once ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (calls 0)
           (driver
            (yunge-reader-register-driver
             'request-test
             :match #'ignore :open #'ignore :close #'ignore
             :outline
             (lambda (_document _arguments complete)
               (funcall complete 'first nil)
               (funcall complete 'second nil)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "request.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (yunge-reader-request
       'outline nil
       (lambda (value error-data)
         (should-not error-data)
         (should (eq value 'first))
         (cl-incf calls)))
      (should (= calls 1)))))

(ert-deftest yunge-reader-request-cancels-its-driver-child ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           cancelled
           driver-complete
           completion
           (child
            (yunge-reader-task--make
             :operation "native-work"
             :state 'sent
             :cancel-function
             (lambda (task reason)
               (setq cancelled reason)
               (setf (yunge-reader-task-state task) 'cancelled)
               t)))
           (driver
            (yunge-reader-register-driver
             'request-cancel-test
             :match #'ignore :open #'ignore :close #'ignore
             :outline
             (lambda (_document _arguments complete)
               (setq driver-complete complete)
               child))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "request.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((task
             (yunge-reader-request
              'outline nil
              (lambda (value error-data)
                (setq completion (list value error-data))))))
        (should (eq (yunge-reader-task-child task) child))
        (should (yunge-reader-task-cancel task "replaced"))
        (should (equal cancelled "replaced"))
        (should (eq (yunge-reader-task-state task) 'cancelled))
        (should
         (eq (car (cadr completion)) 'yunge-reader-task-cancelled))
        (funcall driver-complete 'late nil)
        (should-not (eq (car completion) 'late))))))

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
            (yunge-reader-search-highlight-visible t)
            events)
        (add-hook
         'yunge-reader-search-result-hook
         (lambda () (push 'visit events)) nil t)
        (cl-letf (((symbol-function 'yunge-jump-history-record)
                   (lambda (window)
                     (should (eq window (selected-window)))
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
             :search
             (lambda (_document arguments complete)
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
        (should-not
         (yunge-reader-search-request-case-sensitive (car requests)))
        (should
         (= (yunge-reader-search-request-unit-limit (car requests))
            yunge-reader-search-page-limit))
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results nil
          :cursor (make-yunge-reader-search-cursor :value 'batch-2))
         nil)
        (should (= (length requests) 2))
        (should
         (equal (yunge-reader-search-request-cursor (cadr requests))
                 (make-yunge-reader-search-cursor :value 'batch-2)))
        (let ((inhibit-message t))
          (funcall
           (pop completions)
           (make-yunge-reader-search-batch
            :results (list first second)
            :cursor (make-yunge-reader-search-cursor :value 'batch-3))
           nil))
        (should (eq yunge-reader-search-result first))
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
        (should (= (length requests) 3))
        (should (eq yunge-reader-search-result first))
        (should (equal yunge-reader-search-results (list first second)))
        (should (= hook-calls 4))))))

(ert-deftest yunge-reader-search-completes-results-returned-from-a-boundary ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completion
           messages
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 1 :offset 2)
             :end (make-yunge-reader-position :unit 1 :offset 7)
             :text "match"))
           (second
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 3 :offset 4)
             :end (make-yunge-reader-position :unit 3 :offset 9)
             :text "match"))
           (driver
            (yunge-reader-register-driver
             'boundary-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document arguments complete)
               (push arguments requests)
               (setq completion complete)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (cl-letf (((symbol-function 'yunge-reader--current-position) #'ignore)
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format-message format-string arguments)
                         messages))))
        (yunge-reader-search "match")
        (funcall
         completion
         (make-yunge-reader-search-batch
          :results (list first second) :done t)
         nil)
        (should (eq yunge-reader-search-result first))
        (should (equal (car messages) "Match 1/2: match"))
        (yunge-reader-search-next)
        (should (eq yunge-reader-search-result second))
        (should (equal (car messages) "Match 2/2: match"))
        (yunge-reader-search-previous)
        (should (eq yunge-reader-search-result first))
        (should (equal (car messages) "Match 1/2: match"))
        (should (= (length requests) 1))))))

(ert-deftest yunge-reader-search-completes-an-empty-wrapped-cycle ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completions
           messages
           (origin (make-yunge-reader-position :unit 4 :offset 10))
           (driver
            (yunge-reader-register-driver
             'empty-cycle-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document arguments complete)
               (setq requests (append requests (list arguments))
                     completions (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (cl-letf (((symbol-function 'yunge-reader--current-position)
                 (lambda (&optional _window) origin))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format-message format-string arguments)
                         messages))))
        (yunge-reader-search "absent")
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch :results nil :done t)
         nil)
        (should (= (length requests) 2))
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch :results nil :done t)
         nil)
        (should-not yunge-reader-search-results)
        (should-not yunge-reader-search-result)
        (should (equal (car messages) "No matches for: absent"))
        (yunge-reader-search-next)
        (yunge-reader-search-previous)
        (should (= (length requests) 2))
        (should (equal (car messages) "No matches for: absent"))))))

(ert-deftest yunge-reader-search-retains-results-and-closes-a-wrapped-cycle ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completions
           messages
           (origin (make-yunge-reader-position :unit 4 :offset 10))
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 1 :offset 2)
             :end (make-yunge-reader-position :unit 1 :offset 7)
             :text "match"))
           (middle
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 5 :offset 3)
             :end (make-yunge-reader-position :unit 5 :offset 8)
             :text "match"))
           (middle-again
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 5 :offset 3)
             :end (make-yunge-reader-position :unit 5 :offset 8)
             :text "match" :before "different context "))
           (last
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 9 :offset 4)
             :end (make-yunge-reader-position :unit 9 :offset 9)
             :text "match"))
           (driver
            (yunge-reader-register-driver
             'wrapped-cycle-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document arguments complete)
               (setq requests (append requests (list arguments))
                     completions (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (cl-letf (((symbol-function 'yunge-reader--current-position)
                 (lambda (&optional _window) origin))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format-message format-string arguments)
                         messages))))
        (yunge-reader-search "match")
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results (list middle last) :done t)
         nil)
        (should (eq yunge-reader-search-result middle))
        (should (equal (car messages) "Match 1+: match"))
        (yunge-reader-search-next)
        (should (eq yunge-reader-search-result last))
        (yunge-reader-search-next)
        (should (= (length requests) 2))
        (should (equal yunge-reader-search-results (list middle last)))
        (should (eq yunge-reader-search-result last))
        (should-not
         (yunge-reader-search-request-origin (cadr requests)))
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results (list first middle-again)
          :cursor (make-yunge-reader-search-cursor :value 'past-middle))
         nil)
        (should
         (equal yunge-reader-search-results (list middle last first)))
        (should (eq yunge-reader-search-result first))
        (should (equal (car messages) "Match 3/3: match"))
        (yunge-reader-search-next)
        (should (eq yunge-reader-search-result middle))
        (should (equal (car messages) "Match 1/3: match"))
        (yunge-reader-search-previous)
        (should (eq yunge-reader-search-result first))
        (should (equal (car messages) "Match 3/3: match"))
        (should (= (length requests) 2))))))

(ert-deftest yunge-reader-search-closes-and-reuses-a-backward-cycle ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completions
           messages
           (origin (make-yunge-reader-position :unit 6 :offset 10))
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 1 :offset 2)
             :end (make-yunge-reader-position :unit 1 :offset 7)
             :text "match"))
           (middle
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 5 :offset 3)
             :end (make-yunge-reader-position :unit 5 :offset 8)
             :text "match"))
           (middle-again
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 5 :offset 3)
             :end (make-yunge-reader-position :unit 5 :offset 8)
             :text "match" :after " changed context"))
           (last
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 9 :offset 4)
             :end (make-yunge-reader-position :unit 9 :offset 9)
             :text "match"))
           (driver
            (yunge-reader-register-driver
             'backward-cycle-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document arguments complete)
               (setq requests (append requests (list arguments))
                     completions (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed)
            yunge-reader-search-query "match")
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format-message format-string arguments)
                         messages))))
        (yunge-reader--start-search-run 'backward origin)
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results (list middle first) :done t)
         nil)
        (should (eq yunge-reader-search-result middle))
        (yunge-reader-search-previous)
        (should (eq yunge-reader-search-result first))
        (yunge-reader-search-previous)
        (should (= (length requests) 2))
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results (list last middle-again)
          :cursor (make-yunge-reader-search-cursor :value 'before-middle))
         nil)
        (should
         (equal yunge-reader-search-results (list middle first last)))
        (should (eq yunge-reader-search-result last))
        (should (equal (car messages) "Match 3/3: match"))
        (yunge-reader-search-next)
        (should (eq yunge-reader-search-result first))
        (should (equal (car messages) "Match 2/3: match"))
        (yunge-reader-search-previous)
        (should (eq yunge-reader-search-result last))
        (should (= (length requests) 2))))))

(ert-deftest yunge-reader-search-caches-an-only-match-as-a-complete-cycle ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           requests
           completions
           messages
           (origin (make-yunge-reader-position :unit 5 :offset 1000))
           (result
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 5 :offset 1248)
             :end (make-yunge-reader-position :unit 5 :offset 1254)
             :text "support" :before "received " :after " beyond"))
           (driver
            (yunge-reader-register-driver
             'only-match-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document arguments complete)
               (setq requests (append requests (list arguments))
                     completions (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (cl-letf (((symbol-function 'yunge-reader--current-position)
                 (lambda (&optional _window) origin))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format-message format-string arguments)
                         messages))))
        (yunge-reader-search "support")
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results (list result) :done t)
         nil)
        (yunge-reader-search-next)
        (should (= (length requests) 2))
        (funcall
         (pop completions)
         (make-yunge-reader-search-batch
          :results (list result)
          :cursor (make-yunge-reader-search-cursor :value 'after-wrap))
         nil)
        (should (equal yunge-reader-search-results (list result)))
        (should (eq yunge-reader-search-result result))
        (should (equal (car messages)
                       "Match 1/1: received support beyond"))
        (yunge-reader-search-next)
        (yunge-reader-search-previous)
        (should (= (length requests) 2))
        (should (eq yunge-reader-search-result result))
        (should (equal (car messages)
                       "Match 1/1: received support beyond"))))))

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
             :search
             (lambda (_document arguments complete)
               (push arguments requests)
               (push complete completions)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((inhibit-message t))
        (yunge-reader-search "old")
        (yunge-reader-search "New"))
      (should (= (length requests) 2))
      (should-not
       (yunge-reader-search-request-case-sensitive (cadr requests)))
      (should (yunge-reader-search-request-case-sensitive (car requests)))
      (let ((old-completion (cadr completions))
            (inhibit-message t))
        (funcall
         old-completion
         (make-yunge-reader-search-batch
          :results (list late) :done t)
         nil)
        (should-not yunge-reader-search-result)
        (should (= (length requests) 2))
        (funcall
         (car completions)
         (make-yunge-reader-search-batch
          :results (list current) :done t)
         nil))
      (should (eq yunge-reader-search-result current)))))

(ert-deftest yunge-reader-search-switches-from-the-current-result ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           completion
           arguments
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
             :search
             (lambda (_document actual complete)
               (setq arguments actual
                     completion complete)))))
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
           :cursor (make-yunge-reader-search-cursor :value 'forward-2))
         nil)
        (yunge-reader-search-previous)
        (should
         (eq (yunge-reader-search-request-direction arguments) 'backward))
        (should
         (eq (yunge-reader-search-request-origin arguments)
             (yunge-reader-search-result-start first)))
        (funcall
         completion
         (make-yunge-reader-search-batch
          :results (list last) :done t)
         nil))
      (should (eq yunge-reader-search-result last)))))

(ert-deftest yunge-reader-search-coalesces-navigation-while-loading ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           completions
           request-arguments
           (requests 0)
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 0 :offset 0)
             :end (make-yunge-reader-position :unit 0 :offset 1)))
           (last
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 0 :offset 2)
             :end (make-yunge-reader-position :unit 0 :offset 3)))
           (driver
            (yunge-reader-register-driver
             'coalesced-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document arguments complete)
               (cl-incf requests)
               (setq request-arguments
                     (append request-arguments (list arguments))
                     completions
                     (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((inhibit-message t))
        (yunge-reader-search "x")
        (yunge-reader-search-next)
        (yunge-reader-search-next)
        (yunge-reader-search-previous))
      (should (= requests 2))
      (should
       (eq (yunge-reader-search-request-direction
            (car request-arguments))
           'forward))
      (let ((inhibit-message t))
        (funcall
         (car completions)
         (make-yunge-reader-search-batch
          :results (list first) :done t)
         nil)
        (should-not yunge-reader-search-result)
        (should (= requests 2))
        (should
         (eq (yunge-reader-search-request-direction
              (cadr request-arguments))
             'backward))
        (funcall
         (cadr completions)
         (make-yunge-reader-search-batch
          :results (list last first) :done t)
         nil))
      (should (= requests 2))
      (should (eq yunge-reader-search-result last)))))

(ert-deftest yunge-reader-search-preserves-repeated-pending-moves ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           completions
           messages
           (first
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 0 :offset 0)
             :end (make-yunge-reader-position :unit 0 :offset 1)))
           (second
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 1 :offset 0)
             :end (make-yunge-reader-position :unit 1 :offset 1)))
           (third
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 2 :offset 0)
             :end (make-yunge-reader-position :unit 2 :offset 1)))
           (fourth
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 3 :offset 0)
             :end (make-yunge-reader-position :unit 3 :offset 1)))
           (driver
            (yunge-reader-register-driver
             'repeated-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document _arguments complete)
               (setq completions
                     (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((inhibit-message t))
        (yunge-reader-search "x")
        (funcall
         (car completions)
         (make-yunge-reader-search-batch
          :results (list first)
          :cursor (make-yunge-reader-search-cursor :value 'batch-2))
         nil))
      (should (eq yunge-reader-search-result first))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format-message
                                format-string arguments)
                         messages))))
        (yunge-reader-search-next)
        (yunge-reader-search-next)
        (yunge-reader-search-next))
      (should
       (equal
        (nreverse messages)
        '("Searching forward..."
          "Searching forward... (2 steps pending)"
          "Searching forward... (3 steps pending)")))
      (should (= (length completions) 2))
      (let ((inhibit-message t))
        (funcall
         (cadr completions)
         (make-yunge-reader-search-batch
          :results (list second third fourth) :done t)
         nil))
      (should (eq yunge-reader-search-result fourth)))))

(ert-deftest yunge-reader-quit-cancels-delayed-search-navigation ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           completion
           (requests 0)
           (result
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 1 :offset 0)
             :end (make-yunge-reader-position :unit 1 :offset 1)))
           (driver
            (yunge-reader-register-driver
             'cancelled-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document _arguments complete)
               (cl-incf requests)
               (setq completion complete)))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (let ((inhibit-message t))
        (yunge-reader-search "x")
        (yunge-reader-search-next)
        (yunge-reader-keyboard-quit))
      (should-not yunge-reader-search-highlight-visible)
      (let ((inhibit-message t))
        (funcall
         completion
         (make-yunge-reader-search-batch
          :results (list result)
          :cursor (make-yunge-reader-search-cursor :value 'batch-2))
         nil))
      (should (= requests 1))
      (should (equal yunge-reader-search-query "x"))
      (should (equal yunge-reader-search-results (list result)))
      (should-not yunge-reader-search-result)
      (let ((inhibit-message t))
        (yunge-reader-search-next))
      (should (eq yunge-reader-search-result result))
      (should (= requests 1)))))

(ert-deftest yunge-reader-search-reanchors-after-manual-movement ()
  (with-temp-buffer
    (yunge-reader-mode)
    (let* ((yunge-reader-drivers nil)
           (current (make-yunge-reader-position :unit 2 :offset 4))
           requests
           completions
           (old-result
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 3 :offset 1)
             :end (make-yunge-reader-position :unit 3 :offset 6)))
           (new-result
            (make-yunge-reader-search-result
             :start (make-yunge-reader-position :unit 9 :offset 8)
             :end (make-yunge-reader-position :unit 9 :offset 13)))
           (driver
            (yunge-reader-register-driver
             'reanchored-search-test
             :match #'ignore :open #'ignore :close #'ignore
             :search
             (lambda (_document arguments complete)
               (setq requests (append requests (list arguments))
                     completions (append completions (list complete)))))))
      (setq yunge-reader-document
            (make-yunge-reader-document
             :file "search.pdf" :driver driver :handle 'handle
             :layout 'fixed))
      (cl-letf (((symbol-function 'yunge-reader--current-position)
                 (lambda (&optional _window) current)))
        (let ((inhibit-message t))
          (yunge-reader-search "needle")
          (yunge-reader--detach-search-navigation)
          (setq current
                (make-yunge-reader-position :unit 9 :offset 7))
          (yunge-reader-search-next)))
      (should (= (length requests) 2))
      (should
       (equal (yunge-reader-search-request-origin (car requests))
              (make-yunge-reader-position :unit 2 :offset 4)))
      (let ((inhibit-message t))
        (funcall
         (car completions)
         (make-yunge-reader-search-batch
          :results (list old-result) :done t)
         nil)
        (should-not yunge-reader-search-result)
        (should (= (length requests) 2))
        (should
         (equal (yunge-reader-search-request-origin (cadr requests))
                (make-yunge-reader-position :unit 9 :offset 7)))
        (funcall
         (cadr completions)
         (make-yunge-reader-search-batch
          :results (list new-result) :done t)
         nil))
      (should (eq yunge-reader-search-result new-result)))))

;;; yunge-reader-test.el ends here
