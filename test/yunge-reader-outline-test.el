;;; yunge-reader-outline-test.el --- Outline tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-outline)

(defun yunge-reader-outline-test--item (title depth &optional unit)
  "Return an outline item named TITLE at DEPTH and optional UNIT."
  (make-yunge-reader-outline-item
   :title title
   :depth depth
   :action
   (and unit
        (make-yunge-reader-action
         :type 'location
         :position (make-yunge-reader-position :unit unit)))))

(ert-deftest yunge-reader-outline-binds-list-navigation ()
  (yunge-test-keymap-keys
   yunge-reader-outline-mode-map
   '(("G" . yunge-reader-outline-last-item)
     ("RET" . yunge-reader-outline-visit)
     ("gc" . yunge-reader-outline-current-item)
     ("gg" . yunge-reader-outline-first-item)
     ("gf" . yunge-reader-outline-show)
     ("j" . yunge-reader-outline-next-item)
     ("k" . yunge-reader-outline-previous-item)
     ("q" . quit-window)
     ("<tab>" . yunge-reader-outline-toggle)))
  (yunge-test-enable-evil)
  (with-temp-buffer
    (yunge-reader-outline-mode)
    (yunge-test-evil-keys
     'normal
     '(("G" . yunge-reader-outline-last-item)
       ("RET" . yunge-reader-outline-visit)
       ("gc" . yunge-reader-outline-current-item)
       ("gg" . yunge-reader-outline-first-item)
       ("gf" . yunge-reader-outline-show)
       ("j" . yunge-reader-outline-next-item)
       ("k" . yunge-reader-outline-previous-item)
       ("q" . quit-window)
       ("<tab>" . yunge-reader-outline-toggle)))))

(ert-deftest yunge-reader-outline-renders-and-folds-a-preorder-tree ()
  (with-temp-buffer
    (yunge-reader-outline-mode)
    (yunge-reader-outline-set-data
     (make-yunge-reader-outline-data
      :items
      (list
       (yunge-reader-outline-test--item "Part" 0)
       (yunge-reader-outline-test--item "Chapter A" 1 1)
       (yunge-reader-outline-test--item "Section" 2 2)
       (yunge-reader-outline-test--item "Chapter B" 1 3)
       (yunge-reader-outline-test--item "Appendix" 0 4))))
    (should
     (equal (buffer-string)
            (concat "- Part\n"
                    "  - Chapter A\n"
                    "      Section\n"
                    "    Chapter B\n"
                    "  Appendix\n")))
    (should (zerop (yunge-reader-outline--index-at-point)))
    (should (eq (yunge-reader-outline--item-at-point)
                (aref yunge-reader-outline--items 0)))
    (yunge-reader-outline-toggle)
    (should (equal (buffer-string) "+ Part\n  Appendix\n"))
    (should (zerop (yunge-reader-outline--index-at-point)))
    (yunge-reader-outline-toggle)
    (should (= (count-lines (point-min) (point-max)) 5))
    (yunge-reader-outline-last-item)
    (should (= (yunge-reader-outline--index-at-point) 4))
    (should-error
     (yunge-reader-outline-next-item)
     :type 'user-error)
    (should (= (yunge-reader-outline--index-at-point) 4))
    (yunge-reader-outline-previous-item)
    (should (= (yunge-reader-outline--index-at-point) 3))
    (yunge-reader-outline-first-item)
    (should (zerop (yunge-reader-outline--index-at-point)))))

(ert-deftest yunge-reader-outline-locates-and-reveals-the-current-item ()
  (let* ((reader (generate-new-buffer " *reader outline current*"))
         (driver
         (yunge-reader--make-driver
           :name 'test
           :close-function #'ignore
           :outline-index-function
           (lambda (_document window outline)
             (should (eq window 'reader-window))
             (should (yunge-reader-outline-data-p outline))
             2)))
         (document
          (make-yunge-reader-document :driver driver :layout 'fixed)))
    (unwind-protect
        (progn
          (with-current-buffer reader
            (yunge-reader-mode)
            (setq yunge-reader-document document))
          (with-temp-buffer
            (yunge-reader-outline-mode)
            (setq yunge-reader-outline--reader-buffer reader)
            (yunge-reader-outline-set-data
             (make-yunge-reader-outline-data
              :items
              (list
               (yunge-reader-outline-test--item "Part" 0 1)
               (yunge-reader-outline-test--item "Chapter" 1 2)
               (yunge-reader-outline-test--item "Section" 2 3)
               (yunge-reader-outline-test--item "Appendix" 0 4))))
            (puthash 0 t yunge-reader-outline--collapsed)
            (yunge-reader-outline--render)
            (should (= (count-lines (point-min) (point-max)) 2))
            (cl-letf
                (((symbol-function 'yunge-reader-outline--target)
                  (lambda () (list reader 'reader-window))))
              (yunge-reader-outline-current-item))
            (should-not
             (gethash 0 yunge-reader-outline--collapsed))
            (should (= (yunge-reader-outline--index-at-point) 2))))
      (kill-buffer reader))))

(ert-deftest yunge-reader-outline-routes-visit-and-show-focus ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        (unit 1)
        reader
        outline)
    (unwind-protect
        (save-window-excursion
          (setq reader (generate-new-buffer " *reader-outline-target*"))
          (switch-to-buffer reader)
          (yunge-reader-mode)
          (let* ((driver
                  (yunge-reader-register-driver
                   'test
                   :match (lambda (_file) t)
                   :open #'ignore
                   :close #'ignore
                   :location
                   (lambda (_document _window)
                     (make-yunge-reader-position :unit unit))
                   :restore
                   (lambda (_document position _window)
                     (setq unit (yunge-reader-position-unit position))
                     t)))
                 (file (expand-file-name "outline-target.pdf"))
                 (key (yunge-reader--document-key file driver))
                 (document
                  (make-yunge-reader-document
                   :key key :file file :driver driver :layout 'fixed))
                 (entry
                  (yunge-reader--make-document-entry
                   :key key :file file :driver driver :state 'ready
                   :document document :views (list reader)
                   :primary-view reader :active-view reader))
                 (reader-window (selected-window))
                 (data
                  (make-yunge-reader-outline-data
                   :items
                   (list
                    (yunge-reader-outline-test--item
                     "Destination" 0 9)))))
            (puthash key entry yunge-reader--document-registry)
            (setq yunge-reader-document document
                  yunge-reader--document-entry entry
                  yunge-reader--place-recording-enabled t)
            (setq outline
                  (yunge-reader-outline-create-buffer
                   reader reader-window entry document data))
            (let ((outline-window (split-window-right)))
              (set-window-buffer outline-window outline)
              (select-window outline-window)
              (with-current-buffer outline
                (yunge-reader-outline-show))
              (should (= unit 9))
              (should (eq (selected-window) outline-window))
              (setq unit 1)
              (with-current-buffer outline
                (yunge-reader-outline-visit))
              (should (= unit 9))
              (should (eq (selected-window) reader-window)))))
      (when (buffer-live-p outline)
        (kill-buffer outline))
      (when (buffer-live-p reader)
        (kill-buffer reader)))))

(ert-deftest yunge-reader-outline-rejects-a-dead-reader-view ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        reader
        outline)
    (unwind-protect
        (save-window-excursion
          (setq reader (generate-new-buffer " *reader-outline-dead*"))
          (switch-to-buffer reader)
          (yunge-reader-mode)
          (let* ((driver
                  (yunge-reader-register-driver
                   'test
                   :match #'ignore :open #'ignore :close #'ignore))
                 (file (expand-file-name "outline-dead.pdf"))
                 (key (yunge-reader--document-key file driver))
                 (document
                  (make-yunge-reader-document
                   :key key :file file :driver driver :layout 'fixed))
                 (entry
                  (yunge-reader--make-document-entry
                   :key key :file file :driver driver :state 'ready
                   :document document :views (list reader)
                   :primary-view reader :active-view reader)))
            (puthash key entry yunge-reader--document-registry)
            (setq yunge-reader-document document
                  yunge-reader--document-entry entry)
            (setq outline
                  (yunge-reader-outline-create-buffer
                   reader (selected-window) entry document
                   (make-yunge-reader-outline-data
                    :items
                    (list
                     (yunge-reader-outline-test--item
                      "Destination" 0 1)))))
            (kill-buffer reader)
            (setq reader nil)
            (with-current-buffer outline
              (should-error
               (yunge-reader-outline-show)
               :type 'user-error))))
      (when (buffer-live-p outline)
        (kill-buffer outline))
      (when (buffer-live-p reader)
        (kill-buffer reader)))))

(ert-deftest yunge-reader-outline-rebinds-a-persistent-hidden-view ()
  (let ((yunge-reader--document-registry
         (make-hash-table :test #'equal))
        (yunge-reader-drivers nil)
        (unit 1)
        reader
        outline
        other)
    (unwind-protect
        (save-window-excursion
          (setq reader (generate-new-buffer " *reader-outline-hidden*"))
          (switch-to-buffer reader)
          (yunge-reader-mode)
          (let* ((driver
                  (yunge-reader-register-driver
                   'test
                   :match (lambda (_file) t)
                   :open #'ignore
                   :close #'ignore
                   :location
                   (lambda (_document _window)
                     (make-yunge-reader-position :unit unit))
                   :restore
                   (lambda (_document position _window)
                     (setq unit (yunge-reader-position-unit position))
                     t)))
                 (file (expand-file-name "outline-hidden.pdf"))
                 (key (yunge-reader--document-key file driver))
                 (document
                  (make-yunge-reader-document
                   :key key :file file :driver driver :layout 'fixed))
                 (data
                  (make-yunge-reader-outline-data
                   :items
                   (list
                    (yunge-reader-outline-test--item
                     "Destination" 0 9))))
                 (entry
                  (yunge-reader--make-document-entry
                   :key key :file file :driver driver :state 'ready
                   :document document :views (list reader)
                   :primary-view reader :active-view reader
                   :outline data :outline-loaded t)))
            (puthash key entry yunge-reader--document-registry)
            (setq yunge-reader-document document
                  yunge-reader--document-entry entry
                  yunge-reader--place-recording-enabled t)
            (yunge-reader-outline)
            (setq outline (current-buffer))
            (with-current-buffer outline
              (yunge-reader-outline-visit))
            (should (= unit 9))
            (setq other
                  (generate-new-buffer " *reader-outline-other*"))
            (let ((reader-window (get-buffer-window reader t))
                  (outline-window (get-buffer-window outline t)))
              (set-window-buffer reader-window other)
              (select-window outline-window))
            (should (buffer-live-p reader))
            (should-not (get-buffer-window reader t))
            (should (get-buffer-window outline t))
            (setq unit 1)
            (with-current-buffer outline
              (yunge-reader-outline-visit))
            (should (= unit 9))
            (should (eq (window-buffer (selected-window)) reader))
            (let* ((reader-window (selected-window))
                   (outline-window (get-buffer-window outline t))
                   (other-window
                    (split-window reader-window nil 'below)))
              (set-window-buffer other-window other)
              (delete-window reader-window)
              (select-window outline-window))
            (should (buffer-live-p reader))
            (should-not (get-buffer-window reader t))
            (setq unit 2)
            (with-current-buffer outline
              (yunge-reader-outline-visit))
            (should (= unit 9))
            (should (eq (window-buffer (selected-window)) reader))))
      (when (buffer-live-p outline)
        (kill-buffer outline))
      (when (buffer-live-p reader)
        (kill-buffer reader))
      (when (buffer-live-p other)
        (kill-buffer other)))))

;;; yunge-reader-outline-test.el ends here
