;;; yunge-reader-pdf.el --- PDF reader -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'mwheel)
(require 'yunge-key)
(require 'yunge-reader)
(require 'yunge-reader-pdf-backend)
(require 'yunge-reader-pdf-geometry)
(require 'yunge-reader-pdf-protocol)
(require 'yunge-reader-pdf-viewport)
(require 'yunge-reader-pdf-render)
(require 'yunge-reader-pdf-interaction)

(defvar-keymap yunge-reader-pdf--image-map
  "<down-mouse-1>" #'yunge-reader-pdf-select-with-mouse
  "<mouse-1>" #'ignore
  "C-<mouse-1>" #'yunge-reader-pdf-activate-at-mouse
  "<drag-mouse-1>" #'ignore)

(defconst yunge-reader-pdf-normal-bindings
  '(("RET" yunge-reader-pdf-follow-link "follow link")
    ("C-d" yunge-reader-pdf-scroll-down "scroll down")
    ("C-u" yunge-reader-pdf-scroll-up "scroll up")
    ("G" yunge-reader-pdf-last-page "last page")
    ("J" yunge-reader-pdf-next-page "next page")
    ("K" yunge-reader-pdf-previous-page "previous page")
    ("gg" yunge-reader-pdf-first-page "first page")
    ("gp" yunge-reader-pdf-goto-page "go to page")
    ("gr" yunge-reader-refresh "refresh")
    ("j" yunge-reader-pdf-scroll-down-line "scroll down one line")
    ("k" yunge-reader-pdf-scroll-up-line "scroll up one line"))
  "Normal-state bindings for the PDF view adapter.")

(defvar-keymap yunge-reader-pdf-view-mode-map
  "RET" #'yunge-reader-pdf-follow-link
  "C-d" #'yunge-reader-pdf-scroll-down
  "C-u" #'yunge-reader-pdf-scroll-up
  "G" #'yunge-reader-pdf-last-page
  "J" #'yunge-reader-pdf-next-page
  "K" #'yunge-reader-pdf-previous-page
  "<next>" #'scroll-up-command
  "<prior>" #'scroll-down-command
  "g g" #'yunge-reader-pdf-first-page
  "g p" #'yunge-reader-pdf-goto-page
  "g r" #'yunge-reader-refresh
  "j" #'yunge-reader-pdf-scroll-down-line
  "k" #'yunge-reader-pdf-scroll-up-line
  "<mouse-4>" #'yunge-reader-pdf-scroll-wheel
  "<mouse-5>" #'yunge-reader-pdf-scroll-wheel
  "<wheel-down>" #'yunge-reader-pdf-scroll-wheel
  "<wheel-up>" #'yunge-reader-pdf-scroll-wheel)

(define-minor-mode yunge-reader-pdf-view-mode
  "Display a fixed-layout PDF through the Yunge Reader PDF driver."
  :init-value nil
  :lighter " PDF"
  :keymap yunge-reader-pdf-view-mode-map
  (if yunge-reader-pdf-view-mode
      (progn
        (setq-local yunge-reader-pdf-page 0)
        (setq-local line-spacing yunge-reader-pdf-page-gap)
        (setq-local yunge-reader-pdf--render-results
                    (make-hash-table :test #'equal))
        (setq-local yunge-reader-pdf--render-pending
                    (make-hash-table :test #'equal))
        (setq-local yunge-reader-pdf--text-cache
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--text-hit-cache
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--text-pending
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--link-cache
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--link-pending
                    (make-hash-table :test #'eql))
        (setq-local yunge-reader-pdf--link-activation-generation 0)
        (setq-local yunge-reader-pdf--working-pages nil)
        (setq-local yunge-reader-pdf--prefetch-queue nil)
        (setq-local yunge-reader-pdf--prefetch-active nil)
        (setq-local yunge-reader-pdf--prefetch-running nil)
        (setq-local yunge-reader-pdf--pending-location nil)
        (setq-local yunge-reader-pdf--resize-timer nil)
        (setq-local yunge-reader-pdf--pending-resize nil)
        (setq-local mwheel-coalesce-scroll-events nil)
        (add-hook 'yunge-reader-refresh-hook
                  #'yunge-reader-pdf--refresh nil t)
        (add-hook 'yunge-reader-view-role-change-hook
                  #'yunge-reader-pdf--update-header nil t)
        (add-hook 'yunge-reader-appearance-change-hook
                  #'yunge-reader-pdf--appearance-changed nil t)
        (add-hook 'yunge-reader-search-result-hook
                  #'yunge-reader-pdf--search-result-changed nil t)
        (add-hook 'window-size-change-functions
                  #'yunge-reader-pdf--window-size-change nil t)
        (add-hook 'window-scroll-functions
                  #'yunge-reader-pdf--window-scrolled nil t)
        (add-hook 'kill-buffer-hook
                  #'yunge-reader-pdf--cancel-resize nil t))
    (yunge-reader-pdf--cancel-resize)
    (remove-hook 'yunge-reader-refresh-hook
                 #'yunge-reader-pdf--refresh t)
    (remove-hook 'yunge-reader-view-role-change-hook
                 #'yunge-reader-pdf--update-header t)
    (remove-hook 'yunge-reader-appearance-change-hook
                 #'yunge-reader-pdf--appearance-changed t)
    (remove-hook 'yunge-reader-search-result-hook
                 #'yunge-reader-pdf--search-result-changed t)
    (remove-hook 'window-size-change-functions
                 #'yunge-reader-pdf--window-size-change t)
    (remove-hook 'window-scroll-functions
                 #'yunge-reader-pdf--window-scrolled t)
    (remove-hook 'kill-buffer-hook
                 #'yunge-reader-pdf--cancel-resize t)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'mwheel-coalesce-scroll-events)
    (setq yunge-reader-pdf--page-infos nil
          yunge-reader-pdf--page-positions nil
          yunge-reader-pdf--render-results nil
          yunge-reader-pdf--render-pending nil
          yunge-reader-pdf--displayed-pages nil
          yunge-reader-pdf--pending-location nil
          yunge-reader-pdf--resize-timer nil
          yunge-reader-pdf--pending-resize nil
          yunge-reader-pdf--text-cache nil
          yunge-reader-pdf--text-hit-cache nil
          yunge-reader-pdf--text-pending nil
          yunge-reader-pdf--link-cache nil
          yunge-reader-pdf--link-pending nil
          yunge-reader-pdf--link-activation-generation 0
          yunge-reader-pdf--working-pages nil
          yunge-reader-pdf--prefetch-queue nil
          yunge-reader-pdf--prefetch-active nil
          yunge-reader-pdf--prefetch-running nil)))

(with-eval-after-load 'evil
  (yunge-key-evil-define-minor-mode
   'normal 'yunge-reader-pdf-view-mode
   yunge-reader-pdf-normal-bindings))

(defun yunge-reader-pdf--match-p (file)
  "Return whether FILE has a PDF extension."
  (string-equal (downcase (or (file-name-extension file) "")) "pdf"))

(defun yunge-reader-pdf--search-origin-value (origin)
  "Return native PDF value from stable Reader ORIGIN, or nil."
  (when origin
    (and (yunge-reader-position-p origin)
         (let* ((page (yunge-reader-position-unit origin))
                (offset
                 (or (yunge-reader-position-offset origin)
                     (when (and (numberp (yunge-reader-position-x origin))
                                (numberp (yunge-reader-position-y origin)))
                       (yunge-reader-pdf--nearest-text-offset
                        page
                        (yunge-reader-position-x origin)
                        (yunge-reader-position-y origin))))))
           (and (natnump (yunge-reader-position-unit origin))
                (or (null offset) (natnump offset))
                `((page . ,page)
                  (offset . ,offset)))))))

(defun yunge-reader-pdf--attach (_document)
  "Attach the PDF view adapter to the current Reader buffer."
  (yunge-reader-pdf-view-mode 1))

(defun yunge-reader-pdf--detach (_document)
  "Detach the PDF view adapter from the current Reader buffer."
  (yunge-reader-pdf-view-mode -1))

(defun yunge-reader-pdf--request-search-capability
    (document arguments complete)
  "Search DOCUMENT with typed ARGUMENTS through COMPLETE."
  (unless (yunge-reader-search-request-p arguments)
    (error "Invalid PDF search request: %S" arguments))
  (let* ((origin-value
          (yunge-reader-search-request-origin arguments))
         (origin
          (yunge-reader-pdf--search-origin-value origin-value)))
    (when (and origin-value (null origin))
      (error "Invalid stable PDF search origin: %S" origin-value))
    (yunge-reader-pdf--request
     document 'search
     (list
      :query (yunge-reader-search-request-query arguments)
      :case-sensitive
      (yunge-reader-search-request-case-sensitive arguments)
      :direction (yunge-reader-search-request-direction arguments)
      :origin origin
      :cursor (yunge-reader-search-request-cursor arguments)
      :match-limit (yunge-reader-search-request-match-limit arguments)
      :page-limit (yunge-reader-search-request-unit-limit arguments))
     complete)))


(defun yunge-reader-pdf-register ()
  "Register the PDF driver."
  (yunge-reader-register-driver
   'pdf
   :match #'yunge-reader-pdf--match-p
   :open #'yunge-reader-pdf--open
   :close #'yunge-reader-pdf--close
   :attach #'yunge-reader-pdf--attach
   :detach #'yunge-reader-pdf--detach
   :outline #'yunge-reader-pdf--request-outline
   :search #'yunge-reader-pdf--request-search-capability
   :selection-text #'yunge-reader-pdf--request-selection-text-capability
   :location #'yunge-reader-pdf--location
   :restore #'yunge-reader-pdf--restore-location))

;;;###autoload
(defun yunge-reader-pdf-mode ()
  "Read the PDF visited by the current buffer with Yunge Reader."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a PDF file"))
  (yunge-reader-pdf-register)
  (yunge-reader-visit-file buffer-file-name))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.pdf\\'" . yunge-reader-pdf-mode))

;;;###autoload
(defun yunge-reader-pdf-open (file)
  "Open PDF FILE explicitly with Yunge Reader."
  (interactive "fRead PDF: ")
  (yunge-reader-pdf-register)
  (yunge-reader-open file))



(provide 'yunge-reader-pdf)

;;; yunge-reader-pdf.el ends here
