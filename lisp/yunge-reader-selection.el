;;; yunge-reader-selection.el --- Document selection and copying -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)
(require 'yunge-reader-model)
(require 'yunge-reader-task)

(declare-function yunge-reader-request "yunge-reader"
                  (operation arguments complete &rest options))
(declare-function yunge-reader-refresh "yunge-reader" ())
(defvar yunge-reader-document)

(defcustom yunge-reader-copy-unit-limit 8
  "Maximum document units read by one selection text batch."
  :type '(integer :tag "Units" 1 64)
  :group 'yunge-reader)

(defcustom yunge-reader-copy-character-limit 16384
  "Maximum indexed characters read by one selection text batch."
  :type '(integer :tag "Characters" 1 65536)
  :group 'yunge-reader)

(defvar-local yunge-reader-selection nil
  "Current logical `yunge-reader-selection', or nil.")

(defvar-local yunge-reader-selection--copy-generation 0
  "Generation used to reject late selection copy completions.")

(defvar-local yunge-reader-selection--copy-phase nil
  "Current copy phase: nil, `capture', or `text'.")

(defvar-local yunge-reader-selection--copy-task nil
  "Cancellable task serving the active selection copy phase.")

(defvar-local yunge-reader-selection-change-hook nil
  "Hook run after the logical document selection changes.")

(defun yunge-reader-selection-cancel-copy (&optional reason)
  "Cancel the current selection copy for REASON and return whether one was pending."
  (let ((pending yunge-reader-selection--copy-phase)
        (obsolete yunge-reader-selection--copy-task))
    (cl-incf yunge-reader-selection--copy-generation)
    (setq yunge-reader-selection--copy-phase nil
          yunge-reader-selection--copy-task nil)
    (when (yunge-reader-task-active-p obsolete)
      (yunge-reader-task-cancel
       obsolete (or reason "The selection changed")))
    pending))

(defun yunge-reader-set-selection (start end &optional text)
  "Select the logical document range from START through END.
START and END are `yunge-reader-position' objects.  TEXT may be supplied by a
driver that already resolved the selected glyphs."
  (unless (and (yunge-reader-position-p start)
               (yunge-reader-position-p end))
    (error "Reader selection endpoints must be reader positions"))
  (let ((selection
         (make-yunge-reader-selection
          :start start :end end :text text)))
    (yunge-reader-selection-cancel-copy "The selection changed")
    (unless (equal selection yunge-reader-selection)
      (setq yunge-reader-selection selection)
      (run-hooks 'yunge-reader-selection-change-hook))))

(defun yunge-reader-clear-selection (&optional defer-refresh)
  "Clear the logical selection in the current reader buffer.
When DEFER-REFRESH is non-nil, leave repainting to the caller."
  (interactive)
  (let ((changed yunge-reader-selection))
    (yunge-reader-selection-cancel-copy "The selection was cleared")
    (setq yunge-reader-selection nil)
    (when changed
      (run-hooks 'yunge-reader-selection-change-hook)))
  (unless defer-refresh
    (yunge-reader-refresh)))

(defun yunge-reader-selection--batch-valid-p (batch)
  "Return non-nil when BATCH follows the selection text contract."
  (when (yunge-reader-selection-batch-p batch)
    (let ((cursor (yunge-reader-selection-batch-cursor batch))
          (done (yunge-reader-selection-batch-done batch)))
      (and (stringp (yunge-reader-selection-batch-text batch))
           (memq done '(nil t))
           (if done
               (null cursor)
             (yunge-reader-position-p cursor))))))

(defun yunge-reader-selection--copy-current-p
    (document selection generation)
  "Return whether DOCUMENT copy of SELECTION at GENERATION is current."
  (and (eq yunge-reader-selection--copy-phase 'text)
       (= generation yunge-reader-selection--copy-generation)
       (eq document yunge-reader-document)
       (eq selection yunge-reader-selection)))

(defun yunge-reader-selection--copy-text (text)
  "Put nonempty selected TEXT in the kill ring."
  (unless (and (stringp text) (not (string-empty-p text)))
    (user-error "The document selection contains no text"))
  (kill-new text)
  (message "Copied document text")
  text)

(defun yunge-reader-selection--schedule-batch
    (buffer document selection generation cursor fragments)
  "Schedule the next selection batch for BUFFER and DOCUMENT."
  (run-at-time
   0 nil
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (yunge-reader-selection--copy-current-p
                document selection generation)
           (yunge-reader-selection--request-batch
            buffer document selection generation cursor fragments)))))))

(defun yunge-reader-selection--complete-batch
    (buffer document selection generation old-cursor fragments
            value error-data)
  "Complete one selection text request made from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (yunge-reader-selection--copy-current-p
             document selection generation)
        (setq yunge-reader-selection--copy-task nil)
        (cond
         (error-data
          (setq yunge-reader-selection--copy-phase nil)
          (display-warning
           'yunge-reader
           (format "Could not copy document text: %s"
                   (error-message-string error-data))
           :warning))
         ((not (yunge-reader-selection--batch-valid-p value))
          (setq yunge-reader-selection--copy-phase nil)
          (display-warning
           'yunge-reader
           "Reader driver returned an invalid selection text batch"
           :warning))
         (t
          (let* ((text (yunge-reader-selection-batch-text value))
                 (cursor (yunge-reader-selection-batch-cursor value))
                 (done (yunge-reader-selection-batch-done value))
                 (fragments (cons text fragments)))
            (cond
             (done
              (let ((complete-text
                     (mapconcat #'identity
                                (nreverse fragments) "")))
                (setq yunge-reader-selection--copy-phase nil)
                (condition-case copy-error
                    (progn
                      (yunge-reader-selection--copy-text complete-text)
                      (setf (yunge-reader-selection-text selection)
                            complete-text))
                  (error
                   (display-warning
                    'yunge-reader
                    (format "Could not copy document text: %s"
                            (error-message-string copy-error))
                    :warning)))))
             ((equal cursor old-cursor)
              (setq yunge-reader-selection--copy-phase nil)
              (display-warning
               'yunge-reader
               "Reader selection text cursor did not advance"
               :warning))
             (t
              (yunge-reader-selection--schedule-batch
               buffer document selection generation cursor
               fragments))))))))))

(defun yunge-reader-selection--request-batch
    (buffer document selection generation cursor fragments)
  "Request one bounded text batch for SELECTION in DOCUMENT."
  (let ((task
         (yunge-reader-request
          'selection-text
          (list :start (yunge-reader-selection-start selection)
                :end (yunge-reader-selection-end selection)
                :cursor cursor
                :unit-limit yunge-reader-copy-unit-limit
                :character-limit yunge-reader-copy-character-limit)
          (lambda (value error-data)
            (yunge-reader-selection--complete-batch
             buffer document selection generation cursor fragments
             value error-data))
          :revision generation)))
    (when (and (yunge-reader-task-active-p task)
               (yunge-reader-selection--copy-current-p
                document selection generation))
      (setq yunge-reader-selection--copy-task task))))

(defun yunge-reader-selection--capture-current-p (document generation)
  "Return whether the native selection capture still serves DOCUMENT."
  (and (eq yunge-reader-selection--copy-phase 'capture)
       (= generation yunge-reader-selection--copy-generation)
       (eq document yunge-reader-document)))

(defun yunge-reader-selection--capture-complete
    (buffer document generation selection error-data)
  "Continue BUFFER's copy after capturing SELECTION from DOCUMENT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (yunge-reader-selection--capture-current-p document generation)
        (setq yunge-reader-selection--copy-phase nil
              yunge-reader-selection--copy-task nil)
        (cond
         (error-data
          (display-warning
           'yunge-reader
           (format "Could not read document selection: %s"
                   (error-message-string error-data))
           :warning))
         ((null selection)
          (yunge-reader-clear-selection t)
          (message "There is no document selection"))
         ((not (yunge-reader-selection-p selection))
          (display-warning
           'yunge-reader "Reader view returned an invalid selection"
           :warning))
         (t
          (yunge-reader-set-selection
           (yunge-reader-selection-start selection)
           (yunge-reader-selection-end selection)
           (yunge-reader-selection-text selection))
          (yunge-reader-copy-selection)))))))

(defun yunge-reader-selection--capture (read-current)
  "Begin one copy using READ-CURRENT to capture a live view selection."
  (let ((buffer (current-buffer))
        (document yunge-reader-document)
        (generation (cl-incf yunge-reader-selection--copy-generation)))
    (unless document
      (user-error "This reader buffer has no open document"))
    (setq yunge-reader-selection--copy-phase 'capture)
    (message "Reading document selection...")
    (condition-case error-data
        (let ((task
               (funcall
                read-current
                (lambda (selection error-data)
                  (yunge-reader-selection--capture-complete
                   buffer document generation selection error-data))
                generation)))
          (when (yunge-reader-task-active-p task)
            (if (yunge-reader-selection--capture-current-p document generation)
                (setq yunge-reader-selection--copy-task task)
              (yunge-reader-task-cancel
               task "The selection capture already completed"))))
      (error
       (when (yunge-reader-selection--capture-current-p document generation)
         (yunge-reader-selection-cancel-copy))
       (signal (car error-data) (cdr error-data))))))

(defun yunge-reader-copy-selection (&optional read-current)
  "Copy the current document selection into the kill ring.
With optional READ-CURRENT, capture a live view selection first.  It receives
a completion function and opaque revision, may complete synchronously, and
returns a cancellable task for pending work.  The completion receives a
`yunge-reader-selection' or nil, and an error value.  The selection is
adopted only while the same document and copy request are current.  Without
READ-CURRENT, copy the existing logical selection.  The driver supplies
uncached text in bounded batches before this command publishes it."
  (interactive)
  (cond
   (yunge-reader-selection--copy-phase
    (message "Document selection is still being copied"))
   (read-current
    (yunge-reader-selection--capture read-current))
   ((null yunge-reader-selection)
    (user-error "There is no document selection"))
   ((yunge-reader-selection-text yunge-reader-selection)
    (yunge-reader-selection-cancel-copy "Cached selection text was used")
    (yunge-reader-selection--copy-text
     (yunge-reader-selection-text yunge-reader-selection)))
   (t
    (let ((buffer (current-buffer))
          (document yunge-reader-document)
          (selection yunge-reader-selection)
          (generation (cl-incf yunge-reader-selection--copy-generation)))
      (unless document
        (user-error "This reader buffer has no open document"))
      (setq yunge-reader-selection--copy-phase 'text)
      (message "Copying document text...")
      (yunge-reader-selection--request-batch
       buffer document selection generation nil nil)))))

(provide 'yunge-reader-selection)

;;; yunge-reader-selection.el ends here
