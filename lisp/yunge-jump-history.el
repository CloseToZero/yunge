;;; yunge-jump-history.el --- Jump history -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)

(declare-function dired-noselect "dired")

(defvar evil--jumps-jump-command)

(defconst yunge-jump-history--max-length 100)
(defvar yunge-jump-history--moving nil)

(cl-defstruct (yunge-jump-history--provider
               (:constructor yunge-jump-history--make-provider))
  name
  capture-function
  same-function
  visit-function)

(cl-defstruct (yunge-jump-history--entry
               (:constructor yunge-jump-history--make-entry))
  type
  value)

(cl-defstruct (yunge-jump-history--buffer-target
               (:constructor yunge-jump-history--make-buffer-target))
  marker
  fallback
  position)

(cl-defstruct (yunge-jump-history--history
               (:constructor yunge-jump-history--make-history))
  entries
  (index -1)
  (generation 0)
  pending)

(defvar yunge-jump-history--providers nil
  "Registered jump target providers in precedence order.")

(defun yunge-jump-history--history (&optional window)
  "Return the jump history for WINDOW, creating it when needed."
  (setq window (or window (selected-window)))
  (or (window-parameter window 'yunge-jump-history)
      (let ((history (yunge-jump-history--make-history)))
        (set-window-parameter window 'yunge-jump-history history)
        history)))

(cl-defun yunge-jump-history-register-target
    (name &key capture same visit)
  "Register jump target provider NAME.
CAPTURE runs in a candidate buffer with WINDOW and POSITION and returns an
opaque target value, or nil when it does not accept the location.  SAME
receives two values and identifies equivalent locations.  VISIT receives a
value, destination window, and completion function; it must call completion
exactly once with non-nil after a successful visit, nil for an unavailable
target, or `:cancel' to stop traversal without moving.  Completion may be
called asynchronously.

Registering NAME again replaces the old provider and gives the new provider
highest precedence."
  (unless (symbolp name)
    (error "Jump target name must be a symbol: %S" name))
  (dolist (function (list capture same visit))
    (unless (functionp function)
      (error "Jump target %s has a non-function member: %S"
             name function)))
  (let ((provider
         (yunge-jump-history--make-provider
          :name name
          :capture-function capture
          :same-function same
          :visit-function visit)))
    (setq yunge-jump-history--providers
          (cons
           provider
           (seq-remove
            (lambda (candidate)
              (eq (yunge-jump-history--provider-name candidate) name))
            yunge-jump-history--providers)))
    provider))

(defun yunge-jump-history-unregister-target (name)
  "Unregister jump target provider NAME."
  (setq yunge-jump-history--providers
        (seq-remove
         (lambda (provider)
           (eq (yunge-jump-history--provider-name provider) name))
         yunge-jump-history--providers)))

(defun yunge-jump-history--provider (name)
  "Return the registered jump target provider NAME, or nil."
  (seq-find
   (lambda (provider)
     (eq (yunge-jump-history--provider-name provider) name))
   yunge-jump-history--providers))

(defun yunge-jump-history--fallback ()
  "Return a way to reopen the current buffer, when one exists."
  (cond
   (buffer-file-name (cons 'file buffer-file-name))
   ((derived-mode-p 'dired-mode)
    (cons 'directory default-directory))))

(defun yunge-jump-history--capture-buffer-target (_window position)
  "Capture the default buffer target at POSITION."
  (let ((marker (copy-marker position)))
    (yunge-jump-history--make-buffer-target
     :marker marker
     :fallback (yunge-jump-history--fallback)
     :position (marker-position marker))))

(defun yunge-jump-history--capture-with-provider
    (provider window position)
  "Ask PROVIDER to capture WINDOW at POSITION, or return nil on error."
  (condition-case error-data
      (funcall
       (yunge-jump-history--provider-capture-function provider)
       window position)
    (error
     (display-warning
      'yunge-jump-history
      (format "Could not capture jump target %s: %s"
              (yunge-jump-history--provider-name provider)
              (error-message-string error-data))
      :warning)
     nil)))

(defun yunge-jump-history--entry (&optional position window)
  "Return a provider-backed jump entry for POSITION and WINDOW."
  (let* ((marker
          (cond
           ((markerp position)
            (when (marker-buffer position)
              (copy-marker position)))
           ((integerp position) (copy-marker position))
           ((null position) (point-marker))))
         (buffer (and marker (marker-buffer marker))))
    (when (and buffer (not (minibufferp buffer)))
      (setq window
            (cond
             ((and (window-live-p window)
                   (eq (window-buffer window) buffer))
              window)
             ((eq (window-buffer (selected-window)) buffer)
              (selected-window))
             (t (get-buffer-window buffer t))))
      (with-current-buffer buffer
        (cl-loop
         for provider in yunge-jump-history--providers
         for value =
         (yunge-jump-history--capture-with-provider
          provider window (marker-position marker))
         when value
         return
         (yunge-jump-history--make-entry
          :type (yunge-jump-history--provider-name provider)
          :value value))))))

(defun yunge-jump-history--window-entry (window)
  "Return an entry for WINDOW's current location."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (yunge-jump-history--entry (window-point window) window))))

(defun yunge-jump-history--same-place-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same location."
  (and
   (eq (yunge-jump-history--entry-type left)
       (yunge-jump-history--entry-type right))
   (when-let* ((provider
                (yunge-jump-history--provider
                 (yunge-jump-history--entry-type left))))
     (condition-case error-data
         (funcall
          (yunge-jump-history--provider-same-function provider)
          (yunge-jump-history--entry-value left)
          (yunge-jump-history--entry-value right))
       (error
        (display-warning
         'yunge-jump-history
         (format "Could not compare jump targets %s: %s"
                 (yunge-jump-history--entry-type left)
                 (error-message-string error-data))
         :warning)
        nil)))))

(defun yunge-jump-history--trim (entries)
  "Limit ENTRIES to `yunge-jump-history--max-length'."
  (when-let* ((last (nthcdr (1- yunge-jump-history--max-length) entries)))
    (setcdr last nil))
  entries)

(defun yunge-jump-history--push (history entry &optional branch)
  "Push ENTRY onto HISTORY.
When BRANCH is non-nil, discard entries newer than the current one."
  (let ((entries (yunge-jump-history--history-entries history))
        (index (yunge-jump-history--history-index history)))
    (when (and branch (>= index 0))
      (setq entries (nthcdr index entries)))
    (when branch
      (setf (yunge-jump-history--history-index history) -1))
    (unless (and entries
                 (yunge-jump-history--same-place-p entry (car entries)))
      (setq entries (cons entry entries)))
    (setf (yunge-jump-history--history-entries history)
          (yunge-jump-history--trim entries))))

(defun yunge-jump-history--record (&optional position)
  "Record Evil jump POSITION in the selected window's history."
  (let* ((window (selected-window))
         (history (yunge-jump-history--history window)))
    (unless (or yunge-jump-history--moving
                (yunge-jump-history--history-pending history))
      (when-let* ((entry
                   (yunge-jump-history--entry position window)))
        (yunge-jump-history--push history entry t)))))

(defun yunge-jump-history-record (&optional window)
  "Record WINDOW's current location as a new jump branch."
  (setq window (or window (selected-window)))
  (let ((history (yunge-jump-history--history window)))
    (unless (or yunge-jump-history--moving
                (yunge-jump-history--history-pending history))
      (when-let* ((entry (yunge-jump-history--window-entry window)))
        (yunge-jump-history--push history entry t)))))

(defun yunge-jump-history--track-navigation (function &rest arguments)
  "Record a successful location change made by FUNCTION with ARGUMENTS."
  (if yunge-jump-history--moving
      (apply function arguments)
    (let* ((origin (yunge-jump-history--window-entry (selected-window)))
           ;; Ignore transient locations visited by completion previews.
           (yunge-jump-history--moving t)
           (result (apply function arguments))
           (window (selected-window))
           (history (yunge-jump-history--history window))
           (destination (yunge-jump-history--window-entry window)))
      (when (and origin
                 destination
                 (not (yunge-jump-history--history-pending history))
                 (not
                  (yunge-jump-history--same-place-p origin destination)))
        (yunge-jump-history--push history origin t))
      result)))

(defun yunge-jump-history-track-command (command)
  "Track successful location changes made by COMMAND."
  (unless (advice-member-p #'yunge-jump-history--track-navigation command)
    (advice-add command :around #'yunge-jump-history--track-navigation)))

(defun yunge-jump-history--same-buffer-target-p (left right)
  "Return non-nil when buffer targets LEFT and RIGHT are equivalent."
  (let ((left-marker (yunge-jump-history--buffer-target-marker left))
        (right-marker (yunge-jump-history--buffer-target-marker right))
        (left-fallback (yunge-jump-history--buffer-target-fallback left))
        (right-fallback
         (yunge-jump-history--buffer-target-fallback right)))
    (if (and (marker-buffer left-marker)
             (marker-buffer right-marker))
        (and (eq (marker-buffer left-marker)
                 (marker-buffer right-marker))
             (= (marker-position left-marker)
                (marker-position right-marker)))
      (and left-fallback
           right-fallback
           (equal left-fallback right-fallback)
           (= (yunge-jump-history--buffer-target-position left)
              (yunge-jump-history--buffer-target-position right))))))

(defun yunge-jump-history--buffer-target (value)
  "Return the live buffer and position represented by VALUE."
  (let* ((marker (yunge-jump-history--buffer-target-marker value))
         (buffer (marker-buffer marker)))
    (cond
     (buffer (cons buffer (marker-position marker)))
     ((pcase (yunge-jump-history--buffer-target-fallback value)
        (`(file . ,file)
         (when (file-exists-p file)
           (setq buffer (find-file-noselect file))))
        (`(directory . ,directory)
         (when (file-directory-p directory)
           (setq buffer (dired-noselect directory)))))
      (cons
       buffer (yunge-jump-history--buffer-target-position value))))))

(defun yunge-jump-history--visit-buffer-target
    (value window complete)
  "Visit buffer target VALUE in WINDOW, then call COMPLETE."
  (if-let* ((target (and (window-live-p window)
                         (yunge-jump-history--buffer-target value))))
      (progn
        (select-window window)
        (let ((buffer (car target))
              (position (cdr target)))
          ;; Prevent Evil's buffer-crossing hook from creating another jump.
          (setq evil--jumps-jump-command t)
          (switch-to-buffer buffer)
          (goto-char (min (max position (point-min)) (point-max)))
          (set-marker (yunge-jump-history--buffer-target-marker value)
                      (point) (current-buffer)))
        (funcall complete t))
    (funcall complete nil)))

(defun yunge-jump-history--visit (entry window complete)
  "Ask ENTRY's provider to visit it in WINDOW and call COMPLETE."
  (if-let* ((provider
             (yunge-jump-history--provider
              (yunge-jump-history--entry-type entry))))
      (let ((completed nil)
            (yunge-jump-history--moving t))
        (condition-case error-data
            (funcall
             (yunge-jump-history--provider-visit-function provider)
             (yunge-jump-history--entry-value entry)
             window
             (lambda (result)
               (unless completed
                 (setq completed t)
                 (funcall
                  complete
                  (cond
                   ((eq result :cancel) :cancel)
                   (result t))))))
          (error
           (unless completed
             (setq completed t)
             (funcall complete nil))
           (display-warning
            'yunge-jump-history
            (format "Could not visit jump target %s: %s"
                    (yunge-jump-history--entry-type entry)
                    (error-message-string error-data))
            :warning))))
    (funcall complete nil)))

(defun yunge-jump-history--move
    (history window direction count description)
  "Move through HISTORY in DIRECTION COUNT times.
WINDOW receives each visited location.  DESCRIPTION names the direction for
an error message."
  (when (yunge-jump-history--history-pending history)
    (user-error "Jump navigation is still pending"))
  (let* ((entries (yunge-jump-history--history-entries history))
         (token (cl-incf (yunge-jump-history--history-generation history)))
         (synchronous t)
         immediate-error)
    (setf (yunge-jump-history--history-pending history) token)
    (cl-labels
        ((cancel
          ()
          (when (eq (yunge-jump-history--history-pending history) token)
            (setf (yunge-jump-history--history-pending history) nil)))
         (finish
          (moved)
          (when (eq (yunge-jump-history--history-pending history) token)
            (setf (yunge-jump-history--history-pending history) nil)
            (unless moved
              (if synchronous
                  (setq immediate-error t)
                (message "No %s jump" description)))))
         (seek
          (index remaining moved)
          (when (eq (yunge-jump-history--history-pending history) token)
            (let ((candidate (+ index direction)))
              (if (or (<= remaining 0)
                      (< candidate 0)
                      (>= candidate (length entries)))
                  (finish moved)
                (yunge-jump-history--visit
                 (nth candidate entries) window
                 (lambda (result)
                   (when
                       (eq (yunge-jump-history--history-pending history)
                           token)
                     (cond
                      ((eq result :cancel) (cancel))
                      (result
                       (setf
                        (yunge-jump-history--history-index history)
                        candidate)
                      (seek candidate (1- remaining) t))
                      (t
                       (seek candidate remaining moved)))))))))))
      (seek (yunge-jump-history--history-index history) count nil)
      (setq synchronous nil)
      (when immediate-error
        (user-error "No %s jump" description)))))

(defun yunge-jump-history-backward (&optional count)
  "Go to the COUNTth older location in this window's jump history."
  (interactive "p")
  (let* ((window (selected-window))
         (history (yunge-jump-history--history window)))
    (when (yunge-jump-history--history-pending history)
      (user-error "Jump navigation is still pending"))
    (when (< (yunge-jump-history--history-index history) 0)
      (when-let* ((entry (yunge-jump-history--window-entry window)))
        (yunge-jump-history--push history entry)
        (setf (yunge-jump-history--history-index history) 0)))
    (yunge-jump-history--move
     history window 1 (or count 1) "older")))

(defun yunge-jump-history-forward (&optional count)
  "Go to the COUNTth newer location in this window's jump history."
  (interactive "p")
  (let ((window (selected-window)))
    (yunge-jump-history--move
     (yunge-jump-history--history window)
     window -1 (or count 1) "newer")))

(defun yunge-jump-history--copy-after-split (function &rest arguments)
  "Call split FUNCTION with ARGUMENTS and copy its window history."
  (let* ((source (or (car arguments) (selected-window)))
         (history
          (and (window-live-p source)
               (window-parameter source 'yunge-jump-history)))
         (new-window (apply function arguments)))
    (when (and history (window-live-p new-window))
      (set-window-parameter
       new-window 'yunge-jump-history
       (yunge-jump-history--make-history
        :entries (copy-sequence
                  (yunge-jump-history--history-entries history))
        :index (yunge-jump-history--history-index history))))
    new-window))

(defun yunge-jump-history--register-buffer-target ()
  "Register the default marker, file, and directory jump target."
  (yunge-jump-history-unregister-target 'buffer)
  (setq yunge-jump-history--providers
        (append
         yunge-jump-history--providers
         (list
          (yunge-jump-history--make-provider
           :name 'buffer
           :capture-function
           #'yunge-jump-history--capture-buffer-target
           :same-function #'yunge-jump-history--same-buffer-target-p
           :visit-function
           #'yunge-jump-history--visit-buffer-target)))))

(defun yunge-jump-history--setup ()
  "Replace Evil jump traversal with the window-local history."
  (advice-add 'evil-set-jump :before #'yunge-jump-history--record)
  (advice-add 'split-window :around #'yunge-jump-history--copy-after-split)
  (keymap-set (current-global-map)
              "<remap> <evil-jump-backward>"
              #'yunge-jump-history-backward)
  (keymap-set (current-global-map)
              "<remap> <evil-jump-forward>"
              #'yunge-jump-history-forward))

(with-eval-after-load 'evil
  (yunge-jump-history--setup))

(yunge-jump-history--register-buffer-target)

(provide 'yunge-jump-history)

;;; yunge-jump-history.el ends here
