;;; yunge-jump.el --- Window-local jump history -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(declare-function dired-noselect "dired")

(defvar evil--jumps-jump-command)

(defconst yunge-jump--max-length 100)
(defvar yunge-jump--moving nil)

(cl-defstruct (yunge-jump--entry
               (:constructor yunge-jump--make-entry))
  marker
  fallback
  position)

(cl-defstruct (yunge-jump--history
               (:constructor yunge-jump--make-history))
  entries
  (index -1))

(defun yunge-jump--history (&optional window)
  "Return the jump history for WINDOW, creating it when needed."
  (setq window (or window (selected-window)))
  (or (window-parameter window 'yunge-jump-history)
      (let ((history (yunge-jump--make-history)))
        (set-window-parameter window 'yunge-jump-history history)
        history)))

(defun yunge-jump--fallback ()
  "Return a way to reopen the current buffer, when one exists."
  (cond
   (buffer-file-name (cons 'file buffer-file-name))
   ((derived-mode-p 'dired-mode)
    (cons 'directory default-directory))))

(defun yunge-jump--entry (&optional position)
  "Return a jump entry for POSITION in its current buffer."
  (let ((marker
         (cond
          ((markerp position)
           (when (marker-buffer position)
             (copy-marker position)))
          ((integerp position) (copy-marker position))
          ((null position) (point-marker)))))
    (when-let* ((buffer (and marker (marker-buffer marker))))
      (unless (minibufferp buffer)
        (with-current-buffer buffer
          (yunge-jump--make-entry
           :marker marker
           :fallback (yunge-jump--fallback)
           :position (marker-position marker)))))))

(defun yunge-jump--window-entry (window)
  "Return an entry for WINDOW's current location."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (yunge-jump--entry (window-point window)))))

(defun yunge-jump--same-place-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same location."
  (let ((left-marker (yunge-jump--entry-marker left))
        (right-marker (yunge-jump--entry-marker right))
        (left-fallback (yunge-jump--entry-fallback left))
        (right-fallback (yunge-jump--entry-fallback right)))
    (if (and (marker-buffer left-marker)
             (marker-buffer right-marker))
        (and (eq (marker-buffer left-marker)
                 (marker-buffer right-marker))
             (= (marker-position left-marker)
                (marker-position right-marker)))
      (and left-fallback
           right-fallback
           (equal left-fallback right-fallback)
           (= (yunge-jump--entry-position left)
              (yunge-jump--entry-position right))))))

(defun yunge-jump--trim (entries)
  "Limit ENTRIES to `yunge-jump--max-length'."
  (when-let* ((last (nthcdr (1- yunge-jump--max-length) entries)))
    (setcdr last nil))
  entries)

(defun yunge-jump--push (history entry &optional branch)
  "Push ENTRY onto HISTORY.
When BRANCH is non-nil, discard entries newer than the current one."
  (let ((entries (yunge-jump--history-entries history))
        (index (yunge-jump--history-index history)))
    (when (and branch (>= index 0))
      (setq entries (nthcdr index entries)))
    (when branch
      (setf (yunge-jump--history-index history) -1))
    (unless (and entries
                 (yunge-jump--same-place-p entry (car entries)))
      (setq entries (cons entry entries)))
    (setf (yunge-jump--history-entries history)
          (yunge-jump--trim entries))))

(defun yunge-jump--record (&optional position)
  "Record Evil jump POSITION in the selected window's history."
  (unless yunge-jump--moving
    (when-let* ((entry (yunge-jump--entry position)))
      (yunge-jump--push (yunge-jump--history) entry t))))

(defun yunge-jump--track-navigation (function &rest arguments)
  "Record a successful location change made by FUNCTION with ARGUMENTS."
  (let* ((origin (yunge-jump--window-entry (selected-window)))
         ;; Ignore transient locations visited by completion previews.
         (yunge-jump--moving t)
         (result (apply function arguments))
         (window (selected-window))
         (destination (yunge-jump--window-entry window)))
    (when (and origin
               destination
               (not (yunge-jump--same-place-p origin destination)))
      (yunge-jump--push (yunge-jump--history window) origin t))
    result))

(defun yunge-jump-track-command (command)
  "Track successful location changes made by COMMAND."
  (unless (advice-member-p #'yunge-jump--track-navigation command)
    (advice-add command :around #'yunge-jump--track-navigation)))

(defun yunge-jump--target (entry)
  "Return the live buffer and position represented by ENTRY."
  (let* ((marker (yunge-jump--entry-marker entry))
         (buffer (marker-buffer marker)))
    (cond
     (buffer (cons buffer (marker-position marker)))
     ((pcase (yunge-jump--entry-fallback entry)
        (`(file . ,file)
         (when (file-exists-p file)
           (setq buffer (find-file-noselect file))))
        (`(directory . ,directory)
         (when (file-directory-p directory)
           (setq buffer (dired-noselect directory)))))
      (cons buffer (yunge-jump--entry-position entry))))))

(defun yunge-jump--visit (entry)
  "Visit ENTRY and return non-nil, or return nil if it is dead."
  (when-let* ((target (yunge-jump--target entry)))
    (let ((buffer (car target))
          (position (cdr target))
          (yunge-jump--moving t))
      ;; Prevent Evil's buffer-crossing hook from creating another jump.
      (setq evil--jumps-jump-command t)
      (switch-to-buffer buffer)
      (goto-char (min (max position (point-min)) (point-max)))
      (set-marker (yunge-jump--entry-marker entry)
                  (point) (current-buffer)))
    t))

(defun yunge-jump--move (history direction count description)
  "Move through HISTORY in DIRECTION COUNT times.
DESCRIPTION names the direction for an error message."
  (let ((entries (yunge-jump--history-entries history))
        (index (yunge-jump--history-index history))
        moved)
    (while (> count 0)
      (let ((candidate (+ index direction))
            found)
        (while (and (not found)
                    (<= 0 candidate)
                    (< candidate (length entries)))
          (if (yunge-jump--visit (nth candidate entries))
              (setq found t)
            (setq candidate (+ candidate direction))))
        (if found
            (progn
              (setq index candidate
                    moved t
                    count (1- count))
              (setf (yunge-jump--history-index history) index))
          (setq count 0))))
    (unless moved
      (user-error "No %s jump" description))))

(defun yunge-jump-backward (&optional count)
  "Go to the COUNTth older location in this window's jump history."
  (interactive "p")
  (let ((history (yunge-jump--history)))
    (when (< (yunge-jump--history-index history) 0)
      (when-let* ((entry (yunge-jump--entry)))
        (yunge-jump--push history entry)
        (setf (yunge-jump--history-index history) 0)))
    (yunge-jump--move history 1 (or count 1) "older")))

(defun yunge-jump-forward (&optional count)
  "Go to the COUNTth newer location in this window's jump history."
  (interactive "p")
  (yunge-jump--move (yunge-jump--history) -1
                    (or count 1) "newer"))

(defun yunge-jump--copy-after-split (function &rest arguments)
  "Call split FUNCTION with ARGUMENTS and copy its window history."
  (let* ((source (or (car arguments) (selected-window)))
         (history
          (and (window-live-p source)
               (window-parameter source 'yunge-jump-history)))
         (new-window (apply function arguments)))
    (when (and history (window-live-p new-window))
      (set-window-parameter
       new-window 'yunge-jump-history
       (yunge-jump--make-history
        :entries (copy-sequence
                  (yunge-jump--history-entries history))
        :index (yunge-jump--history-index history))))
    new-window))

(defun yunge-jump--setup ()
  "Replace Evil jump traversal with the window-local history."
  (advice-add 'evil-set-jump :before #'yunge-jump--record)
  (advice-add 'split-window :around #'yunge-jump--copy-after-split)
  (keymap-set (current-global-map)
              "<remap> <evil-jump-backward>"
              #'yunge-jump-backward)
  (keymap-set (current-global-map)
              "<remap> <evil-jump-forward>"
              #'yunge-jump-forward))

(with-eval-after-load 'evil
  (yunge-jump--setup))

(provide 'yunge-jump)

;;; yunge-jump.el ends here
