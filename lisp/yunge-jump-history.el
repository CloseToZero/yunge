;;; yunge-jump-history.el --- Window-local jump history -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(declare-function dired-noselect "dired")

(defvar evil--jumps-jump-command)

(defconst yunge-jump-history--max-length 100)
(defvar yunge-jump-history--moving nil)

(cl-defstruct (yunge-jump-history--entry
               (:constructor yunge-jump-history--make-entry))
  marker
  fallback
  position)

(cl-defstruct (yunge-jump-history--history
               (:constructor yunge-jump-history--make-history))
  entries
  (index -1))

(defun yunge-jump-history--history (&optional window)
  "Return the jump history for WINDOW, creating it when needed."
  (setq window (or window (selected-window)))
  (or (window-parameter window 'yunge-jump-history)
      (let ((history (yunge-jump-history--make-history)))
        (set-window-parameter window 'yunge-jump-history history)
        history)))

(defun yunge-jump-history--fallback ()
  "Return a way to reopen the current buffer, when one exists."
  (cond
   (buffer-file-name (cons 'file buffer-file-name))
   ((derived-mode-p 'dired-mode)
    (cons 'directory default-directory))))

(defun yunge-jump-history--entry (&optional position)
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
          (yunge-jump-history--make-entry
           :marker marker
           :fallback (yunge-jump-history--fallback)
           :position (marker-position marker)))))))

(defun yunge-jump-history--window-entry (window)
  "Return an entry for WINDOW's current location."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (yunge-jump-history--entry (window-point window)))))

(defun yunge-jump-history--same-place-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same location."
  (let ((left-marker (yunge-jump-history--entry-marker left))
        (right-marker (yunge-jump-history--entry-marker right))
        (left-fallback (yunge-jump-history--entry-fallback left))
        (right-fallback (yunge-jump-history--entry-fallback right)))
    (if (and (marker-buffer left-marker)
             (marker-buffer right-marker))
        (and (eq (marker-buffer left-marker)
                 (marker-buffer right-marker))
             (= (marker-position left-marker)
                (marker-position right-marker)))
      (and left-fallback
           right-fallback
           (equal left-fallback right-fallback)
           (= (yunge-jump-history--entry-position left)
              (yunge-jump-history--entry-position right))))))

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
  (unless yunge-jump-history--moving
    (when-let* ((entry (yunge-jump-history--entry position)))
      (yunge-jump-history--push (yunge-jump-history--history) entry t))))

(defun yunge-jump-history--track-navigation (function &rest arguments)
  "Record a successful location change made by FUNCTION with ARGUMENTS."
  (let* ((origin (yunge-jump-history--window-entry (selected-window)))
         ;; Ignore transient locations visited by completion previews.
         (yunge-jump-history--moving t)
         (result (apply function arguments))
         (window (selected-window))
         (destination (yunge-jump-history--window-entry window)))
    (when (and origin
               destination
               (not (yunge-jump-history--same-place-p origin destination)))
      (yunge-jump-history--push (yunge-jump-history--history window) origin t))
    result))

(defun yunge-jump-history-track-command (command)
  "Track successful location changes made by COMMAND."
  (unless (advice-member-p #'yunge-jump-history--track-navigation command)
    (advice-add command :around #'yunge-jump-history--track-navigation)))

(defun yunge-jump-history--target (entry)
  "Return the live buffer and position represented by ENTRY."
  (let* ((marker (yunge-jump-history--entry-marker entry))
         (buffer (marker-buffer marker)))
    (cond
     (buffer (cons buffer (marker-position marker)))
     ((pcase (yunge-jump-history--entry-fallback entry)
        (`(file . ,file)
         (when (file-exists-p file)
           (setq buffer (find-file-noselect file))))
        (`(directory . ,directory)
         (when (file-directory-p directory)
           (setq buffer (dired-noselect directory)))))
      (cons buffer (yunge-jump-history--entry-position entry))))))

(defun yunge-jump-history--visit (entry)
  "Visit ENTRY and return non-nil, or return nil if it is dead."
  (when-let* ((target (yunge-jump-history--target entry)))
    (let ((buffer (car target))
          (position (cdr target))
          (yunge-jump-history--moving t))
      ;; Prevent Evil's buffer-crossing hook from creating another jump.
      (setq evil--jumps-jump-command t)
      (switch-to-buffer buffer)
      (goto-char (min (max position (point-min)) (point-max)))
      (set-marker (yunge-jump-history--entry-marker entry)
                  (point) (current-buffer)))
    t))

(defun yunge-jump-history--move (history direction count description)
  "Move through HISTORY in DIRECTION COUNT times.
DESCRIPTION names the direction for an error message."
  (let ((entries (yunge-jump-history--history-entries history))
        (index (yunge-jump-history--history-index history))
        moved)
    (while (> count 0)
      (let ((candidate (+ index direction))
            found)
        (while (and (not found)
                    (<= 0 candidate)
                    (< candidate (length entries)))
          (if (yunge-jump-history--visit (nth candidate entries))
              (setq found t)
            (setq candidate (+ candidate direction))))
        (if found
            (progn
              (setq index candidate
                    moved t
                    count (1- count))
              (setf (yunge-jump-history--history-index history) index))
          (setq count 0))))
    (unless moved
      (user-error "No %s jump" description))))

(defun yunge-jump-history-backward (&optional count)
  "Go to the COUNTth older location in this window's jump history."
  (interactive "p")
  (let ((history (yunge-jump-history--history)))
    (when (< (yunge-jump-history--history-index history) 0)
      (when-let* ((entry (yunge-jump-history--entry)))
        (yunge-jump-history--push history entry)
        (setf (yunge-jump-history--history-index history) 0)))
    (yunge-jump-history--move history 1 (or count 1) "older")))

(defun yunge-jump-history-forward (&optional count)
  "Go to the COUNTth newer location in this window's jump history."
  (interactive "p")
  (yunge-jump-history--move (yunge-jump-history--history) -1
                    (or count 1) "newer"))

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

(provide 'yunge-jump-history)

;;; yunge-jump-history.el ends here
