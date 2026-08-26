;;; yunge-navigation.el --- Jump landing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'pulse)
(require 'window)

(defface yunge-navigation-pulse
  '((t :inherit highlight :extend t))
  "Face used to reveal a text navigation target."
  :group 'yunge)

(defvar-local yunge-navigation-landing-policy 'text
  "How navigation should present a destination in the current buffer.
The value `text' adaptively centers and pulses point.  Other values mean the
buffer owns its viewport and destination feedback.")

(defvar yunge-navigation--landed-location nil
  "Location already presented during the current command.")

(defun yunge-navigation-reset-command ()
  "Forget the location presented by the preceding command."
  (setq yunge-navigation--landed-location nil))

(defun yunge-navigation--location (window)
  "Return the current location displayed by WINDOW."
  (when (window-live-p window)
    (with-selected-window window
      (list window (current-buffer) (point)))))

(defun yunge-navigation-landed-p (window)
  "Return non-nil when WINDOW's current location was already presented."
  (equal yunge-navigation--landed-location
         (yunge-navigation--location window)))

(defun yunge-navigation--visual-line-bounds ()
  "Return the bounds of the visual line at point."
  (save-excursion
    (let ((start (progn (vertical-motion 0) (point)))
          (end (progn (vertical-motion 1) (point))))
      (when (= start end)
        (setq start (line-beginning-position 0)))
      (cons start end))))

(defun yunge-navigation--pulse (window &optional beginning end)
  "Pulse BEGINNING through END in WINDOW, or its visual line at point."
  (pcase-let* ((`(,start . ,finish)
                 (if (and beginning end)
                     (cons beginning end)
                   (yunge-navigation--visual-line-bounds)))
                (overlay (make-overlay start finish)))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'pulse-delete t)
    (pulse-momentary-highlight-overlay
     overlay 'yunge-navigation-pulse)))

(defun yunge-navigation-land (&optional window beginning end)
  "Present the navigation destination in WINDOW.
Keep a fully visible text target in place; otherwise center it.  Pulse the
visual line at point unless BEGINNING and END identify a more precise locus.
Buffers whose `yunge-navigation-landing-policy' is not `text' retain control
of their own viewport and feedback."
  (setq window (or window (selected-window)))
  (when (window-live-p window)
    (with-selected-window window
      (setq yunge-navigation--landed-location
            (list window (current-buffer) (point)))
      (when (eq yunge-navigation-landing-policy 'text)
        (unless (pos-visible-in-window-group-p (point) window)
          (recenter-window-group))
        (yunge-navigation--pulse window beginning end)))))

(provide 'yunge-navigation)

;;; yunge-navigation.el ends here
