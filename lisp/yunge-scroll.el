;;; yunge-scroll.el --- Scrolling behavior -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'mwheel)

;; Keep point at the window edge instead of recentering the window when
;; ordinary motion carries it just beyond the visible text.  Values above 100
;; have the special meaning of never recentering, so 101 selects that behavior
;; rather than specifying a 101-line scroll limit.
(setq scroll-conservatively 101)

(defun yunge-scroll-mwheel (event &optional argument)
  "Scroll EVENT without needlessly moving a still-visible point.
Optional ARGUMENT retains the horizontal scrolling step accepted by
`mwheel-scroll'."
  (interactive (list last-input-event current-prefix-arg))
  (let* ((window
          (if mouse-wheel-follow-mouse
              (mwheel-event-window event)
            (selected-window)))
         (ordinary-scroll-p
          (and (eq mwheel-scroll-up-function 'scroll-up)
               (eq mwheel-scroll-down-function 'scroll-down)))
         (buffer (and (window-live-p window) (window-buffer window)))
         (old-point (and buffer (window-point window))))
    (mwheel-scroll event argument)
    (when (and ordinary-scroll-p
               (window-live-p window)
               (eq (window-buffer window) buffer)
               (/= (window-point window) old-point)
               (pos-visible-in-window-p old-point window t))
      (set-window-point window old-point))))

(define-key global-map [remap mwheel-scroll] #'yunge-scroll-mwheel)

(provide 'yunge-scroll)

;;; yunge-scroll.el ends here
