;;; yunge-org-shift.el --- Org-aware Evil shifting -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'evil)
(require 'org)
(require 'org-list)
(require 'yunge-key)

(defconst yunge-org-shift-bindings
  '(("<" yunge-org-shift-left "promote or shift left")
    (">" yunge-org-shift-right "demote or shift right")))

(defun yunge-org--shift-headings (beginning end count command)
  "Apply heading COMMAND COUNT times between BEGINNING and END."
  (org-map-region
   (lambda ()
     (dotimes (_ count)
       (funcall command)))
   beginning end))

(defun yunge-org--multi-line-range-p (beginning end)
  "Return non-nil when BEGINNING to END spans multiple lines."
  (save-excursion
    (goto-char beginning)
    (> end (line-beginning-position 2))))

(defun yunge-org--shift-list (beginning end count direction)
  "Shift Org list items between BEGINNING and END COUNT times.
DIRECTION is positive for indentation and negative for outdentation."
  (let ((begin-marker (copy-marker beginning))
        (end-marker (copy-marker end t))
        (region-p (or (org-region-active-p)
                      (yunge-org--multi-line-range-p beginning end))))
    (unwind-protect
        (save-excursion
          (dotimes (_ count)
            (let ((structure
                   (save-excursion
                     (goto-char begin-marker)
                     (org-list-struct))))
              (if region-p
                  (save-mark-and-excursion
                    (let ((transient-mark-mode t))
                      (goto-char end-marker)
                      (set-mark begin-marker)
                      (activate-mark)
                      (org-list-indent-item-generic
                       direction t structure)))
                (goto-char begin-marker)
                (org-list-indent-item-generic
                 direction nil structure)))))
      (set-marker begin-marker nil)
      (set-marker end-marker nil))))

(defun yunge-org--shift (beginning end count direction)
  "Shift the Org structure or text between BEGINNING and END.
COUNT is the number of structural levels or indentation widths.
DIRECTION is positive for right and negative for left."
  (let ((count (or count 1)))
    (cond
     ((save-excursion
        (goto-char beginning)
        (org-at-heading-p))
      (yunge-org--shift-headings
       beginning end count
       (if (> direction 0) #'org-do-demote #'org-do-promote)))
     ((save-excursion
        (goto-char beginning)
        (org-at-item-p))
      (yunge-org--shift-list beginning end count direction))
     ((> direction 0)
      (evil-shift-right beginning end count))
     (t
      (evil-shift-left beginning end count)))))

(evil-define-operator yunge-org-shift-right
    (beginning end count)
  "Demote Org structures or shift text between BEGINNING and END right."
  :type line
  :move-point nil
  (interactive "<r><vc>")
  (yunge-org--shift beginning end count 1))

(evil-define-operator yunge-org-shift-left
    (beginning end count)
  "Promote Org structures or shift text between BEGINNING and END left."
  :type line
  :move-point nil
  (interactive "<r><vc>")
  (yunge-org--shift beginning end count -1))

(yunge-key-evil-define '(normal visual) org-mode-map
                       yunge-org-shift-bindings)

(provide 'yunge-org-shift)

;;; yunge-org-shift.el ends here
