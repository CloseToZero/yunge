;;; yunge-org-reveal.el --- Reveal Org markup -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Keep Org's rendered text presentation while revealing the source syntax of
;; the element at point.  View mode retains presentation even at point, and
;; LaTeX fragments remain under Shuying's independent preview lifecycle.

;;; Code:

(require 'org)
(require 'org-element)

(defvar org-hidden-keywords)
(defvar org-hide-emphasis-markers)
(defvar org-link-descriptive)
(defvar org-pretty-entities)
(defvar org-pretty-entities-include-sub-superscripts)
(defvar org-use-sub-superscripts)
(defvar view-mode)

(defconst yunge-org-reveal--element-types
  '(bold code entity italic keyword link strike-through subscript
         superscript underline verbatim)
  "Org element types whose hidden source can be revealed.")

(defconst yunge-org-reveal--emphasis-types
  '(bold code italic strike-through underline verbatim)
  "Org element types controlled by `org-hide-emphasis-markers'.")

(defvar-local yunge-org-reveal--active-beginning nil
  "Marker at the beginning of the source currently revealed at point.")

(defvar-local yunge-org-reveal--active-end nil
  "Marker after the source currently revealed at point.")

(defvar-local yunge-org-reveal--active-type nil
  "Type of the Org element currently revealed at point.")

(defun yunge-org-reveal--viewing-p ()
  "Return whether the current buffer should retain its rendered view."
  (bound-and-true-p view-mode))

(defun yunge-org-reveal--element-end (element)
  "Return the end of ELEMENT before its trailing whitespace."
  (- (org-element-end element)
     (or (org-element-post-blank element) 0)))

(defun yunge-org-reveal--inside-latex-p (element)
  "Return whether ELEMENT belongs to an Org LaTeX fragment."
  (org-element-lineage
   element '(latex-fragment latex-environment) t))

(defun yunge-org-reveal--latex-entity-at-point ()
  "Return a composed entity at point inside LaTeX, or nil."
  (when (and org-pretty-entities
             (yunge-org-reveal--inside-latex-p
              (org-element-context)))
    (let* ((composition (find-composition (point)))
           (beginning
            (if composition
                (nth 0 composition)
              (and (eq yunge-org-reveal--active-type 'entity)
                   (marker-position
                    yunge-org-reveal--active-beginning))))
           (end
            (if composition
                (nth 1 composition)
              (and beginning
                   (marker-position yunge-org-reveal--active-end)))))
      (when (and beginning end
                 (<= beginning (point))
                 (< (point) end))
        (org-element-create
         'entity
         (list :begin beginning :end end :post-blank 0))))))

(defun yunge-org-reveal--eligible-p (element)
  "Return whether ELEMENT has presentation syntax to reveal."
  (let ((type (org-element-type element)))
    (and
     (not (yunge-org-reveal--inside-latex-p element))
     (pcase type
       ((pred (lambda (candidate)
                (memq candidate yunge-org-reveal--emphasis-types)))
        org-hide-emphasis-markers)
       ('link
        (and org-link-descriptive
             (eq (org-element-property :format element) 'bracket)))
       ('entity org-pretty-entities)
       ((or 'subscript 'superscript)
        (and org-pretty-entities
             org-pretty-entities-include-sub-superscripts
             (or (eq org-use-sub-superscripts t)
                 (org-element-property :use-brackets-p element))))
       ('keyword
        (memq (intern
               (downcase (org-element-property :key element)))
              org-hidden-keywords))
       (_ nil)))))

(defun yunge-org-reveal--element-at-point ()
  "Return the revealable Org element containing point, or nil."
  (or
   (yunge-org-reveal--latex-entity-at-point)
   (when-let* ((context (org-element-context))
               (element
                (org-element-lineage
                 context yunge-org-reveal--element-types t))
               ((yunge-org-reveal--eligible-p element))
               ((< (point) (yunge-org-reveal--element-end element))))
     element)))

(defun yunge-org-reveal--refontify (beginning end)
  "Restore Org presentation properties between BEGINNING and END."
  (save-restriction
    (widen)
    (let ((beginning (max (point-min) beginning))
          (end (min (point-max) end)))
      (when (< beginning end)
        (font-lock-flush beginning end)
        (font-lock-ensure beginning end)))))

(defun yunge-org-reveal--clear-active ()
  "Restore and forget the source currently revealed at point."
  (when (and (markerp yunge-org-reveal--active-beginning)
             (marker-position yunge-org-reveal--active-beginning)
             (markerp yunge-org-reveal--active-end)
             (marker-position yunge-org-reveal--active-end))
    (let ((beginning
           (marker-position yunge-org-reveal--active-beginning))
          (end (marker-position yunge-org-reveal--active-end)))
      (set-marker yunge-org-reveal--active-beginning nil)
      (set-marker yunge-org-reveal--active-end nil)
      (setq yunge-org-reveal--active-type nil)
      (yunge-org-reveal--refontify beginning end))))

(defun yunge-org-reveal--same-element-p (element)
  "Return whether ELEMENT is the source currently revealed at point."
  (and (markerp yunge-org-reveal--active-beginning)
       (marker-position yunge-org-reveal--active-beginning)
       (= (marker-position yunge-org-reveal--active-beginning)
          (org-element-begin element))
       (eq yunge-org-reveal--active-type (org-element-type element))))

(defun yunge-org-reveal--remember (element)
  "Remember ELEMENT as the source currently revealed at point."
  (unless (markerp yunge-org-reveal--active-beginning)
    (setq yunge-org-reveal--active-beginning (make-marker)))
  (unless (markerp yunge-org-reveal--active-end)
    (setq yunge-org-reveal--active-end (make-marker))
    (set-marker-insertion-type yunge-org-reveal--active-end t))
  (set-marker yunge-org-reveal--active-beginning
              (org-element-begin element) (current-buffer))
  (set-marker yunge-org-reveal--active-end
              (yunge-org-reveal--element-end element) (current-buffer))
  (setq yunge-org-reveal--active-type (org-element-type element)))

(defun yunge-org-reveal--show (element)
  "Remove presentation properties that conceal the source of ELEMENT."
  (let ((beginning (org-element-begin element))
        (end (yunge-org-reveal--element-end element))
        (type (org-element-type element)))
    ;; Finish pending lazy fontification before selectively removing its
    ;; presentation properties.  Otherwise redisplay can immediately add them
    ;; back after this command.
    (font-lock-ensure beginning end)
    (with-silent-modifications
      (pcase type
        ('entity
         (decompose-region beginning end))
        ((or 'subscript 'superscript)
         (remove-text-properties
          beginning end '(display nil invisible nil)))
        (_
         (remove-text-properties beginning end '(invisible nil)))))
    (yunge-org-reveal--remember element)))

(defun yunge-org-reveal--sync ()
  "Synchronize Org markup presentation with point and View mode."
  (if (yunge-org-reveal--viewing-p)
      (yunge-org-reveal--clear-active)
    (if-let* ((element (yunge-org-reveal--element-at-point)))
        (progn
          (unless (yunge-org-reveal--same-element-p element)
            (yunge-org-reveal--clear-active))
          ;; Editing can refontify an element without moving point.  Reveal it
          ;; again and refresh the remembered end after every command.
          (yunge-org-reveal--show element))
      (yunge-org-reveal--clear-active))))

(defun yunge-org-reveal--before-command ()
  "Restore presentation before commands that depend on display columns."
  (when (memq this-command '(org-ctrl-c-ctrl-c org-fill-paragraph))
    (yunge-org-reveal--clear-active)))

;;;###autoload
(define-minor-mode yunge-org-reveal-mode
  "Reveal hidden Org markup at point while editing.

View mode always retains the rendered presentation, including for the element
at point.  LaTeX previews remain managed by `shuying-org-mode', while composed
entities in revealed LaTeX source are restored to their literal spelling."
  :lighter nil
  (if yunge-org-reveal-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq yunge-org-reveal-mode nil)
          (user-error "Org reveal mode requires an Org buffer"))
        (add-hook 'post-command-hook #'yunge-org-reveal--sync nil t)
        (add-hook 'pre-command-hook
                  #'yunge-org-reveal--before-command nil t)
        (add-hook 'view-mode-hook #'yunge-org-reveal--sync nil t)
        (yunge-org-reveal--sync))
    (remove-hook 'post-command-hook #'yunge-org-reveal--sync t)
    (remove-hook 'pre-command-hook #'yunge-org-reveal--before-command t)
    (remove-hook 'view-mode-hook #'yunge-org-reveal--sync t)
    (yunge-org-reveal--clear-active)))

(provide 'yunge-org-reveal)

;;; yunge-org-reveal.el ends here
