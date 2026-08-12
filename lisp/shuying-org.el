;;; shuying-org.el --- Shuying previews in Org -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'org)
(require 'org-element)
(require 'seq)
(require 'shuying-latex)

(declare-function org-export-get-backend "ox" (name))
(declare-function org-export-get-environment
                  "ox" (&optional backend subtreep ext-plist))
(declare-function org-latex-make-preamble
                  "ox-latex" (info &optional template snippetp))

(defvar-local shuying-org--active-start nil
  "Marker at the fragment currently containing point.")

(defvar-local shuying-org--previous-point nil
  "Point before the most recent command.")

(defvar-local shuying-org--visible-window-state nil
  "Last window state populated with Shuying previews.")

(defvar-local shuying-org--changed-overlays nil
  "Preview overlays modified by the current command.")

(defun shuying-org--fragment-bounds (fragment)
  "Return the source bounds of Org LaTeX FRAGMENT."
  (cons
   (org-element-begin fragment)
   (- (org-element-end fragment)
      (or (org-element-property :post-blank fragment) 0))))

(defun shuying-org--latex-fragment-p (datum)
  "Return whether Org DATUM is previewable LaTeX."
  (org-element-type-p datum '(latex-fragment latex-environment)))

(defun shuying-org--fragment-at-position (position)
  "Return the Org LaTeX fragment containing POSITION, or nil."
  (when (and position
             (<= (point-min) position)
             (< position (point-max)))
    (save-excursion
      (goto-char position)
      (let ((fragment (org-element-context)))
        (when (shuying-org--latex-fragment-p fragment)
          (pcase-let ((`(,beginning . ,end)
                       (shuying-org--fragment-bounds fragment)))
            (when (and (<= beginning position) (< position end))
              fragment)))))))

(defun shuying-org--fragment-at-point ()
  "Return the Org LaTeX fragment containing point, or nil."
  (shuying-org--fragment-at-position (point)))

(defun shuying-org--fragment-overlays (beginning end)
  "Return Shuying overlays between BEGINNING and END."
  (seq-filter
   (lambda (overlay)
     (overlay-get overlay 'shuying-org))
   (overlays-in beginning end)))

(defun shuying-org--fragment-overlay (fragment)
  "Return the Shuying overlay for FRAGMENT, or nil."
  (pcase-let ((`(,beginning . ,end)
               (shuying-org--fragment-bounds fragment)))
    (seq-find
     (lambda (overlay)
       (and (= (overlay-start overlay) beginning)
            (= (overlay-end overlay) end)))
     (shuying-org--fragment-overlays beginning end))))

(defun shuying-org--modified
    (overlay after _beginning _end &optional _length)
  "Mark OVERLAY dirty after its source is modified."
  (overlay-put overlay 'display nil)
  (overlay-put overlay 'shuying-org-dirty t)
  (when after
    ;; Invalidate a render that began before this edit.
    (overlay-put
     overlay 'shuying-org-generation
     (1+ (or (overlay-get overlay 'shuying-org-generation) 0)))
    (cl-pushnew overlay shuying-org--changed-overlays)))

(defun shuying-org--ensure-overlay (fragment)
  "Return the display overlay for FRAGMENT, creating it if needed."
  (pcase-let* ((`(,beginning . ,end)
                (shuying-org--fragment-bounds fragment))
               (overlay (shuying-org--fragment-overlay fragment)))
    (dolist (candidate (overlays-in beginning end))
      (when (eq (overlay-get candidate 'org-overlay-type)
                'org-latex-overlay)
        (delete-overlay candidate)))
    (unless overlay
      (setq overlay (make-overlay beginning end nil nil nil))
      (overlay-put overlay 'shuying-org t)
      (overlay-put overlay 'modification-hooks
                   '(shuying-org--modified)))
    (move-overlay overlay beginning end)
    overlay))

(defun shuying-org--face-color (option attribute)
  "Resolve Org LaTeX color OPTION for face ATTRIBUTE at point."
  (let ((value (plist-get org-format-latex-options option))
        (face (face-at-point))
        resolved)
    (setq resolved
          (cond
           ((eq value 'auto)
            (face-attribute face attribute nil 'default))
           ((eq value 'default)
            (face-attribute 'default attribute nil 'default))
           (t value)))
    (if (or (not (stringp resolved))
            (string-prefix-p "unspecified" resolved))
        (if (eq attribute :foreground) "Black" "Transparent")
      resolved)))

(defun shuying-org--preamble ()
  "Return the LaTeX preamble for the current Org buffer."
  (require 'ox-latex)
  (org-latex-make-preamble
   (org-export-get-environment (org-export-get-backend 'latex))
   org-format-latex-header
   'snippet))

(defun shuying-org--render-spec (fragment)
  "Return the Shuying render specification for Org FRAGMENT."
  (let ((bounds (shuying-org--fragment-bounds fragment)))
    (save-excursion
      (goto-char (car bounds))
      (make-shuying-render-spec
       :source
       (substring-no-properties
        (org-element-property :value fragment))
       :preamble (shuying-org--preamble)
       :engine shuying-latex-engine-command
       :backend 'shuying-latex
       :backend-options
       (list :converter shuying-latex-converter-command)
       :output-format "svg"
       :foreground
       (shuying-org--face-color :foreground :foreground)
       :background
       (shuying-org--face-color :background :background)
       ;; Preserve Org's established dvisvgm preview size.  Its process
       ;; definition applies this adjustment before the user scale.
       :scale (* 1.7
                 (or (plist-get org-format-latex-options :scale) 1.0))
       :cache-version shuying-cache-format-version))))

(defun shuying-org--point-inside-overlay-p (overlay)
  "Return whether point is inside OVERLAY."
  (and (<= (overlay-start overlay) (point))
       (< (point) (overlay-end overlay))))

(defun shuying-org--show-overlay (overlay)
  "Show the cached image belonging to OVERLAY."
  (when-let* ((image (overlay-get overlay 'shuying-org-image)))
    (overlay-put overlay 'display image)))

(defun shuying-org--finish-render
    (buffer overlay generation artifact error-data report-error)
  "Finish rendering OVERLAY in BUFFER for GENERATION.
ARTIFACT is the completed cache file, or nil when ERROR-DATA is non-nil.
REPORT-ERROR reports a current render failure without duplicating its batch."
  (when (and (buffer-live-p buffer)
             (overlay-buffer overlay)
             (= generation
                (overlay-get overlay 'shuying-org-generation)))
    (with-current-buffer buffer
      (if error-data
          (progn
            (overlay-put overlay 'display nil)
            (overlay-put overlay 'shuying-org-dirty t)
            (overlay-put overlay 'shuying-org-error error-data)
            (funcall report-error error-data))
        (let ((image (create-image artifact nil nil :ascent 'center)))
          (overlay-put overlay 'shuying-org-artifact artifact)
          (overlay-put overlay 'shuying-org-image image)
          (overlay-put overlay 'shuying-org-dirty nil)
          (overlay-put overlay 'shuying-org-error nil)
          (if (shuying-org--point-inside-overlay-p overlay)
              (overlay-put overlay 'display nil)
            (shuying-org--show-overlay overlay)))))))

(defun shuying-org--render-request (fragment report-error)
  "Return a Shuying render request for Org FRAGMENT."
  (let* ((overlay (shuying-org--ensure-overlay fragment))
         (generation
          (1+ (or (overlay-get overlay 'shuying-org-generation) 0)))
         (buffer (current-buffer))
         (specification (shuying-org--render-spec fragment)))
    (overlay-put overlay 'display nil)
    (overlay-put overlay 'shuying-org-generation generation)
    (cons
     specification
     (lambda (artifact error-data)
       (shuying-org--finish-render
        buffer overlay generation artifact error-data report-error)))))

(defun shuying-org--preview-fragments (fragments)
  "Request previews for Org FRAGMENTS as one render group."
  (let (error-reported)
    (shuying-render-batch
     (mapcar
      (lambda (fragment)
        (shuying-org--render-request
         fragment
         (lambda (error-data)
           (unless error-reported
             (setq error-reported t)
             (display-warning
              'shuying (error-message-string error-data) :error)))))
      fragments))))

(defun shuying-org--preview-fragment (fragment)
  "Request a preview for Org FRAGMENT."
  (shuying-org--preview-fragments (list fragment)))

(defun shuying-org--leave-fragment (fragment)
  "Show or refresh Org FRAGMENT after point leaves it."
  (if-let* ((overlay (shuying-org--fragment-overlay fragment)))
      (let ((artifact
             (overlay-get overlay 'shuying-org-artifact)))
        (if (and (not (overlay-get overlay 'shuying-org-dirty))
                 artifact
                 (file-exists-p artifact))
            (shuying-org--show-overlay overlay)
          (shuying-org--preview-fragment fragment)))
    (shuying-org--preview-fragment fragment)))

(defun shuying-org--enter-fragment (fragment)
  "Reveal the source of Org FRAGMENT."
  (when-let* ((overlay (shuying-org--fragment-overlay fragment)))
    (overlay-put overlay 'display nil)))

(defun shuying-org--set-active-fragment (fragment)
  "Remember FRAGMENT as the one containing point."
  (unless (markerp shuying-org--active-start)
    (setq shuying-org--active-start (make-marker)))
  (set-marker
   shuying-org--active-start
   (org-element-begin fragment)
   (current-buffer)))

(defun shuying-org--clear-active-fragment ()
  "Forget the fragment previously containing point."
  (when (markerp shuying-org--active-start)
    (set-marker shuying-org--active-start nil))
  (setq shuying-org--active-start nil))

(defun shuying-org--post-command ()
  "Update preview visibility after point moves or source changes."
  (let* ((current (shuying-org--fragment-at-point))
         (current-start (and current (org-element-begin current)))
         (active-position
          (and (markerp shuying-org--active-start)
               (marker-position shuying-org--active-start)))
         (changed-overlays
          (prog1 shuying-org--changed-overlays
            (setq shuying-org--changed-overlays nil)))
         processed-starts)
    (unless (equal current-start active-position)
      (when active-position
        (if-let* ((active
                   (shuying-org--fragment-at-position
                    active-position)))
            (progn
              (push (org-element-begin active) processed-starts)
              (shuying-org--leave-fragment active))
          (dolist (overlay
                   (shuying-org--fragment-overlays
                    active-position (1+ active-position)))
            (delete-overlay overlay))))
      (shuying-org--clear-active-fragment)
      (when current
        (shuying-org--enter-fragment current)
        (shuying-org--set-active-fragment current)))
    (unless current
      (when-let* ((completed
                  (shuying-org--fragment-at-position
                   shuying-org--previous-point)))
        (unless (memq (org-element-begin completed) processed-starts)
          (push (org-element-begin completed) processed-starts)
          (shuying-org--leave-fragment completed))))
    ;; Undo and programmatic edits can modify a preview while point remains
    ;; outside it, so they cannot rely on a later cursor-leave transition.
    (dolist (overlay changed-overlays)
      (when (overlay-buffer overlay)
        (if-let* ((fragment
                   (shuying-org--fragment-at-position
                    (overlay-start overlay))))
            (pcase-let ((`(,beginning . ,end)
                         (shuying-org--fragment-bounds fragment)))
              (unless (or (memq beginning processed-starts)
                          (and (<= beginning (point))
                               (< (point) end)))
                (push beginning processed-starts)
                (shuying-org--leave-fragment fragment)))
          (delete-overlay overlay))))
    (setq shuying-org--previous-point (point))))

(defun shuying-org--fragments-in-region (beginning end)
  "Return Org LaTeX fragments between BEGINNING and END."
  (save-restriction
    (narrow-to-region beginning end)
    (org-element-map
        (org-element-parse-buffer)
        '(latex-fragment latex-environment)
      #'identity)))

(defun shuying-org--preview-region (beginning end)
  "Preview Org LaTeX fragments between BEGINNING and END."
  (shuying-org--preview-fragments
   (shuying-org--fragments-in-region beginning end)))

(defun shuying-org--visible-ranges ()
  "Return the ranges visible in windows showing the current buffer."
  (mapcar
   (lambda (window)
     (cons (window-start window)
           (or (window-end window t) (point-max))))
   (get-buffer-window-list (current-buffer) nil t)))

(defun shuying-org--window-state ()
  "Return state sufficient to notice changes to visible buffer ranges."
  (mapcar
   (lambda (window)
     (list window (window-start window) (window-body-height window)))
   (get-buffer-window-list (current-buffer) nil t)))

(defun shuying-org--preview-visible-windows ()
  "Populate previews when the visible windows change."
  (let ((window-state (shuying-org--window-state)))
    (unless (equal window-state shuying-org--visible-window-state)
      (setq shuying-org--visible-window-state window-state)
      (when (and (bound-and-true-p shuying-org-mode) window-state)
        (let ((ranges (shuying-org--visible-ranges))
              fragments)
          ;; Parse the complete buffer so an environment crossing a window
          ;; edge is still discovered, but only submit visible fragments.
          (dolist (fragment
                   (shuying-org--fragments-in-region
                    (point-min) (point-max)))
            (pcase-let ((`(,beginning . ,end)
                         (shuying-org--fragment-bounds fragment)))
              (when (and
                     (not (shuying-org--fragment-overlay fragment))
                     (seq-some
                      (lambda (range)
                        (and (< beginning (cdr range))
                             (< (car range) end)))
                      ranges))
                (push fragment fragments))))
          (shuying-org--preview-fragments (nreverse fragments)))))))

;;;###autoload
(defun shuying-org-startup ()
  "Populate previews as windows expose parts of the current Org buffer."
  (setq shuying-org--visible-window-state nil)
  (add-hook 'post-command-hook
            #'shuying-org--preview-visible-windows nil t))

(defun shuying-org--clear-region (beginning end)
  "Remove Shuying overlays between BEGINNING and END."
  (dolist (overlay (shuying-org--fragment-overlays beginning end))
    (delete-overlay overlay)))

(defun shuying-org--section-bounds ()
  "Return the current Org section bounds."
  (cons
   (if (org-before-first-heading-p)
       (point-min)
     (save-excursion
       (org-with-limited-levels
        (org-back-to-heading t)
        (point))))
   (org-with-limited-levels (org-entry-end-position))))

;;;###autoload
(defun shuying-org-preview-buffer ()
  "Preview every Org LaTeX fragment in the current buffer."
  (interactive)
  (shuying-org--preview-region (point-min) (point-max)))

;;;###autoload
(defun shuying-org-clear-buffer ()
  "Remove every Shuying preview from the current buffer."
  (interactive)
  (shuying-org--clear-region (point-min) (point-max)))

;;;###autoload
(defun shuying-org-preview (&optional argument)
  "Preview or clear Org LaTeX in the current context.
With one universal prefix, clear the region or current section.  With two,
preview the whole buffer.  With three, clear the whole buffer."
  (interactive "P")
  (cond
   ((equal argument '(64))
    (shuying-org-clear-buffer))
   ((equal argument '(16))
    (shuying-org-preview-buffer))
   ((equal argument '(4))
    (pcase-let ((`(,beginning . ,end)
                 (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (shuying-org--section-bounds))))
      (shuying-org--clear-region beginning end)))
   ((use-region-p)
    (shuying-org--preview-region
     (region-beginning) (region-end)))
   (t
    (if-let* ((fragment (shuying-org--fragment-at-point)))
        (shuying-org--preview-fragment fragment)
      (pcase-let ((`(,beginning . ,end)
                   (shuying-org--section-bounds)))
        (shuying-org--preview-region beginning end))))))

;;;###autoload
(define-minor-mode shuying-org-mode
  "Reveal Org LaTeX source at point and preview it after leaving."
  :lighter " Shuying"
  (if shuying-org-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq shuying-org-mode nil)
          (user-error "Shuying Org mode requires an Org buffer"))
        (setq shuying-org--previous-point (point))
        (add-hook 'post-command-hook
                  #'shuying-org--post-command nil t))
    (remove-hook 'post-command-hook #'shuying-org--post-command t)
    (remove-hook 'post-command-hook
                 #'shuying-org--preview-visible-windows t)
    (setq shuying-org--visible-window-state nil)
    (setq shuying-org--changed-overlays nil)
    (shuying-org--clear-active-fragment)
    (shuying-org-clear-buffer)))

(provide 'shuying-org)

;;; shuying-org.el ends here
