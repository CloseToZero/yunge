;;; yunge-pair.el --- Paired delimiter insertion -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)

(declare-function evil-delete-backward-char-and-join
                  "evil-commands" (count))
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-element-context "org-element" ())
(declare-function org-element-lineage
                  "org-element" (datum &optional types with-self))
(declare-function org-in-src-block-p
                  "org" (&optional inside element))
(declare-function org-inside-LaTeX-fragment-p
                  "org" (&optional element))
(declare-function smartparens-mode "smartparens" (&optional arg))
(declare-function sp-get-enclosing-sexp "smartparens" (&optional arg))
(declare-function sp-local-pair
                  "smartparens" (modes open close &rest properties))

(defvar evil-replace-state-entry-hook)
(defvar evil-replace-state-exit-hook)
(defvar smartparens-mode)
(defvar smartparens-mode-map)
(defvar sp-cancel-autoskip-on-backward-movement)
(defvar sp-highlight-pair-overlay)
(defvar sp-highlight-wrap-overlay)
(defvar sp-highlight-wrap-tag-overlay)
(defvar sp-show-pair-from-inside)

(defvar-local yunge-pair--disabled-for-replace nil
  "Non-nil when Replace state temporarily disabled Smartparens.")

(defconst yunge-pair-insert-bindings
  '((")" yunge-pair-close "close pair")
    ("]" yunge-pair-close "close pair")
    ("}" yunge-pair-close "close pair")
    ("SPC" yunge-pair-space "insert space")
    ("DEL" yunge-pair-backward-delete "delete backward")))

(defconst yunge-pair--org-block-types
  '(center-block comment-block dynamic-block example-block export-block
    quote-block special-block src-block verse-block)
  "Org block types whose contents cannot start a heading.")

(defconst yunge-pair--org-chinese-pairs
  '(("“" . "”")
    ("‘" . "’")
    ("（" . "）")
    ("【" . "】")
    ("《" . "》")
    ("〈" . "〉")
    ("「" . "」")
    ("『" . "』"))
  "Chinese punctuation pairs used in Org prose.")

(defun yunge-pair--org-literal-context-p (_id action _context)
  "Return non-nil when Org markup pairing would alter literal text.
ACTION is the Smartparens operation being considered."
  (when (eq action 'insert)
    (let ((element (org-element-context)))
      (or (org-in-src-block-p t)
          (org-inside-LaTeX-fragment-p element)
           (org-element-lineage
            element
            '(code comment-block example-block export-block fixed-width
              inline-src-block src-block verbatim)
            t)))))

(defun yunge-pair--org-heading-star-p (_match beginning _end)
  "Return non-nil when the star at BEGINNING is an Org heading marker."
  (save-excursion
    (goto-char beginning)
    (and (org-at-heading-p)
         (progn
           (beginning-of-line)
           (skip-chars-forward "*")
           (< beginning (point))))))

(defun yunge-pair--org-heading-start-p (id action _context)
  "Return non-nil when pair ID starts a possible Org heading for ACTION."
  (when (eq action 'insert)
    (let ((beginning (- (point) (length id))))
      (and (= beginning (line-beginning-position))
           (save-excursion
             (goto-char beginning)
             (not
              (org-element-lineage
               (org-element-context)
               yunge-pair--org-block-types t)))))))

(defun yunge-pair--org-after-left-bracket-p (id action _context)
  "Return non-nil when Org pair ID follows a left bracket for ACTION."
  (when (eq action 'insert)
    (let ((opener-position (- (point) (length id))))
      (and (> opener-position (point-min))
           (eq (char-before opener-position) ?\[)))))

(defun yunge-pair-space ()
  "Add inner spaces to an empty math pair, or insert one space."
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (self-insert-command 1)
    (let* ((expression (sp-get-enclosing-sexp))
           (open (plist-get expression :op))
           (close (plist-get expression :cl))
           (beginning (plist-get expression :beg))
           (end (plist-get expression :end)))
      (if (and (member open '("\\(" "\\["))
               close beginning end
               (= (point) (+ beginning (length open)))
               (= (point) (- end (length close))))
          (progn
            (insert "  ")
            (backward-char))
        (self-insert-command 1)))))

(defun yunge-pair-close ()
  "Move over a closing delimiter at point, or insert the typed character.
This also handles Smartparens delimiters whose closing string has more
than one character, such as `\\)' and `\\]'."
  (interactive)
  (let* ((expression (sp-get-enclosing-sexp))
         (close (plist-get expression :cl))
         (end (plist-get expression :end))
         (close-start
          (and close end (- end (length close)))))
    (if (and close-start
             (<= (point) close-start)
             (string-match-p
              "\\`[ \t]*\\'"
              (buffer-substring-no-properties
               (point) close-start))
             (> (length close) 0)
             (characterp last-command-event)
             (eq last-command-event
                 (aref close (1- (length close)))))
        (goto-char end)
      (self-insert-command 1))))

(defun yunge-pair--delete-spaced-math-pair ()
  "Delete an empty spaced Org math pair at point and return non-nil."
  (when (derived-mode-p 'org-mode)
    (when-let* ((expression (sp-get-enclosing-sexp))
                (open (plist-get expression :op))
                ((member open '("\\(" "\\[")))
                (close (plist-get expression :cl))
                (beginning (plist-get expression :beg))
                (end (plist-get expression :end))
                (inside-beginning (+ beginning (length open)))
                (inside-end (- end (length close)))
                ((= (point) (1+ inside-beginning)))
                ((equal (buffer-substring-no-properties
                         inside-beginning inside-end)
                        "  ")))
      (delete-region beginning end)
      t)))

(defun yunge-pair-backward-delete ()
  "Delete a generated empty math pair, or use Evil's normal Backspace."
  (interactive)
  (unless (yunge-pair--delete-spaced-math-pair)
    (call-interactively #'evil-delete-backward-char-and-join)))

(defun yunge-pair--disable-for-replace ()
  "Disable Smartparens while entering Evil Replace state."
  (when (bound-and-true-p smartparens-mode)
    (setq yunge-pair--disabled-for-replace t)
    (smartparens-mode -1)))

(defun yunge-pair--restore-after-replace ()
  "Restore Smartparens after leaving Evil Replace state."
  (when yunge-pair--disabled-for-replace
    (setq yunge-pair--disabled-for-replace nil)
    (smartparens-mode 1)))

(defun yunge-pair--setup-evil ()
  "Configure Smartparens behavior for Evil states."
  (yunge-key-evil-define 'insert smartparens-mode-map
                         yunge-pair-insert-bindings)
  (add-hook 'evil-replace-state-entry-hook
            #'yunge-pair--disable-for-replace)
  (add-hook 'evil-replace-state-exit-hook
            #'yunge-pair--restore-after-replace))

(defun yunge-pair--enable ()
  "Enable Smartparens in the current buffer."
  (smartparens-mode 1))

(defun yunge-pair--setup-existing-buffers ()
  "Enable Smartparens in supported buffers opened before readiness."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'prog-mode 'org-mode)
        (yunge-pair--enable)))))

(defun yunge-pair--setup-org-pairs ()
  "Configure Org-specific pairs."
  (sp-local-pair 'org-mode "\\(" "\\)")
  (sp-local-pair 'org-mode "\\[" "\\]")
  (sp-local-pair
   'org-mode "*" "*"
   :unless '(sp-point-after-word-p
             yunge-pair--org-heading-start-p
             yunge-pair--org-literal-context-p)
   :post-handlers '(("[d1]" "SPC"))
   :skip-match 'yunge-pair--org-heading-star-p)
  (sp-local-pair
   'org-mode "_" "_"
   :unless '(sp-point-after-word-p
             yunge-pair--org-literal-context-p)
   :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair
   'org-mode "/" "/"
   :unless '(sp-point-after-word-p
             yunge-pair--org-after-left-bracket-p
             yunge-pair--org-literal-context-p)
   :post-handlers '(("[d1]" "SPC")))
  (dolist (delimiter '("~" "="))
    (sp-local-pair
     'org-mode delimiter delimiter
     :unless '(sp-point-after-word-p
               yunge-pair--org-literal-context-p)
     :post-handlers '(("[d1]" "SPC"))))
  (dolist (pair yunge-pair--org-chinese-pairs)
    (sp-local-pair
     'org-mode (car pair) (cdr pair)
     :unless '(yunge-pair--org-literal-context-p))))

(defun yunge-pair--setup ()
  "Configure paired delimiter insertion."
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-show-pair-from-inside t)
  (yunge-pair--setup-org-pairs)
  (add-hook 'prog-mode-hook #'yunge-pair--enable)
  (add-hook 'org-mode-hook #'yunge-pair--enable)
  (yunge-pair--setup-existing-buffers)
  (with-eval-after-load 'evil
    (yunge-pair--setup-evil)))

(elpaca smartparens
  (require 'smartparens)
  (yunge-pair--setup))

(provide 'yunge-pair)

;;; yunge-pair.el ends here
