;;; yunge-ghostel-evil.el --- Evil editing for Ghostel -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Evil integration for Ghostel.  Ghostel owns the rendered buffer, so
;; editing commands are translated to shell line-editor events instead of
;; changing buffer text.  Buffer positions are measured as characters/glyphs;
;; terminal cell columns are deliberately not used because wide and composed
;; characters do not map one-to-one to shell cursor movements.

;;; Code:

(require 'evil)
(require 'seq)

(declare-function ghostel--apply-cursor-style "ghostel" ())
(declare-function ghostel--cursor-blink-stop "ghostel" ())
(declare-function ghostel--filter-soft-wraps "ghostel" (text))
(declare-function ghostel--paste-text "ghostel" (text))
(declare-function ghostel--redraw "ghostel"
                  (term &optional full force-sync))
(declare-function ghostel--redraw-now "ghostel" (buffer))
(declare-function ghostel--send-encoded
                  "ghostel" (key-name mods &optional utf8))
(declare-function ghostel-alt-screen-p "ghostel" ())
(declare-function ghostel-beginning-of-input-or-line "ghostel" ())
(declare-function ghostel-copy-mode "ghostel" ())
(declare-function ghostel-cursor-point "ghostel" ())
(declare-function ghostel-input-start-point "ghostel" ())
(declare-function ghostel-point-on-cursor-row-p "ghostel" (&optional pos))
(declare-function ghostel-next-prompt "ghostel" ())
(declare-function ghostel-previous-prompt "ghostel" ())
(declare-function ghostel-readonly-exit "ghostel" ())

(defvar evil-change-commands)
(defvar evil-emacs-state-entry-hook)
(defvar evil-insert-state-entry-hook)
(defvar evil-insert-state-map)
(defvar evil-move-beyond-eol)
(defvar evil-move-cursor-back)
(defvar evil-state)
(defvar ghostel--cursor-char-pos)
(defvar ghostel--input-mode)
(defvar ghostel--line-input-end)
(defvar ghostel--line-input-start)
(defvar ghostel--process)
(defvar ghostel--redraw-timer)
(defvar ghostel--term)
(defvar ghostel-inhibit-anchor-functions)
(defvar ghostel-mark-activation-input-mode)
(defvar yunge-ghostel-evil-mode)

(defgroup yunge-ghostel-evil nil
  "Evil integration for Ghostel."
  :group 'ghostel)

(defcustom yunge-ghostel-evil-sync-iterations 6
  "Maximum number of short waits after sending terminal editing events."
  :type 'integer
  :group 'yunge-ghostel-evil)

(defvar-local yunge-ghostel-evil--escape-mode 'auto
  "Where insert-state Escape is sent: `auto', `terminal', or `evil'.")

(defvar yunge-ghostel-evil--entry-position-synchronized nil
  "Dynamically non-nil when an editing command synchronized insert entry.")

(defvar yunge-ghostel-evil-mode-map (make-sparse-keymap)
  "Keymap for `yunge-ghostel-evil-mode'.")

(evil-set-initial-state 'ghostel-mode 'insert)

(defun yunge-ghostel-evil--terminal-live-p ()
  "Return non-nil when the current Ghostel process accepts terminal input."
  (and yunge-ghostel-evil-mode
       (eq major-mode 'ghostel-mode)
       ghostel--term
       ghostel--process
       (process-live-p ghostel--process)))

(defun yunge-ghostel-evil--prompt-session-p ()
  "Return non-nil when an Evil command should edit the live shell prompt.
Read-only `copy' and `emacs' modes count as prompt sessions so an editing
command can resume terminal input instead of falling through to a direct
buffer modification."
  (and (yunge-ghostel-evil--terminal-live-p)
       (not (ghostel-alt-screen-p))
       (memq ghostel--input-mode '(semi-char copy emacs))))

(defun yunge-ghostel-evil--prompt-active-p ()
  "Return non-nil when shell input can be sent immediately."
  (and (yunge-ghostel-evil--prompt-session-p)
       (eq ghostel--input-mode 'semi-char)))

(defun yunge-ghostel-evil--line-mode-p ()
  "Return non-nil when Ghostel exposes an editable line-mode region."
  (and yunge-ghostel-evil-mode
       (eq ghostel--input-mode 'line)
       (markerp ghostel--line-input-start)
       (markerp ghostel--line-input-end)))

(defun yunge-ghostel-evil--sync-render ()
  "Briefly drain process output and apply Ghostel's pending redraw."
  (when (processp ghostel--process)
    (let ((iteration 0))
      (while (and (< iteration yunge-ghostel-evil-sync-iterations)
                  (accept-process-output ghostel--process 0.02 nil t))
        (setq iteration (1+ iteration))))
    (when ghostel--redraw-timer
      (ghostel--redraw-now (current-buffer)))))

(defun yunge-ghostel-evil--ensure-prompt-input ()
  "Leave a hidden read-only mode and make the live prompt writable.
Return non-nil only when Ghostel ends in semi-char input mode."
  (when (yunge-ghostel-evil--prompt-session-p)
    (when (memq ghostel--input-mode '(copy emacs))
      (ghostel-readonly-exit)
      (yunge-ghostel-evil--sync-render))
    (eq ghostel--input-mode 'semi-char)))

(defun yunge-ghostel-evil--foreground-at (position)
  "Return the foreground color at POSITION, when one is explicit."
  (let ((face (get-text-property position 'face)))
    (cond
     ((and (listp face) (plist-member face :foreground))
      (plist-get face :foreground))
     ((facep face) (face-foreground face nil t)))))

(defun yunge-ghostel-evil--dim-grey-p (color)
  "Return non-nil when COLOR resembles a dim autosuggestion face."
  (when-let* ((rgb (and (stringp color)
                        (ignore-errors (color-values color)))))
    (let ((brightest (apply #'max rgb))
          (darkest (apply #'min rgb)))
      (and (< brightest 49152)
           (< (- brightest darkest) 13107)))))

(defun yunge-ghostel-evil--suggestion-p (cursor end)
  "Return non-nil when CURSOR..END is a uniform dim suggestion."
  (and (< cursor end)
       (> cursor (line-beginning-position))
       (let ((typed (yunge-ghostel-evil--foreground-at (1- cursor)))
             (trailing (yunge-ghostel-evil--foreground-at cursor)))
         (and trailing
              (not (equal typed trailing))
              (yunge-ghostel-evil--dim-grey-p trailing)
              (>= (or (next-single-property-change
                       cursor 'face nil end)
                      end)
                  end)))))

(defun yunge-ghostel-evil--logical-line-bounds (position)
  "Return the materialized bounds of POSITION's soft-wrapped logical line."
  (save-excursion
    (goto-char position)
    (let ((start (line-beginning-position))
          end)
      (while (and (> start (point-min))
                  (get-text-property (1- start) 'ghostel-wrap))
        (goto-char (1- start))
        (setq start (line-beginning-position)))
      (goto-char position)
      (setq end (line-end-position))
      (while (and (< end (point-max))
                  (get-text-property end 'ghostel-wrap))
        (goto-char (1+ end))
        (setq end (line-end-position)))
      (cons start end))))

(defun yunge-ghostel-evil--input-start ()
  "Return the safe left boundary of live shell input."
  (when-let* ((cursor (ghostel-cursor-point)))
    (pcase-let* ((`(,logical-start . ,_)
                  (yunge-ghostel-evil--logical-line-bounds cursor))
                 (property-start
                  (text-property-any logical-start cursor
                                     'ghostel-input t))
                 (prompt-end nil)
                 (scan logical-start))
      (while-let ((prompt
                   (text-property-any scan cursor 'ghostel-prompt t)))
        (setq prompt-end
              (or (next-single-property-change
                   prompt 'ghostel-prompt nil cursor)
                  cursor)
              scan prompt-end))
      (or property-start
          prompt-end
          (let ((fallback (ghostel-input-start-point)))
            (and fallback (<= logical-start fallback cursor) fallback))
          cursor))))

(defun yunge-ghostel-evil--input-end ()
  "Return the position after typed input, excluding padding or suggestions."
  (when-let* ((cursor (ghostel-cursor-point)))
    (save-excursion
      (goto-char cursor)
      (let* ((bounds (yunge-ghostel-evil--logical-line-bounds cursor))
             (beginning (car bounds))
             (line-end (cdr bounds))
             (property-start
              (text-property-any beginning line-end 'ghostel-input t))
             (property-end
              (and property-start
                   (next-single-property-change
                    property-start 'ghostel-input nil line-end))))
        (cond
         ((and property-end
               (yunge-ghostel-evil--suggestion-p cursor property-end))
          cursor)
         (property-end)
         (t
          (goto-char line-end)
          (skip-chars-backward " \t" beginning)
          (max cursor (point))))))))

(defun yunge-ghostel-evil--clamp (begin end)
  "Clamp BEGIN..END to the current shell input and return a cons."
  (let* ((start (or (yunge-ghostel-evil--input-start)
                    (ghostel-cursor-point)
                    begin))
         (finish (or (yunge-ghostel-evil--input-end)
                     (ghostel-cursor-point)
                     end))
         (left (max start (min (or begin start) finish)))
         (right (max left (min (or end left) finish))))
    (cons left right)))

(defun yunge-ghostel-evil--soft-wrap-p (position)
  "Return non-nil when POSITION is a renderer-inserted wrap newline."
  (and (< position (point-max))
       (eq (char-after position) ?\n)
       (get-text-property position 'ghostel-wrap)))

(defun yunge-ghostel-evil--position-index (position)
  "Return POSITION's character index within live input, or nil.
Renderer-inserted soft-wrap newlines do not consume an index."
  (when-let* ((input-start (yunge-ghostel-evil--input-start))
              (input-end (yunge-ghostel-evil--input-end)))
    (when (<= input-start position input-end)
      (let ((target position)
            (index 0)
            (scan input-start))
      (while (< scan target)
        (unless (yunge-ghostel-evil--soft-wrap-p scan)
          (setq index (1+ index)))
        (setq scan (1+ scan)))
        index))))

(defun yunge-ghostel-evil--position-at-index (index)
  "Return the live input position at character INDEX."
  (let ((position (or (yunge-ghostel-evil--input-start)
                      (ghostel-cursor-point)
                      (point)))
        (end (or (yunge-ghostel-evil--input-end)
                 (ghostel-cursor-point)
                 (point)))
        (count 0))
    (while (and (< position end) (< count index))
      (unless (yunge-ghostel-evil--soft-wrap-p position)
        (setq count (1+ count)))
      (setq position (1+ position)))
    position))

(defun yunge-ghostel-evil--next-glyph-position (position limit)
  "Return the position after the glyph at POSITION, not past LIMIT."
  (if (>= position limit)
      limit
    (let* ((text (buffer-substring position limit))
           (clean (ghostel--filter-soft-wraps text))
           (glyph (car (string-glyph-split clean))))
      (if (not glyph)
          position
        (let ((remaining (length glyph))
              (scan position))
          (while (and (< scan limit) (> remaining 0))
            (unless (yunge-ghostel-evil--soft-wrap-p scan)
              (setq remaining (1- remaining)))
            (setq scan (1+ scan)))
          scan)))))

(defun yunge-ghostel-evil--glyph-count (begin end)
  "Return the number of shell cursor steps between BEGIN and END."
  (let ((text (ghostel--filter-soft-wraps
               (buffer-substring (min begin end) (max begin end)))))
    (length (string-glyph-split text))))

(defun yunge-ghostel-evil--restore-target (index fallback)
  "Resolve saved input INDEX after a redraw, or use FALLBACK."
  (if (integerp index)
      (yunge-ghostel-evil--position-at-index index)
    (or fallback (ghostel-cursor-point)
        (yunge-ghostel-evil--input-start) (point))))

(defun yunge-ghostel-evil--goto-input-position (position)
  "Move the shell line-editor cursor to buffer POSITION.
Movement counts Unicode glyphs, not terminal display cells."
  (when (and (yunge-ghostel-evil--prompt-active-p)
             (ghostel-cursor-point))
    (let* ((range (yunge-ghostel-evil--clamp position position))
           (target (car range))
           (cursor (ghostel-cursor-point))
           (count (yunge-ghostel-evil--glyph-count cursor target))
           (key (if (< target cursor) "left" "right")))
      (dotimes (_ count)
        (ghostel--send-encoded key ""))
      (when (> count 0)
        (yunge-ghostel-evil--sync-render))
      ;; TARGET was derived from the exact Ghostel cursor character position.
      ;; Keeping it avoids reintroducing a cell-column reconstruction here.
      (goto-char target)
      t)))

(defun yunge-ghostel-evil--resume-target (position &optional fallback)
  "Resume prompt input and translate POSITION across the resulting redraw."
  (let ((index (yunge-ghostel-evil--position-index position)))
    (unless (yunge-ghostel-evil--ensure-prompt-input)
      (user-error "Ghostel prompt is not accepting input"))
    (yunge-ghostel-evil--restore-target index fallback)))

(defun yunge-ghostel-evil--delete-input-region (begin end)
  "Delete BEGIN..END through the shell line editor and return its glyph count."
  (let ((count (yunge-ghostel-evil--glyph-count begin end)))
    (when (> count 0)
      (yunge-ghostel-evil--goto-input-position end)
      (dotimes (_ count)
        (ghostel--send-encoded "backspace" ""))
      (yunge-ghostel-evil--sync-render)
      (goto-char begin))
    count))

(defun yunge-ghostel-evil--prepare-range (begin end)
  "Resume prompt input and translate BEGIN..END across a redraw."
  (pcase-let* ((`(,left . ,right)
                (yunge-ghostel-evil--clamp begin end))
               (left-index (yunge-ghostel-evil--position-index left))
               (right-index (yunge-ghostel-evil--position-index right)))
    (unless (yunge-ghostel-evil--ensure-prompt-input)
      (user-error "Ghostel prompt is not accepting input"))
    (cons (yunge-ghostel-evil--restore-target
           left-index (yunge-ghostel-evil--input-start))
          (yunge-ghostel-evil--restore-target
           right-index (yunge-ghostel-evil--input-end)))))

(defun yunge-ghostel-evil--enter-insert-state ()
  "Enter Evil insert state without synchronizing the same position twice."
  (let ((yunge-ghostel-evil--entry-position-synchronized t))
    (evil-insert-state 1)))

(defun yunge-ghostel-evil-insert ()
  "Enter insert state at point and move the shell cursor there."
  (interactive)
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (let ((target (yunge-ghostel-evil--resume-target
                   (point) (yunge-ghostel-evil--input-start))))
      (yunge-ghostel-evil--goto-input-position target)
      (yunge-ghostel-evil--enter-insert-state)))
   ((yunge-ghostel-evil--line-mode-p)
    (call-interactively #'evil-insert))
   (t (user-error "No editable Ghostel prompt"))))

(defun yunge-ghostel-evil-insert-line ()
  "Enter insert state at the beginning of shell input."
  (interactive)
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (yunge-ghostel-evil--ensure-prompt-input)
    (yunge-ghostel-evil--goto-input-position
     (or (yunge-ghostel-evil--input-start) (ghostel-cursor-point)))
    (yunge-ghostel-evil--enter-insert-state))
   ((yunge-ghostel-evil--line-mode-p)
    (goto-char ghostel--line-input-start)
    (evil-insert-state 1))
   (t (user-error "No editable Ghostel prompt"))))

(defun yunge-ghostel-evil-append ()
  "Enter insert state after the glyph at point."
  (interactive)
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (let* ((end (or (yunge-ghostel-evil--input-end) (point)))
           (next (yunge-ghostel-evil--next-glyph-position (point) end))
           (target (yunge-ghostel-evil--resume-target next end)))
      (yunge-ghostel-evil--goto-input-position target)
      (yunge-ghostel-evil--enter-insert-state)))
   ((yunge-ghostel-evil--line-mode-p)
    (call-interactively #'evil-append))
   (t (user-error "No editable Ghostel prompt"))))

(defun yunge-ghostel-evil-append-line ()
  "Enter insert state after the final typed shell-input glyph."
  (interactive)
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (yunge-ghostel-evil--ensure-prompt-input)
    (yunge-ghostel-evil--goto-input-position
     (or (yunge-ghostel-evil--input-end) (ghostel-cursor-point)))
    (yunge-ghostel-evil--enter-insert-state))
   ((yunge-ghostel-evil--line-mode-p)
    (goto-char ghostel--line-input-end)
    (evil-insert-state 1))
   (t (user-error "No editable Ghostel prompt"))))

(evil-define-operator yunge-ghostel-evil-delete
  (begin end type register yank-handler)
  "Delete BEGIN..END through the shell line editor."
  (interactive "<R><x><y>")
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (when (eq type 'block)
      (user-error "Visual block editing is not supported in Ghostel input"))
    (pcase-let* ((original (yunge-ghostel-evil--clamp begin end))
                 (`(,old-begin . ,old-end) original)
                 (prepared nil))
      (unless register
        (let ((text (filter-buffer-substring old-begin old-end)))
          (unless (string-match-p "\n" text)
            (evil-set-register ?- text))))
      (let ((evil-was-yanked-without-register nil))
        (evil-yank old-begin old-end type register yank-handler))
      (setq prepared
            (yunge-ghostel-evil--prepare-range old-begin old-end))
      (yunge-ghostel-evil--delete-input-region
       (car prepared) (cdr prepared))))
   ((yunge-ghostel-evil--line-mode-p)
    (evil-delete begin end type register yank-handler))
   (t (user-error "Ghostel renderer buffers cannot be edited directly"))))

(evil-define-operator yunge-ghostel-evil-delete-line
  (begin end type register yank-handler)
  "Delete the selected input range, clamped to the live prompt."
  :motion nil
  :keep-visual t
  (interactive "<R><x><y>")
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (yunge-ghostel-evil-delete
     begin
     (if (and begin (not (evil-visual-state-p)))
         (save-excursion (goto-char begin) (line-end-position))
       end)
     type register yank-handler))
   ((yunge-ghostel-evil--line-mode-p)
    (evil-delete-line begin end type register yank-handler))
   (t (user-error "Ghostel renderer buffers cannot be edited directly"))))

(evil-define-operator yunge-ghostel-evil-delete-char
  (begin end type register)
  "Delete the glyph under point through the shell line editor."
  :motion evil-forward-char
  (interactive "<R><x>")
  (yunge-ghostel-evil-delete begin end type register))

(evil-define-operator yunge-ghostel-evil-delete-backward-char
  (begin end type register)
  "Delete the glyph before point through the shell line editor."
  :motion evil-backward-char
  (interactive "<R><x>")
  (yunge-ghostel-evil-delete begin end type register))

(evil-define-operator yunge-ghostel-evil-change
  (begin end type register yank-handler delete-function)
  "Change BEGIN..END through the shell line editor, then enter insert state."
  (interactive "<R><x><y>")
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (ignore delete-function)
    (yunge-ghostel-evil-delete begin end type register yank-handler)
    (yunge-ghostel-evil--enter-insert-state))
   ((yunge-ghostel-evil--line-mode-p)
    (evil-change begin end type register yank-handler delete-function))
   (t (user-error "Ghostel renderer buffers cannot be edited directly"))))

(evil-define-operator yunge-ghostel-evil-change-line
  (begin end type register yank-handler)
  "Change from point through the end of live shell input."
  :motion evil-end-of-line-or-visual-line
  (interactive "<R><x><y>")
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (yunge-ghostel-evil-delete-line
     begin end type register yank-handler)
    (yunge-ghostel-evil--enter-insert-state))
   ((yunge-ghostel-evil--line-mode-p)
    (evil-change-line begin end type register yank-handler))
   (t (user-error "Ghostel renderer buffers cannot be edited directly"))))

(evil-define-operator yunge-ghostel-evil-substitute
  (begin end type register)
  "Substitute the next input glyph."
  :motion evil-forward-char
  (interactive "<R><x>")
  (yunge-ghostel-evil-change begin end type register))

(evil-define-operator yunge-ghostel-evil-substitute-line
  (begin end register yank-handler)
  "Substitute the current shell input line."
  :motion evil-line-or-visual-line
  :type line
  (interactive "<r><x>")
  (yunge-ghostel-evil-change begin end 'line register yank-handler))

(add-to-list 'evil-change-commands 'yunge-ghostel-evil-change)

(evil-define-operator yunge-ghostel-evil-replace
  (begin end type character)
  "Replace BEGIN..END with CHARACTER through the shell line editor."
  :motion evil-forward-char
  (interactive
   "<R>"
   (unwind-protect
       (let ((evil-force-cursor 'replace))
         (evil-refresh-cursor)
         (list (evil-read-key)))
     (evil-refresh-cursor)))
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (pcase-let* ((prepared (yunge-ghostel-evil--prepare-range begin end))
                 (`(,left . ,right) prepared)
                 (count (yunge-ghostel-evil--delete-input-region left right)))
      (when (and character (> count 0))
        (ghostel--paste-text (make-string count character)))))
   ((yunge-ghostel-evil--line-mode-p)
    (evil-replace begin end type character))
   (t (user-error "Ghostel renderer buffers cannot be edited directly"))))

(defun yunge-ghostel-evil--paste (count register after yank-handler)
  "Paste COUNT copies from REGISTER, positioning AFTER point when non-nil."
  (cond
   ((yunge-ghostel-evil--prompt-session-p)
    (let* ((text (if register
                     (evil-get-register register)
                   (current-kill 0)))
           (end (or (yunge-ghostel-evil--input-end) (point)))
           (position (if after
                         (yunge-ghostel-evil--next-glyph-position (point) end)
                       (point)))
           (target (yunge-ghostel-evil--resume-target
                    position (ghostel-cursor-point))))
      (yunge-ghostel-evil--goto-input-position target)
      (dotimes (_ (prefix-numeric-value count))
        (ghostel--paste-text text))
      (yunge-ghostel-evil--sync-render)))
   ((yunge-ghostel-evil--line-mode-p)
    (if after
        (evil-paste-after count register yank-handler)
      (evil-paste-before count register yank-handler)))
   (t (user-error "Ghostel renderer buffers cannot be pasted into directly"))))

(defun yunge-ghostel-evil-paste-after (&optional count register yank-handler)
  "Paste after point through Ghostel's bracketed-paste channel."
  (interactive "P")
  (yunge-ghostel-evil--paste count register t yank-handler))

(defun yunge-ghostel-evil-paste-before (&optional count register yank-handler)
  "Paste before point through Ghostel's bracketed-paste channel."
  (interactive "P")
  (yunge-ghostel-evil--paste count register nil yank-handler))

(evil-define-motion yunge-ghostel-evil-first-non-blank ()
  "Move to shell input start when point is on the live prompt."
  :type exclusive
  (if (or (yunge-ghostel-evil--prompt-session-p)
          (yunge-ghostel-evil--line-mode-p))
      (ghostel-beginning-of-input-or-line)
    (evil-first-non-blank)))

(evil-define-motion yunge-ghostel-evil-end-of-line (count)
  "Move to the final typed input glyph without entering renderer padding."
  :type inclusive
  (let ((input-end (and (yunge-ghostel-evil--prompt-session-p)
                        (ghostel-point-on-cursor-row-p)
                        (yunge-ghostel-evil--input-end))))
    (evil-end-of-line count)
    (when (and input-end (>= (point) input-end)
               (> input-end (line-beginning-position)))
      (goto-char (1- input-end)))))

(evil-define-motion yunge-ghostel-evil-goto-cursor (count)
  "Move to Ghostel's exact terminal cursor, or use `evil-goto-line'."
  :type line
  :jump t
  (if (and (yunge-ghostel-evil--prompt-session-p)
           (ghostel-cursor-point))
      (goto-char (ghostel-cursor-point))
    (evil-goto-line count)))

(defun yunge-ghostel-evil--insert-state-entry ()
  "Synchronize unhandled transitions into insert or Emacs state."
  (when (and yunge-ghostel-evil-mode
             (not yunge-ghostel-evil--entry-position-synchronized)
             (yunge-ghostel-evil--prompt-session-p))
    (let ((target (yunge-ghostel-evil--resume-target
                   (point) (ghostel-cursor-point))))
      (yunge-ghostel-evil--goto-input-position target))))

(defun yunge-ghostel-evil--visual-state-entry ()
  "Freeze terminal rendering while Evil owns a Visual selection."
  (when (and yunge-ghostel-evil-mode
             (yunge-ghostel-evil--prompt-active-p))
    (ghostel-copy-mode)))

(defun yunge-ghostel-evil--anchor-inhibit (_window force)
  "Keep Ghostel from snapping a roaming Evil point unless FORCE is non-nil."
  (and (not force)
       (yunge-ghostel-evil--prompt-session-p)
       (memq evil-state '(normal visual operator motion))
       (ghostel-cursor-point)
       (/= (point) (ghostel-cursor-point))))

(defun yunge-ghostel-evil--around-redraw
    (function term &optional full force-sync)
  "Keep tracked Evil point on Ghostel's exact cursor around redraw."
  (if (or (not yunge-ghostel-evil-mode)
          (ghostel-alt-screen-p))
      (funcall function term full force-sync)
    (let* ((cursor (ghostel-cursor-point))
           (track (or (memq evil-state '(insert emacs))
                      (and cursor (= (point) cursor))))
           (rendered (funcall function term full force-sync)))
      (when (and rendered track (ghostel-cursor-point))
        (goto-char (ghostel-cursor-point)))
      rendered)))

(defun yunge-ghostel-evil--around-cursor-style (function &rest arguments)
  "Let Evil render the cursor on the main terminal screen."
  (if (and yunge-ghostel-evil-mode
           ghostel--term
           (not (ghostel-alt-screen-p)))
      (progn
        (ghostel--cursor-blink-stop)
        (evil-refresh-cursor))
    (apply function arguments)))

(defun yunge-ghostel-evil--fallback-insert-key (key)
  "Run KEY from Ghostel's local map or Evil's insert-state map."
  (let ((command (or (lookup-key (current-local-map) key)
                     (lookup-key evil-insert-state-map key))))
    (when (commandp command)
      (call-interactively command))))

(defun yunge-ghostel-evil--send-control (key)
  "Send Control-KEY to a live terminal, otherwise use the local binding."
  (if (and (yunge-ghostel-evil--terminal-live-p)
           (eq ghostel--input-mode 'semi-char))
      (ghostel--send-encoded key "ctrl")
    (yunge-ghostel-evil--fallback-insert-key
     (kbd (concat "C-" key)))))

(defun yunge-ghostel-evil--send-delete ()
  "Send terminal Delete in semi-char mode, otherwise use the local binding."
  (interactive)
  (if (and (yunge-ghostel-evil--terminal-live-p)
           (eq ghostel--input-mode 'semi-char))
      (ghostel--send-encoded "delete" "")
    (yunge-ghostel-evil--fallback-insert-key (kbd "<delete>"))))

(defun yunge-ghostel-evil--escape ()
  "Route insert-state Escape to a TUI or back to Evil Normal state."
  (interactive)
  (if (or (eq yunge-ghostel-evil--escape-mode 'terminal)
          (and (eq yunge-ghostel-evil--escape-mode 'auto)
               (ghostel-alt-screen-p)))
      (ghostel--send-encoded "escape" "")
    (evil-force-normal-state)))

(defun yunge-ghostel-evil-toggle-escape (&optional argument)
  "Toggle Escape routing, or select auto/terminal/evil with ARGUMENT 1/2/3."
  (interactive "P")
  (setq yunge-ghostel-evil--escape-mode
        (if argument
            (or (nth (1- (prefix-numeric-value argument))
                     '(auto terminal evil))
                (user-error "Use prefix 1 (auto), 2 (terminal), or 3 (evil)"))
          (if (eq yunge-ghostel-evil--escape-mode 'auto)
              (if (ghostel-alt-screen-p) 'evil 'terminal)
            'auto)))
  (message "Ghostel Escape routing: %s" yunge-ghostel-evil--escape-mode))

(defconst yunge-ghostel-evil--control-keys
  '("a" "b" "d" "e" "f" "k" "l" "n" "o" "p" "q" "r" "s"
    "t" "u" "v" "w" "y")
  "Control keys forwarded to the terminal while Evil is inserting.")

(dolist (key yunge-ghostel-evil--control-keys)
  (let ((command-name (intern (format "yunge-ghostel-evil--control-%s" key))))
    (defalias command-name
      (let ((control-key key))
        (lambda ()
          (interactive)
          (yunge-ghostel-evil--send-control control-key)))
      (format "Send C-%s to Ghostel's terminal." key))
    (evil-define-key* 'insert yunge-ghostel-evil-mode-map
      (kbd (concat "C-" key)) command-name)))

(evil-define-key* 'insert yunge-ghostel-evil-mode-map
  (kbd "<delete>") #'yunge-ghostel-evil--send-delete
  (kbd "<escape>") #'yunge-ghostel-evil--escape)

(evil-define-key* '(normal visual) yunge-ghostel-evil-mode-map
  [remap evil-delete] #'yunge-ghostel-evil-delete
  [remap evil-delete-line] #'yunge-ghostel-evil-delete-line
  [remap evil-delete-char] #'yunge-ghostel-evil-delete-char
  [remap evil-delete-backward-char] #'yunge-ghostel-evil-delete-backward-char
  [remap evil-change] #'yunge-ghostel-evil-change
  [remap evil-change-line] #'yunge-ghostel-evil-change-line
  [remap evil-substitute] #'yunge-ghostel-evil-substitute
  [remap evil-change-whole-line] #'yunge-ghostel-evil-substitute-line
  [remap evil-replace] #'yunge-ghostel-evil-replace
  [remap evil-paste-after] #'yunge-ghostel-evil-paste-after
  [remap evil-paste-before] #'yunge-ghostel-evil-paste-before)

(evil-define-key* 'normal yunge-ghostel-evil-mode-map
  [remap evil-insert] #'yunge-ghostel-evil-insert
  [remap evil-insert-line] #'yunge-ghostel-evil-insert-line
  [remap evil-append] #'yunge-ghostel-evil-append
  [remap evil-append-line] #'yunge-ghostel-evil-append-line
  [remap evil-goto-line] #'yunge-ghostel-evil-goto-cursor
  "[[" #'ghostel-previous-prompt
  "]]" #'ghostel-next-prompt)

(evil-define-key* '(normal visual operator motion)
  yunge-ghostel-evil-mode-map
  [remap evil-first-non-blank] #'yunge-ghostel-evil-first-non-blank
  [remap evil-end-of-line] #'yunge-ghostel-evil-end-of-line)

(define-key yunge-ghostel-evil-mode-map (kbd "C-c C-r")
            #'yunge-ghostel-evil-toggle-escape)
(define-key yunge-ghostel-evil-mode-map (kbd "C-c <escape>")
            #'evil-force-normal-state)

(defun yunge-ghostel-evil--active-in-other-buffer-p (current)
  "Return non-nil when the mode is active outside CURRENT."
  (seq-some
   (lambda (buffer)
     (and (not (eq buffer current))
          (buffer-local-value 'yunge-ghostel-evil-mode buffer)))
   (buffer-list)))

;;;###autoload
(define-minor-mode yunge-ghostel-evil-mode
  "Integrate Evil with Ghostel without editing its renderer-owned buffer."
  :lighter nil
  :keymap yunge-ghostel-evil-mode-map
  (if yunge-ghostel-evil-mode
      (progn
        ;; Ghostel's cursor is an insertion position and therefore often sits
        ;; just after the final character.  Keeping that exact EOL position is
        ;; also what lets later chunks of asynchronous echo remain tracked.
        (setq-local evil-move-beyond-eol t
                    evil-move-cursor-back nil)
        ;; Evil Visual owns keyboard selections.  Its entry hook below freezes
        ;; the renderer explicitly, so Ghostel must not race it via mark hooks.
        (setq-local ghostel-mark-activation-input-mode nil)
        (add-hook 'evil-insert-state-entry-hook
                  #'yunge-ghostel-evil--insert-state-entry nil t)
        (add-hook 'evil-emacs-state-entry-hook
                  #'yunge-ghostel-evil--insert-state-entry nil t)
        (add-hook 'evil-visual-state-entry-hook
                  #'yunge-ghostel-evil--visual-state-entry nil t)
        (add-hook 'ghostel-inhibit-anchor-functions
                  #'yunge-ghostel-evil--anchor-inhibit nil t)
        (advice-add 'ghostel--redraw :around
                    #'yunge-ghostel-evil--around-redraw)
        (advice-add 'ghostel--apply-cursor-style :around
                    #'yunge-ghostel-evil--around-cursor-style)
        (evil-refresh-cursor))
    (remove-hook 'evil-insert-state-entry-hook
                 #'yunge-ghostel-evil--insert-state-entry t)
    (remove-hook 'evil-emacs-state-entry-hook
                 #'yunge-ghostel-evil--insert-state-entry t)
    (remove-hook 'evil-visual-state-entry-hook
                 #'yunge-ghostel-evil--visual-state-entry t)
    (remove-hook 'ghostel-inhibit-anchor-functions
                 #'yunge-ghostel-evil--anchor-inhibit t)
    (kill-local-variable 'evil-move-beyond-eol)
    (kill-local-variable 'evil-move-cursor-back)
    (kill-local-variable 'ghostel-mark-activation-input-mode)
    (unless (yunge-ghostel-evil--active-in-other-buffer-p (current-buffer))
      (advice-remove 'ghostel--redraw #'yunge-ghostel-evil--around-redraw)
      (advice-remove 'ghostel--apply-cursor-style
                     #'yunge-ghostel-evil--around-cursor-style))))

(provide 'yunge-ghostel-evil)

;;; yunge-ghostel-evil.el ends here
