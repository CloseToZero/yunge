;;; yunge-evil.el --- Evil modal editing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-key)
(require 'yunge-jump-history)
(require 'yunge-minibuffer)
(require 'yunge-pinyin)
(require 'yunge-state)

(declare-function evil-add-command-properties "evil-common")
(declare-function evil-ex-delete-hl "evil-search" (name))
(declare-function evil-ex-make-search-pattern "evil-search" (regexp))
(declare-function evil-ex-nohighlight "evil-search" ())
(declare-function evil-ex-search-backward "evil-commands" (count))
(declare-function evil-ex-search-forward "evil-commands" (count))
(declare-function evil-ex-search-next "evil-commands" (count))
(declare-function evil-exit-visual-state "evil-states" (&optional later buffer))
(declare-function evil-eolp "evil-common" ())
(declare-function evil-forward-char "evil-commands"
                  (count &optional crosslines noerror))
(declare-function evil-make-intercept-map "evil-core")
(declare-function evil-push-search-history "evil-search" (regexp forward))
(declare-function evil-state-auxiliary-keymaps "evil-core" (state))

(defvar evil-cross-lines)
(defvar evil-ex-last-was-search)
(defvar evil-ex-search-count)
(defvar evil-ex-search-direction)
(defvar evil-ex-search-history)
(defvar evil-ex-search-offset)
(defvar evil-ex-search-pattern)
(defvar evil-ex-search-vim-style-regexp)
(defvar evil-command-line-map)
(defvar evil-last-find)
(defvar evil-local-mode)
(defvar evil-motion-state-map)
(defvar evil-repeat-find-to-skip-next)
(defvar evil-respect-visual-line-mode)
(defvar evil-state)
(defvar evil-visual-selection)
(defvar help-map)
(defvar project-prefix-map)
(defvar visual-line-mode)

(defvar yunge-evil--pinyin-search nil
  "Non-nil while an Evil search command should expand Pinyin.")

(defconst yunge-evil--regexp-search-prefix ":re:"
  "Prefix that selects regexp matching during Pinyin search.")

(defun yunge-evil-call-after-normal-state-eol (function &rest arguments)
  "Call FUNCTION with point after Evil's Normal-state EOL character.
Restore point if FUNCTION signals an error or quit."
  (let* ((move-after-eol
          (and (bound-and-true-p evil-local-mode)
               (eq evil-state 'normal)
               (evil-eolp)
               (not (eolp))))
         (origin (and move-after-eol (copy-marker (point)))))
    (when move-after-eol
      (forward-char))
    (condition-case error-data
        (prog1 (apply function arguments)
          (when origin
            (set-marker origin nil)))
      ((error quit)
       (when origin
         (goto-char origin)
         (set-marker origin nil))
       (signal (car error-data) (cdr error-data))))))

(defun yunge-evil--nohighlight-after-force-normal-state (&rest _arguments)
  "Clear highlights after an interactive `evil-force-normal-state'."
  (when (eq this-command 'evil-force-normal-state)
    (evil-ex-nohighlight)))

(defun yunge-evil--handle-interactive-search-failure
    (function &rest arguments)
  "Call FUNCTION with ARGUMENTS, concisely reporting interactive search misses.
Preserve `search-failed' for non-interactive callers."
  (let ((interactivep (called-interactively-p 'any)))
    (condition-case error-data
        (apply function arguments)
      (search-failed
       (if interactivep
           (let ((query (car evil-ex-search-history)))
             (if (and (stringp query) (> (length query) 0))
                 (message "Search failed: %s"
                          (truncate-string-to-width query 60 nil nil "…"))
               (message "Search failed")))
         (signal (car error-data) (cdr error-data)))))))

(defun yunge-evil--pinyin-search-pattern (function regexp)
  "Call FUNCTION for REGEXP using the active search syntax."
  (cond
   ((not yunge-evil--pinyin-search)
    (funcall function regexp))
   ((string-prefix-p yunge-evil--regexp-search-prefix regexp)
    (funcall function
             (substring regexp
                        (length yunge-evil--regexp-search-prefix))))
   (t
    (let ((pattern (funcall function regexp)))
      ;; Evil returns a fresh (REGEXP IGNORE-CASE WHOLE-LINE) pattern.
      ;; Replacing only its regexp preserves Vim's smart-case decision.
      (setcar pattern (yunge-pinyin-query-regexp regexp))
      pattern))))

(defun yunge-evil--split-pinyin-search-pattern
    (function pattern direction)
  "Split PATTERN for DIRECTION using the active search syntax.
Pinyin search is literal, so its `/` and `?` remain part of PATTERN."
  (if (and yunge-evil--pinyin-search
           (not (string-prefix-p yunge-evil--regexp-search-prefix
                                 pattern)))
      (list pattern nil nil)
    (funcall function pattern direction)))

(defun yunge-evil-pinyin-search-forward (count)
  "Start a forward search with Pinyin expansion."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let ((yunge-evil--pinyin-search t))
    (evil-ex-search-forward count)))

(defun yunge-evil-pinyin-search-backward (count)
  "Start a backward search with Pinyin expansion."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let ((yunge-evil--pinyin-search t))
    (evil-ex-search-backward count)))

(defun yunge-evil--visual-search (beginning end direction)
  "Search in DIRECTION for the text from BEGINNING to END."
  (when (eq evil-visual-selection 'block)
    (user-error "Visual block search is not supported"))
  (let ((regexp
         (regexp-quote
           (buffer-substring-no-properties beginning end))))
    (evil-exit-visual-state)
    (goto-char (if (eq direction 'forward) end beginning))
    (setq evil-ex-search-count 1
          evil-ex-search-direction direction
          evil-ex-search-pattern
          (let ((evil-ex-search-vim-style-regexp nil))
            (evil-ex-make-search-pattern regexp))
          evil-ex-search-offset nil
          evil-ex-last-was-search t)
    (unless (equal regexp (car evil-ex-search-history))
      (push regexp evil-ex-search-history))
    (evil-push-search-history regexp (eq direction 'forward))
    (evil-ex-delete-hl 'evil-ex-search)
    (evil-ex-search-next 1)))

(defun yunge-evil-visual-search-forward (beginning end)
  "Search forward for the text selected in Visual state."
  (interactive "r")
  (yunge-evil--visual-search beginning end 'forward))

(defun yunge-evil-visual-search-backward (beginning end)
  "Search backward for the text selected in Visual state."
  (interactive "r")
  (yunge-evil--visual-search beginning end 'backward))

(defgroup yunge nil
  "Personal Emacs configuration."
  :group 'emacs)

(defcustom yunge-evil-find-char-punctuation-alist
  '((?, . "，")
    (?. . "。．")
    (?? . "？")
    (?: . "：")
    (?! . "！")
    (?- . "－—")
    (?_ . "＿")
    (?~ . "～")
    (?/ . "／")
    (?\\ . "、＼")
    (?\; . "；")
    (?\( . "（")
    (?\) . "）")
    (?\[ . "［【「")
    (?\] . "］】」")
    (?{ . "｛〖『")
    (?} . "｝〗』")
    (?< . "＜《〈")
    (?> . "＞》〉")
    (?\' . "‘’")
    (?\" . "“”"))
  "ASCII prompts and their additional character-find equivalents.
The prompt itself is always matched.  Each alist value is a string whose
characters are also matched by Evil's `f', `F', `t', and `T' commands."
  :type '(alist :key-type character :value-type string)
  :group 'yunge)

(defun yunge-evil--find-char-regexp (char)
  "Return the expanded character-find regexp for CHAR, or nil.
Lowercase ASCII letters expand through Pinyin initials.  Configured ASCII
punctuation expands through `yunge-evil-find-char-punctuation-alist'."
  (cond
   ((and (<= ?a char) (<= char ?z))
    (yunge-pinyin-regexp (char-to-string char)))
   (t
    (when-let* ((equivalents
                 (alist-get char
                            yunge-evil-find-char-punctuation-alist)))
      (regexp-opt-charset
       (cons char (string-to-list equivalents)))))))

(defun yunge-evil--find-char-expanded (count char regexp)
  "Move to the COUNTth match for CHAR using expanded REGEXP.
This preserves Evil's character-find bounds, cursor placement, and repeat
state while replacing its literal search only for expanded prompts."
  (setq count (or count 1))
  (let ((forwardp (> count 0))
        (visual (and evil-respect-visual-line-mode visual-line-mode))
        case-fold-search)
    (setq evil-last-find (list #'evil-find-char char forwardp))
    (when forwardp
      (evil-forward-char 1 evil-cross-lines))
    (unless
        (prog1
            (re-search-forward
             regexp
             (cond
              (evil-cross-lines nil)
              ((and forwardp visual)
               (save-excursion
                 (end-of-visual-line)
                 (point)))
              (forwardp (line-end-position))
              (visual
               (save-excursion
                 (beginning-of-visual-line)
                 (point)))
              (t (line-beginning-position)))
             t count)
          (when forwardp
            (backward-char)))
      (user-error "Can't find `%c'" char))))

(defun yunge-evil--find-char-with-pinyin (function count char)
  "Call FUNCTION for COUNT and CHAR, expanding Pinyin and punctuation."
  (if-let* ((regexp (yunge-evil--find-char-regexp char)))
      (yunge-evil--find-char-expanded count char regexp)
    (funcall function count char)))

(defun yunge-evil--find-char-matches-p (char candidate)
  "Return non-nil when CANDIDATE matches expanded prompt CHAR."
  (when-let* ((regexp (and candidate
                           (yunge-evil--find-char-regexp char))))
    (string-match-p regexp (char-to-string candidate))))

(defun yunge-evil--repeat-find-char-with-pinyin (function count)
  "Call repeat FUNCTION with COUNT, respecting expanded `t' and `T' matches.
Evil increments a one-step repeat when the previous to-character target is
adjacent.  Apply the same rule when that target matched CHAR by Pinyin or
punctuation equivalence rather than literally."
  (let* ((effective-count (or count 1))
         (command (car-safe evil-last-find))
         (char (nth 1 evil-last-find))
         (forwardp (nth 2 evil-last-find)))
    (when (and (eq command #'evil-find-char-to)
               evil-repeat-find-to-skip-next
               (= (abs effective-count) 1)
               (integerp char))
      (when (< effective-count 0)
        (setq forwardp (not forwardp)))
      (when (yunge-evil--find-char-matches-p
             char
             (if forwardp
                 (char-after (1+ (point)))
               (char-before)))
        (setq effective-count (if (< effective-count 0) -2 2))))
    (funcall function effective-count)))

(defvar-keymap yunge-leader-map
  :doc "Global leader map.")

(defvar-keymap yunge-buffer-map
  :doc "Global buffer command map.")

(defvar-keymap yunge-file-map
  :doc "Global file command map.")

(defvar-keymap yunge-go-map
  :doc "Global context-sensitive destination map.")

(defvar-keymap yunge-jump-map
  :doc "Global jump command map.")

(defvar-keymap yunge-marker-map
  :doc "Global Evil marker command map.")

(defconst yunge-marker-bindings
  '(("s" evil-set-marker "set marker")
    ("j" evil-goto-mark "jump to marker")
    ("l" evil-goto-mark-line "jump to marker line")))

(defconst yunge-jump-bindings
  `(("m" ,yunge-marker-map "marker")))

(defvar-keymap yunge-note-map
  :doc "Global note command map.")

(defvar-keymap yunge-quit-map
  :doc "Global quit command map.")

(defvar-keymap yunge-search-map
  :doc "Global search command map.")

(defvar-keymap yunge-toggle-map
  :doc "Global toggle and terminal command map.")

(defvar-keymap yunge-window-map
  :doc "Global window command map.")

(defvar-keymap yunge-evil--empty-localleader-map
  :doc "Fallback map when the current mode has no local leader.")

(defconst yunge-buffer-bindings
  '(("b" switch-to-buffer "switch buffer")
    ("j" next-buffer "next buffer")
    ("k" previous-buffer "previous buffer")
    ("q" kill-current-buffer "close buffer")
    ("r" revert-buffer "revert buffer")))

(defun yunge-open-config-directory ()
  "Open `yunge-config-directory' in Dired."
  (interactive)
  (dired yunge-config-directory))

(defconst yunge-file-bindings
  '(("c" yunge-open-config-directory "open config directory")
    ("d" dired "open directory")
    ("f" find-file "find file")
    ("s" save-buffer "save file")))

(defconst yunge-quit-bindings
  '(("f" delete-frame "delete frame")
    ("q" save-buffers-kill-terminal "quit")
    ("r" restart-emacs "restart Emacs")))

(defconst yunge-leader-map-bindings
  `(("SPC" execute-extended-command "execute command")
    ("b" ,yunge-buffer-map "buffer")
    ("f" ,yunge-file-map "file")
    ("g" ,yunge-go-map "go")
    ("h" ,help-map "help")
    ("j" ,yunge-jump-map "jump")
    ("n" ,yunge-note-map "note")
    ("p" ,project-prefix-map "project")
    ("q" ,yunge-quit-map "quit")
    ("s" ,yunge-search-map "search")
    ("t" ,yunge-toggle-map "toggle/terminal")
    ("w" ,yunge-window-map "window")))

(yunge-key-define yunge-leader-map
                  yunge-leader-map-bindings)
(yunge-key-define yunge-buffer-map yunge-buffer-bindings)
(yunge-key-define yunge-file-map yunge-file-bindings)
(yunge-key-define yunge-quit-map yunge-quit-bindings)

(defun yunge-evil--normal-localleader ()
  "Return the current mode's Normal-state local leader, or nil."
  (catch 'binding
    (dolist (entry (evil-state-auxiliary-keymaps 'normal))
      (let ((binding (lookup-key (cdr entry) [localleader])))
        (when (and binding (not (numberp binding)))
          (throw 'binding binding))))))

(defun yunge-evil--localleader-binding (_binding)
  "Return the current mode's labelled local leader binding."
  (cons "mode"
        (or (key-binding [localleader])
            (yunge-evil--normal-localleader)
            yunge-evil--empty-localleader-map)))

(keymap-set
 yunge-leader-map "m"
 '(menu-item "mode" nil :filter yunge-evil--localleader-binding))

(defvar-keymap yunge-leader-mode-map)

(define-minor-mode yunge-leader-mode
  "Keep the global leader above ordinary mode-specific Evil maps."
  :global t
  :group 'yunge
  :keymap yunge-leader-mode-map)

(defconst yunge-evil-leader-bindings
  `(("SPC" ,yunge-leader-map nil)))

(defconst yunge-evil-alternate-leader-bindings
  `(("M-m" ,yunge-leader-map nil)))

(defun yunge-evil--setup-leader ()
  "Set up the leader after Evil has loaded."
  (yunge-key-evil-define '(normal visual)
                         yunge-leader-mode-map
                         yunge-evil-leader-bindings)
  (yunge-key-evil-define '(insert replace emacs)
                         yunge-leader-mode-map
                         yunge-evil-alternate-leader-bindings)
  ;; Mode-specific Evil maps must not replace either global leader.
  (dolist (state '(normal visual insert replace emacs))
    (evil-make-intercept-map yunge-leader-mode-map state t))
  (yunge-leader-mode 1))

(with-eval-after-load 'evil
  (require 'yunge-comment)
  (advice-add 'evil-force-normal-state :after
              #'yunge-evil--nohighlight-after-force-normal-state)
  (advice-add 'evil-ex-search-next :around
              #'yunge-evil--handle-interactive-search-failure)
  (advice-add 'evil-ex-search-previous :around
              #'yunge-evil--handle-interactive-search-failure)
  (advice-add 'evil-find-char :around
              #'yunge-evil--find-char-with-pinyin)
  (advice-add 'evil-repeat-find-char :around
              #'yunge-evil--repeat-find-char-with-pinyin)
  (yunge-key-define yunge-marker-map yunge-marker-bindings)
  (yunge-key-define yunge-jump-map yunge-jump-bindings)
  (yunge-evil--setup-leader)
  (yunge-key-define
   evil-motion-state-map
   '(("/" yunge-evil-pinyin-search-forward "search forward")
     ("?" yunge-evil-pinyin-search-backward "search backward")))
  (yunge-key-define
   evil-command-line-map
   '(("M-n" next-history-element "next history")
     ("M-p" previous-history-element "previous history")))
  (yunge-key-evil-define
   'visual global-map
   '(("*" yunge-evil-visual-search-forward "search selection forward")
     ("#" yunge-evil-visual-search-backward
      "search selection backward")))
  (evil-add-command-properties 'yunge-evil-pinyin-search-forward
                               :jump t :type 'exclusive
                               :repeat 'evil-repeat-ex-search
                               :keep-visual t)
  (evil-add-command-properties 'yunge-evil-pinyin-search-backward
                               :jump t
                               :repeat 'evil-repeat-ex-search
                               :keep-visual t)
  (evil-add-command-properties 'yunge-evil-visual-search-forward
                               :jump t :repeat nil)
  (evil-add-command-properties 'yunge-evil-visual-search-backward
                               :jump t :repeat nil))

(with-eval-after-load 'which-key
  (yunge-key-add-which-key-descriptions
   yunge-leader-map yunge-leader-map-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-buffer-map yunge-buffer-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-file-map yunge-file-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-jump-map yunge-jump-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-marker-map yunge-marker-bindings)
  (yunge-key-add-which-key-descriptions
   yunge-quit-map yunge-quit-bindings))

(elpaca evil
  ;; Keep Evil's command semantics, but own all mode-specific bindings.
  (setq evil-emacs-state-modes nil
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-search-module 'evil-search
        evil-symbol-word-search t
        evil-undo-system 'undo-redo
        evil-want-C-u-delete t
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil)
  (advice-add 'evil-ex-make-search-pattern
              :around #'yunge-evil--pinyin-search-pattern)
  (advice-add 'evil-ex-split-search-pattern
              :around #'yunge-evil--split-pinyin-search-pattern)
  (evil-mode 1))

(provide 'yunge-evil)

;;; yunge-evil.el ends here
