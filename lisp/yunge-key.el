;;; yunge-key.el --- Keybinding helpers -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function evil-define-key* "evil-core")
(declare-function evil-define-minor-mode-key "evil-core")
(declare-function evil-get-auxiliary-keymap "evil-core")
(declare-function evil-get-minor-mode-keymap "evil-core")
(declare-function which-key-add-keymap-based-replacements "which-key")

(defconst yunge-key-button-navigation-bindings
  '(("g]" forward-button "next button")
    ("g[" backward-button "previous button")
    ("<tab>" forward-button "next button")
    ("S-TAB" backward-button "previous button")
    ("<backtab>" backward-button nil)))

(defun yunge-key--parse (key)
  "Return the key sequence represented by KEY."
  (if (stringp key) (kbd key) key))

(defun yunge-key-define (map bindings)
  "Define BINDINGS in MAP.
Each binding has the form (KEY DEFINITION DESCRIPTION).  KEY may be a
string accepted by `kbd' or a vector key sequence."
  (dolist (binding bindings)
    (define-key map (yunge-key--parse (car binding)) (nth 1 binding))))

(defun yunge-key-evil-define (state map bindings)
  "Define Evil STATE BINDINGS in MAP.
Each binding has the form (KEY DEFINITION DESCRIPTION).  KEY may be a
string accepted by `kbd' or a vector key sequence."
  (dolist (binding bindings)
    (evil-define-key* state map
      (yunge-key--parse (car binding)) (nth 1 binding)))
  (with-eval-after-load 'which-key
    (dolist (current (if (listp state) state (list state)))
      (yunge-key-add-which-key-descriptions
       (evil-get-auxiliary-keymap map current t t) bindings))))

(defun yunge-key-evil-define-minor-mode (state mode bindings)
  "Define Evil STATE BINDINGS while minor MODE is active."
  (dolist (binding bindings)
    (evil-define-minor-mode-key state mode
      (yunge-key--parse (car binding)) (nth 1 binding)))
  (with-eval-after-load 'which-key
    (dolist (current (if (listp state) state (list state)))
      (yunge-key-add-which-key-descriptions
       (evil-get-minor-mode-keymap current mode) bindings))))

(defun yunge-key-add-which-key-descriptions (map bindings)
  "Add the descriptions from BINDINGS directly to MAP for Which-Key."
  (dolist (binding bindings)
    (when-let* ((description (nth 2 binding)))
      (which-key-add-keymap-based-replacements
        map (car binding) (cons description (nth 1 binding))))))

(provide 'yunge-key)

;;; yunge-key.el ends here
