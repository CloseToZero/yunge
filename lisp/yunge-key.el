;;; yunge-key.el --- Keybinding helpers -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function evil-define-key* "evil-core")
(declare-function which-key-add-keymap-based-replacements "which-key")
(declare-function which-key-add-major-mode-key-based-replacements
                  "which-key")

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
      (yunge-key--parse (car binding)) (nth 1 binding))))

(defun yunge-key-which-key-describe-map (map bindings)
  "Add the descriptions from BINDINGS directly to MAP for Which-Key."
  (dolist (binding bindings)
    (when-let* ((description (nth 2 binding)))
      (which-key-add-keymap-based-replacements
        map (car binding) (cons description (nth 1 binding))))))

(defun yunge-key-which-key-describe (mode bindings &optional prefix)
  "Describe MODE BINDINGS to Which-Key, optionally below PREFIX."
  (let (replacements)
    (dolist (binding bindings)
      (let ((description (nth 2 binding)))
        (when description
          (push (if prefix
                    (concat prefix " " (car binding))
                  (car binding))
                replacements)
          (push description replacements))))
    (apply #'which-key-add-major-mode-key-based-replacements
           mode (nreverse replacements))))

(provide 'yunge-key)

;;; yunge-key.el ends here
