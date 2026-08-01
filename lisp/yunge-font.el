;;; yunge-font.el --- Font selection -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defconst yunge-font-profile-alist
  '((windows-nt
     ("IBM Plex Mono" "IBM Plex Sans SC")
     ("Monaspace Neon" "Noto Sans SC")
     ("Cascadia Code" "Noto Sans SC")
     ("JetBrains Mono" "Noto Sans SC")
     ("Maple Mono CN" "Maple Mono CN"))
    (darwin
     ("SF Mono" "PingFang SC")
     ("Monaspace Neon" "PingFang SC")
     ("IBM Plex Mono" "IBM Plex Sans SC")
     ("JetBrains Mono" "PingFang SC")
     ("Menlo" "PingFang SC"))
    (gnu/linux
     ("Monaspace Neon" "Noto Sans CJK SC")
     ("IBM Plex Mono" "IBM Plex Sans SC")
     ("JetBrains Mono" "Noto Sans CJK SC")
     ("Iosevka" "Source Han Sans SC")
     ("Maple Mono CN" "Maple Mono CN")))
  "Preferred Latin and Han font pairs by operating system.")

(defconst yunge-font-height 120
  "Default font height in tenths of a point.")

(defun yunge-font--profiles ()
  "Return the font profiles for the current operating system."
  (cdr (assq system-type yunge-font-profile-alist)))

(defun yunge-font--supported-p (family frame)
  "Return non-nil when FAMILY is available on FRAME."
  (display-supports-face-attributes-p
   (list :family family) frame))

(defun yunge-font--select-profile (frame)
  "Return the first complete font profile available on FRAME."
  (catch 'profile
    (dolist (profile (yunge-font--profiles))
      (when (and (yunge-font--supported-p (car profile) frame)
                 (yunge-font--supported-p (cadr profile) frame))
        (throw 'profile profile)))))

(defun yunge-font--face-spec ()
  "Return a face spec that selects the first complete font profile."
  (mapcar
   (lambda (profile)
     `(((type graphic)
        (supports :family ,(car profile))
        (supports :family ,(cadr profile)))
       (:family ,(car profile) :height ,yunge-font-height)))
   (yunge-font--profiles)))

(defun yunge-font-setup-frame (frame)
  "Configure the Han font for graphical FRAME."
  (when (display-graphic-p frame)
    (when-let* ((profile (yunge-font--select-profile frame))
                (font (font-spec :family (cadr profile))))
      (dolist (target '(han cjk-misc))
        (set-fontset-font nil target font frame)))))

;; Face specs are resolved while each frame is created, avoiding a visible
;; change of the Latin font after startup.
(let ((spec (yunge-font--face-spec)))
  (face-spec-set 'default spec)
  (face-spec-set 'fixed-pitch spec))

(add-hook 'after-make-frame-functions #'yunge-font-setup-frame)

;; Also support reloading this library after graphical frames already exist.
(dolist (frame (frame-list))
  (yunge-font-setup-frame frame))

(provide 'yunge-font)

;;; yunge-font.el ends here
