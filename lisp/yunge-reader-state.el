;;; yunge-reader-state.el --- Persistent Reader document state -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defcustom yunge-reader-document-state-limit 1000
  "Maximum number of persistent Reader document records to retain."
  :type '(integer :tag "Documents" 1)
  :group 'yunge-reader)

(defcustom yunge-reader-document-alias-limit 8
  "Maximum number of known paths retained for one Reader document."
  :type '(integer :tag "Paths" 1 64)
  :group 'yunge-reader)

(defcustom yunge-reader-fingerprint-sample-bytes (* 64 1024)
  "Maximum bytes sampled from each end of a Reader document."
  :type '(integer :tag "Bytes" 4096)
  :group 'yunge-reader)

(defconst yunge-reader-document-state-version 1
  "Current persistent Reader document record version.")

(defconst yunge-reader-document-fingerprint-version
  "sampled-sha256-v1"
  "Algorithm identifier used in persistent Reader document fingerprints.")

(defvar yunge-reader-saved-document-state nil
  "Most recently used persistent Reader document records.
Each entry maps `(DRIVER FINGERPRINT)' to a versioned plist containing path
aliases, place, appearance, and named marks.")

(defvar yunge-reader-state--fingerprint-cache
  (make-hash-table :test #'equal)
  "Process-local file-attribute to fingerprint cache.")

(defun yunge-reader-state-canonical-path (file)
  "Return a printable canonical path for FILE."
  (let ((absolute (expand-file-name file)))
    (or (ignore-errors (file-truename absolute)) absolute)))

(defun yunge-reader-state--attribute-signature (attributes)
  "Return the cache signature represented by file ATTRIBUTES."
  (list (file-attribute-size attributes)
        (file-attribute-modification-time attributes)
        (file-attribute-status-change-time attributes)
        (file-attribute-file-identifier attributes)))

(defun yunge-reader-state--sampled-fingerprint (file size)
  "Return a bounded content fingerprint for FILE with SIZE bytes."
  (let* ((sample yunge-reader-fingerprint-sample-bytes)
         (head-end (min size sample))
         (tail-start (max head-end (- size sample))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (format "%s\0%d\0"
                      yunge-reader-document-fingerprint-version size))
      (when (> head-end 0)
        (insert-file-contents-literally file nil 0 head-end))
      (when (< tail-start size)
        (insert "\0tail\0")
        (insert-file-contents-literally file nil tail-start size))
      (format "%s:%d:%s"
              yunge-reader-document-fingerprint-version
              size
              (secure-hash 'sha256 (current-buffer))))))

(defun yunge-reader-state-fingerprint (file)
  "Return a stable bounded fingerprint for FILE.
Missing, non-regular, and unreadable files receive a path-derived fallback so
the state API remains deterministic without probing arbitrary resources."
  (let* ((path (yunge-reader-state-canonical-path file))
         (attributes (ignore-errors (file-attributes path 'integer))))
    (if (and attributes
             (null (file-attribute-type attributes))
             (file-readable-p path))
        (let* ((signature
                (yunge-reader-state--attribute-signature attributes))
               (cached
                (gethash path yunge-reader-state--fingerprint-cache)))
          (if (equal (car-safe cached) signature)
              (cdr cached)
            (let ((fingerprint
                   (yunge-reader-state--sampled-fingerprint
                    path (file-attribute-size attributes))))
              (puthash path (cons signature fingerprint)
                       yunge-reader-state--fingerprint-cache)
              fingerprint)))
      (concat "path-sha256-v1:"
              (secure-hash 'sha256 path)))))

(defun yunge-reader-state-key (file driver)
  "Return the persistent record key for FILE and DRIVER symbol."
  (unless (symbolp driver)
    (error "Invalid Reader state driver: %S" driver))
  (list driver (yunge-reader-state-fingerprint file)))

(defun yunge-reader-state--record-p (record)
  "Return non-nil when RECORD has the current bounded state shape."
  (and (listp record)
       (equal (plist-get record :version)
              yunge-reader-document-state-version)
       (let ((aliases (plist-get record :aliases)))
         (and (listp aliases)
              (seq-every-p #'stringp aliases)))))

(defun yunge-reader-state--entry (key)
  "Return the valid saved entry matching KEY, or nil."
  (let ((entry (assoc key yunge-reader-saved-document-state)))
    (and entry
         (yunge-reader-state--record-p (cdr entry))
         entry)))

(defun yunge-reader-state--bounded-aliases (path aliases)
  "Return PATH followed by unique ALIASES within the configured bound."
  (seq-take
   (cons path (delete path (copy-sequence aliases)))
   (max 1 yunge-reader-document-alias-limit)))

(defun yunge-reader-state--records-after-path-claim
    (entries path driver &optional replaced-key)
  "Return ENTRIES after DRIVER claims PATH for its current fingerprint.
REPLACED-KEY, invalid entries, and records left without aliases are discarded."
  (let (retained)
    (dolist (candidate entries)
      (when (yunge-reader-state--record-p (cdr-safe candidate))
        (let ((key (car-safe candidate)))
          (unless (equal key replaced-key)
            (if (not (eq (car-safe key) driver))
                (push candidate retained)
              (let* ((record (copy-tree (cdr candidate) t))
                     (aliases
                      (delete path
                              (copy-sequence
                               (plist-get record :aliases)))))
                (when aliases
                  (push
                   (cons key (plist-put record :aliases aliases))
                   retained))))))))
    (nreverse retained)))

(defun yunge-reader-state--retain-entry (entry path)
  "Make ENTRY most recent and record PATH as an alias."
  (let* ((key (car entry))
         (driver (car-safe key))
         (record (copy-tree (cdr entry) t))
         (others
          (yunge-reader-state--records-after-path-claim
           yunge-reader-saved-document-state path driver key)))
    (setq record
          (plist-put
           record :aliases
           (yunge-reader-state--bounded-aliases
            path (plist-get record :aliases)))
          entry (cons key record))
    (setq yunge-reader-saved-document-state
          (cons entry others))
    (when (> (length yunge-reader-saved-document-state)
             yunge-reader-document-state-limit)
      (setcdr
       (nthcdr (1- yunge-reader-document-state-limit)
               yunge-reader-saved-document-state)
       nil))
    entry))

(defun yunge-reader-state-record (file driver &optional create)
  "Return FILE's persistent DRIVER record, optionally CREATE it.
The returned plist is a copy and cannot mutate the saved record."
  (let* ((path (yunge-reader-state-canonical-path file))
         (key (yunge-reader-state-key path driver))
         (entry (yunge-reader-state--entry key)))
    (if (or entry create)
        (copy-tree
         (cdr
          (yunge-reader-state--retain-entry
           (or entry
               (cons key
                     (list :version yunge-reader-document-state-version
                           :aliases nil)))
           path))
         t)
      (setq yunge-reader-saved-document-state
            (yunge-reader-state--records-after-path-claim
             yunge-reader-saved-document-state path driver))
      nil)))

(defun yunge-reader-state-value (file driver property)
  "Return FILE's copied persistent DRIVER PROPERTY, or nil."
  (copy-tree
   (plist-get (yunge-reader-state-record file driver) property)
   t))

(defun yunge-reader-state-put (file driver property value)
  "Store copied VALUE as FILE's persistent DRIVER PROPERTY."
  (unless (memq property '(:place :appearance :marks))
    (error "Invalid Reader document state property: %S" property))
  (let* ((path (yunge-reader-state-canonical-path file))
         (key (yunge-reader-state-key path driver))
         (record (or (yunge-reader-state-record path driver t)
                     (error "Could not create Reader document state")))
         (entry
          (cons key (plist-put record property (copy-tree value t)))))
    (yunge-reader-state--retain-entry entry path)
    (copy-tree value t)))

(defun yunge-reader-state-cleanup-missing ()
  "Remove missing aliases and return the number of forgotten records."
  (let ((before (length yunge-reader-saved-document-state))
        retained)
    (dolist (entry yunge-reader-saved-document-state)
      (when (yunge-reader-state--record-p (cdr-safe entry))
        (let* ((record (copy-tree (cdr entry) t))
               (aliases
                (seq-filter #'file-exists-p
                            (plist-get record :aliases))))
          (when aliases
            (push
             (cons (car entry) (plist-put record :aliases aliases))
             retained)))))
    (setq yunge-reader-saved-document-state (nreverse retained))
    (- before (length yunge-reader-saved-document-state))))

(provide 'yunge-reader-state)

;;; yunge-reader-state.el ends here
