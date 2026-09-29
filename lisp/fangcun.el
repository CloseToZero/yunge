;;; fangcun.el --- ID-based Org note navigation -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'button)
(require 'crm)
(require 'fangcun-loader)
(require 'fangcun-model)
(require 'fangcun-org)
(require 'fangcun-store)
(require 'json)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(defun fangcun--set-state-directory (symbol value)
  "Set SYMBOL to the normalized absolute directory VALUE."
  (unless (and (stringp value) (file-name-absolute-p value))
    (error "%s must be an absolute directory: %S" symbol value))
  (set-default symbol (file-name-as-directory (expand-file-name value))))

(defcustom fangcun-state-directory
  (expand-file-name "var/fangcun/" user-emacs-directory)
  "Directory for Fangcun state and native build output.
Set this before loading Fangcun to change the default database file.
An explicit `fangcun-database-file' overrides this default."
  :type 'directory
  :set #'fangcun--set-state-directory
  :group 'fangcun)

(defun fangcun--set-database-file (symbol value)
  "Set SYMBOL to the normalized absolute file name VALUE."
  (unless (and (stringp value) (file-name-absolute-p value))
    (error "%s must be an absolute file name: %S" symbol value))
  (set-default symbol (expand-file-name value)))

(defcustom fangcun-database-file
  (expand-file-name "fangcun.sqlite" fangcun-state-directory)
  "Absolute file name of the SQLite database used by Fangcun."
  :type 'file
  :set #'fangcun--set-database-file
  :group 'fangcun)

(defcustom fangcun-db-update-on-save t
  "Whether saving a Fangcun Org file updates its database entries."
  :type 'boolean
  :group 'fangcun)

(defvar fangcun--session-active-p)
(defvar fangcun--session-yiyus)

(defun fangcun--set-native-helper-enabled (symbol value)
  "Set SYMBOL to VALUE and apply it to the running native helper."
  (set-default symbol value)
  (when (featurep 'fangcun)
    (if value
        (when fangcun--session-active-p
          (fangcun--ensure-native-helper fangcun--session-yiyus))
      (fangcun--stop-native-helper))))

(defcustom fangcun-native-helper-enabled t
  "Whether Fangcun may use its native scanner and directory monitor.
When the helper is unavailable, synchronization falls back to Emacs.
Use `setopt' or Customize to apply changes to a running helper.  Disabling
it stops native processes but keeps Emacs file updates active."
  :type 'boolean
  :set #'fangcun--set-native-helper-enabled
  :group 'fangcun)

(defconst fangcun-backlinks-buffer-name "*Fangcun Backlinks*")

(defconst fangcun-check-buffer-name "*Fangcun Check*")

(defvar-local fangcun-backlinks-target-id nil
  "ID of the node shown in the current Fangcun backlinks buffer.")

(defconst fangcun--native-event-idle-delay 1
  "Idle seconds before Fangcun processes native file events.")

(defconst fangcun--source-directory
  (file-name-directory
   (or load-file-name
       (locate-library "fangcun")
       (error "Cannot locate the Fangcun library")))
  "Directory containing the loaded Fangcun library.")

(defconst fangcun--native-helper-manifest
  (expand-file-name
   "../native/fangcun-watch/Cargo.toml"
   fangcun--source-directory)
  "Cargo manifest of the Fangcun native helper.")

(defconst fangcun--native-helper-source-hash-file
  (expand-file-name
   "source.sha256"
   (file-name-directory fangcun--native-helper-manifest))
  "Tracked source hash embedded in the Fangcun native helper.")

(defconst fangcun--native-build-buffer-name "*Fangcun Helper Build*"
  "Name of the Fangcun native helper build buffer.")

(define-error 'fangcun-native-helper-outdated
  "Fangcun native helper is outdated")

(defvar fangcun--session-active-p nil
  "Whether synchronization is active for `fangcun--session-yiyus'.")

(defvar fangcun--session-yiyus nil
  "Normalized yiyus used by the active Fangcun session.")

(defvar fangcun--native-build-process nil
  "Process building the Fangcun native helper, or nil.")

(defvar fangcun--native-watch-process nil
  "Process monitoring Fangcun yiyus, or nil.")

(defvar fangcun--native-watch-yiyus nil
  "Signature of yiyus watched by the native monitor.")

(defvar fangcun--native-restart-count 0
  "Number of native monitor restarts attempted in the current Fangcun session.")

(defvar fangcun--native-event-timer nil
  "Timer for pending native file events.")

(defvar fangcun--native-pending-files
  (make-hash-table :test #'equal)
  "Files awaiting reconciliation after native events.")

(defvar fangcun--native-pending-full-sync-p nil
  "Whether native events require a complete incremental sync.")

(defvar fangcun--native-warning-shown-p nil
  "Whether helper degradation was reported in the current Fangcun session.")

(defvar-keymap fangcun-backlinks-mode-map
  :parent special-mode-map
  "g f" #'fangcun-backlink-show
  "g r" #'revert-buffer
  "RET" #'fangcun-backlink-visit)

(define-derived-mode fangcun-backlinks-mode special-mode "Fangcun Backlinks"
  "Major mode for displaying backlinks to one Fangcun node."
  (setq-local revert-buffer-function #'fangcun-backlinks-refresh))

(defvar-keymap fangcun-check-mode-map
  :parent special-mode-map
  "g f" #'fangcun-check-show
  "g r" #'revert-buffer
  "RET" #'fangcun-check-visit)

(define-derived-mode fangcun-check-mode special-mode "Fangcun Check"
  "Major mode for displaying Fangcun consistency checks."
  (setq-local revert-buffer-function #'fangcun-check-refresh))

(defun fangcun--configured-yiyus ()
  "Return normalized entries from `fangcun-yiyus'."
  (let ((yiyus
         (mapcar
          (lambda (entry)
            (let ((id (car entry))
                  (name (plist-get (cdr entry) :name))
                  (root (plist-get (cdr entry) :root)))
              (unless (and (symbolp id)
                           (stringp name)
                           (not (string-empty-p name))
                           (stringp root)
                           (not (string-empty-p root)))
                (user-error "Invalid Fangcun yiyu: %S" entry))
              (setq root
                    (file-name-as-directory (expand-file-name root)))
              (when (file-remote-p root)
                (user-error
                 "Remote Fangcun yiyus are not supported: %s" root))
              (make-fangcun-yiyu
               :id (symbol-name id)
               :name name
               :root root)))
          fangcun-yiyus))
        (ids (make-hash-table :test #'equal)))
    (dolist (yiyu yiyus)
      (let ((id (fangcun-yiyu-id yiyu)))
        (when (gethash id ids)
          (user-error "Duplicate Fangcun yiyu ID: %s" id))
        (puthash id t ids)))
    (cl-loop
     for (yiyu . rest) on yiyus
     do (dolist (other rest)
          (let ((root (fangcun-yiyu-root yiyu))
                (other-root (fangcun-yiyu-root other)))
            (when (or (equal root other-root)
                      (file-equal-p root other-root)
                      (file-in-directory-p root other-root)
                      (file-in-directory-p other-root root))
              (user-error
               "Fangcun yiyu roots overlap: %s and %s"
               root other-root)))))
    yiyus))

(defun fangcun--normalize-yiyu-id (id)
  "Return ID as a valid Fangcun yiyu symbol."
  (let ((name
         (string-trim
          (cond
           ((symbolp id) (symbol-name id))
           ((stringp id) id)
           (t "")))))
    (unless (string-match-p
             "\\`[[:alnum:]][[:alnum:]_-]*\\'" name)
      (user-error
       (concat
        "Fangcun yiyu ID must start with a letter or number and contain "
        "only letters, numbers, underscores, or hyphens: %S")
       id))
    (intern name)))

(defun fangcun--apply-yiyu-configuration ()
  "Synchronize Fangcun after `fangcun-yiyus' changes."
  (let ((yiyus (fangcun--configured-yiyus)))
    (if yiyus
        (prog1 (fangcun--sync-yiyus yiyus t)
          (fangcun--activate-session yiyus))
      (fangcun--stop-session)
      (fangcun--rebuild-database nil nil t))))

(defun fangcun--read-new-yiyu ()
  "Read arguments for `fangcun-yiyu-add'."
  (let* ((root
          (file-name-as-directory
           (expand-file-name
            (read-directory-name "Yiyu root: " nil nil t))))
         (basename
          (file-name-nondirectory (directory-file-name root)))
         (id (read-string "Yiyu ID: " nil nil basename))
         (name (read-string "Yiyu display name: " nil nil basename)))
    (list id name root)))

;;;###autoload
(defun fangcun-yiyu-add (id name root)
  "Add and persist a Fangcun yiyu named ID, NAME, and ROOT.
ROOT must be an existing local directory which does not overlap another
configured yiyu.  Synchronize the Fangcun index after saving the setting."
  (interactive (fangcun--read-new-yiyu))
  (setq id (fangcun--normalize-yiyu-id id)
        name (and (stringp name) (string-trim name))
        root (and (stringp root)
                  (file-name-as-directory (expand-file-name root))))
  (unless (and name (not (string-empty-p name)))
    (user-error "Fangcun yiyu display name cannot be empty"))
  (when (file-remote-p root)
    (user-error "Remote Fangcun yiyus are not supported: %s" root))
  (unless (and root (file-directory-p root))
    (user-error "Fangcun yiyu root does not exist: %s" root))
  (let ((value
         (append fangcun-yiyus
                 (list (list id :name name :root root)))))
    (let ((fangcun-yiyus value))
      (fangcun--configured-yiyus))
    (customize-save-variable 'fangcun-yiyus value)
    (fangcun--apply-yiyu-configuration)
    (message "Added Fangcun yiyu %s (%s)" name id)))

(defun fangcun--read-yiyu-to-remove ()
  "Read arguments for `fangcun-yiyu-remove'."
  (let* ((yiyus (fangcun--configured-yiyus))
         (_ (unless yiyus
              (user-error "No Fangcun yiyus are configured")))
         (candidates
          (mapcar
           (lambda (yiyu)
             (cons
              (format "%s (%s) — %s"
                      (fangcun-yiyu-name yiyu)
                      (fangcun-yiyu-id yiyu)
                      (abbreviate-file-name (fangcun-yiyu-root yiyu)))
              yiyu))
           yiyus))
         (choice
          (completing-read "Remove yiyu: " candidates nil t))
         (yiyu (cdr (assoc choice candidates))))
    (unless
        (yes-or-no-p
         (format
          "Remove %s from Fangcun?  Notes in %s will not be deleted. "
          (fangcun-yiyu-name yiyu)
          (abbreviate-file-name (fangcun-yiyu-root yiyu))))
      (user-error "Removing Fangcun yiyu cancelled"))
    (list (fangcun-yiyu-id yiyu))))

;;;###autoload
(defun fangcun-yiyu-remove (id)
  "Stop indexing and forget the configured Fangcun yiyu ID.
When called interactively, ask for confirmation.  Never delete the root
directory or any notes below it."
  (interactive (fangcun--read-yiyu-to-remove))
  (let* ((id (symbol-name (fangcun--normalize-yiyu-id id)))
         (entry
          (seq-find
           (lambda (candidate)
             (equal (symbol-name (car candidate)) id))
           fangcun-yiyus)))
    (unless entry
      (user-error "Unknown Fangcun yiyu ID: %s" id))
    (let ((value (delq entry (copy-sequence fangcun-yiyus))))
      (customize-save-variable 'fangcun-yiyus value)
      (fangcun--apply-yiyu-configuration)
      (message "Removed Fangcun yiyu %s; notes were not deleted" id))))

(defun fangcun--yiyu-containing-file (file yiyus)
  "Return the member of YIYUS containing FILE, or nil."
  (seq-find
   (lambda (yiyu)
     (file-in-directory-p file (fangcun-yiyu-root yiyu)))
   yiyus))

(defun fangcun--portable-file-name-error (name)
  "Return why file NAME is not portable, or nil."
  (let ((invalid
         (delete-dups
          (seq-filter
           (lambda (character)
             (or (< character 32)
                 (memq character
                       '(?< ?> ?: ?\" ?/ ?\\ ?| ?? ?*))))
           (string-to-list name)))))
    (cond
     ((member name '("" "." ".."))
      (format "%S is not a file name" name))
     (invalid
      (format
       "File name %S contains non-portable characters: %s"
       name
       (mapconcat
        (lambda (character)
          (if (< character 32)
              (format "U+%04X" character)
            (char-to-string character)))
        invalid ", ")))
     ((or (string-prefix-p " " name)
          (string-suffix-p " " name)
          (string-suffix-p "." name))
      (format
       "File name %S starts or ends with a non-portable character"
       name))
     ((string-match-p
       (concat
        "\\`\\(?:con\\|prn\\|aux\\|nul\\|"
        "com[1-9]\\|lpt[1-9]\\)"
        "\\(?:\\..*\\)?\\'")
       (downcase name))
      (format "File name %S is reserved on Windows" name)))))

(defun fangcun--new-file-name-error (file root)
  "Return why FILE cannot name a new file below ROOT, or nil."
  (setq file (expand-file-name file))
  (let ((directory (file-name-directory file))
        (name (file-name-nondirectory file))
        relative-components)
    (cond
     ((file-remote-p file)
      "Fangcun files must be local")
     ((not (file-in-directory-p file root))
      (format "Fangcun files must stay below %s" root))
     ((progn
        (setq relative-components
              (split-string
               (subst-char-in-string
                ?\\ ?/ (file-relative-name file root))
               "/" t))
        (seq-some
         #'fangcun--portable-file-name-error
         relative-components)))
     ((not (string-suffix-p ".org" name))
      "Fangcun file names must end with .org")
     ((file-exists-p file)
      (format "File already exists: %s" file))
     ((find-buffer-visiting file)
      (format "A buffer is already visiting: %s" file))
     ((when (file-directory-p directory)
        (when-let* ((conflict
                     (seq-find
                      (lambda (entry)
                        (and (not (equal entry name))
                             (string-equal (downcase entry)
                                           (downcase name))))
                      (directory-files directory nil nil t))))
          (format
           "File name differs only by case from existing %S"
           conflict)))))))

(defun fangcun--read-file-state (yiyu file)
  "Return the synchronization state of FILE owned by YIYU."
  (let ((attributes (file-attributes file 'string)))
    (make-fangcun-file-state
     :yiyu yiyu
     :relative-file
     (file-relative-name file (fangcun-yiyu-root yiyu))
     :absolute-file file
     :mtime
     (float-time (file-attribute-modification-time attributes))
     :size (file-attribute-size attributes))))

(defun fangcun--cargo-target-directory ()
  "Return the Cargo target directory for Fangcun native packages."
  (file-name-as-directory
   (expand-file-name "cargo-target" fangcun-state-directory)))

(defun fangcun--native-helper-program ()
  "Return the expected Fangcun helper executable."
  (expand-file-name
   (concat "release/fangcun-watch"
           (when (eq system-type 'windows-nt) ".exe"))
   (fangcun--cargo-target-directory)))

(defun fangcun--native-helper-build-id ()
  "Return the expected Fangcun native helper build ID, or nil."
  (when (file-readable-p fangcun--native-helper-source-hash-file)
    (with-temp-buffer
      (insert-file-contents fangcun--native-helper-source-hash-file)
      (let ((build-id (string-trim (buffer-string))))
        (unless (string-empty-p build-id)
          build-id)))))

(defun fangcun--native-helper-available-p ()
  "Return whether the Fangcun native helper executable is available."
  (file-executable-p (fangcun--native-helper-program)))

(defun fangcun--validate-native-ready-message (message)
  "Validate native helper ready MESSAGE against the tracked build ID."
  (let ((expected (fangcun--native-helper-build-id))
        (actual (alist-get 'build-id message)))
    (unless expected
      (error "Fangcun native helper source hash is unavailable"))
    (unless (and (equal (alist-get 'kind message) "ready")
                 (equal actual expected))
      (signal
       'fangcun-native-helper-outdated
       (list
        (format "Expected build %s, got %s"
                expected (or actual "an unversioned helper")))))))

(defun fangcun--elisp-scan-file-states (yiyus)
  "Return Org file states below YIYUS using Emacs file operations."
  (let (states)
    (dolist (yiyu yiyus)
      (dolist (file
               (directory-files-recursively
                (fangcun-yiyu-root yiyu) "\\.org\\'"))
        (push (fangcun--read-file-state yiyu file) states)))
    (nreverse states)))

(defun fangcun--native-command-arguments (command yiyus)
  "Return helper arguments for COMMAND and YIYUS."
  ;; The helper receives no stdin.  Its argv is COMMAND followed by repeated
  ;; YIYU-ID ROOT pairs, and its stdout is the NDJSON protocol documented in
  ;; native/fangcun-watch/README.org.
  (cons
   command
   (mapcan
    (lambda (yiyu)
      (list (fangcun-yiyu-id yiyu)
            (fangcun-yiyu-root yiyu)))
    yiyus)))

(defun fangcun--native-scan-file-states (yiyus)
  "Return Org file states below YIYUS using the native helper."
  (let ((program (fangcun--native-helper-program))
        (yiyus-by-id (make-hash-table :test #'equal))
        ready
        states)
    (dolist (yiyu yiyus)
      (puthash (fangcun-yiyu-id yiyu) yiyu yiyus-by-id))
    (with-temp-buffer
      (let ((status
             (apply
              #'process-file program nil t nil
              (fangcun--native-command-arguments "scan" yiyus))))
        (unless (zerop status)
          (error "Native Fangcun scan failed: %s"
                 (string-trim (buffer-string)))))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((message
                (json-parse-string
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))
                 :object-type 'alist))
               (kind (alist-get 'kind message)))
          (if ready
              (progn
                (unless (equal kind "state")
                  (error "Unexpected native Fangcun scan message: %S"
                         message))
                (let* ((id (alist-get 'yiyu message))
                       (yiyu (gethash id yiyus-by-id))
                       (file
                        (expand-file-name (alist-get 'file message))))
                  (unless yiyu
                    (error
                     "Native Fangcun scan returned unknown yiyu: %s"
                     id))
                  (push
                   (make-fangcun-file-state
                    :yiyu yiyu
                    :relative-file
                    (file-relative-name file
                                        (fangcun-yiyu-root yiyu))
                    :absolute-file file
                    :mtime (alist-get 'mtime message)
                    :size (alist-get 'size message))
                   states)))
            (fangcun--validate-native-ready-message message)
            (setq ready t)))
        (forward-line 1)))
    (unless ready
      (error "Native Fangcun scan returned no ready message"))
    (nreverse states)))

(defun fangcun--validate-yiyu-roots (yiyus)
  "Signal a user error if any root in YIYUS is missing."
  (dolist (yiyu yiyus)
    (unless (file-directory-p (fangcun-yiyu-root yiyu))
      (user-error "Fangcun yiyu does not exist: %s"
                  (fangcun-yiyu-root yiyu)))))

(defun fangcun--scan-file-states (yiyus)
  "Return the current Org file states below YIYUS."
  (fangcun--validate-yiyu-roots yiyus)
  (if (and fangcun-native-helper-enabled
           yiyus (fangcun--native-helper-available-p))
      (condition-case error-data
          (fangcun--native-scan-file-states yiyus)
        (fangcun-native-helper-outdated
         (fangcun--build-native-helper)
         (fangcun--elisp-scan-file-states yiyus))
        (error
         (display-warning
          'fangcun
          (concat
           (error-message-string error-data)
           "; falling back to Emacs")
          :warning)
         (fangcun--elisp-scan-file-states yiyus)))
    (fangcun--elisp-scan-file-states yiyus)))

(defun fangcun--parse-file-states (states)
  "Read saved Org contents for STATES before changing the database."
  (mapcar
   (lambda (state)
     (cons state
           (fangcun-org-read-file
            (fangcun-file-state-yiyu state)
            (fangcun-file-state-absolute-file state))))
   states))

(defun fangcun--rebuild-database (yiyus states &optional no-message)
  "Replace the Fangcun index for YIYUS and file STATES.
When NO-MESSAGE is non-nil, do not report the indexed counts."
  (let ((result
         (fangcun-store-rebuild
          fangcun-database-file yiyus (fangcun--parse-file-states states))))
    (unless no-message
      (message
       (concat
        "Fangcun indexed %d nodes, %d aliases, %d tags, and %d links "
        "from %d files in %d yiyu roots")
       (plist-get result :nodes)
       (plist-get result :aliases)
       (plist-get result :tags)
       (plist-get result :links)
       (plist-get result :files)
       (plist-get result :yiyus)))
    result))

(defun fangcun--apply-file-changes (database changed deleted)
  "Parse CHANGED files, replace their entries, and remove DELETED keys."
  (fangcun-store-replace-files
   database (fangcun--parse-file-states changed) deleted))

(defun fangcun--sync-database (states &optional no-message)
  "Synchronize an existing Fangcun database with file STATES.
When NO-MESSAGE is non-nil, do not report the changed file counts."
  (fangcun-store-call-with-database fangcun-database-file
   (lambda (database)
     (let ((database-states
            (fangcun-store-file-states database))
           (current-keys (make-hash-table :test #'equal))
           (missing (make-symbol "missing"))
           changed deleted
           (added-count 0)
           (updated-count 0))
       (dolist (state states)
         (let* ((key (fangcun-store-file-state-key state))
                (current
                 (cons (fangcun-file-state-mtime state)
                       (fangcun-file-state-size state)))
                (stored (gethash key database-states missing)))
           (puthash key t current-keys)
           (unless (equal stored current)
             (if (eq stored missing)
                 (cl-incf added-count)
               (cl-incf updated-count))
             (push state changed))))
       (maphash
        (lambda (key _state)
          (unless (gethash key current-keys)
            (push key deleted)))
        database-states)
       (setq changed (nreverse changed)
             deleted (nreverse deleted))
       (fangcun--apply-file-changes database changed deleted)
       (unless no-message
         (message
          "Fangcun synchronized files: %d added, %d updated, %d removed"
          added-count updated-count (length deleted)))
       (fangcun-store-counts database)))))

;;;###autoload
(defun fangcun-db-rebuild ()
  "Rebuild the Fangcun database from every configured yiyu file."
  (interactive)
  (let ((yiyus (fangcun--configured-yiyus)))
    (unless yiyus
      (fangcun--stop-session)
      (user-error "Configure `fangcun-yiyus' before syncing"))
    (when fangcun--session-active-p
      (fangcun--stop-session))
    (prog1
        (fangcun--rebuild-database
         yiyus (fangcun--scan-file-states yiyus))
      (fangcun--activate-session yiyus))))

(defun fangcun--sync-yiyus (yiyus no-message)
  "Synchronize YIYUS, suppressing results when NO-MESSAGE is non-nil."
  (when (and fangcun--session-active-p
             (not (equal yiyus fangcun--session-yiyus)))
    (fangcun--stop-session))
  (let ((states (fangcun--scan-file-states yiyus)))
    (if (and
         (file-exists-p fangcun-database-file)
         (fangcun-store-call-with-database fangcun-database-file
          (lambda (database)
            (fangcun-store-roots-match-p database yiyus))))
        (fangcun--sync-database states no-message)
      (fangcun--rebuild-database yiyus states no-message))))

;;;###autoload
(defun fangcun-db-sync (&optional no-message)
  "Synchronize the Fangcun database with configured yiyu files.
When NO-MESSAGE is non-nil, do not report synchronization results."
  (interactive)
  (let ((yiyus (fangcun--configured-yiyus)))
    (unless yiyus
      (fangcun--stop-session)
      (user-error "Configure `fangcun-yiyus' before syncing"))
    (prog1 (fangcun--sync-yiyus yiyus no-message)
      (fangcun--activate-session yiyus))))

(defun fangcun--native-yiyu-signature (yiyus)
  "Return the monitor-relevant signature of YIYUS."
  (mapcar
   (lambda (yiyu)
     (cons (fangcun-yiyu-id yiyu)
           (fangcun-yiyu-root yiyu)))
   yiyus))

(defun fangcun--native-warning (format-string &rest arguments)
  "Report one native helper warning using FORMAT-STRING and ARGUMENTS."
  (unless fangcun--native-warning-shown-p
    (setq fangcun--native-warning-shown-p t)
    (display-warning
     'fangcun
     (apply #'format format-string arguments)
     :warning)))

(defun fangcun--stop-native-watch ()
  "Stop the Fangcun native monitor intentionally."
  (let ((process fangcun--native-watch-process))
    (setq fangcun--native-watch-process nil
          fangcun--native-watch-yiyus nil)
    (when (process-live-p process)
      (delete-process process))))

(defun fangcun--schedule-native-events ()
  "Schedule processing of queued native monitor events."
  (when (timerp fangcun--native-event-timer)
    (cancel-timer fangcun--native-event-timer))
  (setq fangcun--native-event-timer
        (run-with-idle-timer
         fangcun--native-event-idle-delay nil
         #'fangcun--process-native-events)))

(defun fangcun--queue-native-full-sync ()
  "Request a complete incremental sync after Emacs becomes idle."
  (setq fangcun--native-pending-full-sync-p t)
  (clrhash fangcun--native-pending-files)
  (fangcun--schedule-native-events))

(defun fangcun--queue-native-files (files)
  "Queue absolute FILES for reconciliation after Emacs becomes idle."
  (unless fangcun--native-pending-full-sync-p
    (dolist (file files)
      (puthash (expand-file-name file) t
               fangcun--native-pending-files)))
  (fangcun--schedule-native-events))

(defun fangcun--handle-native-message (process line)
  "Handle one NDJSON LINE from native monitor PROCESS."
  (let* ((message
          (json-parse-string line
                             :object-type 'alist
                             :array-type 'list))
          (kind (alist-get 'kind message)))
    (if (process-get process 'fangcun-ready)
        (pcase kind
          ("event"
           (fangcun--queue-native-files (alist-get 'paths message)))
          ((or "rescan" "error")
           (when (equal kind "error")
             (display-warning
              'fangcun
              (format "Native monitor reported: %s"
                      (alist-get 'message message))
              :warning))
           (fangcun--queue-native-full-sync))
          (_
           (error "Unexpected native Fangcun monitor message: %S"
                  message)))
      (fangcun--validate-native-ready-message message)
      (process-put process 'fangcun-ready t)
      ;; A full scan closes the gap before the monitor became ready.
      (fangcun--queue-native-full-sync))))

(defun fangcun--native-watch-filter (process output)
  "Collect and handle complete NDJSON lines in native monitor OUTPUT."
  (when (eq process fangcun--native-watch-process)
    (process-put
     process 'fangcun-output
     (concat (or (process-get process 'fangcun-output) "") output))
    (let (newline)
      (while
          (and
           (eq process fangcun--native-watch-process)
           (setq newline
                 (string-match
                  "\n" (process-get process 'fangcun-output))))
        (let* ((pending (process-get process 'fangcun-output))
               (line
                (string-trim-right
                 (substring pending 0 newline) "\r")))
          (process-put process 'fangcun-output
                       (substring pending (1+ newline)))
          (unless (string-empty-p line)
            (condition-case error-data
                (fangcun--handle-native-message process line)
              (fangcun-native-helper-outdated
               (fangcun--stop-native-watch)
               (fangcun--build-native-helper))
              (error
               (display-warning
                'fangcun
                (format "Invalid native monitor output: %s"
                        (error-message-string error-data))
                :warning)
               (fangcun--queue-native-full-sync)))))))))

(defun fangcun--native-watch-sentinel (process _event)
  "Recover when native monitor PROCESS exits unexpectedly."
  (when (and (memq (process-status process) '(exit signal failed))
             (eq process fangcun--native-watch-process))
    (setq fangcun--native-watch-process nil)
    (if (and fangcun--session-active-p
             (< fangcun--native-restart-count 1))
        (progn
          (cl-incf fangcun--native-restart-count)
          (fangcun--start-native-watch fangcun--session-yiyus))
      (fangcun--native-warning
       (concat
        "Fangcun native monitoring stopped; external changes require "
        "`fangcun-db-sync'")))))

(defun fangcun--start-native-watch (yiyus)
  "Start recursively monitoring YIYUS."
  (let ((signature (fangcun--native-yiyu-signature yiyus)))
    (when (and fangcun-native-helper-enabled
               yiyus (fangcun--native-helper-available-p))
      (unless (and (process-live-p fangcun--native-watch-process)
                   (equal signature fangcun--native-watch-yiyus))
        (fangcun--stop-native-watch)
        (setq fangcun--native-watch-yiyus signature)
        (condition-case error-data
            (setq fangcun--native-watch-process
                  (make-process
                   :name "fangcun-watch"
                   :command
                   (cons
                    (fangcun--native-helper-program)
                    (fangcun--native-command-arguments
                     "watch" yiyus))
                   :coding 'utf-8-unix
                   :connection-type 'pipe
                   :noquery t
                   :filter #'fangcun--native-watch-filter
                   :sentinel #'fangcun--native-watch-sentinel))
          (error
           (setq fangcun--native-watch-process nil
                 fangcun--native-watch-yiyus nil)
           (fangcun--native-warning
            "Cannot start Fangcun native monitoring: %s"
            (error-message-string error-data))))))))

(defun fangcun--native-build-sentinel (process _event)
  "Start native monitoring when helper build PROCESS succeeds."
  (when (and (memq (process-status process) '(exit signal failed))
             (eq process fangcun--native-build-process)
             (not (process-get process 'fangcun-build-finished)))
    (process-put process 'fangcun-build-finished t)
    (setq fangcun--native-build-process nil)
    (let ((failure (unless (and (eq (process-status process) 'exit)
                                (zerop (process-exit-status process))
                                (fangcun--native-helper-available-p))
                     (list 'error "Fangcun native helper build failed"))))
      (if (not failure)
          (progn
            (message "Built Fangcun native helper")
            (when (and fangcun-native-helper-enabled
                       fangcun--session-active-p)
              (fangcun--start-native-watch fangcun--session-yiyus)))
        (fangcun--native-warning
         (concat
          "Fangcun native helper build failed; external changes require "
          "`fangcun-db-sync'.  See %s")
         (buffer-name (process-buffer process))))
      (when-let* ((complete (process-get process 'fangcun-build-complete)))
        (funcall complete failure)))))

(defun fangcun--build-native-helper (&optional complete)
  "Build Fangcun asynchronously and call COMPLETE with nil or an error."
  (when (process-live-p fangcun--native-build-process)
    (when complete
      (user-error "Fangcun native helper is already being built")))
  (unless (process-live-p fangcun--native-build-process)
    (if-let* ((cargo (executable-find "cargo")))
        (let ((buffer (get-buffer-create fangcun--native-build-buffer-name))
              (target (fangcun--cargo-target-directory)))
          (fangcun--stop-native-watch)
          (make-directory target t)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Fangcun native helper build\n\n"))
            (setq default-directory fangcun--source-directory)
            (compilation-mode))
          (let ((process
                 (make-process
                  :name "fangcun-helper-build"
                  :buffer buffer
                  :command
                  (list cargo "build" "--release" "--locked"
                        "--manifest-path"
                        fangcun--native-helper-manifest
                        "--target-dir" target)
                  :noquery t
                  :sentinel #'fangcun--native-build-sentinel)))
            (setq fangcun--native-build-process process)
            (process-put process 'fangcun-build-complete complete)
            (when (memq (process-status process) '(exit signal failed))
              (fangcun--native-build-sentinel process "finished")))
          (display-buffer buffer)
          (message "Building Fangcun native helper..."))
      (if complete
          (user-error "Cargo is required to build Fangcun native helper")
        (fangcun--native-warning
         (concat
          "Cargo is unavailable; Fangcun external changes require "
          "`fangcun-db-sync'"))))))

(defun fangcun--ensure-native-helper (yiyus)
  "Start or build the native helper for YIYUS when enabled."
  (if (not fangcun-native-helper-enabled)
      (fangcun--stop-native-helper)
    (when yiyus
      (cond
       ((process-live-p fangcun--native-build-process))
       ((fangcun--native-helper-available-p)
        (fangcun--start-native-watch yiyus))
       (t
        (fangcun--build-native-helper))))))

;;;###autoload
(defun fangcun-native-build (&optional complete)
  "Build the Fangcun native helper and call COMPLETE with nil or an error."
  (interactive)
  (fangcun--build-native-helper complete))

(defun fangcun--install-operation-advice ()
  "Install file-operation updates once."
  ;; These operations are infrequent and give immediate database state.  Keep
  ;; them active with the monitor; its duplicate event will compare unchanged.
  (unless (advice-member-p #'fangcun--around-rename-file
                           'rename-file)
    (advice-add 'rename-file :around #'fangcun--around-rename-file))
  (unless (advice-member-p #'fangcun--around-delete-file
                           'delete-file)
    (advice-add 'delete-file :around #'fangcun--around-delete-file))
  (unless (advice-member-p #'fangcun--around-vc-delete-file
                           'vc-delete-file)
    (advice-add 'vc-delete-file :around
                #'fangcun--around-vc-delete-file)))

(defun fangcun--activate-session (yiyus)
  "Activate synchronization for the current Fangcun session and YIYUS."
  (setq fangcun--session-active-p t
        fangcun--session-yiyus yiyus)
  (fangcun--install-operation-advice)
  (fangcun--ensure-native-helper yiyus))

(defun fangcun--stop-native-helper ()
  "Stop native processes and discard their pending file events."
  (fangcun--stop-native-watch)
  (when (timerp fangcun--native-event-timer)
    (cancel-timer fangcun--native-event-timer))
  (setq fangcun--native-event-timer nil
        fangcun--native-pending-full-sync-p nil
        fangcun--native-restart-count 0
        fangcun--native-warning-shown-p nil)
  (clrhash fangcun--native-pending-files)
  (let ((process fangcun--native-build-process))
    (setq fangcun--native-build-process nil)
    (when (process-live-p process)
      (process-put process 'fangcun-build-finished t)
      (delete-process process)
      (when-let* ((complete (process-get process 'fangcun-build-complete)))
        (funcall complete '(error "Fangcun native helper build was cancelled"))))))

(defun fangcun--stop-session ()
  "Stop synchronization and discard work belonging to the current session."
  (setq fangcun--session-active-p nil
        fangcun--session-yiyus nil)
  (fangcun--stop-native-helper))

(add-hook 'kill-emacs-hook #'fangcun--stop-session)

(defun fangcun--ensure-session (&optional yiyus)
  "Synchronize Fangcun on first use or after its configured roots change.
Return the normalized configured YIYUS, obtaining them when omitted."
  (setq yiyus (or yiyus (fangcun--configured-yiyus)))
  (unless yiyus
    (fangcun--stop-session)
    (user-error "Configure `fangcun-yiyus' before using Fangcun"))
  (unless (and fangcun--session-active-p
               (equal yiyus fangcun--session-yiyus)
               (file-exists-p fangcun-database-file))
    (fangcun--sync-yiyus yiyus t)
    (fangcun--activate-session yiyus))
  yiyus)

(defun fangcun--db-update-file-in-yiyu
    (file yiyu &optional no-message)
  "Replace database entries for saved Org FILE owned by YIYU.
When NO-MESSAGE is non-nil, do not report the indexed counts."
  (unless (file-regular-p file)
    (user-error "Fangcun file does not exist: %s" file))
  (unless (string-match-p "\\.org\\'" file)
    (user-error "Fangcun only indexes Org files: %s" file))
  (when-let* ((buffer (find-buffer-visiting file)))
    (when (buffer-modified-p buffer)
      (user-error "Save the Fangcun file before updating it")))
  (unless (file-exists-p fangcun-database-file)
    (user-error "Run fangcun-db-sync before updating individual files"))
  (let* ((state (fangcun--read-file-state yiyu file))
         (data (fangcun-org-read-file yiyu file))
         (result
          (fangcun-store-call-with-database
           fangcun-database-file
           (lambda (database)
             (unless (fangcun-store-yiyu-match-p database yiyu)
               (user-error
                "Fangcun yiyu configuration changed; run fangcun-db-sync"))
             (fangcun-store-replace-files
              database (list (cons state data)) nil)))))
    (unless no-message
      (message
       (concat
        "Fangcun indexed %d nodes, %d aliases, %d tags, and %d links "
        "from %s")
       (plist-get result :nodes)
       (plist-get result :aliases)
       (plist-get result :tags)
       (plist-get result :links)
       (abbreviate-file-name file)))
    result))

(defun fangcun--reconcile-files (files)
  "Update indexed Org FILES from one event batch atomically."
  (when (and files
             fangcun--session-active-p
             (file-exists-p fangcun-database-file))
    (let ((managed
           (delq nil
                 (mapcar
                  (lambda (file)
                    (when (and file (string-match-p "\\.org\\'" file))
                      (when-let* ((absolute-file (expand-file-name file))
                                  (yiyu
                                   (fangcun--yiyu-containing-file
                                    absolute-file fangcun--session-yiyus)))
                        (cons absolute-file yiyu))))
                  files))))
      (when managed
        (fangcun-store-call-with-database fangcun-database-file
         (lambda (database)
           (unless (fangcun-store-roots-match-p
                    database fangcun--session-yiyus)
             (user-error
              "Fangcun yiyu configuration changed; run fangcun-db-sync"))
           (let ((seen (make-hash-table :test #'equal))
                 changed deleted)
             (dolist (entry managed)
               (let* ((file (car entry))
                      (yiyu (cdr entry))
                      (relative-file
                       (file-relative-name file
                                           (fangcun-yiyu-root yiyu)))
                      (key (cons (fangcun-yiyu-id yiyu) relative-file)))
                 (unless (gethash key seen)
                   (puthash key t seen)
                   (let ((stored
                          (fangcun-store-file-state
                           database yiyu relative-file)))
                     (if (file-regular-p file)
                         (let* ((state (fangcun--read-file-state yiyu file))
                                (current
                                 (cons (fangcun-file-state-mtime state)
                                       (fangcun-file-state-size state))))
                           (unless (equal stored current)
                             (push state changed)))
                       (when stored
                         (push key deleted)))))))
             (fangcun--apply-file-changes
              database (nreverse changed) (nreverse deleted)))))))))

(defun fangcun--process-native-events ()
  "Reconcile file events queued by the native monitor."
  (setq fangcun--native-event-timer nil)
  (let ((full-sync fangcun--native-pending-full-sync-p)
        files)
    (setq fangcun--native-pending-full-sync-p nil)
    (maphash
     (lambda (file _value)
       (push file files))
     fangcun--native-pending-files)
    (clrhash fangcun--native-pending-files)
    (when (and fangcun--session-active-p
               (file-exists-p fangcun-database-file))
      (condition-case error-data
          (if full-sync
              (fangcun--sync-yiyus fangcun--session-yiyus t)
            (fangcun--validate-yiyu-roots fangcun--session-yiyus)
            (fangcun--reconcile-files files))
        (error
         (display-warning
          'fangcun
          (format "Automatic database reconciliation failed: %s"
                  (error-message-string error-data))
          :warning))))))

(defun fangcun--reconcile-file-operation (old-files &optional new-file)
  "Reconcile OLD-FILES and optional NEW-FILE after an operation."
  (when (and fangcun--session-active-p
             (file-exists-p fangcun-database-file))
    (condition-case error-data
        (fangcun--reconcile-files
         (append old-files (when new-file (list new-file))))
      (error
       (display-warning
        'fangcun
        (format "File operation database update failed: %s"
                (error-message-string error-data))
        :warning)))))

(defun fangcun--rename-destination (file new-name)
  "Return the final destination when FILE is renamed to NEW-NAME."
  (let ((file (expand-file-name file))
        (new-name (expand-file-name new-name)))
    (if (file-directory-p new-name)
        (expand-file-name (file-name-nondirectory file) new-name)
      new-name)))

(defun fangcun--around-rename-file
    (function file new-name &rest arguments)
  "Call FUNCTION to rename FILE to NEW-NAME, then reconcile Fangcun."
  (let ((old-file (expand-file-name file))
        (new-file (fangcun--rename-destination file new-name)))
    (prog1 (apply function file new-name arguments)
      (fangcun--reconcile-file-operation
       (list old-file) new-file))))

(defun fangcun--around-delete-file (function file &rest arguments)
  "Call FUNCTION to delete FILE, then reconcile Fangcun."
  (let ((absolute-file (expand-file-name file)))
    (prog1 (apply function file arguments)
      (fangcun--reconcile-file-operation
       (list absolute-file)))))

(defun fangcun--around-vc-delete-file
    (function file-or-files &rest arguments)
  "Call FUNCTION to delete FILE-OR-FILES, then reconcile Fangcun."
  (let ((files
         (mapcar #'expand-file-name
                 (ensure-list file-or-files))))
    (prog1 (apply function file-or-files arguments)
      (fangcun--reconcile-file-operation files))))

;;;###autoload
(defun fangcun-db-update-file (file &optional no-message)
  "Replace database entries for saved Org FILE.
When NO-MESSAGE is non-nil, do not report the indexed counts."
  (interactive
   (list
    (or buffer-file-name
        (user-error "The current buffer is not visiting a file"))))
  (setq file (expand-file-name file))
  (let* ((yiyus (fangcun--ensure-session))
         (yiyu
          (or (fangcun--yiyu-containing-file file yiyus)
              (user-error
               "File is outside the configured Fangcun yiyus: %s"
               file))))
    (fangcun--db-update-file-in-yiyu file yiyu no-message)))

(defun fangcun--update-current-file ()
  "Update the current unmodified file when Fangcun manages it."
  (when (and buffer-file-name
             (file-exists-p fangcun-database-file)
             (string-match-p "\\.org\\'" buffer-file-name))
    (let* ((yiyus (fangcun--configured-yiyus))
           (yiyu
            (fangcun--yiyu-containing-file buffer-file-name yiyus)))
      (when yiyu
        (fangcun--db-update-file-in-yiyu buffer-file-name yiyu t)))))

(defun fangcun--update-after-save ()
  "Update the current Fangcun file after saving it."
  (when fangcun-db-update-on-save
    (fangcun--update-current-file)))

(defun fangcun--update-after-revert ()
  "Update the current Fangcun file after reverting it from disk."
  (fangcun--update-current-file))

(defun fangcun--setup-file-updates ()
  "Arrange for the current Org buffer to update Fangcun from disk."
  (add-hook 'after-save-hook #'fangcun--update-after-save nil t)
  (add-hook 'after-revert-hook #'fangcun--update-after-revert nil t))

(defun fangcun-node-from-id (id)
  "Return the Fangcun node named ID, or nil when it is not indexed."
  (fangcun-store-node-from-id fangcun-database-file id))

(defun fangcun-node-list ()
  "Return all nodes currently stored in the Fangcun database."
  (fangcun-store-node-list fangcun-database-file))

;;;###autoload
(defun fangcun--id-find (id &optional markerp)
  "Return the Fangcun location of ID, or nil to let Org continue.
When MARKERP is non-nil, return the location as a marker."
  (setq id
        (cond
         ((symbolp id) (symbol-name id))
         ((numberp id) (number-to-string id))
         (t id)))
  (when (file-exists-p fangcun-database-file)
    (when-let* ((location
                 (fangcun-store-id-location fangcun-database-file id)))
      (org-id-find-id-in-file
       id (expand-file-name (cdr location) (car location)) markerp))))

(defun fangcun-backlink-list (target-id)
  "Return unique source nodes linking to TARGET-ID.
When one source contains several links, retain its first occurrence."
  (fangcun-store-backlink-list fangcun-database-file target-id))

(defun fangcun-backlink-occurrence-list (target-id)
  "Return every indexed backlink occurrence to TARGET-ID."
  (fangcun-store-backlink-occurrence-list fangcun-database-file target-id))

(defun fangcun--node-candidate (node)
  "Return a unique completion candidate for NODE."
  (let ((title (fangcun-node-title node))
        (tags (fangcun-node-tags node)))
    (propertize
     (concat
      title
      (when tags
        (propertize
         (concat "  #" (string-join tags " #"))
         'face 'org-tag))
      (propertize
       (concat "\u2063" (fangcun-node-id node))
       'invisible t))
     'fangcun-node node)))

(defun fangcun--node-candidates (node)
  "Return completion pairs for NODE's title and aliases."
  (mapcar
   (lambda (title)
     (let ((candidate-node (copy-fangcun-node node)))
       (setf (fangcun-node-title candidate-node) title)
       (cons (fangcun--node-candidate candidate-node)
             candidate-node)))
   (delete-dups
    (cons (fangcun-node-title node)
          (copy-sequence (fangcun-node-aliases node))))))

(defun fangcun--node-annotation (candidate)
  "Return the location annotation for CANDIDATE."
  (when-let* ((node (get-text-property 0 'fangcun-node candidate)))
    (propertize
     (format "  %s › %s%s"
             (fangcun-node-yiyu-name node)
             (fangcun-node-file node)
             (if-let* ((outline (fangcun-node-outline-path node)))
                 (concat " › " (string-join outline " › "))
               " (file)"))
     'face 'completions-annotations)))

(defun fangcun--read-node (&optional initial-input)
  "Read and return a Fangcun node.
INITIAL-INPUT seeds the node completion minibuffer."
  (let* ((nodes (fangcun-node-list))
          (candidates
           (mapcan #'fangcun--node-candidates nodes))
         (completion-extra-properties
          '(:category fangcun-node
            :annotation-function fangcun--node-annotation)))
    (unless candidates
      (user-error "No Fangcun nodes; run `fangcun-db-sync' first"))
    (let ((choice
           (completing-read
            "Fangcun node: " candidates nil t initial-input)))
      (cdr (assoc choice candidates)))))

(defun fangcun--org-id-native-candidate ()
  "Return the completion candidate dispatching to native Org IDs."
  (propertize
   (concat
    "Org heading…"
    (propertize "⁣org-id" 'invisible t))
   'fangcun-org-id-native t))

(defun fangcun--id-complete (function &optional argument)
  "Complete an Org ID through Fangcun before calling FUNCTION.
ARGUMENT is the optional argument originally passed to
`org-id-complete'.  Native Org heading completion remains available as a
distinguished candidate."
  (if (null fangcun-yiyus)
      (funcall function argument)
    (fangcun--ensure-session)
    (let* ((nodes (fangcun-node-list))
           (candidates
            (mapcan #'fangcun--node-candidates nodes)))
      (if (null candidates)
          (funcall function argument)
        (let* ((native (fangcun--org-id-native-candidate))
               (completion-extra-properties
                '(:category fangcun-node
                  :annotation-function fangcun--node-annotation))
               (choice
                (completing-read
                 "ID target: "
                 (append candidates (list native)) nil t)))
          (if (equal choice native)
              (funcall function argument)
            (concat
             "id:"
             (fangcun-node-id (cdr (assoc choice candidates))))))))))

(defun fangcun--id-description (link description)
  "Return Fangcun's description for Org ID LINK, or nil.
An existing non-empty DESCRIPTION always wins.  Returning nil lets
`org-id-description' handle IDs outside the Fangcun database."
  (or (org-string-nw-p description)
      (when (and (stringp link)
                 (string-prefix-p "id:" link)
                 (file-exists-p fangcun-database-file))
        (let* ((path (org-link-unescape (substring link 3)))
               (id
                (if (string-match "::" path)
                    (substring path 0 (match-beginning 0))
                  path)))
          (when-let* ((node (fangcun-node-from-id id)))
            (fangcun-node-title node))))))

(defun fangcun--valid-tag-p (tag)
  "Return non-nil when TAG is a valid Org tag name."
  (and (stringp tag)
       (string-match-p (concat "\\`\\(?:" org-tag-re "\\)\\'") tag)))

(defun fangcun--validate-tags (tags)
  "Return validated TAGS without text properties or duplicates."
  (setq tags
        (delete-dups
         (mapcar #'substring-no-properties tags)))
  (dolist (tag tags)
    (unless (fangcun--valid-tag-p tag)
      (user-error
       (concat
        "Invalid Org tag %S; use letters, numbers, _, @, #, or %%")
       tag)))
  tags)

(defun fangcun--tag-completions ()
  "Return known Org and Fangcun tags for completion."
  (let (tags)
    (dolist (entry
             (append org-current-tag-alist
                     org-tag-persistent-alist
                     org-tag-alist
                     (org-get-buffer-tags)))
      (when-let* ((tag
                   (cond
                    ((stringp entry) entry)
                    ((and (consp entry) (stringp (car entry)))
                     (car entry)))))
        (when (fangcun--valid-tag-p tag)
          (push (substring-no-properties tag) tags))))
    (setq tags (append (fangcun-store-tags fangcun-database-file) tags))
    (sort (delete-dups tags) #'string-lessp)))

(defun fangcun--read-tags (current-tags)
  "Read node tags, initially offering CURRENT-TAGS."
  (let ((crm-separator "[ \t]*:[ \t]*")
        (completion-extra-properties '(:category org-tag))
        (prompt "Node tags: ")
        (initial (org-make-tag-string current-tags))
        tags invalid)
    (while
        (progn
          (setq tags
                (delete
                 ""
                 (mapcar
                  #'string-trim
                  (completing-read-multiple
                   prompt (fangcun--tag-completions)
                   nil nil initial 'org-tags-history)))
                invalid
                (seq-find
                 (lambda (tag)
                   (not (fangcun--valid-tag-p tag)))
                 tags))
          (when invalid
            (setq prompt (format "Node tags [invalid %S]: " invalid)
                  initial (org-make-tag-string tags)))
          invalid))
    (fangcun--validate-tags tags)))

(defun fangcun--local-tags-at-point ()
  "Return the local Org tags of the node at point."
  (mapcar
   #'substring-no-properties
   (if (= (org-outline-level) 0)
       org-file-tags
     (org-get-tags nil t))))

(defun fangcun--set-file-tags (tags)
  "Replace the current file node's FILETAGS keywords with TAGS."
  (let ((case-fold-search t)
        (value (org-make-tag-string tags))
        (limit
         (save-excursion
           (goto-char (point-min))
           (if (re-search-forward org-outline-regexp-bol nil t)
               (line-beginning-position)
             (point-max))))
        ranges)
    (goto-char (point-min))
    (while (re-search-forward "^#\\+filetags:[ \t]*.*$" limit t)
      (push (cons (line-beginning-position)
                  (line-end-position))
            ranges))
    (setq ranges (nreverse ranges))
    (cond
     ((and tags ranges)
      (dolist (range (reverse (cdr ranges)))
        (delete-region
         (car range)
         (min (point-max) (1+ (cdr range)))))
      (goto-char (caar ranges))
      (delete-region (caar ranges) (cdar ranges))
      (insert "#+filetags: " value))
     (tags
      (goto-char
       (or (cdr (org-get-property-block (point-min)))
           (user-error "The Fangcun file node has no property drawer")))
      (forward-line)
      (insert "#+filetags: " value "\n"))
     (t
      (dolist (range (reverse ranges))
        (delete-region
         (car range)
         (min (point-max) (1+ (cdr range)))))))
    (org-set-regexps-and-options 'tags-only)))

(defun fangcun--set-local-tags-at-point (tags)
  "Replace the local Org tags of the node at point with TAGS."
  ;; `org-set-tags' only supports headlines, so file nodes need to update
  ;; FILETAGS separately.
  (if (= (org-outline-level) 0)
      (fangcun--set-file-tags tags)
    (org-set-tags tags)))

(defun fangcun--read-backlink (target-id)
  "Read and return a backlink to TARGET-ID."
  (let* ((backlinks (fangcun-backlink-list target-id))
         (candidates
           (mapcan
            (lambda (backlink)
              (mapcar
               (lambda (candidate)
                 (let ((candidate-backlink
                        (copy-fangcun-backlink backlink)))
                   (setf
                    (fangcun-backlink-node candidate-backlink)
                    (cdr candidate))
                   (cons (car candidate) candidate-backlink)))
               (fangcun--node-candidates
                (fangcun-backlink-node backlink))))
            backlinks))
         (completion-extra-properties
          '(:category fangcun-node
            :annotation-function fangcun--node-annotation)))
    (unless candidates
      (user-error "No backlinks to the current Fangcun node"))
    (let ((choice
           (completing-read "Fangcun backlink: " candidates nil t)))
      (cdr (assoc choice candidates)))))

(defun fangcun--node-absolute-file (node)
  "Return the absolute file name containing NODE."
  (expand-file-name
   (fangcun-node-file node)
   (fangcun-node-yiyu-root node)))

(defun fangcun-node-visit (node &optional other-window)
  "Visit the Org NODE and return it.
When OTHER-WINDOW is non-nil, use another window."
  (let ((file (fangcun--node-absolute-file node)))
    (unless (file-exists-p file)
      (user-error "Fangcun node file no longer exists: %s" file))
    (if other-window
        (find-file-other-window file)
      (find-file file))
    (widen)
    (if-let* ((position
               (org-find-entry-with-id (fangcun-node-id node))))
        (goto-char position)
      (user-error "Fangcun node ID no longer exists: %s"
                  (fangcun-node-id node)))
    (org-fold-show-context 'link-search)
    node))

(defun fangcun--read-new-file (title directory root)
  "Read a new file below ROOT, starting in DIRECTORY.
Suggest a file name from TITLE when it is non-empty."
  (let ((initial
         (unless (string-empty-p title)
           (concat title ".org")))
        (prompt "New Fangcun file: ")
        file error)
    (while
        (progn
          (setq file
                (expand-file-name
                 (read-file-name prompt directory nil nil initial)
                 directory)
                error
                (fangcun--new-file-name-error file root))
          (when error
            (setq prompt
                  (format "New Fangcun file [%s]: " error)
                  initial
                  (if (file-remote-p file)
                      file
                    (file-relative-name file directory))))
          error))
    file))

(defun fangcun--node-id-get-create ()
  "Return the current Org entry's local ID, creating one if needed."
  (or (org-id-get)
      (let ((id (org-id-new)))
        (org-entry-put (point) "ID" id)
        id)))

;;;###autoload
(defun fangcun-file-node-create ()
  "Visit a new unsaved Org file with a Fangcun file node."
  (interactive)
  (let* ((yiyus (fangcun--configured-yiyus))
         (current-yiyu
          (and buffer-file-name
               (fangcun--yiyu-containing-file buffer-file-name yiyus))))
    (unless yiyus
      (user-error "Configure `fangcun-yiyus' before creating a node"))
    (fangcun--ensure-session yiyus)
    (let* ((yiyu
            (or current-yiyu
                (if (null (cdr yiyus))
                    (car yiyus)
                  (let* ((candidates
                          (mapcar
                           (lambda (entry)
                             (cons (fangcun-yiyu-name entry) entry))
                           yiyus))
                         (choice
                          (completing-read
                           "Fangcun yiyu: " candidates nil t)))
                    (cdr (assoc choice candidates))))))
           (root (fangcun-yiyu-root yiyu))
           (directory
            (if current-yiyu
                (file-name-directory buffer-file-name)
              root)))
      (unless (file-directory-p root)
        (user-error "Fangcun yiyu does not exist: %s" root))
      (let* ((title (read-string "Node title (empty to omit): "))
             (file
              (fangcun--read-new-file title directory root)))
        (make-directory (file-name-directory file) t)
        (find-file file)
        (goto-char (point-min))
        (unless (string-empty-p title)
          (insert "#+title: " title "\n"))
        (insert "\n")
        (goto-char (point-min))
        (let ((id (fangcun--node-id-get-create)))
          (org-cycle-set-startup-visibility)
          (goto-char (point-max))
          id)))))

;;;###autoload
(defun fangcun-heading-node-create ()
  "Give the current Org heading its own Fangcun node ID.
Return an existing local ID without replacing it.  Saving the file makes a
new ID available through the Fangcun index."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun heading nodes require an Org buffer"))
  (let ((heading
         (save-excursion
           (save-restriction
             (widen)
             (org-back-to-heading t)
             (point))))
        (yiyus (fangcun--configured-yiyus)))
    (unless (and buffer-file-name
                 (fangcun--yiyu-containing-file buffer-file-name yiyus))
      (user-error "Current Org file does not belong to a Fangcun yiyu"))
    (fangcun--ensure-session yiyus)
    (let (created id)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char heading)
          (setq id (org-id-get))
          (unless id
            (setq created t
                  id (fangcun--node-id-get-create)))))
      (when (called-interactively-p 'interactive)
        (message "%s Fangcun heading node %s"
                 (if created "Created" "Found") id))
      id)))

;;;###autoload
(defun fangcun-node-find ()
  "Choose a Fangcun node by title or alias and visit it."
  (interactive)
  (fangcun--ensure-session)
  (fangcun-node-visit (fangcun--read-node)))

;;;###autoload
(defun fangcun-node-insert ()
  "Choose a Fangcun node and insert an Org ID link to it.
An active region becomes the link description without filtering completion.
When point is on an existing ID link, replace its target while preserving its
description.  Otherwise, the chosen title or alias becomes the description."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun node links can only be inserted in Org buffers"))
  (fangcun--ensure-session)
  (let* ((regionp (org-region-active-p))
         (element
          (and (not regionp)
               (let ((context (org-element-context)))
                 (and (eq (org-element-type context) 'link)
                      (equal (org-element-property :type context) "id")
                      context))))
         (begin
          (cond
           (regionp (region-beginning))
           (element (org-element-property :begin element))))
         (end
          (cond
           (regionp (region-end))
           (element (org-element-property :end element))))
         (description
          (cond
           (regionp
            (org-link-display-format
             (buffer-substring-no-properties begin end)))
           ((and element
                 (org-element-property :contents-begin element))
            (buffer-substring-no-properties
             (org-element-property :contents-begin element)
             (org-element-property :contents-end element)))))
         (begin-marker
          (and begin (set-marker (make-marker) begin)))
         (end-marker
          (and end (set-marker (make-marker) end))))
    (unwind-protect
        (atomic-change-group
          (let ((node (fangcun--read-node (and element description))))
            (when (and begin-marker end-marker)
              (delete-region begin-marker end-marker)
              (goto-char begin-marker))
            (insert
             (org-link-make-string
              (concat "id:" (fangcun-node-id node))
              (or description (fangcun-node-title node))))
            node))
      (when begin-marker
        (set-marker begin-marker nil))
      (when end-marker
        (set-marker end-marker nil))
      (deactivate-mark))))

;;;###autoload
(defun fangcun-node-set-tags (&optional tags)
  "Set local TAGS on the nearest enclosing Fangcun node.
Interactively, edit the current local tags with completion."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun node tags are only available in Org buffers"))
  (fangcun--ensure-session)
  (save-excursion
    (save-restriction
      (widen)
      (org-set-regexps-and-options 'tags-only)
      (unless (fangcun-org-goto-node)
        (user-error "Point is not inside a Fangcun node"))
      (when (called-interactively-p 'interactive)
        (setq tags
              (fangcun--read-tags
               (fangcun--local-tags-at-point))))
      (setq tags (fangcun--validate-tags tags))
      (fangcun--set-local-tags-at-point tags)
      tags)))

(defun fangcun--backlink-at-point ()
  "Return the Fangcun backlink represented by the button at point."
  (when-let* ((button (button-at (point))))
    (button-get button 'fangcun-backlink)))

(defun fangcun-backlink-visit (backlink)
  "Visit the indexed link represented by BACKLINK and return it.
From a backlinks buffer, select its source in another window."
  (interactive
   (list
    (or (fangcun--backlink-at-point)
        (user-error "No Fangcun backlink at point"))))
  (fangcun-node-visit (fangcun-backlink-node backlink)
                     (derived-mode-p 'fangcun-backlinks-mode))
  (goto-char (fangcun-backlink-position backlink))
  (org-fold-show-context 'link-search)
  backlink)

(defun fangcun-backlink-show ()
  "Show the backlink at point without leaving the results window."
  (interactive)
  (save-selected-window
    (call-interactively #'fangcun-backlink-visit)))

;;;###autoload
(defun fangcun-backlink-find ()
  "Choose a backlink to the current Fangcun node and visit its link."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun backlinks are only available in Org buffers"))
  (let ((target-id
         (or (fangcun-org-node-id-at-point)
             (user-error "Point is not inside a Fangcun node"))))
    (fangcun--ensure-session)
    (let ((backlink (fangcun--read-backlink target-id)))
      (fangcun-backlink-visit backlink))))

(defun fangcun--backlink-preview (backlink buffer)
  "Return a one-line preview of BACKLINK from BUFFER."
  (let ((position (fangcun-backlink-position backlink)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (if (> position (point-max))
              "[Link position is stale; synchronize Fangcun]"
            (goto-char position)
            (string-trim
             (substring-no-properties
              (org-link-display-format
               (buffer-substring
                (line-beginning-position)
                (line-end-position)))))))))))

(defun fangcun--backlink-previews (backlinks)
  "Return an eq table mapping BACKLINKS to one-line previews.
Each source file is read from disk at most once."
  (let ((backlinks-by-file (make-hash-table :test #'equal))
        (previews (make-hash-table :test #'eq)))
    (dolist (backlink backlinks)
      (let ((file
             (fangcun--node-absolute-file
              (fangcun-backlink-node backlink))))
        (puthash file
                 (cons backlink (gethash file backlinks-by-file))
                 backlinks-by-file)))
    (cl-labels
        ((record-previews
          (file-backlinks buffer)
          (dolist (backlink file-backlinks)
            (puthash backlink
                     (fangcun--backlink-preview backlink buffer)
                     previews))))
      (maphash
       (lambda (file file-backlinks)
         (if (not (file-readable-p file))
             (dolist (backlink file-backlinks)
               (puthash backlink
                        "[Source file is unavailable]"
                        previews))
           (if-let* ((buffer (fangcun-org-saved-file-buffer file)))
               (record-previews file-backlinks buffer)
             (with-temp-buffer
               (insert-file-contents file)
               (record-previews
                file-backlinks (current-buffer))))))
       backlinks-by-file))
    previews))

(defun fangcun--backlink-button-action (button)
  "Visit the Fangcun backlink represented by BUTTON."
  (fangcun-backlink-visit
   (button-get button 'fangcun-backlink)))

(define-button-type 'fangcun-backlink-button
  'action #'fangcun--backlink-button-action
  'face 'link
  'follow-link t
  'help-echo "Visit this backlink")

(defun fangcun-backlinks-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the current Fangcun backlinks buffer."
  (interactive)
  (unless (derived-mode-p 'fangcun-backlinks-mode)
    (user-error "This is not a Fangcun backlinks buffer"))
  (let* ((target-id fangcun-backlinks-target-id)
         (target
          (or (fangcun-node-from-id target-id)
              (user-error "Fangcun node is no longer indexed: %s"
                          target-id)))
         (backlinks (fangcun-backlink-occurrence-list target-id))
         (previews (fangcun--backlink-previews backlinks))
         (inhibit-read-only t)
         previous-source-id)
    (erase-buffer)
    (insert (propertize
             (format "Backlinks to %s" (fangcun-node-title target))
             'face 'bold)
            "\n\n")
    (if (null backlinks)
        (insert "No backlinks.\n")
      (dolist (backlink backlinks)
        (let* ((source (fangcun-backlink-node backlink))
               (source-id (fangcun-node-id source)))
          (unless (equal source-id previous-source-id)
            (when previous-source-id
              (insert "\n"))
            (insert (propertize (fangcun-node-title source) 'face 'bold)
                    (propertize
                     (format "  %s — %s"
                             (fangcun-node-yiyu-name source)
                             (fangcun-node-file source))
                     'face 'shadow)
                    "\n")
            (setq previous-source-id source-id))
          (insert "  ")
          (insert-text-button
           (gethash backlink previews)
           :type 'fangcun-backlink-button
           'fangcun-backlink backlink)
          (insert "\n"))))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (when-let* ((button (next-button (point))))
      (goto-char (button-start button)))))

;;;###autoload
(defun fangcun-backlinks ()
  "Display every backlink occurrence to the Fangcun node at point."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Fangcun backlinks are only available in Org buffers"))
  (let ((target-id
         (or (fangcun-org-node-id-at-point)
             (user-error "Point is not inside a Fangcun node")))
        (buffer (get-buffer-create fangcun-backlinks-buffer-name)))
    (fangcun--ensure-session)
    (with-current-buffer buffer
      (fangcun-backlinks-mode)
      (setq fangcun-backlinks-target-id target-id)
      (fangcun-backlinks-refresh))
    (pop-to-buffer buffer)
    buffer))

(defun fangcun--check-issue-for-node (node count)
  "Return a duplicate-ID check issue for NODE among COUNT occurrences."
  (make-fangcun-check-issue
   :severity 'error
   :file (fangcun--node-absolute-file node)
   :position (fangcun-node-position node)
   :line (fangcun-node-line node)
   :message
   (format "Duplicate Fangcun node ID %S (%d occurrences)"
           (fangcun-node-id node) count)))

(defun fangcun--check-issue-for-link (state link severity message)
  "Return a check issue for LINK in STATE with SEVERITY and MESSAGE."
  (make-fangcun-check-issue
   :severity severity
   :file (fangcun-file-state-absolute-file state)
   :position (fangcun-link-position link)
   :line (fangcun-link-line link)
   :message message))

(defun fangcun--check-id-resolves-externally-p (id cache)
  "Return whether Org resolves ID outside the scanned nodes, caching in CACHE."
  (let ((status (gethash id cache 'unchecked)))
    (when (eq status 'unchecked)
      (setq status
            (if-let* ((file (ignore-errors (org-id-find-id-file id))))
                (if (ignore-errors (org-id-find-id-in-file id file))
                    'resolved
                  'unresolved)
              'unresolved))
      (puthash id status cache))
    (eq status 'resolved)))

(defun fangcun--check-issue-before-p (left right)
  "Return non-nil when check issue LEFT should appear before RIGHT."
  (let ((left-file (fangcun-check-issue-file left))
        (right-file (fangcun-check-issue-file right)))
    (if (equal left-file right-file)
        (< (fangcun-check-issue-position left)
           (fangcun-check-issue-position right))
      (string-lessp left-file right-file))))

(defun fangcun--check-scan ()
  "Scan configured yiyus and return Fangcun consistency results."
  (let ((yiyus (fangcun--configured-yiyus)))
    (unless yiyus
      (user-error "Configure `fangcun-yiyus' before checking Fangcun"))
    (let ((states (fangcun--scan-file-states yiyus))
          (nodes-by-id (make-hash-table :test #'equal))
          (external-ids (make-hash-table :test #'equal))
          links
          issues)
      (dolist (state states)
        (let ((data
               (fangcun-org-read-file
                (fangcun-file-state-yiyu state)
                (fangcun-file-state-absolute-file state)
                t)))
          (dolist (node (plist-get data :nodes))
            (push node (gethash (fangcun-node-id node) nodes-by-id)))
          (dolist (link (plist-get data :links))
            (push (cons state link) links))))
      (maphash
       (lambda (_id nodes)
         (when (cdr nodes)
           (dolist (node nodes)
             (push (fangcun--check-issue-for-node node (length nodes))
                   issues))))
       nodes-by-id)
      (dolist (entry links)
        (let* ((state (car entry))
               (link (cdr entry))
               (target-id (fangcun-link-target-id link)))
          (unless (fangcun-link-source-id link)
            (push
             (fangcun--check-issue-for-link
              state link 'warning
              (format "ID link to %S has no owning Fangcun node" target-id))
             issues))
          (unless (or (gethash target-id nodes-by-id)
                      (fangcun--check-id-resolves-externally-p
                       target-id external-ids))
            (push
             (fangcun--check-issue-for-link
              state link 'error
              (format "Unresolved Org ID link target %S" target-id))
             issues))))
      (list :yiyus (length yiyus)
            :files (length states)
            :issues (sort issues #'fangcun--check-issue-before-p)))))

(defun fangcun--check-issue-at-point ()
  "Return the Fangcun check issue represented by the button at point."
  (when-let* ((button (button-at (point))))
    (button-get button 'fangcun-check-issue)))

(defun fangcun-check-visit (issue)
  "Visit Fangcun check ISSUE's source location in another window."
  (interactive
   (list
    (or (fangcun--check-issue-at-point)
        (user-error "No Fangcun check result at point"))))
  (let ((file (fangcun-check-issue-file issue)))
    (unless (file-exists-p file)
      (user-error "Fangcun check source file no longer exists: %s" file))
    (find-file-other-window file)
    (widen)
    (goto-char
     (min (or (fangcun-check-issue-position issue) (point-min))
          (point-max)))
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context 'link-search))
    issue))

(defun fangcun-check-show ()
  "Show the check result at point without leaving the results window."
  (interactive)
  (save-selected-window
    (call-interactively #'fangcun-check-visit)))

(defun fangcun--check-button-action (button)
  "Visit the Fangcun check issue represented by BUTTON."
  (fangcun-check-visit (button-get button 'fangcun-check-issue)))

(define-button-type 'fangcun-check-button
  'action #'fangcun--check-button-action
  'face nil
  'follow-link t
  'mouse-face 'highlight
  'help-echo "Visit this Fangcun check result")

(defun fangcun--insert-check-issue (issue)
  "Insert one Fangcun check ISSUE into the current buffer."
  (let* ((severity (fangcun-check-issue-severity issue))
         (label
          (concat
           (propertize (upcase (symbol-name severity))
                       'face (if (eq severity 'error) 'error 'warning))
           "  "
           (abbreviate-file-name (fangcun-check-issue-file issue))
           ":" (number-to-string (fangcun-check-issue-line issue))
           "  " (fangcun-check-issue-message issue))))
    (insert-text-button
     label :type 'fangcun-check-button 'fangcun-check-issue issue)
    (insert "\n")))

(defun fangcun-check-refresh (&optional _ignore-auto _noconfirm)
  "Rerun and display the current Fangcun consistency checks."
  (interactive)
  (unless (derived-mode-p 'fangcun-check-mode)
    (user-error "This is not a Fangcun check buffer"))
  (message "Checking Fangcun...")
  (let* ((result (fangcun--check-scan))
         (issues (plist-get result :issues))
         (errors
          (seq-count
           (lambda (issue)
             (eq (fangcun-check-issue-severity issue) 'error))
           issues))
         (warnings (- (length issues) errors))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Fangcun Check" 'face 'bold) "\n\n")
    (insert
     (format "Checked %d files in %d yiyu roots.\n"
             (plist-get result :files) (plist-get result :yiyus)))
    (insert "gf previews, RET visits an issue, gr checks again, q quits.\n")
    (if issues
        (progn
          (insert (format "Found %d errors and %d warnings.\n\n"
                          errors warnings))
          (dolist (issue issues)
            (fangcun--insert-check-issue issue)))
      (insert "Fangcun found no problems.\n"))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (when-let* ((button (next-button (point))))
      (goto-char (button-start button)))
    (if issues
        (message "Fangcun check found %d errors and %d warnings"
                 errors warnings)
      (message "Fangcun found no problems"))))

;;;###autoload
(defun fangcun-check ()
  "Scan configured Fangcun files and display consistency problems."
  (interactive)
  (let ((buffer (get-buffer-create fangcun-check-buffer-name)))
    (with-current-buffer buffer
      (fangcun-check-mode)
      (fangcun-check-refresh))
    (pop-to-buffer buffer)
    buffer))

(provide 'fangcun)

;;; fangcun.el ends here
