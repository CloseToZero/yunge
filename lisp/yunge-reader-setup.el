;;; yunge-reader-setup.el --- PDFium setup -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'compile)
(require 'seq)
(require 'subr-x)
(require 'yunge-reader-native)
(require 'yunge-state)

(defconst yunge-reader-setup--buffer-name "*Yunge Reader setup*"
  "Name of the PDFium setup log buffer.")

(defconst yunge-reader-setup--manifest-file
  (expand-file-name
   "../native/yunge-reader/pdfium-manifest.eld"
   yunge-reader-native--source-directory)
  "Tracked manifest describing pinned PDFium assets.")

(defconst yunge-reader-setup--release-base-url
  "https://github.com/bblanchon/pdfium-binaries/releases/download/"
  "Base URL of the pinned PDFium binary distribution.")

(defvar yunge-reader-setup--process nil
  "Current PDFium download or extraction process, or nil.")

(defvar yunge-reader-setup--work-directory nil
  "Private work directory of the current setup operation, or nil.")

(defvar yunge-reader-setup--asset nil
  "PDFium asset selected for the current setup operation, or nil.")

(defun yunge-reader-setup--manifest ()
  "Read and validate the tracked PDFium manifest."
  (unless (file-readable-p yunge-reader-setup--manifest-file)
    (error "PDFium manifest is unavailable: %s"
           yunge-reader-setup--manifest-file))
  (let ((manifest
         (with-temp-buffer
           (insert-file-contents yunge-reader-setup--manifest-file)
           (read (current-buffer)))))
    (unless (and (= (plist-get manifest :schema-version) 1)
                 (equal (plist-get manifest :pdfium-api)
                        yunge-reader-native-pdfium-api)
                 (stringp (plist-get manifest :pdfium-version))
                 (stringp (plist-get manifest :release))
                 (listp (plist-get manifest :assets)))
      (error "Invalid Yunge Reader PDFium manifest"))
    manifest))

(defun yunge-reader-setup--architecture ()
  "Return the supported architecture of this Emacs build."
  (cond
   ((string-match-p
     "\\`\\(?:aarch64\\|arm64\\)" system-configuration)
    'arm64)
   ((string-match-p
     "\\`\\(?:x86_64\\|amd64\\)" system-configuration)
    'x64)
   (t
    (user-error "Unsupported Yunge Reader architecture: %s"
                system-configuration))))

(defun yunge-reader-setup--asset (manifest)
  "Return the platform asset in MANIFEST for this Emacs."
  (let* ((architecture (yunge-reader-setup--architecture))
         (asset
          (seq-find
           (lambda (candidate)
             (and (eq (plist-get candidate :system) system-type)
                  (eq (plist-get candidate :architecture)
                      architecture)))
           (plist-get manifest :assets))))
    (unless asset
      (user-error "No PDFium asset for %s %s"
                  system-type architecture))
    (unless (and (string-match-p
                  "\\`pdfium-[[:alnum:]-]+\\.tgz\\'"
                  (plist-get asset :file))
                 (string-match-p
                  "\\`[[:xdigit:]]\\{64\\}\\'"
                  (plist-get asset :sha256))
                 (member (plist-get asset :library)
                         '("bin/pdfium.dll"
                           "bin/libpdfium.so"
                           "bin/libpdfium.dylib")))
      (error "Invalid PDFium asset manifest: %S" asset))
    asset))

(defun yunge-reader-setup--asset-url (manifest asset)
  "Return the download URL for ASSET in MANIFEST."
  (concat yunge-reader-setup--release-base-url
          (plist-get manifest :release) "/"
          (plist-get asset :file)))

(defun yunge-reader-setup--file-sha256 (file)
  "Return the SHA-256 digest of the literal bytes in FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun yunge-reader-setup--version (file)
  "Return the dotted PDFium version recorded in FILE, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let (values)
        (dolist (component '("MAJOR" "MINOR" "BUILD" "PATCH"))
          (goto-char (point-min))
          (if (re-search-forward
               (format "^%s=\\([0-9]+\\)$" component) nil t)
              (push (match-string 1) values)
            (setq values nil)
            (goto-char (point-max))))
        (when (= (length values) 4)
          (string-join (nreverse values) "."))))))

(defun yunge-reader-setup--installed-p (manifest asset)
  "Return whether MANIFEST and ASSET are installed and coherent."
  (yunge-reader-setup--installed-p-in
   manifest asset (yunge-reader-native-pdfium-directory)))

(defun yunge-reader-setup--cleanup-work ()
  "Remove only the private work directory of the current setup."
  (when yunge-reader-setup--work-directory
    (let* ((root
            (file-truename
             (yunge-var-subdirectory "yunge-reader/setup")))
           (work
            (file-name-as-directory
             (expand-file-name yunge-reader-setup--work-directory))))
      (when (and (file-directory-p work)
                 (file-in-directory-p work root))
        (delete-directory work t))))
  (setq yunge-reader-setup--work-directory nil
        yunge-reader-setup--asset nil))

(defun yunge-reader-setup--fail (format-string &rest arguments)
  "Finish setup unsuccessfully with FORMAT-STRING and ARGUMENTS."
  (setq yunge-reader-setup--process nil)
  (yunge-reader-setup--cleanup-work)
  (display-buffer yunge-reader-setup--buffer-name)
  (display-warning
   'yunge-reader
   (apply #'format format-string arguments)
   :error))

(defun yunge-reader-setup--archive-entries (tar archive)
  "Return normalized entries listed by TAR in ARCHIVE."
  (with-temp-buffer
    (let ((status
           (process-file tar nil t nil "-tzf" archive)))
      (unless (zerop status)
        (error "Cannot list PDFium archive")))
    (split-string (buffer-string) "[\r\n]+" t)))

(defun yunge-reader-setup--safe-archive-entry-p (entry)
  "Return whether archive ENTRY is a safe relative path."
  (and (not (file-name-absolute-p entry))
       (not (string-match-p "\\`[[:alpha:]]:" entry))
       (not (member ".." (split-string entry "/" t)))
       (not (string-match-p "\\\\" entry))))

(defun yunge-reader-setup--validate-archive (tar archive asset)
  "Validate ARCHIVE members listed by TAR for ASSET."
  (let ((entries (yunge-reader-setup--archive-entries tar archive)))
    (unless (seq-every-p
             #'yunge-reader-setup--safe-archive-entry-p entries)
      (error "PDFium archive contains an unsafe path"))
    (dolist (required
             (list "LICENSE" "VERSION" (plist-get asset :library)))
      (unless (member required entries)
        (error "PDFium archive is missing %s" required)))
    (unless (seq-some
             (lambda (entry)
               (string-prefix-p "licenses/" entry))
             entries)
      (error "PDFium archive is missing licenses"))))

(defun yunge-reader-setup--publish (manifest asset staging)
  "Publish extracted STAGING for MANIFEST and ASSET atomically."
  (unless (yunge-reader-setup--installed-p-in
           manifest asset staging)
    (error "Extracted PDFium files failed validation"))
  (let* ((target (directory-file-name
                  (yunge-reader-native-pdfium-directory)))
         (parent (file-name-directory target)))
    (make-directory parent t)
    (when (file-exists-p target)
      (let ((quarantine
             (concat target ".invalid-"
                     (format-time-string "%Y%m%d%H%M%S"))))
        (rename-file target quarantine)
        (display-warning
         'yunge-reader
         (format "Preserved invalid PDFium installation at %s"
                 quarantine)
         :warning)))
    (rename-file staging target)))

(defun yunge-reader-setup--installed-p-in (manifest asset directory)
  "Return whether MANIFEST and ASSET are valid below DIRECTORY."
  (let ((version-file (expand-file-name "VERSION" directory))
        (library
         (expand-file-name (plist-get asset :library) directory)))
    (and (file-regular-p library)
         (file-regular-p (expand-file-name "LICENSE" directory))
         (file-directory-p (expand-file-name "licenses" directory))
         (equal (yunge-reader-setup--version version-file)
                (plist-get manifest :pdfium-version)))))

(defun yunge-reader-setup--extract-sentinel
    (process _event manifest asset staging)
  "Finish extraction by PROCESS into STAGING for MANIFEST and ASSET."
  (when (and (memq (process-status process) '(exit signal failed))
             (not (process-get process 'yunge-reader-finished)))
    (process-put process 'yunge-reader-finished t)
    (if (not (and (eq (process-status process) 'exit)
                  (zerop (process-exit-status process))))
        (yunge-reader-setup--fail
         "PDFium extraction failed; see %s"
         yunge-reader-setup--buffer-name)
      (condition-case error-data
          (progn
            (yunge-reader-setup--publish manifest asset staging)
            (setq yunge-reader-setup--process nil)
            (yunge-reader-setup--cleanup-work)
            (message "Installed PDFium %s"
                     (plist-get manifest :pdfium-version))
            (yunge-reader-native--start-build))
        (error
         (yunge-reader-setup--fail
          "Could not install PDFium: %s"
          (error-message-string error-data)))))))

(defun yunge-reader-setup--start-extraction
    (tar archive manifest asset)
  "Extract pinned files from ARCHIVE with TAR for MANIFEST and ASSET."
  (yunge-reader-setup--validate-archive tar archive asset)
  (let ((staging
         (expand-file-name "extracted"
                           yunge-reader-setup--work-directory)))
    (make-directory staging t)
    (setq yunge-reader-setup--process
          (make-process
           :name "yunge-reader-pdfium-extract"
           :buffer (get-buffer-create yunge-reader-setup--buffer-name)
           :command
           (list tar "-xzf" archive "-C" staging
                 "LICENSE" "VERSION" "licenses"
                 (plist-get asset :library))
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :sentinel
           (lambda (child event)
             (yunge-reader-setup--extract-sentinel
              child event manifest asset staging))))))

(defun yunge-reader-setup--download-sentinel
    (process _event archive manifest asset tar)
  "Verify ARCHIVE downloaded by PROCESS, then extract MANIFEST ASSET."
  (when (and (memq (process-status process) '(exit signal failed))
             (not (process-get process 'yunge-reader-finished)))
    (process-put process 'yunge-reader-finished t)
    (if (not (and (eq (process-status process) 'exit)
                  (zerop (process-exit-status process))))
        (yunge-reader-setup--fail
         "PDFium download failed; see %s"
         yunge-reader-setup--buffer-name)
      (condition-case error-data
          (let ((actual (yunge-reader-setup--file-sha256 archive)))
            (unless (equal actual (plist-get asset :sha256))
              (error "PDFium archive SHA-256 mismatch"))
            (yunge-reader-setup--start-extraction
             tar archive manifest asset))
        (error
         (yunge-reader-setup--fail
          "Could not verify PDFium: %s"
          (error-message-string error-data)))))))

(defun yunge-reader-setup--start-download
    (curl tar manifest asset)
  "Download MANIFEST ASSET with CURL and prepare extraction with TAR."
  (let* ((root (yunge-var-subdirectory "yunge-reader/setup"))
         (work
          (progn
            (make-directory root t)
            (make-temp-file
             (expand-file-name "operation-" root) t)))
         (archive
          (expand-file-name (plist-get asset :file) work))
         (buffer (get-buffer-create yunge-reader-setup--buffer-name))
         (url (yunge-reader-setup--asset-url manifest asset)))
    (setq yunge-reader-setup--work-directory work
          yunge-reader-setup--asset asset)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Yunge Reader PDFium setup\n\n"
                "Asset: " (plist-get asset :file) "\n"
                "URL: " url "\n\n"))
      (setq default-directory root)
      (compilation-mode))
    (condition-case error-data
        (setq yunge-reader-setup--process
              (make-process
               :name "yunge-reader-pdfium-download"
               :buffer buffer
               :command
               (list curl "--fail" "--location"
                     "--silent" "--show-error"
                     "--output" archive url)
               :connection-type 'pipe
               :coding 'utf-8-unix
               :noquery t
               :sentinel
               (lambda (child event)
                 (yunge-reader-setup--download-sentinel
                  child event archive manifest asset tar))))
      (error
       (yunge-reader-setup--fail
        "Could not start PDFium download: %s"
        (error-message-string error-data))))
    (display-buffer buffer)
    (message "Downloading pinned PDFium %s..."
             (plist-get manifest :pdfium-version))))

(defun yunge-reader-setup--begin ()
  "Start or resume explicit PDFium setup."
  (when (process-live-p yunge-reader-setup--process)
    (user-error "Yunge Reader PDFium setup is already running"))
  (when (process-live-p yunge-reader-native--build-process)
    (user-error "Yunge Reader native helper is already being built"))
  (let* ((manifest (yunge-reader-setup--manifest))
         (asset (yunge-reader-setup--asset manifest)))
    (if (yunge-reader-setup--installed-p manifest asset)
        (yunge-reader-native--start-build)
      (let ((curl (executable-find "curl"))
            (tar (executable-find "tar")))
        (unless curl
          (user-error "curl is required to download PDFium"))
        (unless tar
          (user-error "tar is required to extract PDFium"))
        (yunge-reader-setup--start-download
         curl tar manifest asset)))))

(defun yunge-reader-setup ()
  "Install pinned PDFium and build the Yunge Reader native helper."
  (interactive)
  (when (> yunge-reader-native--client-count 0)
    (user-error "Close active Yunge Reader documents before setup"))
  (if (process-live-p yunge-reader-native--process)
      (progn
        (setq yunge-reader-native--build-after-stop 'setup)
        (yunge-reader-native-stop)
        (message "Stopping Yunge Reader helper before setup..."))
    (yunge-reader-setup--begin)))

(provide 'yunge-reader-setup)

;;; yunge-reader-setup.el ends here
