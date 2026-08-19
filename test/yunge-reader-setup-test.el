;;; yunge-reader-setup-test.el --- Setup tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'yunge-reader-setup)

(ert-deftest yunge-reader-setup-selects-the-pinned-windows-asset ()
  (let* ((system-type 'windows-nt)
         (system-configuration "x86_64-w64-mingw32")
         (manifest (yunge-reader-setup--manifest))
         (asset (yunge-reader-setup--asset manifest)))
    (should (equal (plist-get manifest :pdfium-version)
                   "151.0.7881.0"))
    (should (equal (plist-get asset :file)
                   "pdfium-win-x64.tgz"))
    (should (equal (plist-get asset :library)
                   "bin/pdfium.dll"))
    (should
     (equal
      (yunge-reader-setup--asset-url manifest asset)
      (concat
       "https://github.com/bblanchon/pdfium-binaries/"
       "releases/download/chromium/7881/pdfium-win-x64.tgz")))))

(ert-deftest yunge-reader-setup-selects-the-pinned-macos-asset-layout ()
  (let* ((system-type 'darwin)
         (system-configuration "aarch64-apple-darwin")
         (manifest (yunge-reader-setup--manifest))
         (asset (yunge-reader-setup--asset manifest)))
    (should (equal (plist-get asset :file)
                   "pdfium-mac-arm64.tgz"))
    (should (equal (plist-get asset :library)
                   "lib/libpdfium.dylib"))))

(ert-deftest yunge-reader-setup-selects-the-pinned-linux-asset-layout ()
  (let* ((system-type 'gnu/linux)
         (system-configuration "x86_64-pc-linux-gnu")
         (manifest (yunge-reader-setup--manifest))
         (asset (yunge-reader-setup--asset manifest)))
    (should (equal (plist-get asset :file)
                   "pdfium-linux-x64.tgz"))
    (should (equal (plist-get asset :library)
                   "lib/libpdfium.so"))))

(ert-deftest yunge-reader-setup-rejects-unsafe-archive-paths ()
  (dolist (path '("../escape" "inside/../../escape"
                  "/absolute" "C:/absolute" "inside\\escape"))
    (should-not
     (yunge-reader-setup--safe-archive-entry-p path)))
  (dolist (path '("LICENSE" "VERSION" "bin/pdfium.dll"
                  "licenses/icu/LICENSE"))
    (should (yunge-reader-setup--safe-archive-entry-p path))))

(ert-deftest yunge-reader-setup-validates-the-platform-library-path ()
  (let ((asset '(:library "lib/libpdfium.dylib")))
    (cl-letf (((symbol-function 'yunge-reader-setup--archive-entries)
               (lambda (_tar _archive)
                 '("LICENSE" "VERSION" "licenses/"
                   "licenses/pdfium.txt" "lib/libpdfium.dylib"))))
      (should-not
       (yunge-reader-setup--validate-archive "tar" "archive" asset)))
    (cl-letf (((symbol-function 'yunge-reader-setup--archive-entries)
               (lambda (_tar _archive)
                 '("LICENSE" "VERSION" "licenses/"
                   "licenses/pdfium.txt" "bin/libpdfium.dylib"))))
      (should-error
       (yunge-reader-setup--validate-archive "tar" "archive" asset)
       :type 'error))))

(ert-deftest yunge-reader-setup-hashes-file-bytes-not-its-name ()
  (let ((file (make-temp-file "yunge-reader-hash-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert "abc"))
          (should
           (equal
            (yunge-reader-setup--file-sha256 file)
            (concat "ba7816bf8f01cfea414140de5dae2223"
                    "b00361a396177a9cb410ff61f20015ad"))))
      (delete-file file))))

(ert-deftest yunge-reader-setup-validates-an-installed-pdfium-tree ()
  (let* ((root (make-temp-file "yunge-reader-pdfium-" t))
         (asset '(:library "bin/pdfium.dll"))
         (manifest '(:pdfium-version "151.0.7881.0")))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "bin" root))
          (make-directory (expand-file-name "licenses" root))
          (with-temp-file (expand-file-name "bin/pdfium.dll" root))
          (with-temp-file (expand-file-name "LICENSE" root))
          (with-temp-file (expand-file-name "VERSION" root)
            (insert "MAJOR=151\nMINOR=0\nBUILD=7881\nPATCH=0\n"))
          (should
           (yunge-reader-setup--installed-p-in
            manifest asset root))
          (with-temp-file (expand-file-name "VERSION" root)
            (insert "MAJOR=151\nMINOR=0\nBUILD=7882\nPATCH=0\n"))
          (should-not
           (yunge-reader-setup--installed-p-in
            manifest asset root)))
      (delete-directory root t))))

(ert-deftest yunge-reader-setup-is-idempotent-after-installation ()
  (let ((yunge-reader-setup--process nil)
        (yunge-reader-native--build-process nil)
        built)
    (cl-letf (((symbol-function 'yunge-reader-setup--manifest)
               (lambda () 'manifest))
              ((symbol-function 'yunge-reader-setup--asset)
               (lambda (_manifest) 'asset))
              ((symbol-function 'yunge-reader-setup--installed-p)
               (lambda (_manifest _asset) t))
              ((symbol-function 'yunge-reader-native--start-build)
               (lambda () (setq built t)))
              ((symbol-function 'process-live-p)
               (lambda (_process) nil)))
      (yunge-reader-setup--begin))
    (should built)))

(ert-deftest yunge-reader-setup-stops-an-idle-running-helper ()
  (let ((yunge-reader-native--process 'helper)
        (yunge-reader-native--client-count 0)
        (yunge-reader-native--build-after-stop nil)
        stopped)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (process) (eq process 'helper)))
              ((symbol-function 'yunge-reader-native-stop)
               (lambda (&optional _force) (setq stopped t))))
      (yunge-reader-setup))
    (should stopped)
    (should (eq yunge-reader-native--build-after-stop 'setup))))

(ert-deftest yunge-reader-setup-refuses-to-disrupt-active-readers ()
  (let ((yunge-reader-native--client-count 1))
    (should-error (yunge-reader-setup) :type 'user-error)))

;;; yunge-reader-setup-test.el ends here
