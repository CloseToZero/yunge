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

(ert-deftest yunge-reader-setup-completes-with-installed-pdfium ()
  (let ((yunge-reader-setup--process nil)
        (yunge-reader-native--build-process nil)
        (yunge-reader-setup--running-p nil)
        completions)
    (cl-letf (((symbol-function 'yunge-reader-setup--manifest)
               (lambda () 'manifest))
              ((symbol-function 'yunge-reader-setup--asset)
               (lambda (_manifest) 'asset))
              ((symbol-function 'yunge-reader-setup--installed-p)
               (lambda (_manifest _asset) t))
              ((symbol-function 'yunge-reader-native--start-build)
               (lambda (complete) (funcall complete nil)))
              ((symbol-function 'process-live-p)
               (lambda (_process) nil)))
      (yunge-reader-setup
       (lambda (failure) (push failure completions))))
    (should (equal completions '(nil)))))

(ert-deftest yunge-reader-setup-waits-for-an-idle-helper-to-stop ()
  (let* ((helper (make-pipe-process
                  :name "yunge-reader-setup-stop-test" :noquery t
                  :sentinel #'yunge-reader-native--sentinel))
         (yunge-reader-native--process helper)
         (yunge-reader-native--transport nil)
         (yunge-reader-native--client-count 0)
         (yunge-reader-native--build-after-stop nil)
         (yunge-reader-setup--running-p nil)
         completions)
    (unwind-protect
        (cl-letf (((symbol-function 'yunge-reader-setup--manifest)
                   (lambda () 'manifest))
                  ((symbol-function 'yunge-reader-setup--asset)
                   (lambda (_manifest) 'asset))
                  ((symbol-function 'yunge-reader-setup--installed-p)
                   (lambda (_manifest _asset) t))
                  ((symbol-function 'yunge-reader-native--start-build)
                   (lambda (complete) (funcall complete nil)))
                  ((symbol-function 'yunge-reader-native-stop)
                   (lambda (&optional _force)
                     (process-put helper 'yunge-reader-intentional-stop t)
                     (delete-process helper))))
          (yunge-reader-setup
           (lambda (failure) (push failure completions)))
          (let ((deadline (+ (float-time) 2)))
            (while (and (null completions) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should-not (process-live-p helper))
          (should (equal completions '(nil))))
      (when (process-live-p helper)
        (delete-process helper)))))

(ert-deftest yunge-reader-setup-completes-after-build-and-smoke-test ()
  (let* ((directory (make-temp-file "yunge-reader-build-" t))
         (yunge-var-directory (file-name-as-directory directory))
         (yunge-reader-setup--process nil)
         (yunge-reader-setup--running-p nil)
         (yunge-reader-native--build-process nil)
         (yunge-reader-native--process nil)
         (yunge-reader-native--client-count 0)
         (emacs-program (expand-file-name invocation-name invocation-directory))
         (real-make-process (symbol-function 'make-process))
         failed completions)
    (unwind-protect
        (cl-letf (((symbol-function 'yunge-reader-setup--manifest)
                   (lambda () 'manifest))
                  ((symbol-function 'yunge-reader-setup--asset)
                   (lambda (_manifest) 'asset))
                  ((symbol-function 'yunge-reader-setup--installed-p)
                   (lambda (_manifest _asset) t))
                  ((symbol-function 'executable-find)
                   (lambda (name) (when (equal name "cargo") emacs-program)))
                  ((symbol-function 'yunge-reader-native--artifacts-available-p)
                   (lambda () t))
                  ((symbol-function 'yunge-reader-native--publish-build-id)
                   #'ignore)
                  ((symbol-function 'yunge-reader-native-start) #'ignore)
                  ((symbol-function 'yunge-reader-native-request)
                   (lambda (_operation _parameters complete &rest _options)
                     (funcall complete '((backend . "pdfium")) nil)))
                  ((symbol-function 'make-process)
                   (lambda (&rest options)
                     (apply real-make-process
                            (plist-put options :command
                                       (list emacs-program "--batch" "-Q"
                                             "--eval"
                                             (if failed "(kill-emacs 1)"
                                               "(kill-emacs 0)")))))))
          (dotimes (_attempt 2)
            (yunge-reader-setup
             (lambda (failure) (push failure completions)))
            (let ((deadline (+ (float-time) 10)))
              (while (and (null completions) (< (float-time) deadline))
                (accept-process-output nil 0.1)))
            (should completions)
            (if failed
                (should (eq (caar completions) 'error))
              (should (equal completions '(nil))))
            (setq failed t
                  completions nil)))
      (delete-directory directory t))))

(ert-deftest yunge-reader-setup-refuses-to-disrupt-active-readers ()
  (let ((yunge-reader-native--client-count 1))
    (should-error (yunge-reader-setup) :type 'user-error)))

;;; yunge-reader-setup-test.el ends here
