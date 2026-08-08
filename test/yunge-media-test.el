;;; yunge-media-test.el --- Media processing tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(declare-function dired-goto-file "dired" (file))
(declare-function dired-mark "dired" (arg &optional interactive))
(declare-function dired-unmark-all-marks "dired")
(declare-function yunge-media--compression-command
                  "yunge-media" (program input output))
(declare-function yunge-media--dired-files "yunge-media")
(declare-function yunge-media--sentinel "yunge-media" (process event))
(declare-function yunge-media--start-next "yunge-media")

(defvar yunge-media-log-buffer-name)
(defvar yunge-media--cancelled)
(defvar yunge-media--current-job)
(defvar yunge-media--dired-buffer)
(defvar yunge-media--failed)
(defvar yunge-media--index)
(defvar yunge-media--process)
(defvar yunge-media--program)
(defvar yunge-media--queue)
(defvar yunge-media--skipped)
(defvar yunge-media--succeeded)
(defvar yunge-media--total)

(yunge-test-deftest-lazy-load yunge-media (dired))

(ert-deftest yunge-media-builds-direct-ffmpeg-command ()
  (require 'yunge-media)
  (let ((input "C:/video/中文 clip.mov")
        (output "C:/video/中文 clip-compressed.mp4"))
    (should
     (equal
      (yunge-media--compression-command "ffmpeg" input output)
      (list "ffmpeg"
            "-hide_banner"
            "-nostdin"
            "-n"
            "-i" input
            "-map" "0:v:0"
            "-map" "0:a?"
            "-c:v" "libx264"
            "-crf" "23"
            "-preset" "slow"
            "-pix_fmt" "yuv420p"
            "-c:a" "aac"
            "-b:a" "160k"
            "-movflags" "+faststart"
            output)))))

(ert-deftest yunge-media-prefers-dired-marks-over-point ()
  (require 'yunge-media)
  (require 'dired)
  (let* ((directory (make-temp-file "yunge-media-files-" t))
         (first (expand-file-name "first.mp4" directory))
         (second (expand-file-name "second.mp4" directory))
         (third (expand-file-name "third.mp4" directory))
         buffer)
    (unwind-protect
        (progn
          (dolist (file (list first second third))
            (write-region "" nil file nil 'silent))
          (setq buffer (dired-noselect directory))
          (with-current-buffer buffer
            (dired-goto-file first)
            (dired-mark 1)
            (dired-mark 1)
            (dired-goto-file third)
            (should (equal (yunge-media--dired-files)
                           (list first second)))
            (dired-unmark-all-marks)
            (should (equal (yunge-media--dired-files)
                           (list third)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest yunge-media-runs-one-ffmpeg-process-at-a-time ()
  (require 'yunge-media)
  (let ((yunge-media-log-buffer-name " *yunge-media-test*")
        (yunge-media--program "ffmpeg")
        (yunge-media--queue
         '(("first.mov" . "first-compressed.mp4")
           ("second.mov" . "second-compressed.mp4")))
        (yunge-media--process nil)
        (yunge-media--current-job nil)
        (yunge-media--dired-buffer nil)
        (yunge-media--total 2)
        (yunge-media--index 0)
        (yunge-media--succeeded 0)
        (yunge-media--failed 0)
        (yunge-media--skipped 0)
        (yunge-media--cancelled nil)
        (starts 0))
    (unwind-protect
        (let ((inhibit-message t))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest _arguments)
                       (setq starts (1+ starts))
                       'fake-process)))
            (yunge-media--start-next)
            (should (= starts 1))
            (should (= (length yunge-media--queue) 1))
            (cl-letf (((symbol-function 'process-status)
                       (lambda (_process) 'exit))
                      ((symbol-function 'process-exit-status)
                       (lambda (_process) 0)))
              (yunge-media--sentinel 'fake-process "finished"))
            (should (= starts 2))
            (should-not yunge-media--queue)))
      (when-let* ((buffer (get-buffer yunge-media-log-buffer-name)))
        (kill-buffer buffer)))))

;;; yunge-media-test.el ends here
