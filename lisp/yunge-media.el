;;; yunge-media.el --- Media processing -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(declare-function dired-get-marked-files "dired"
                  (&optional localp arg filter distinguish-one-marked error))

(defvar yunge-media-ffmpeg-program "ffmpeg"
  "FFmpeg executable used for media processing.")

(defconst yunge-media-log-buffer-name "*yunge-media*"
  "Buffer containing media processing output.")

(defvar yunge-media--program nil)
(defvar yunge-media--queue nil)
(defvar yunge-media--process nil)
(defvar yunge-media--current-job nil)
(defvar yunge-media--dired-buffer nil)
(defvar yunge-media--total 0)
(defvar yunge-media--index 0)
(defvar yunge-media--succeeded 0)
(defvar yunge-media--failed 0)
(defvar yunge-media--skipped 0)
(defvar yunge-media--cancelled nil)

(defun yunge-media--log-buffer ()
  "Return the media processing log buffer."
  (let ((buffer (get-buffer-create yunge-media-log-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode)))
    buffer))

(defun yunge-media--log (format-string &rest arguments)
  "Append FORMAT-STRING and ARGUMENTS to the media processing log."
  (with-current-buffer (yunge-media--log-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (apply #'format format-string arguments) "\n"))))

(defun yunge-media--output-file (input)
  "Return the compressed output file for INPUT."
  (concat (file-name-sans-extension input) "-compressed.mp4"))

(defun yunge-media--compression-command (program input output)
  "Return a direct PROGRAM invocation compressing INPUT into OUTPUT."
  ;; Avoid a shell so spaces and non-ASCII file names remain ordinary
  ;; process arguments on every supported platform.
  (list program
        "-hide_banner"
        "-nostdin"
        "-n"
        "-i" input
        ;; Produce a broadly playable MP4.  Keep the primary video and every
        ;; audio stream, but omit container-specific subtitles and attachments.
        "-map" "0:v:0"
        "-map" "0:a?"
        "-c:v" "libx264"
        "-crf" "23"
        "-preset" "slow"
        "-pix_fmt" "yuv420p"
        "-c:a" "aac"
        "-b:a" "160k"
        "-movflags" "+faststart"
        output))

(defun yunge-media--dired-files ()
  "Return marked Dired files, or the file at point when none are marked."
  (dired-get-marked-files nil nil nil nil "No media file specified"))

(defun yunge-media--validate-files (files)
  "Validate that FILES are local regular files."
  (dolist (file files)
    (when (file-remote-p file)
      (user-error "Cannot process a remote media file: %s" file))
    (unless (file-regular-p file)
      (user-error "Not a regular file: %s" file))))

(defun yunge-media--prepare-jobs (files)
  "Return compression jobs for FILES and log files that are skipped."
  (let (jobs outputs)
    (dolist (input files)
      (let* ((base (file-name-base input))
             (output (yunge-media--output-file input)))
        (cond
         ((string-suffix-p "-compressed" base t)
          (setq yunge-media--skipped (1+ yunge-media--skipped))
          (yunge-media--log "Skip %s: already compressed" input))
         ((file-exists-p output)
          (setq yunge-media--skipped (1+ yunge-media--skipped))
          (yunge-media--log "Skip %s: output already exists" input))
         ((member output outputs)
          (user-error "Selected files produce the same output: %s" output))
         (t
          (push output outputs)
          (push (cons input output) jobs)))))
    (nreverse jobs)))

(defun yunge-media--refresh-dired ()
  "Refresh the Dired buffer that started the current batch."
  (when (buffer-live-p yunge-media--dired-buffer)
    (with-current-buffer yunge-media--dired-buffer
      (unless (buffer-modified-p)
        (revert-buffer nil t)))))

(defun yunge-media--finish ()
  "Finish the current media processing batch."
  (setq yunge-media--process nil
        yunge-media--current-job nil
        yunge-media--program nil)
  (yunge-media--refresh-dired)
  (yunge-media--log
   "Finished: %d succeeded, %d failed, %d skipped"
   yunge-media--succeeded yunge-media--failed yunge-media--skipped)
  (if (zerop yunge-media--failed)
      (message "Media compression finished: %d succeeded, %d skipped"
               yunge-media--succeeded yunge-media--skipped)
    (display-buffer (yunge-media--log-buffer))
    (message "Media compression finished with %d failure(s)"
             yunge-media--failed)))

(defun yunge-media--sentinel (process _event)
  "Continue the media queue after PROCESS exits."
  (when (and (eq process yunge-media--process)
             (memq (process-status process) '(exit signal)))
    (let* ((status (process-exit-status process))
           (input (car yunge-media--current-job)))
      (setq yunge-media--process nil)
      (unless yunge-media--cancelled
        (if (zerop status)
            (progn
              (setq yunge-media--succeeded
                    (1+ yunge-media--succeeded))
              (yunge-media--log "Done %s" input))
          (setq yunge-media--failed (1+ yunge-media--failed))
          (yunge-media--log "Failed %s (exit %d)" input status))
        (yunge-media--start-next)))))

(defun yunge-media--start-next ()
  "Start the next queued media processing job."
  (if (null yunge-media--queue)
      (yunge-media--finish)
    (let* ((job (pop yunge-media--queue))
           (input (car job))
           (output (cdr job)))
      (setq yunge-media--current-job job
            yunge-media--index (1+ yunge-media--index))
      (yunge-media--log "[%d/%d] Compressing %s"
                        yunge-media--index yunge-media--total input)
      (message "Compressing video [%d/%d]: %s"
               yunge-media--index yunge-media--total
               (file-name-nondirectory input))
      ;; A single FFmpeg encode already uses multiple worker threads.  Start
      ;; the next file from the sentinel instead of competing for resources.
      (setq yunge-media--process
            (make-process
             :name "yunge-media-ffmpeg"
             :buffer (yunge-media--log-buffer)
             :command
             (yunge-media--compression-command
              yunge-media--program input output)
             :connection-type 'pipe
             :noquery t
             :sentinel #'yunge-media--sentinel)))))

;;;###autoload
(defun yunge-media-compress-video ()
  "Compress marked Dired videos, or the video at point.
Process files serially and keep the original files."
  (interactive)
  (require 'dired)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command must be run from Dired"))
  (when yunge-media--process
    (user-error "A media processing batch is already running"))
  (let ((program (executable-find yunge-media-ffmpeg-program))
        (files (yunge-media--dired-files)))
    (unless program
      (user-error "FFmpeg is not available in PATH"))
    (yunge-media--validate-files files)
    (with-current-buffer (yunge-media--log-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (setq yunge-media--program program
          yunge-media--queue nil
          yunge-media--process nil
          yunge-media--current-job nil
          yunge-media--dired-buffer (current-buffer)
          yunge-media--total 0
          yunge-media--index 0
          yunge-media--succeeded 0
          yunge-media--failed 0
          yunge-media--skipped 0
          yunge-media--cancelled nil)
    (setq yunge-media--queue (yunge-media--prepare-jobs files)
          yunge-media--total (length yunge-media--queue))
    (if yunge-media--queue
        (yunge-media--start-next)
      (yunge-media--finish))))

;;;###autoload
(defun yunge-media-cancel ()
  "Cancel the active media processing batch."
  (interactive)
  (unless (or yunge-media--process yunge-media--queue)
    (user-error "No media processing batch is running"))
  (setq yunge-media--cancelled t
        yunge-media--queue nil)
  (when (process-live-p yunge-media--process)
    (delete-process yunge-media--process))
  (setq yunge-media--process nil
        yunge-media--current-job nil
        yunge-media--program nil)
  (yunge-media--refresh-dired)
  (yunge-media--log "Cancelled")
  (message "Media compression cancelled"))

;;;###autoload
(defun yunge-media-show-log ()
  "Display the media processing log."
  (interactive)
  (if-let* ((buffer (get-buffer yunge-media-log-buffer-name)))
      (pop-to-buffer buffer)
    (user-error "No media processing log exists")))

(provide 'yunge-media)

;;; yunge-media.el ends here
