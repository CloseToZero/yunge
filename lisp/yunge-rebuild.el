;;; yunge-rebuild.el --- Rebuild local configuration artifacts -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'yunge-autoload)
(require 'yunge-mcp-clients)

(declare-function fangcun-native-build "fangcun" (&optional complete))
(declare-function yunge-mcp-install "yunge-mcp-setup" (&optional complete))
(declare-function yunge-mcp-setup "yunge-mcp-setup"
                  (&optional complete initial-choice))
(declare-function yunge-reader-setup "yunge-reader-setup" (&optional complete))

(defconst yunge-rebuild--buffer-name "*Yunge Rebuild*"
  "Buffer showing progress and results of the current rebuild.")

(defvar yunge-rebuild--running-p nil
  "Whether a rebuild is currently in progress.")

(defvar yunge-rebuild--step 0
  "Index of the current rebuild step.")

(defvar yunge-rebuild--results nil
  "Completed step results, newest first.")

(defvar yunge-rebuild--initial-choice nil
  "First MCP client choice, as (t . CLIENTS), or nil when recorded already.")

(defun yunge-rebuild--stop-on-exit ()
  "Prevent process teardown callbacks from starting another build on exit."
  (setq yunge-rebuild--running-p nil))

(add-hook 'kill-emacs-hook #'yunge-rebuild--stop-on-exit -100)

(defun yunge-rebuild--log (format-string &rest arguments)
  "Append FORMAT-STRING and ARGUMENTS to the rebuild summary."
  (with-current-buffer (get-buffer-create yunge-rebuild--buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (apply #'format format-string arguments) "\n"))))

(defun yunge-rebuild--name (step)
  "Return the display name of STEP."
  (aref ["Autoloads" "Fangcun helper" "MCP helper" "Reader and PDFium"] step))

(defun yunge-rebuild--logs (step)
  "Return existing detailed log buffers for STEP."
  (pcase step
    (1 "*Fangcun Helper Build*")
    (2 "*Yunge MCP build*")
    (3 "*Yunge Reader setup*, *Yunge Reader native build*")))

(defun yunge-rebuild--finish-step (step failure)
  "Record STEP outcome, then start the next step; FAILURE is an error value."
  (when (and yunge-rebuild--running-p (= step yunge-rebuild--step))
    (push (cons (yunge-rebuild--name step) failure) yunge-rebuild--results)
    (yunge-rebuild--log "%s: %s%s" (yunge-rebuild--name step)
                        (if failure (error-message-string failure) "success")
                        (if (and failure (yunge-rebuild--logs step))
                            (format " (logs: %s)" (yunge-rebuild--logs step))
                          ""))
    (setq yunge-rebuild--step (1+ step))
    (run-at-time 0 nil #'yunge-rebuild--next)))

(defun yunge-rebuild--next ()
  "Start the next fixed rebuild step or report the final result."
  (when yunge-rebuild--running-p
    (if (= yunge-rebuild--step 4)
        (let* ((failures (cl-count-if #'cdr yunge-rebuild--results))
               (summary (format "Yunge rebuild: %d succeeded, %d failed"
                                (- 4 failures) failures)))
          (setq yunge-rebuild--running-p nil
                yunge-rebuild--initial-choice nil)
          (yunge-rebuild--log "%s" summary)
          (display-buffer yunge-rebuild--buffer-name)
          (message "%s" summary))
      (let* ((step yunge-rebuild--step)
             (complete (lambda (failure)
                         (yunge-rebuild--finish-step step failure))))
        (yunge-rebuild--log "%s: starting" (yunge-rebuild--name step))
        (message "Rebuilding %s..." (yunge-rebuild--name step))
        (condition-case error-data
            (pcase step
              (0 (yunge-autoload-generate)
                 (funcall complete nil))
              (1 (require 'fangcun)
                 (fangcun-native-build complete))
              (2 (require 'yunge-mcp-setup)
                 (if yunge-rebuild--initial-choice
                     (yunge-mcp-setup complete yunge-rebuild--initial-choice)
                   (yunge-mcp-install complete)))
              (3 (require 'yunge-reader-setup)
                 (yunge-reader-setup complete)))
          (error (yunge-rebuild--finish-step step error-data)))))))

(defun yunge-rebuild ()
  "Rebuild local autoloads, Fangcun, MCP, and Reader artifacts asynchronously."
  (interactive)
  (when yunge-rebuild--running-p
    (user-error "Yunge rebuild is already running"))
  (let ((choice (yunge-mcp-clients-choice)))
    (setq yunge-rebuild--initial-choice
          (when (eq choice :unselected)
            (cons t (yunge-mcp-clients-read)))))
  (setq yunge-rebuild--running-p t
        yunge-rebuild--step 0
        yunge-rebuild--results nil)
  (with-current-buffer (get-buffer-create yunge-rebuild--buffer-name)
    (let ((inhibit-read-only t)) (erase-buffer))
    (special-mode))
  (display-buffer yunge-rebuild--buffer-name)
  (yunge-rebuild--next))

(provide 'yunge-rebuild)

;;; yunge-rebuild.el ends here
