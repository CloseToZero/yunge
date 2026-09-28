;;; fangcun-org.el --- Read Fangcun nodes from Org -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'fangcun-model)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(defun fangcun-org--display-title (title fallback)
  "Return a plain display TITLE, or FALLBACK when it is empty."
  (let ((display
         (and title
              (string-trim
               (substring-no-properties
                (org-link-display-format title))))))
    (if (and display (not (string-empty-p display)))
        display
      fallback)))

(defun fangcun-org--file-title (relative-file)
  "Return the current Org file title, falling back to RELATIVE-FILE."
  (let* ((keywords (org-collect-keywords '("title")))
         (titles (cdr (assoc "TITLE" keywords))))
    (fangcun-org--display-title
     (and titles (string-join titles " "))
     (file-name-sans-extension relative-file))))

(defun fangcun-org--aliases-at-point ()
  "Return the aliases assigned to the current Org entry."
  (when-let* ((value (org-entry-get (point) "ALIASES")))
    (delete-dups (split-string-and-unquote value))))

(defun fangcun-org-goto-node ()
  "Move to the nearest enclosing Fangcun node and return its ID, or nil."
  (org-back-to-heading-or-point-min t)
  (let ((id (org-id-get)))
    (while (and (not id) (not (bobp)))
      (if (org-up-heading-safe)
          (setq id (org-id-get))
        (goto-char (point-min))
        (setq id (org-id-get))))
    id))

(defun fangcun-org-node-id-at-point ()
  "Return the nearest enclosing Fangcun node ID, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (fangcun-org-goto-node))))

(defun fangcun-org--effective-tags-at-point ()
  "Return the effective Org tags of the node at point."
  (delete-dups
   (mapcar
    #'substring-no-properties
    (if (= (org-outline-level) 0)
        org-file-tags
      (org-get-tags)))))

(defun fangcun-org--element-owner-id (element)
  "Return the nearest Fangcun node ID containing Org ELEMENT, or nil."
  (seq-some
   (lambda (ancestor)
     (when (memq (org-element-type ancestor) '(headline org-data))
       (org-element-property :ID ancestor)))
   (org-element-lineage element)))

(defun fangcun-org--collect-nodes-from-buffer
  (buffer yiyu relative-file)
  "Return Fangcun nodes parsed from Org BUFFER.
RELATIVE-FILE names BUFFER's file relative to YIYU's root."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        ;; A reused Org buffer may have stale file-tag options after an
        ;; external edit.  Refresh them before collecting effective tags.
        (org-set-regexps-and-options 'tags-only)
        (let ((file-title (fangcun-org--file-title relative-file))
              (case-fold-search t)
              (id-property-re (org-re-property "ID"))
              nodes)
          (goto-char (point-min))
          ;; Point may already be on the first heading.  Without this check,
          ;; its ID would be collected here and again below.
          (when (= (org-outline-level) 0)
            (when-let* ((id (org-id-get)))
              (push
               (make-fangcun-node
                :id id
                :yiyu-id (fangcun-yiyu-id yiyu)
                :yiyu-name (fangcun-yiyu-name yiyu)
                :yiyu-root (fangcun-yiyu-root yiyu)
                :file relative-file
                :title file-title
                :outline-path nil
                :aliases (fangcun-org--aliases-at-point)
                :tags (fangcun-org--effective-tags-at-point)
                :position (point)
                :line (line-number-at-pos))
                nodes)))
          ;; Fangcun nodes are sparse among Org headings.  Search possible ID
          ;; properties directly, then let Org reject lookalikes outside a
          ;; property drawer.
          (goto-char (point-min))
          (while (re-search-forward id-property-re nil t)
            (let ((id (match-string-no-properties 3)))
              (when (org-at-property-p)
                (save-excursion
                  (org-back-to-heading-or-point-min t)
                  (unless (= (org-outline-level) 0)
                    (push
                     (make-fangcun-node
                      :id id
                      :yiyu-id (fangcun-yiyu-id yiyu)
                      :yiyu-name (fangcun-yiyu-name yiyu)
                      :yiyu-root (fangcun-yiyu-root yiyu)
                      :file relative-file
                      :title
                      (fangcun-org--display-title
                       (org-get-heading t t t) id)
                      :outline-path
                      (mapcar
                       #'substring-no-properties
                       (org-get-outline-path t))
                      :aliases (fangcun-org--aliases-at-point)
                      :tags (fangcun-org--effective-tags-at-point)
                      :position (point)
                      :line (line-number-at-pos))
                      nodes))))))
          (nreverse nodes))))))

(defun fangcun-org--collect-links-from-buffer (buffer &optional include-unowned)
  "Return ID links from Org BUFFER.
Unless INCLUDE-UNOWNED is non-nil, omit links outside Fangcun nodes."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (links)
          (while (re-search-forward org-link-any-re nil t)
            ;; The search leaves point after the link.  Move onto it so Org
            ;; can reject matches in source blocks, comments, properties, and
            ;; keywords.
            (backward-char)
            (let ((element (org-element-context)))
              (when (and (eq (org-element-type element) 'link)
                         (equal
                          (org-element-property :type element)
                          "id"))
                (let ((source-id (fangcun-org--element-owner-id element)))
                  (when (or source-id include-unowned)
                    (let* ((path
                            (org-element-property :path element))
                           (target-id
                            ;; A search suffix selects a location inside the
                            ;; target node; the backlink belongs to the node.
                            (if (string-match "::.*\\'" path)
                                (substring path 0 (match-beginning 0))
                              path))
                           (position
                            (org-element-property :begin element)))
                      (push
                       (make-fangcun-link
                        :source-id source-id
                        :target-id target-id
                        :position position
                        :line (line-number-at-pos position))
                       links)))))))
          (nreverse links))))))

(defun fangcun-org-read-buffer
    (buffer yiyu relative-file &optional include-unowned)
  "Return (:nodes NODES :links LINKS) parsed from Org BUFFER's current text.
RELATIVE-FILE names BUFFER's file relative to YIYU's root.
When INCLUDE-UNOWNED is non-nil, retain links outside Fangcun nodes.
Preserve BUFFER's point and narrowing."
  (list :nodes
        (fangcun-org--collect-nodes-from-buffer
         buffer yiyu relative-file)
        :links
        (fangcun-org--collect-links-from-buffer buffer include-unowned)))

(defun fangcun-org-saved-file-buffer (file)
  "Return FILE's visited Org buffer when it still matches the file on disk."
  (when-let* ((buffer (find-buffer-visiting file)))
    (when (with-current-buffer buffer
            (and (derived-mode-p 'org-mode)
                 (not (buffer-modified-p))
                 (verify-visited-file-modtime buffer)))
      buffer)))

(defun fangcun-org-read-file (yiyu file &optional include-unowned)
  "Return (:nodes NODES :links LINKS) from saved Org FILE in YIYU.
Use a visiting buffer only when it matches disk; preserve its point and
narrowing.  INCLUDE-UNOWNED retains links outside Fangcun nodes."
  (let* ((relative-file
          (file-relative-name file (fangcun-yiyu-root yiyu)))
         (buffer (fangcun-org-saved-file-buffer file)))
    (if buffer
        (fangcun-org-read-buffer
         buffer yiyu relative-file include-unowned)
      (with-temp-buffer
        (setq default-directory (file-name-directory file))
        (insert-file-contents file)
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (fangcun-org-read-buffer
         (current-buffer) yiyu relative-file include-unowned)))))

(provide 'fangcun-org)

;;; fangcun-org.el ends here
