;;; fangcun-model.el --- Fangcun records -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(cl-defstruct fangcun-yiyu
  "A configured note root with a stable ID and absolute ROOT."
  id
  name
  root)

(cl-defstruct fangcun-node
  "An Org node; FILE is relative to its yiyu root.
POSITION and LINE describe a parsed buffer and may be absent after a
database lookup."
  id
  yiyu-id
  yiyu-name
  yiyu-root
  file
  title
  outline-path
  aliases
  tags
  position
  line)

(cl-defstruct fangcun-link
  "An ID link in a parsed Org snapshot; SOURCE-ID is nil for an unowned link."
  source-id
  target-id
  position
  line)

(cl-defstruct fangcun-file-state
  "One saved Org file; MTIME is in seconds and SIZE in bytes."
  yiyu
  relative-file
  absolute-file
  mtime
  size)

(cl-defstruct fangcun-backlink
  "A source node and one or more links to a target ID."
  node
  position
  count)

(cl-defstruct fangcun-check-issue
  "A note-graph issue with an optional source position and line."
  severity
  file
  position
  line
  message)

(provide 'fangcun-model)

;;; fangcun-model.el ends here
