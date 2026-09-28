;;; yunge-reader-model.el --- Reader document contracts -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)

(defgroup yunge-reader nil
  "Read fixed-layout and reflowable documents."
  :group 'applications)

(cl-defstruct (yunge-reader-driver
               (:constructor yunge-reader--make-driver))
  "A document format implementation.
MATCH-FUNCTION receives an absolute file name.  OPEN-FUNCTION receives that
file and a completion function, which it calls with HANDLE, a properties
plist, and an error value.  CLOSE-FUNCTION receives a `yunge-reader-document'.
ATTACH-FUNCTION receives a document and its optional initial place in the
Reader buffer whose format-specific view it initializes.  DETACH-FUNCTION
receives the document whose view it tears down.  OUTLINE-FUNCTION,
SEARCH-FUNCTION, and SELECTION-TEXT-FUNCTION are explicit asynchronous
capabilities.  Each receives a document, its argument value, and a completion
function, which it calls with a value and an error value.
OUTLINE-INDEX-FUNCTION receives a document, window, and loaded outline and
returns the zero-based item index nearest the current reading location.
LOCATION-FUNCTION receives a document and window and returns a stable
`yunge-reader-position'.  RESTORE-FUNCTION receives a document, position,
and window and returns non-nil after accepting the location."
  name
  match-function
  open-function
  close-function
  attach-function
  detach-function
  outline-function
  outline-index-function
  search-function
  selection-text-function
  location-function
  restore-function)

(cl-defstruct yunge-reader-document
  "An open document resource owned by one reader driver."
  key
  file
  driver
  handle
  layout
  metadata)

(cl-defstruct yunge-reader-position
  "A stable position in a document.
UNIT names a page or reflowable content unit.  OFFSET is a text offset or
driver-defined stable anchor.  X and Y are optional coordinates in the
unscaled coordinate system of UNIT."
  unit
  offset
  x
  y)

(cl-defstruct yunge-reader-selection
  "A logical document selection independent of its painted highlight."
  start
  end
  text)

(cl-defstruct yunge-reader-selection-batch
  "One concatenable batch of selected document text."
  text
  cursor
  done)

(cl-defstruct yunge-reader-search-result
  "One driver-neutral document search result."
  start
  end
  text
  before
  after)

(cl-defstruct yunge-reader-search-cursor
  "Opaque driver-owned continuation for one directional search run."
  value)

(cl-defstruct yunge-reader-search-request
  "One typed, bounded request for a document search batch."
  query
  case-sensitive
  direction
  origin
  cursor
  match-limit
  unit-limit)

(cl-defstruct yunge-reader-search-batch
  "One bounded batch of driver search results."
  results
  cursor
  done)

(cl-defstruct yunge-reader-selection-text-request
  "One typed, bounded request for selected document text."
  start
  end
  cursor
  unit-limit
  character-limit)

(cl-defstruct yunge-reader-action
  "One format-independent action exposed by a document."
  type
  position
  zoom-mode
  scale
  uri)

(cl-defstruct yunge-reader-outline-item
  "One entry in a flattened document outline."
  title
  depth
  action)

(cl-defstruct yunge-reader-outline-data
  "One bounded document outline returned by a driver."
  items
  truncated)

(provide 'yunge-reader-model)

;;; yunge-reader-model.el ends here
