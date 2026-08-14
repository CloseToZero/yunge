;;; yunge-project.el --- Project configuration -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(defvar project-vc-merge-submodules)

;; A submodule has its own history, files, and tooling.  Keep it independent
;; unless a project explicitly opts back into the merged view with dir locals.
(setq project-vc-merge-submodules nil)

(provide 'yunge-project)

;;; yunge-project.el ends here
