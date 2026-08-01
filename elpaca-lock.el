;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

((evil
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "evil"
   :repo "emacs-evil/evil"
   :fetcher github
   :files (:defaults)
   :ref "6a3e1ddd04ac504a016590940d0af2a3361b9efd"))
 (goto-chg
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "goto-chg"
   :repo "emacs-evil/goto-chg"
   :fetcher github
   :files (:defaults)
   :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7")))
