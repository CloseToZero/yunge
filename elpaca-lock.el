;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

((compat
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "compat"
   :repo ("https://github.com/emacs-compat/compat" . "compat")
   :tar "31.0.0.2"
   :host gnu
   :files ("*" (:exclude ".git"))
   :source "GNU ELPA"
   :ref "1f3d7596173cf2851d3c4181b15d6c3573a38252"))
 (evil
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
   :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (marginalia
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "marginalia"
   :repo "minad/marginalia"
   :fetcher github
   :files (:defaults)
   :ref "10b170ad8006bad535599e5b3e007e643e34345a"))
 (orderless
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "orderless"
   :repo "oantolin/orderless"
   :fetcher github
   :files (:defaults)
   :ref "0ffd9d6903714c1f6d8fcbb6a20941fb33dd7ae5"))
 (vertico
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "vertico"
   :repo "minad/vertico"
   :fetcher github
   :files (:defaults "extensions/vertico-*.el")
   :ref "77808caeaa658e95e04b1c6a519be1722e9f1a70"))
 (which-key
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "which-key"
   :repo "justbur/emacs-which-key"
   :fetcher github
   :files (:defaults)
   :ref "38d4308d1143b61e4004b6e7a940686784e51500")))
