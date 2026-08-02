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
 (cond-let
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "cond-let"
   :repo "tarsius/cond-let"
   :fetcher github
   :files (:defaults)
   :source "MELPA"
   :ref "c48600dfab6372670225f046cace263700c78eab"))
 (consult
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "consult"
   :repo "minad/consult"
   :fetcher github
   :files (:defaults)
   :ref "6af0dc99ff8eb8da3ca24bf9abd9a72354dcc5e1"))
 (corfu
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "corfu"
   :repo "minad/corfu"
   :files (:defaults "extensions/corfu-*.el")
   :fetcher github
   :ref "f6306d8c5ba540e75c208c8069b3b677de48a183"))
 (embark
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "embark"
   :repo "oantolin/embark"
   :fetcher github
   :files ("embark.el" "embark-org.el" "embark.texi")
   :source "MELPA"
   :ref "350ca86924c5027e80875943fba7b912a71e5791"))
 (embark-consult
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "embark-consult"
   :repo "oantolin/embark"
   :fetcher github
   :files ("embark-consult.el")
   :source "MELPA"
   :ref "350ca86924c5027e80875943fba7b912a71e5791"))
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
 (llama
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "llama"
   :repo "tarsius/llama"
   :fetcher github
   :files ("llama.el" ".dir-locals.el")
   :source "MELPA"
   :ref "4d4024048053b898a01521046e0f063ee47615b0"))
 (magit
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "magit"
   :repo "magit/magit"
   :fetcher github
   :files
   ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
    "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
    ("githooks" "githooks/*")
    ("git-hooks" "git-hooks/*")
    (:exclude "lisp/magit-section.el"))
   :source "MELPA"
   :ref "67f203853e74e926e2c99f60ed508840714f7ced"))
 (magit-section
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "magit-section"
   :repo "magit/magit"
   :fetcher github
   :files
   ("lisp/magit-section.el" "docs/magit-section.texi"
    "magit-section-pkg.el")
   :source "MELPA"
   :ref "67f203853e74e926e2c99f60ed508840714f7ced"))
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
   :ref "38d4308d1143b61e4004b6e7a940686784e51500"))
 (with-editor
  :source "elpaca-menu-lock-file"
  :recipe
  (:package "with-editor"
   :repo "magit/with-editor"
   :fetcher github
   :files (:defaults)
   :source "MELPA"
   :ref "a1f92a26e53033ec58e1d2ce9b132da7ebae816e")))
