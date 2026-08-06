;;; yunge-treesit.el --- Tree-sitter language support -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'treesit)
(require 'yunge-state)

(defconst yunge-treesit-grammar-directory
  (yunge-var-subdirectory "tree-sitter")
  "Directory containing locally compiled tree-sitter grammars.")

(make-directory yunge-treesit-grammar-directory t)
(add-to-list 'treesit-extra-load-path yunge-treesit-grammar-directory)

(defconst yunge-treesit-language-source-alist
  '((bash
     "https://github.com/tree-sitter/tree-sitter-bash"
     :commit "a06c2e4415e9bc0346c6b86d401879ffb44058f7")
    (c
     "https://github.com/tree-sitter/tree-sitter-c"
     :commit "3aa2995549d5d8b26928e8d3fa2770fd4327414e")
    (c-sharp
     "https://github.com/tree-sitter/tree-sitter-c-sharp"
     :commit "cac6d5fb595f5811a076336682d5d595ac1c9e85")
    (cmake
     "https://github.com/uyha/tree-sitter-cmake"
     :commit "e409ae33f00e04cde30f2bcffb979caf1a33562a")
    (cpp
     "https://github.com/tree-sitter/tree-sitter-cpp"
     :commit "f41b4f66a42100be405f96bdc4ebc4a61095d3e8")
    (css
     "https://github.com/tree-sitter/tree-sitter-css"
     :commit "6a442a3cf461b0ce275339e5afa178693484c927")
    (dockerfile
     "https://github.com/camdencheek/tree-sitter-dockerfile"
     :commit "087daa20438a6cc01fa5e6fe6906d77c869d19fe")
    (doxygen
     "https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
     :commit "1e28054cb5be80d5febac082706225e42eff14e6")
    (elixir
     "https://github.com/elixir-lang/tree-sitter-elixir"
     :commit "02a6f7fd4be28dd94ee4dd2ca19cb777053ea74e")
    (go
     "https://github.com/tree-sitter/tree-sitter-go"
     :commit "12fe553fdaaa7449f764bc876fd777704d4fb752")
    (gomod
     "https://github.com/camdencheek/tree-sitter-go-mod"
     :commit "3b01edce2b9ea6766ca19328d1850e456fde3103")
    (gowork
     "https://github.com/omertuc/tree-sitter-go-work"
     :commit "949a8a470559543857a62102c84700d291fc984c")
    (heex
     "https://github.com/phoenixframework/tree-sitter-heex"
     :commit "f6b83f305a755cd49cf5f6a66b2b789be93dc7b9")
    (html
     "https://github.com/tree-sitter/tree-sitter-html"
     :commit "d9219ada6e1a2c8f0ab0304a8bd9ca4285ae0468")
    (java
     "https://github.com/tree-sitter/tree-sitter-java"
     :commit "94703d5a6bed02b98e438d7cad1136c01a60ba2c")
    (javascript
     "https://github.com/tree-sitter/tree-sitter-javascript"
     :commit "108b2d4d17a04356a340aea809e4dd5b801eb40d")
    (jsdoc
     "https://github.com/tree-sitter/tree-sitter-jsdoc"
     :commit "b253abf68a73217b7a52c0ec254f4b6a7bb86665")
    (json
     "https://github.com/tree-sitter/tree-sitter-json"
     :commit "4d770d31f732d50d3ec373865822fbe659e47c75")
    (lua
     "https://github.com/tree-sitter-grammars/tree-sitter-lua"
     :commit "db16e76558122e834ee214c8dc755b4a3edc82a9")
    (markdown
     "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     :commit "413285231ce8fa8b11e7074bbe265b48aa7277f9"
     :source-dir "tree-sitter-markdown/src")
    (markdown-inline
     "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
     :commit "413285231ce8fa8b11e7074bbe265b48aa7277f9"
     :source-dir "tree-sitter-markdown-inline/src")
    (php
     "https://github.com/tree-sitter/tree-sitter-php"
     :commit "5b5627faaa290d89eb3d01b9bf47c3bb9e797dea"
     :source-dir "php/src")
    (phpdoc
     "https://github.com/claytonrcarter/tree-sitter-phpdoc"
     :commit "03bb10330704b0b371b044e937d5cc7cd40b4999")
    (python
     "https://github.com/tree-sitter/tree-sitter-python"
     :commit "293fdc02038ee2bf0e2e206711b69c90ac0d413f")
    (ruby
     "https://github.com/tree-sitter/tree-sitter-ruby"
     :commit "71bd32fb7607035768799732addba884a37a6210")
    (rust
     "https://github.com/tree-sitter/tree-sitter-rust"
     :commit "18b0515fca567f5a10aee9978c6d2640e878671a")
    (toml
     "https://github.com/tree-sitter-grammars/tree-sitter-toml"
     :commit "64b56832c2cffe41758f28e05c756a3a98d16f41")
    (tsx
     "https://github.com/tree-sitter/tree-sitter-typescript"
     :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
     :source-dir "tsx/src")
    (typescript
     "https://github.com/tree-sitter/tree-sitter-typescript"
     :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
     :source-dir "typescript/src")
    (yaml
     "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
     :commit "7708026449bed86239b1cd5bce6e3c34dbca6415"))
  "Pinned build recipes for grammars used by built-in tree-sitter modes.")

(setq treesit-language-source-alist
      (copy-tree yunge-treesit-language-source-alist))
(setopt treesit-auto-install-grammar 'ask)

(defconst yunge-treesit-mode-grammars
  '((bash-ts-mode bash)
    (c++-ts-mode cpp)
    (c-ts-mode c)
    (cmake-ts-mode cmake)
    (csharp-ts-mode c-sharp)
    (css-ts-mode css)
    (dockerfile-ts-mode dockerfile)
    (elixir-ts-mode elixir)
    (go-mod-ts-mode gomod)
    (go-ts-mode go)
    (go-work-ts-mode gowork)
    (heex-ts-mode heex)
    (html-ts-mode html)
    (java-ts-mode java)
    (js-ts-mode javascript)
    (json-ts-mode json)
    (lua-ts-mode lua)
    (markdown-ts-mode markdown markdown-inline)
    (mhtml-ts-mode html javascript css)
    (php-ts-mode php phpdoc html javascript jsdoc css)
    (python-ts-mode python)
    (ruby-ts-mode ruby)
    (rust-ts-mode rust)
    (toml-ts-mode toml)
    (tsx-ts-mode tsx)
    (typescript-ts-mode typescript)
    (yaml-ts-mode yaml))
  "Required grammars for every built-in tree-sitter major mode.")

(defconst yunge-treesit-fallback-modes
  '((bash-ts-mode . sh-mode)
    (c++-ts-mode . c++-mode)
    (c-ts-mode . c-mode)
    (cmake-ts-mode . cmake-mode)
    (csharp-ts-mode . csharp-mode)
    (css-ts-mode . css-mode)
    (dockerfile-ts-mode . dockerfile-mode)
    (elixir-ts-mode . elixir-mode)
    (go-mod-ts-mode . go-mod-mode)
    (go-ts-mode . go-mode)
    (go-work-ts-mode . go-work-mode)
    (heex-ts-mode . heex-mode)
    (html-ts-mode . html-mode)
    (java-ts-mode . java-mode)
    (js-ts-mode . javascript-mode)
    (json-ts-mode . js-json-mode)
    (lua-ts-mode . lua-mode)
    (markdown-ts-mode . text-mode)
    (mhtml-ts-mode . mhtml-mode)
    (php-ts-mode . php-mode)
    (python-ts-mode . python-mode)
    (ruby-ts-mode . ruby-mode)
    (rust-ts-mode . rust-mode)
    (toml-ts-mode . conf-toml-mode)
    (tsx-ts-mode . tsx-mode)
    (typescript-ts-mode . typescript-mode)
    (yaml-ts-mode . yaml-mode))
  "Conventional modes used when a tree-sitter mode cannot start.")

(defconst yunge-treesit-major-mode-remap-alist
  '((yaml-mode . yaml-ts-mode)
    (tsx-mode . tsx-ts-mode)
    (typescript-mode . typescript-ts-mode)
    (conf-toml-mode . toml-ts-mode)
    (sh-mode . bash-ts-mode)
    (rust-mode . rust-ts-mode)
    (ruby-mode . ruby-ts-mode)
    (python-mode . python-ts-mode)
    (php-mode . php-ts-mode)
    (mhtml-mode . mhtml-ts-mode)
    (lua-mode . lua-ts-mode)
    (js-json-mode . json-ts-mode)
    (javascript-mode . js-ts-mode)
    (java-mode . java-ts-mode)
    (heex-mode . heex-ts-mode)
    (go-work-mode . go-work-ts-mode)
    (go-mod-mode . go-mod-ts-mode)
    (go-mode . go-ts-mode)
    (elixir-mode . elixir-ts-mode)
    (dockerfile-mode . dockerfile-ts-mode)
    (css-mode . css-ts-mode)
    (csharp-mode . csharp-ts-mode)
    (cmake-mode . cmake-ts-mode)
    (c-or-c++-mode . c-or-c++-ts-mode)
    (c++-mode . c++-ts-mode)
    (c-mode . c-ts-mode))
  "Built-in major-mode remaps enabled by this configuration.")

(defconst yunge-treesit-maybe-mode-alist
  '((cmake-ts-mode-maybe . cmake-ts-mode)
    (dockerfile-ts-mode-maybe . dockerfile-ts-mode)
    (elixir-ts-mode-maybe . elixir-ts-mode)
    (go-mod-ts-mode-maybe . go-mod-ts-mode)
    (go-ts-mode-maybe . go-ts-mode)
    (go-work-ts-mode-maybe . go-work-ts-mode)
    (heex-ts-mode-maybe . heex-ts-mode)
    (php-ts-mode-maybe . php-ts-mode)
    (rust-ts-mode-maybe . rust-ts-mode)
    (tsx-ts-mode-maybe . tsx-ts-mode)
    (typescript-ts-mode-maybe . typescript-ts-mode)
    (yaml-ts-mode-maybe . yaml-ts-mode))
  "Built-in fallback entry points replaced with robust wrappers.")

(defun yunge-treesit--wrapper-symbol (mode)
  "Return the fallback wrapper symbol for tree-sitter MODE."
  (intern (format "yunge-%s-maybe" mode)))

(defun yunge-treesit--fallback (mode &optional error-data)
  "Enter the conventional fallback for MODE.
When ERROR-DATA is non-nil, explain why MODE could not start."
  (let* ((configured (alist-get mode yunge-treesit-fallback-modes))
         (fallback (if (and configured (fboundp configured))
                       configured
                     'fundamental-mode)))
    (when error-data
      (message "Could not start %s (%s); using %s"
               mode (error-message-string error-data) fallback))
    (funcall fallback)))

(defun yunge-treesit--ensure-grammars (languages)
  "Return non-nil after ensuring that all LANGUAGES are installed."
  (catch 'missing
    (dolist (language languages)
      (unless (treesit-ensure-installed language)
        (throw 'missing nil)))
    t))

(defun yunge-treesit--activate (mode)
  "Start tree-sitter MODE, falling back if its grammars are unavailable."
  (let ((languages (cdr (assq mode yunge-treesit-mode-grammars))))
    (condition-case error-data
        (if (and (treesit-available-p)
                 (yunge-treesit--ensure-grammars languages))
            (funcall mode)
          (yunge-treesit--fallback mode))
      (error (yunge-treesit--fallback mode error-data)))))

(dolist (entry yunge-treesit-mode-grammars)
  (let ((mode (car entry)))
    (defalias (yunge-treesit--wrapper-symbol mode)
      (lambda ()
        (interactive)
        (yunge-treesit--activate mode))
      (format "Start `%s', or fall back when its grammars are unavailable."
              mode))))

(defconst yunge-treesit-supported-modes
  (cons 'c-or-c++-ts-mode (mapcar #'car yunge-treesit-mode-grammars))
  "All built-in tree-sitter modes supported by this configuration.")

(defconst yunge-treesit-enabled-modes
  (delete-dups (mapcar #'cdr yunge-treesit-major-mode-remap-alist))
  "Built-in tree-sitter modes that participate in automatic remapping.")

;; Let built-in `*-ts-mode-maybe' entry points know that grammars may be
;; installed, then replace their fragile fallback behavior below.
(setopt treesit-enabled-modes yunge-treesit-enabled-modes)

(dolist (entry yunge-treesit-major-mode-remap-alist)
  (setf (alist-get (car entry) major-mode-remap-alist)
        (if (eq (cdr entry) 'c-or-c++-ts-mode)
            'c-or-c++-ts-mode
          (yunge-treesit--wrapper-symbol (cdr entry)))))

;; `c-or-c++-ts-mode' dispatches through `major-mode-remap-alist' after it
;; inspects ambiguous header files, so wrap those destination modes as well.
(setf (alist-get 'c-ts-mode major-mode-remap-alist)
      (yunge-treesit--wrapper-symbol 'c-ts-mode)
      (alist-get 'c++-ts-mode major-mode-remap-alist)
      (yunge-treesit--wrapper-symbol 'c++-ts-mode))

(dolist (entry yunge-treesit-maybe-mode-alist)
  (let ((wrapper (yunge-treesit--wrapper-symbol (cdr entry))))
    (unless (advice-member-p wrapper (car entry))
      (advice-add (car entry) :override wrapper))))

;; `markdown-ts-mode' is built in but experimental and intentionally has no
;; default file association in Emacs 31.  Keep it lazy while making it usable.
(autoload 'markdown-ts-mode "markdown-ts-mode" nil t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\)\\'" . yunge-markdown-ts-mode-maybe))

(provide 'yunge-treesit)

;;; yunge-treesit.el ends here
