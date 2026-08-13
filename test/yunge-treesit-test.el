;;; yunge-treesit-test.el --- Tree-sitter configuration tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)

(require 'yunge-treesit)

(yunge-test-deftest-lazy-load yunge-treesit
  (c-ts-mode
   cmake-ts-mode
   csharp-mode
   css-mode
   dockerfile-ts-mode
   elixir-ts-mode
   go-ts-mode
   heex-ts-mode
   html-ts-mode
   java-ts-mode
   js
   json-ts-mode
   lua-ts-mode
   markdown-ts-mode
   mhtml-ts-mode
   php-ts-mode
   python
   ruby-ts-mode
   rust-ts-mode
   sh-script
   toml-ts-mode
   typescript-ts-mode
   yaml-ts-mode))

(defconst yunge-treesit-test--languages
  '(bash
    c
    c-sharp
    cmake
    cpp
    css
    dockerfile
    doxygen
    elixir
    go
    gomod
    gowork
    heex
    html
    java
    javascript
    jsdoc
    json
    lua
    markdown
    markdown-inline
    php
    phpdoc
    python
    ruby
    rust
    toml
    tsx
    typescript
    yaml))

(defconst yunge-treesit-test--supported-modes
  '(c-or-c++-ts-mode
    bash-ts-mode
    c++-ts-mode
    c-ts-mode
    cmake-ts-mode
    csharp-ts-mode
    css-ts-mode
    dockerfile-ts-mode
    elixir-ts-mode
    go-mod-ts-mode
    go-ts-mode
    go-work-ts-mode
    heex-ts-mode
    html-ts-mode
    java-ts-mode
    js-ts-mode
    json-ts-mode
    lua-ts-mode
    markdown-ts-mode
    mhtml-ts-mode
    php-ts-mode
    python-ts-mode
    ruby-ts-mode
    rust-ts-mode
    toml-ts-mode
    tsx-ts-mode
    typescript-ts-mode
    yaml-ts-mode))

(ert-deftest yunge-treesit-grammars-are-complete-and-pinned ()
  (should (equal (mapcar #'car yunge-treesit-language-source-alist)
                 yunge-treesit-test--languages))
  (dolist (recipe yunge-treesit-language-source-alist)
    (let ((url (cadr recipe))
          (arguments (cddr recipe)))
      (should (string-match-p
               "\\`https://github\\.com/.+/tree-sitter-.+\\'" url))
      (should (string-match-p
               "\\`[[:xdigit:]]\\{40\\}\\'"
               (plist-get arguments :commit)))))
  (dolist (entry yunge-treesit-mode-grammars)
    (dolist (language (cdr entry))
      (should (assq language yunge-treesit-language-source-alist))))
  ;; Optional embedded parsers used by the built-in modes have recipes too.
  (dolist (language '(doxygen heex jsdoc phpdoc))
    (should (assq language yunge-treesit-language-source-alist))))

(ert-deftest yunge-treesit-multi-grammar-source-directories-are-explicit ()
  (dolist (expected
           '((markdown . "tree-sitter-markdown/src")
             (markdown-inline . "tree-sitter-markdown-inline/src")
             (php . "php/src")
             (tsx . "tsx/src")
             (typescript . "typescript/src")))
    (let ((recipe (assq (car expected)
                        yunge-treesit-language-source-alist)))
      (should (equal (plist-get (cddr recipe) :source-dir)
                     (cdr expected))))))

(ert-deftest yunge-treesit-keeps-grammars-under-var ()
  (should (equal yunge-treesit-grammar-directory
                 (expand-file-name "tree-sitter/"
                                   yunge-var-directory)))
  (should (file-directory-p yunge-treesit-grammar-directory))
  (should (member yunge-treesit-grammar-directory
                  treesit-extra-load-path))
  (should (eq treesit-auto-install-grammar 'ask)))

(ert-deftest yunge-treesit-covers-every-built-in-mode ()
  (should (equal yunge-treesit-supported-modes
                 yunge-treesit-test--supported-modes))
  (should (equal (sort (copy-sequence yunge-treesit-enabled-modes)
                       #'string-lessp)
                 (sort (delete-dups
                        (mapcar #'cdr
                                yunge-treesit-major-mode-remap-alist))
                       #'string-lessp)))
  (should (memq 'html-ts-mode yunge-treesit-supported-modes))
  (should (memq 'markdown-ts-mode yunge-treesit-supported-modes)))

(ert-deftest yunge-treesit-remaps-use-fallback-wrappers ()
  (dolist (entry yunge-treesit-major-mode-remap-alist)
    (let ((expected
           (if (eq (cdr entry) 'c-or-c++-ts-mode)
               'c-or-c++-ts-mode
             (yunge-treesit--wrapper-symbol (cdr entry)))))
      (should (eq (alist-get (car entry) major-mode-remap-alist)
                  expected))))
  ;; Ambiguous headers are dispatched to one of these destination modes.
  (should (eq (alist-get 'c-ts-mode major-mode-remap-alist)
              'yunge-c-ts-mode-maybe))
  (should (eq (alist-get 'c++-ts-mode major-mode-remap-alist)
              'yunge-c++-ts-mode-maybe)))

(ert-deftest yunge-treesit-built-in-maybe-entry-points-use-wrappers ()
  (dolist (entry yunge-treesit-maybe-mode-alist)
    (should (advice-member-p
             (yunge-treesit--wrapper-symbol (cdr entry))
             (car entry))))
  (should (equal (cdr (assoc "\\.\\(?:md\\|markdown\\)\\'"
                             auto-mode-alist))
                 'yunge-markdown-ts-mode-maybe)))

(ert-deftest yunge-treesit-wrapper-starts-ts-mode-after-install ()
  (let (started)
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
              ((symbol-function 'treesit-ensure-installed) (lambda (_lang) t))
              ((symbol-function 'python-ts-mode)
               (lambda () (setq started 'treesit)))
              ((symbol-function 'python-mode)
               (lambda () (setq started 'fallback))))
      (yunge-python-ts-mode-maybe))
    (should (eq started 'treesit))))

(ert-deftest yunge-treesit-wrapper-falls-back-when-install-is-declined ()
  (let (started)
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
              ((symbol-function 'treesit-ensure-installed) (lambda (_lang) nil))
              ((symbol-function 'python-ts-mode)
               (lambda () (setq started 'treesit)))
              ((symbol-function 'python-mode)
               (lambda () (setq started 'fallback))))
      (yunge-python-ts-mode-maybe))
    (should (eq started 'fallback))))

(ert-deftest yunge-treesit-wrapper-falls-back-after-install-error ()
  (let (started)
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
              ((symbol-function 'treesit-ensure-installed)
               (lambda (_lang) (error "compiler failed")))
              ((symbol-function 'python-ts-mode)
               (lambda () (setq started 'treesit)))
              ((symbol-function 'python-mode)
               (lambda () (setq started 'fallback))))
      (yunge-python-ts-mode-maybe))
    (should (eq started 'fallback))))

(ert-deftest yunge-treesit-stops-after-a-required-grammar-is-declined ()
  (let (checked started)
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
              ((symbol-function 'treesit-ensure-installed)
               (lambda (language)
                 (push language checked)
                 (not (eq language 'html))))
              ((symbol-function 'php-ts-mode)
               (lambda () (setq started 'treesit)))
              ((symbol-function 'php-mode)
               (lambda () (setq started 'fallback))))
      (yunge-php-ts-mode-maybe))
    (should (equal (nreverse checked) '(php phpdoc html)))
    (should (eq started 'fallback))))

(provide 'yunge-treesit-test)

;;; yunge-treesit-test.el ends here
