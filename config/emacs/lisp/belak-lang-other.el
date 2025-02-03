;;; belak-lang-other.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

(use-package bazel
  :mode
  ("\\.bzl\\'"   . bazel-mode)
  ("\\.bazel\\'" . bazel-mode))

(use-package cmake-mode
  :mode
  "CMakeLists\\.txt\\'"
  "\\.cmake\\'")

(use-package dockerfile-mode
  ;; It may seem slightly odd to match all files containing the name
  ;; Dockerfile rather than the exact name, but we also want to match
  ;; filenames like Dockerfile-beta.
  :mode "Dockerfile")

(use-package git-modes
  :mode
  ;; We only include a subset of patterns from the relevant modes, since we need
  ;; to put something in here to get autoload working with `use-package' and
  ;; these are the primary ones we use.
  ("/\\.gitconfig\\'"     . gitconfig-mode)
  ("/\\.git/config\\'"    . gitconfig-mode)
  ("/\\.gitmodules\\'"    . gitconfig-mode)
  ("/\\.gitignore\\'"     . gitignore-mode)
  ("/\\.gitattributes\\'" . gitattributes-mode))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-feature! ruby-mode
  :mode "\\.rb\\'"
  :config
  :hook
  (ruby-mode . subword-mode))

(use-feature! sh-mode
  :mode
  "\\.zsh\\'"
  "\\.sh\\'"
  "zshrc\\'"
  "zshenv\\'")

(use-package toml-mode
  :mode
  "\\.toml\\'"
  "Pipfile\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-mode . subword-mode))


;;
;;; Tweaks

;; Make the lisp mode names a bit shorter
(blackout 'lisp-interaction-mode "λ»")
(blackout 'emacs-lisp-mode "Eλ")
(blackout 'lisp-mode "λ")

;; Some extra files to support with basic modes
(push '("LICENSE\\'"   . text-mode)      auto-mode-alist)
(push '("\\.log\\'"    . text-mode)      auto-mode-alist)
(push '("\\.env\\'"    . sh-mode)        auto-mode-alist)

(provide 'belak-lang-other)
;;; belak-lang-other.el ends here.
