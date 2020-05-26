;;; belak-lang-other.el -*- lexical-binding: t; -*-

(require 'belak-core)

;;
;;; Packages

(use-package cmake-mode
  :mode
  "CMakeLists\\.txt"
  "\\.cmake\\'")

(use-package dockerfile-mode
  ;; It may seem slightly odd to match all files containing the name Dockerfile
  ;; rather than the exact name, but we also want to match filenames like
  ;; Dockerfile-beta.
  :mode "Dockerfile")

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package php-mode
  :mode "\\.php\\'")

(use-package pkgbuild-mode
  :mode "PKGBUILD\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'")

;; TODO: look at Sacha's ruby/robe config
(use-package ruby-mode
  :mode "\\.rb\\'"
  :config
  :hook
  (ruby-mode . subword-mode))

(use-feature sh-mode
  :mode
  "\\.zsh\\'"
  "\\.sh\\'"
  "zshrc\\'"
  "zshenv\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-mode . subword-mode))

(provide 'belak-lang-other)
;;; belak-lang-other.el ends here
