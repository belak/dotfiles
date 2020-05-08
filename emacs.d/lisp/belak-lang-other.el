;;; belak-lang-other.el -*- lexical-binding: t; -*-

(use-package cmake-mode
  :mode
  "CMakeLists.txt"
  "\\.cmake\\'")

(use-package dockerfile-mode
  ;; It may seem slightly odd to match all files containing the name Dockerfile,
  ;; but we also want to match filenames like Dockerfile-beta.
  :mode "Dockerfile")

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package php-mode
  :mode "\\.php\\'")

(use-package ruby-mode
  :mode "\\.rb\\'"
  :config
  :hook
  (ruby-mode . subword-mode))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

(provide 'belak-lang-other)
;;; belak-lang-other.el ends here
