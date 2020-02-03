;;; belak-lang-other --- file types needing less configuration

;;; Commentary:

;;; Code:

(use-package cmake-mode
  :mode
  "CMakeLists.txt"
  "\\.cmake\\'")

(use-package dockerfile-mode
  :mode
  "Dockerfile\(-.*\)?")

(use-package emmet-mode
  :after web-mode
  :hook (web-mode . emmet-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2))

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

(use-package web-mode
  :mode
  "\\.erb\\'"
  "\\.html\\'"
  "\\.jinja\\'"
  "\\.mustache\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package yaml-mode
  :mode "\\.yml\\'")

(provide 'belak-lang-other)

;;; belak-lang-other.el ends here
