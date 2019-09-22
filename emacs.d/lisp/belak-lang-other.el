;;; belak-lang-other --- file types needing less configuration

;;; Commentary:

;;; Code:

(use-package dockerfile-mode
  :mode
  "Dockerfile\(-.*\)?")

(use-package emmet-mode
  :after web-mode
  :hook (web-mode-hook . emmet-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

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
