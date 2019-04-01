;;; belak-dev-various.el --- leftover dev settings and packages

;;; Commentary:

;;; Code:

(use-package cmake-mode
  :mode
  "CMakeLists.txt"
  "\\.cmake\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\(-.*\)?")

;; web-mode is designed to handle HTML-ish templates.

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

(use-package emmet-mode
  :after web-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode))

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
  (add-hook 'ruby-mode-hook #'subword-mode))

(use-package systemd
  :mode ("\\.service\'" . systemd-mode))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

(provide 'belak-dev-various)

;;; belak-dev-various.el ends here
