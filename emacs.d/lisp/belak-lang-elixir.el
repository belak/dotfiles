;;; belak-lang-elixir -*- lexical-binding: t; -*-

(require 'belak-core)
(require 'belak-dev)

;;
;;; Packages

(use-package elixir-mode
  :mode
  ("\\.ex\\'" . elixir-mode)
  ("\\.exs\\'" . elixir-mode))

(use-package alchemist
  :after elixir-mode
  :hook (elixir-mode . alchemist-mode)
  :config (set-company-backend! elixir-mode-hook alchemist-company))

(use-package flycheck-credo
  :after (alchemist flycheck)
  :config (flycheck-credo-setup))

(provide 'belak-lang-elixir)
;;; belak-lang-elixir.el ends here.
