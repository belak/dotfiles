;;; belak-lang-elixir -- Elixir related packages and settings

;;; Commentary:

;;; Code:

(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode)))

(use-package alchemist
  :after elixir-mode
  :hook (elixir-mode . alchemist-mode)
  :config (add-to-list 'company-backends 'alchemist-company))

(use-package flycheck-credo
  :after (flycheck alchemist)
  :config (flycheck-credo-setup))

(provide 'belak-lang-elixir)

;;; belak-lang-elixir.el ends here
