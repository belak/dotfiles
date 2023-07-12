;;; belak-lang-nix.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

(use-package! nix-mode
  :mode "\\.nix\\'"
  :hook ((nix-mode . subword-mode)
         (nix-mode . eglot-ensure))
  :config
  (after! eglot
    (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))))

(provide 'belak-lang-nix)
;;; belak-lang-nix.el ends here.
