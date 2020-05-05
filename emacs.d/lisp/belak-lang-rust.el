;;; belak-lang-rust.el -*- lexical-binding: t; -*-

;; TODO: look into rustic

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package flycheck-rust
  :requires flycheck
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'belak-lang-rust)
;;; belak-lang-rust.el ends here
