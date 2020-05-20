;;; belak-lang-rust.el -*- lexical-binding: t; -*-

;; TODO: look into rustic

(require 'belak-core)
(require 'belak-dev)

;;
;;; Packages

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . subword-mode))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after (rust-mode flycheck)
  :hook (flycheck-mode . flycheck-rust-setup))

;;
;;; Tweaks

(use-feature projectile
  :config
  (add-to-list 'projectile-project-root-files "Cargo.toml")
  (add-to-list 'projectile-globally-ignored-directories "target"))

(provide 'belak-lang-rust)
;;; belak-lang-rust.el ends here.
