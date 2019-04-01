;;; belak-lang-rust.el --- rust related settings and packages

;;; Commentary:

;; Rust is a fairly recent addition which I'd like to learn more
;; about, which is why the settings here are fairly minimal.

;;; Code:

(use-package rust-mode
  :mode
  "\\.rs\\'")

(use-package racer
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package flycheck-rust
  :after (rust-mode flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rustfmt
  :disabled t
  :after rust
  :general
  (:keymaps 'rust-mode-map
            "C-c C-f" 'rustfmt-format-buffer))

(provide 'belak-lang-rust)

;;; belak-lang-rust.el ends here
