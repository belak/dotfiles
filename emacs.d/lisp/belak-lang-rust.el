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
  :hook (rust-mode . racer-mode))

(use-package flycheck-rust
  :requires flycheck
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'belak-lang-rust)

;;; belak-lang-rust.el ends here
