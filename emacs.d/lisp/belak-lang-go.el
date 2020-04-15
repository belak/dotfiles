;;; belak-lang-go --- go related settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook ((go-mode . belak--go-mode-hook)
	 (go-mode . subword-mode))
  :config
  ;; goimports is generally preferred by the lazy as it will try and
  ;; fix extra and missing imports.
  ;; TODO: look into running both goimports and gofmt
  (setq gofmt-command "goimports")

  (defun belak--go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(use-package company-go
  :requires company
  :after go-mode
  :config
  (setq company-go-show-annotation t)

  ;; TODO: See if this can be done with :hook
  (belak--register-company-backend 'go-mode-hook 'company-go))

(use-package go-eldoc
  :requires eldoc
  :after go-mode
  :hook (go-mode . go-eldoc-setup))

(provide 'belak-lang-go)

;;; belak-lang-go.el ends here
