;;; belak-lang-go --- go related packages and settings

;;; Commentary:

;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode-hook . belak--go-mode-hook)
  :config
  (setq gofmt-command "goimports")

  ;; TODO: move these to the :hook config
  (defun belak--go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)
    (subword-mode 1)))

(use-package company-go
  :after (go-mode company)
  :config
  (setq company-go-show-annotation t)
  (add-to-list 'company-backends 'company-go))

(use-package go-eldoc
  :after (go-mode eldoc)
  :hook (go-mode-hook . go-eldoc-setup))

(provide 'belak-lang-go)

;;; belak-lang-go.el ends here
