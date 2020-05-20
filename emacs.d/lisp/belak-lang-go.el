;;; belak-lang-go.el -*- lexical-binding: t; -*-

(require 'belak-core)
(require 'belak-dev)

;;
;;; Packages

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . belak--go-mode-hook)
         (go-mode . subword-mode))
  :config
  ;; Ignore go test -c output files
  (add-to-list 'completion-ignored-extensions ".test")

  ;; Prefer goimports to gofmt if installed
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))

  (defun belak--go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(use-package company-go
  :demand t
  :after (go-mode company)
  :config
  (setq company-go-show-annotation t)
  (set-company-backend! go-mode-hook company-go))

(use-package go-eldoc
  :demand t
  :after go-mode
  :hook (go-mode . go-eldoc-setup))

;;
;;; Tweaks

(after! projectile
  (add-to-list 'projectile-project-root-files "go.mod"))

(provide 'belak-lang-go)
;;; belak-lang-go.el ends here
