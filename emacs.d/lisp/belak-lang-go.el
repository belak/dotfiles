;;; belak-lang-go.el -*- lexical-binding: t; -*-

(require 'belak-lib)
(require 'belak-dev)

;;
;;; Packages

(use-package! go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . belak--go-mode-hook)
         (go-mode . subword-mode)
         (go-mode . eglot-ensure))
  :config
  ;; Ignore go test -c output files
  (add-to-list 'completion-ignored-extensions ".test")

  ;; Prefer goimports to gofmt if installed
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))

  (defun belak--go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-tests t))

(use-package! go-tag
  :commands
  go-tag-add
  go-tag-refresh
  go-tag-remove
  :config
  (setq go-tag-args (list "-transform" "camelcase")))

;;
;;; Tweaks

(after! projectile
  (add-to-list 'projectile-project-root-files "go.mod"))

(provide 'belak-lang-go)
;;; belak-lang-go.el ends here
