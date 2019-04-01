;;; belak-dev-go.el --- go related packages and settings

;;; Commentary:

;;; Code:

(use-package go-mode
  :mode "\\.go\\'"
  ;; TODO: Fix go-guru
  ;;:general
  ;;(:keymaps 'go-mode-map
  ;; "M-."   'go-guru-definition
  ;; "C-c o" 'go-guru-map)
  :config
  (setq gofmt-command "goimports")

  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)
    (subword-mode 1))

  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package company-go
  :after (go-mode company)
  :config
  (setq company-go-show-annotation t)
  (belak/register-company-backend 'go-mode-hook 'company-go))

;; These are helper functions, initially taken from dominikh's dotfiles.

(defun go-instrument-returns ()
  "Add print statements before each return call.

  Originally taken from https://github.com/dominikh/dotfiles/blob/master/emacs.d/go.el"
  (interactive)
  (save-excursion
    (save-restriction
      (let ((cnt 0))
        (narrow-to-defun)
        (beginning-of-defun)
        (while (re-search-forward "^[[:space:]]+return")
          (setq cnt (1+ cnt))
          (beginning-of-line)
          (open-line 1)
          (funcall indent-line-function)
          (insert (format "log.Println(\"return statement %d\") /* RETURN INSTRUMENT */" cnt))
          (forward-line 2))))))

(defun go-deinstrument-returns ()
  "Remove print statements added by `go-instrument-returns'.

  Originally taken from https://github.com/dominikh/dotfiles/blob/master/emacs.d/go.el"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (beginning-of-defun)
      (while (re-search-forward "^.+/\\* RETURN INSTRUMENT \\*/\n" nil t)
        (replace-match "" nil nil)))))

(provide 'belak-dev-go)

;;; belak-dev-go.el ends here
