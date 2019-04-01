;;; belak-dev-js.el --- Javascript related packages and settings

;;; Commentary:

;;; Code:

;; js2-mode is a wrapper around js-mode which cleans it up and adds a
;; bunch of features.

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  (when (fboundp 'flycheck-mode)
    (set-face-attribute 'js2-error nil
                        :inherit 'flycheck-error-list-error
                        :underline '(:color foreground-color :style wave))
    (set-face-attribute 'js2-warning nil
                        :inherit 'flycheck-error-list-warning
                        :underline '(:color foreground-color :style wave))))

;; tern is a js navigation package which extends js-mode.

(use-package tern
  :after js2-mode
  :diminish tern-mode
  :config
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :after (js2-mode tern company)
  :config
  (add-to-list 'company-backends 'company-tern)
  (setq company-tern-property-marker ""))

(provide 'belak-dev-js)

;;; belak-dev-js.el ends here
