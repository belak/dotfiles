;;; belak-themes --- theme related settings

;;; Commentary:

;; Logically theme-related settings would go in `belak-ui.el'.
;; However, because I maintain a large number of themes, I often end
;; up switching between them so keeping these settings separate
;; actually makes my workflow easier.

;;; Code:

(use-package base16-theme
  :disabled t
  :config (load-theme 'base16-default-dark t))

(use-package grayscale-theme
 :disabled t
 :config (load-theme 'grayscale t))

(use-package monokai-pro-theme
  :config (load-theme 'monokai-pro t))

(provide 'belak-themes)

;;; belak-themes.el ends here
