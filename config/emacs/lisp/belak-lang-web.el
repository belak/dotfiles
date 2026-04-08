;;; belak-lang-web.el --- Javascript/HTML and friends -*- lexical-binding: t; -*-

(require 'belak-lib)
(require 'belak-dev)

(use-package typescript-mode
  :blackout "Typescript"
  :mode ("\\.tsx?\\'" . typescript-mode))


;;
;;; CSS

(use-package css-mode
  :mode "\\.css\\'")


;;
;;; HTML

(use-package nxml-mode
  :mode "\\.xml\\'"
  :config
  ;; Automatically complete closing tags
  (setq nxml-slash-auto-complete-flag t))

;; Add support for lots of dynamic template types.
(use-package web-mode
  :after nxml-mode
  :mode
  "\\.erb\\'"
  "\\.hbs\\'"
  "\\.html?\\'"
  "\\.jinja\\'"
  "\\.mustache\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-current-element-highlight t))

;;
;;; Other

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2))

;;
;;; Tweaks

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(provide 'belak-lang-web)
;;; belak-lang-web.el ends here
