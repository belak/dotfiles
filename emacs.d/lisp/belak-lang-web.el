;;; belak-lang-web.el --- Javascript/HTML and friends -*- lexical-binding: t; -*-

;;
;;; Javascript

;; After trying a number of js setups, I've settled on this one because it's
;; simple and doesn't try to do too much. When the community moves as fast as
;; the JS community does, you don't want to have to constantly update your
;; config to keep working.

(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-jsx-mode))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))


;;
;;; HTML

;; Automatically complete closing tags
(setq nxml-slash-auto-complete-flag t)

;; Add support for lots of dynamic template types.
(use-package web-mode
  :mode
  "\\.erb\\'"
  "\\.html?\\'"
  "\\.jinja\\'"
  "\\.mustache\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package emmet-mode
  :after web-mode
  :hook (web-mode . emmet-mode))


;;
;;; Other

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2))


(provide 'belak-lang-web)
;;; belak-lang-web.el ends here
