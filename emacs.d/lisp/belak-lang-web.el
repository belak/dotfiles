;;; belak-lang-web.el --- Javascript/HTML and friends -*- lexical-binding: t; -*-

(require 'belak-core)
(require 'belak-dev)

;;
;;; JavaScript

;; After trying a number of js setups, I've settled on this one because it's
;; simple and doesn't try to do too much. When the community moves as fast as
;; the JS community does, you don't want to have to constantly update your
;; config to keep working.

(use-package js2-mode
  :delight
  (js2-mode "JS")
  (js2-jsx-mode "JSX")
  :mode
  ("\\.js\\'" . js2-mode)
  ("\\.jsx\\'" . js2-jsx-mode)
  :interpreter ("node" . js2-mode))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode))


;;
;;; HTML

(use-package nxml-mode
  :straight nil
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
        web-mode-code-indent-offset 2))

(use-package emmet-mode
  :after web-mode
  :hook (web-mode . emmet-mode))


;;
;;; Other

(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

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
