;;; belak-lang-web.el --- Javascript/HTML and friends -*- lexical-binding: t; -*-

(require 'belak-lib)
(require 'belak-dev)

;;
;;; JavaScript

;; After trying a number of js setups, I've settled on this one because it's
;; simple and doesn't try to do too much. When the community moves as fast as
;; the JS community does, you don't want to have to constantly update your
;; config to keep working.

;; TODO: look at Sacha's debug-counter

(use-package! js2-mode
  :blackout ((js2-mode     . "JS")
             (js2-jsx-mode . "JSX"))
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode))

(use-package! typescript-mode
  :blackout "Typescript"
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package! tide
  :blackout
  :after (typescript-mode company flycheck)
  :hook
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode))


;;
;;; CSS

(use-feature! css-mode
  :mode "\\.css\\'")


;;
;;; HTML

(use-feature! nxml-mode
  :mode "\\.xml\\'"
  :config
  ;; Automatically complete closing tags
  (setq nxml-slash-auto-complete-flag t))

;; Add support for lots of dynamic template types.
(use-package! web-mode
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

(use-package! emmet-mode
  :after web-mode
  :hook (web-mode . emmet-mode))


;;
;;; Other

(use-package! json-mode
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2))


;;
;;; Tweaks

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(provide 'belak-lang-web)
;;; belak-lang-web.el ends here
