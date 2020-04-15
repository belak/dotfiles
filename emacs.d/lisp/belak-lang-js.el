;;; belak-lang-js --- javascript ecosystem related settings -*- lexical-binding: t; -*-

;;; Commentary:

;; After trying a number of js setups, I've settled on this one
;; because it's simple and doesn't try to do too much.  When the
;; community moves as fast as the JS community does, you don't want to
;; have to constantly update your config to keep working.

;;; Code:

(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-jsx-mode))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))

(provide 'belak-lang-js)

;;; belak-lang-js.el ends here
