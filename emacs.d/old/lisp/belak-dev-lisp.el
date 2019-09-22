;;; belak-dev-lisp --- lisp related settings

;;; Commentary:

;;; Code:

;; macrostep is a really useful way to debug macros by expanding them.

(use-package macrostep
  :general
  (:keymaps 'emacs-lisp-mode-map
   "C-x e" 'macrostep-expand))

(use-package slime
  :config
  (setq slime-contribs '(fancy)))

(provide 'belak-dev-lisp)

;;; belak-dev-lisp.el ends here
