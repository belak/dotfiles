;;; belak-lang-ruby.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

;; TODO: look at Sacha's ruby/robe config
(use-feature! ruby-mode
  :mode "\\.rb\\'"
  :config
  :hook
  (ruby-mode . subword-mode))

(use-package! rbenv
  :after ruby-mode
  :demand t
  :config
  (global-rbenv-mode))

(provide 'belak-lang-ruby)
;;; belak-lang-ruby.el ends here
