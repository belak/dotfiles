;;; belak-lang-elixir -- Elixir related packages and settings

;;; Commentary:

;;; Code:

(use-package js2-mode
  :mode ("\\.js\\'" . js2-jsx-mode))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode))

(provide 'belak-lang-javascript)

;;; belak-lang-javascript.el ends here
