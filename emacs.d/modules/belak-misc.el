;;; belak-misc --- miscellaneous packages and settings

;;; Commentary:

;;; Code:

;; helpful is a replacement for the built-in help pages which are much
;; prettier and easier to read.

(use-package helpful
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-h ." 'helpful-at-point))

(use-package paradox
  :commands
  paradox-list-packages)

(provide 'belak-misc)
;;; belak-misc.el ends here
