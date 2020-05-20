;;; belak-selectrum.el -*- lexical-binding: t; -*-

(require 'belak-core)

;;
;;; Packages

(use-package selectrum
  :demand t
  :config
  ;; Make the count display a little more like anzu.
  (setq selectrum-count-style 'current/matches)
  (selectrum-mode +1))

(use-package prescient
  :demand t
  :config
  ;; This is essentially the default with fuzzy matching appended.
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :demand t
  :after (selectrum prescient)
  :config
  (selectrum-prescient-mode +1))

(provide 'belak-selectrum)
;;; belak-selectrum.el ends here.
