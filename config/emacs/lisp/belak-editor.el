;;; belak-editor.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Completion

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete)))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))


;;
;;; Packages

(use-package delsel
  :hook (after-init . delete-selection-mode))

(provide 'belak-editor)
;;; belak-editor.el ends here.
