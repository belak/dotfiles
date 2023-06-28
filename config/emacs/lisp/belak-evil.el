;;; belak-evil.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

(use-package! evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil))

(use-package! evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package! evil-commentary
  :demand t
  :blackout
  :after evil
  :config
  (evil-commentary-mode))

(use-package! evil-leader
  :demand t
  :blackout
  :after evil
  :config
  (evil-leader-mode))

(use-package! evil-rsi
  :demand t
  :blackout
  :after evil
  (evil-rsi-mode))

(use-package! evil-surround
  :demand t
  :after evil
  :config
  (global-evil-surround-mode t))

(provide 'belak-evil)
;;; belak-evil.el ends here
