;;; belak-evil.el --- evil-mode related settings

;;; Commentary:

;;; Code:

;; There's an evil-mode killswitch here because there are so many
;; packages which we would have to manually add =:disabled t= to
;; otherwise.
;;
;; evil-mode is a vim emulation layer for Emacs. It's currently
;; disabled as I've switched back to Emacs keybinds.
;;
;; Note that it is important to load this early because of how many
;; packages actually use it.

(use-package evil
  :demand
  :if belak/evil-enabled
  :general
  ("C-'" 'evil-toggle-key)
  :config
  ;; We need to do the evil setup for general here, because I disable
  ;; evil-mode fairly frequently.
  (general-evil-setup)

  (evil-mode 1)
  (setq evil-echo-state nil
        evil-vsplit-window-right t
        evil-split-window-below t)

  ;; Set the cursor color based on the evil state
  (if belak/base16-colors
      (setq evil-emacs-state-cursor   `(,(plist-get belak/base16-colors :base0D) box)
            evil-insert-state-cursor  `(,(plist-get belak/base16-colors :base0D) bar)
            evil-motion-state-cursor  `(,(plist-get belak/base16-colors :base0E) box)
            evil-normal-state-cursor  `(,(plist-get belak/base16-colors :base0B) box)
            evil-replace-state-cursor `(,(plist-get belak/base16-colors :base08) bar)
            evil-visual-state-cursor  `(,(plist-get belak/base16-colors :base09) box)))

  ;; Evil selection shouldn't update the clipboard
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; For the operator state, the only thing we want to change is the
  ;; size. We can keep the same color.
  (setq evil-operator-state-cursor 'evil-half-cursor))

;; I really like C-a and C-e for beginning and end of the line, so
;; evil-rsi is included which tries to add back useful emacs bindings.

(use-package evil-rsi
  :if belak/evil-enabled
  :after evil-mode
  :config (evil-rsi-mode))

;; This is a port of tpope's vim-surround which adds text objects for
;; surrounding characters.

(use-package evil-surround
  :if belak/evil-enabled
  :after evil-mode
  :config
  (global-evil-surround-mode 1))

(provide 'belak-evil)

;;; belak-evil.el ends here
