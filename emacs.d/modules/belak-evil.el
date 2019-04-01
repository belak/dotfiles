;;; belak-evil.el --- evil-mode related packages and settings

;;; Commentary:

;;; Settings:

(defvar belak-evil-enabled t)

;;; Code:

(use-package evil
  :if belak-evil-enabled
  :defer nil
  :diminish evil-mode
  :init
  (setq evil-echo-state t
	evil-want-C-w-in-emacs-state t
	;; We want to let evil-collection set keybinds for any
	;; additional modes.
	evil-want-integration t
	evil-want-keybinding nil)
  :config
  ;; TODO: Not sure why this has to be called here.
  ;;(general-evil-setup)
  ;; Actually enable evil-mode.
  (evil-mode 1)

  (defun belak--update-evil-state-cursor-colors ()
    ;; Pull evil-mode faces from the current theme
    (setq evil-emacs-state-cursor   `(,(face-background 'spaceline-evil-emacs   nil t) box)
          evil-insert-state-cursor  `(,(face-background 'spaceline-evil-insert  nil t) bar)
          evil-motion-state-cursor  `(,(face-background 'spaceline-evil-motion  nil t) box)
          evil-normal-state-cursor  `(,(face-background 'spaceline-evil-normal  nil t) box)
          evil-replace-state-cursor `(,(face-background 'spaceline-evil-replace nil t) hbar)
          evil-visual-state-cursor  `(,(face-background 'spaceline-evil-visual  nil t) hbar)))

  ;; Wrap enable-theme and disable-theme to update the cursor colors
  ;; whenever the theme changes.
  (advice-add 'enable-theme  :after 'belak--update-evil-state-cursor-colors)
  (advice-add 'disable-theme :after 'belak--update-evil-state-cursor-colors)

  (belak--update-evil-state-cursor-colors))

;; Add gc for commenting

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

(use-package evil-easymotion
  :after evil
  :config
  ;; TODO: Find a better prefix because we want to use space as our
  ;; leader.
  (evilem-default-keybindings "SPC"))

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)

  ;; TODO: Add these to base16 and monokai-pro.
  (evil-goggles-use-diff-faces))

;; Add % as a bind to jump between matching tags.

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; I prefer using C-a and C-e because they work both inside and
;; outside normal mode.

(use-package evil-rsi
  :after evil
  :diminish evil-rsi-mode
  :config
  (evil-rsi-mode 1))

;; Include community-maintained keybinds for additional packages.

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'belak-evil)

;;; belak-evil.el ends here
