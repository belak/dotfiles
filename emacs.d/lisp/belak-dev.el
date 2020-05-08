;;; belak-dev.el -*- lexical-binding: t; -*-

(use-package eldoc
  :straight nil
  :delight eldoc-mode
  :hook
  (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1))

(use-package remember
  :straight nil
  :commands remember remember-notes
  :config
  (setq remember-notes-initial-major-mode 'gfm-mode))

(use-package company
  :init
  (add-transient-hook! pre-command-hook (global-company-mode +1))
  :commands global-company-mode
  :config
  ;; TODO: move this to lib?
  (defmacro belak--register-company-backend (hook backend)
    `(add-hook ,hook (lambda ()
                       (set (make-local-variable 'company-backends) (list ,backend)))))

  ;; TODO: look into tab-n-go.

  (setq company-idle-delay 0.25
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never

        ;; Improve basic text matching
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

;; hl-todo simply highlights TODO and other similar comments to make
;; them easier to find. I originally used fic-mode, but it appears
;; that hl-todo is a little better and is updated more frequently.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; TODO: tweak hl-todo-keyword-faces, maybe remove most of them
  (setq hl-todo-highlight-punctuation ":"))

(use-package diff-hl
  :hook
  (prog-mode . belak--diff-hl-mode)
  :config
  (defun belak--diff-hl-mode (&optional arg)
    (if IS-GUI
        (diff-hl-mode arg)
      (diff-hl-margin-mode arg))))

;; flycheck-mode is used for linters and catching compilation errors.
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :delight flycheck-mode
  :config
  ;; The default flycheck settings are a bit too agressive - we really only want
  ;; to check when the file is loaded or saved.
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch)))

(use-package flyspell
  :straight nil
  :delight flyspell-mode
  :general
  ("C-c g" 'magit-status)
  :hook
  (text-mode . flyspell-mode))

(use-package magit
  ;; TODO: set some keybinds for this?
  :config
  (setq magit-push-current-set-remote-if-missing t))

(use-package editorconfig
  :delight editorconfig-mode
  :hook
  (prog-mode . editorconfig-mode)
  (text-mode . editorconfig-mode))

(use-package projectile
  :delight projectile-mode
  :commands projectile-project-p
  :general ("C-c p" '(:keymap projectile-command-map))
  :config (projectile-mode +1))

;; rainbow-mode makes it easier to see colors, but I don't use it very
;; often so I leave it disabled unless called.
(use-package rainbow-mode
  :commands rainbow-mode)


;;
;;; Mode Tweaks

;; Make the lisp mode names a bit shorter
(delight 'lisp-interaction-mode "λ»")
(delight 'emacs-lisp-mode "Eλ")
(delight 'lisp-mode "λ")

;; Auto wrap text in text-mode.
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Some extra files to support with basic modes
(push '("LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'"  . text-mode) auto-mode-alist)
(push '("\\.env\\'"  . sh-mode)   auto-mode-alist)

(provide 'belak-dev)
;;; belak-dev.el ends here
