;;; belak-dev --- development related settings

;;; Commentary:

;;; Code:

;; Ensure we show trailing whitespace in modes we care about.  This
;; includes everything derived from prog-mode or text-mode.  We
;; unfortunately can't just use setq-default because that includes
;; buffers like ido.
(add-hook 'prog-mode-hook (setq show-trailing-whitespace t))
(add-hook 'text-mode-hook (setq show-trailing-whitespace t))

;; company-mode is used as a completion system.
(use-package company
  :disabled t
  :defer 2
  :delight company-mode
  :config
  (defmacro belak--register-company-backend (hook backend)
    `(add-hook ,hook (lambda ()
                       (set (make-local-variable 'company-backends) (list ,backend)))))

  (setq company-tooltip-limit 20
        company-idle-delay 0
        ;;company-echo-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t)

  ;; Tab-N-Go seems to align better than the defaults with how I like
  ;; tab completion to work. This allows tab to cycle through entries
  ;; and makes the enter key work as the enter key.
  (company-tng-configure-default)

  (global-company-mode))

(use-package diff-hl
  :defer 2
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; We can't use the normal diff-hl mode in a terminal, but luckily a
  ;; fallback is provided.
  (if IS-GUI
      (global-diff-hl-mode 1)
    (diff-hl-margin-mode 1)))

;; display-line-numbers is built-in to emacs 26.1 and above. It's
;; similar to linum-mode, but it performs much better.
(use-package display-line-numbers
  :straight nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'visual))

;; editorconfig is a simple way to share indentation settings between
;; editors. Because I sometimes dabble in vim, sublime etc, it's nice
;; to not have to re-do these settings at a project level between
;; editors.
(use-package editorconfig
  :delight editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Eldoc is used to show relevant documentation in the echo area.
;; However, this block is mostly so we can call delight to hide it in
;; the modes display.
(use-package eldoc
  :straight nil
  :delight eldoc-mode
  :config
  (setq eldoc-idle-delay 0.1))

;; Grab important variables from the shell. This is only needed in the
;; GUI because the shell will already inherit the environment
;; directly.
(use-package exec-path-from-shell
  :if IS-GUI
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package expand-region
  :functions (er/expand-region)
  :bind ("C-=" . er/expand-region))

;; flycheck-mode is used for linters and catching compilation errors.
(use-package flycheck
  :delight flycheck-mode
  :config
  ;;(defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  (global-flycheck-mode))

(use-package flyspell
  :straight nil
  :delight flyspell-mode
  :hook
  (text-mode . flyspell-mode))

;; hl-todo simply highlights TODO and other similar comments to make
;; them easier to find. I originally used fic-mode, but it appears
;; that hl-todo is a little better and is updated more frequently.
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; magit is the best git interface in an editor I've used.
(use-package magit
  :general
  ("M-g M-g" 'magit-status)
  :config
  (setq magit-push-current-set-remote-if-missing t))

;; Project based navigation is pretty much the best thing ever.
(use-package projectile
  :delight projectile-mode
  :commands
  projectile-project-p
  :general
  ("C-c p" '(:keymap projectile-command-map))
  :config
  (projectile-mode +1))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

;; rainbow-mode makes it easier to see colors, but I don't use it very
;; often so I leave it disabled unless called.
(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'belak-dev)

;;; belak-dev.el ends here
