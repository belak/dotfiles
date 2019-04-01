;;; belak-dev -- Common development related settings

;;; Commentary:

;;; Code:

;; company-mode is used as a completion system.

(use-package company
  :defer 2
  :diminish company-mode
  :config
  (defmacro belak--register-company-backend (hook backend)
    `(add-hook ,hook (lambda ()
                       (set (make-local-variable 'company-backends) (list ,backend)))))

  (setq company-tooltip-limit 20
        company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t)

  ;; Tab-N-Go seems to align better than the defaults with how I like
  ;; tab completion to work. This allows tab to cycle through entries
  ;; and makes the enter key work as the enter key.
  (company-tng-configure-default)

  (global-company-mode))

;; editorconfig is a simple way to share indentation settings between
;; editors. Because I sometimes dabble in vim, sublime etc, it's nice
;; to not have to re-do these settings at a project level between
;; editors.

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Used to show relevant documentation in the echo area.

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

;; Grab important variables from our shell.

(use-package exec-path-from-shell
  :if IS-GUI
  :config
  (exec-path-from-shell-initialize))

;; flycheck-mode is used for linters and catching compilation errors.

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

;; magit is the best git interface in an editor I've used.

(use-package magit
  :general
  ("M-g M-g" 'magit-status)
  :config
  (setq magit-push-current-set-remote-if-missing t))

;; Project based navigation is pretty much the best thing ever.

(use-package projectile
  :diminish projectile-mode
  :commands
  projectile-project-p
  :general
  ("C-c p" '(:keymap projectile-command-map))
  :config
  (setq projectile-known-projects-file (concat belak-local-dir "projectile-bookmarks.eld"))
  (projectile-mode +1))

;; rainbow-mode makes it easier to see colors
(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'belak-dev)

;;; belak-dev.el ends here
