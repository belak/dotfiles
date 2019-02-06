;;; belak-common.el --- common packages which need to be loaded early

;;; Commentary:

;; Portions of this config depend on other components, so we load the
;; common components here.  The only things which should be here are
;; packages which are needed for the organization of the rest of the
;; file and stuff which needs to be loaded very early.

;;; Code:

;; Because I maintain these packages, I have them checked out to a
;; directory, rather than dealing with installing the package from
;; melpa. Because of this we need to do a little bit of acrobatics to
;; make sure all the proper directories are in the proper load paths.

;; We load the theme as early as humanly possible so we're not waiting
;; for other packages to load before fixing the colors.

(use-package falcon-theme
  :ensure nil
  :if (eq belak/theme 'falcon)
  :load-path "site-lisp/falcon-theme"
  :config (load-theme 'falcon t))

(defvar belak/base16-colors nil)
(use-package base16-theme
  :ensure nil
  :if (eq belak/theme 'base16)
  :load-path "site-lisp/base16-theme"
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/base16-theme/build")
  :config
  (setq base16-theme-256-color-source "colors")
  (load-theme 'base16-tomorrow-night t)
  (setq belak/base16-colors base16-tomorrow-night-colors))

(use-package grayscale-theme
  :ensure nil
  :if (eq belak/theme 'grayscale)
  :load-path "site-lisp/grayscale-theme"
  :config
  (load-theme 'grayscale t))

(use-package solarized-theme
  :if (eq belak/theme 'solarized)
  :config
  (setq solarized-use-variable-pitch nil
        solarized-distinct-fringe-background t
        solarized-scale-org-headlines nil)
  (load-theme 'solarized-light t))

;; company-mode is used as a completion system.

(use-package company
  :diminish company-mode
  :config
  (defmacro belak/register-company-backend (hook backend)
    `(add-hook ,hook (lambda ()
                       (set (make-local-variable 'company-backends) (list ,backend)))))

  (setq company-tooltip-limit 20
        company-idle-delay 0
        company-echo-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t)

  (global-company-mode))

;; flycheck-mode is used for linters and catching compilation errors.

(use-package flycheck
  :diminish flycheck-mode
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  (global-flycheck-mode))

;; Project based navigation is pretty much the best thing ever.

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  (projectile-mode +1))

;; spaceline is a better modeline with simple config. It's up here
;; because it needs to be loaded before persistent-scratch and anzu.

(use-package spaceline
  :demand
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'bar
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(provide 'belak-common)

;;; belak-common.el ends here
