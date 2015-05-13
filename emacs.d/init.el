(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Make sure we have the package archive loaded
(unless package-archive-contents
  (package-refresh-contents))

;; Helper function to install any missing packages
(defun require-package (p)
  (when (not (package-installed-p p))
    (package-install p)))

;; Treat option as meta and command as super
(when (equal system-type 'darwin)
  (setq mac-option-key-is-meta t)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

;; Better defaults
(require-package 'better-defaults)

;; Random utils
(require-package 'magit)

;; Load the theme
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;; ido stuff
(require-package 'smex)
(require-package 'ido-ubiquitous)
(require-package 'ido-vertical-mode)
(require-package 'flx-ido)
(ido-ubiquitous-mode t)
(ido-vertical-mode)
(flx-ido-mode t)

;; Better mode-line
(require-package 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'respectful)

;; Rainbow-mode
(require-package 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; Better less support
(require-package 'less-css-mode)

;; Golang
(require-package 'go-mode)

;; Syntax checking
(require-package 'flycheck)

;; Snippets
(require-package 'yasnippet)

;; Autocomplete
(require-package 'auto-complete)

;; Random settings
(add-hook 'prog-mode-hook 'linum-mode)
(setq initial-buffer-choice t)
(setq inhibit-startup-screen t)
