(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)

;; Simple settings changes
(require 'init-settings)

;; Appearance - this is loaded before system specific stuff so the default font
;; doesn't cause trouble.
(require 'init-appearance)

;; System specific stuff
(require 'init-osx)
(require 'init-linux)

;; Random utils
(require-package 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

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
