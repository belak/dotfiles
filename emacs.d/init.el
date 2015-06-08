(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Make sure we have use-package installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Explicitly load use-package
(require 'use-package)
(require 'bind-key)
(require 'diminish)

(setq use-package-always-ensure t)

;; Simple settings changes
(use-package better-defaults)

;; Random settings
(add-hook 'prog-mode-hook 'linum-mode)
(setq initial-buffer-choice 't
      inhibit-startup-screen 't
      show-trailing-whitespace 't
      ring-bell-function 'ignore
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      ;mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1)

;; Set a default font which we may override later
(set-frame-font "Monospace 12")
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; System specific stuff
(cond ((eq system-type 'gnu/linux)
       (set-frame-font "Terminus 8"))

      ((eq system-type 'darwin)
      (setq ns-use-native-fullscreen t
            mac-option-key-is-meta t
            mac-command-key-is-meta nil
            mac-command-modifier 'super
            mac-option-modifier 'meta)

       (set-frame-font "Menlo 12")))

;; https://github.com/sandhu/emacs.d/blob/master/lisp/teppoudo-diminish.el
(defmacro diminish-major-mode (mode new-name)
  `(add-hook (intern (concat (symbol-name ,mode) "-hook"))
             '(lambda () (setq mode-name ,new-name))))

(diminish-major-mode 'lisp-interaction-mode "λ»")
(diminish-major-mode 'emacs-lisp-mode "Eλ")
(diminish-major-mode 'lisp-mode "λ")

;; Helm
(use-package helm
  :demand
  :diminish helm-mode
  :bind
  ("M-x"     . helm-M-x)
  ("C-x b"   . helm-buffers-list)
  ("C-x C-f" . helm-find-files)
  ("C-c o"   . helm-occur)
  ("M-/"     . helm-dabbrev)
  :config
  ; Reverse tab and C-z
  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action)
             ("C-z"   . helm-select-action))
  (helm-mode 1)
  (helm-autoresize-mode 1))

;; Random utils
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (magit-auto-revert-mode -1))

;; ido stuff
(use-package smex
  :disabled t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (use-package ido-ubiquitous
    :config
    (ido-ubiquitous-mode 1))
  (use-package ido-vertical-mode
    :config
    (ido-vertical-mode 1))
  (use-package flx-ido
    :config
    (flx-ido-mode 1)))

;; Better mode-line
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful))

(use-package fic-mode
  :diminish fic-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'turn-on-fic-mode)))

;; Rainbow-mode
(use-package rainbow-mode
  :commands rainbow-mode)

;; Org-mode
(use-package org
  :mode ("\\.org\\'" . org-mode))

;; Better less support
;(require-package 'less-css-mode)

;; Golang
;(require-package 'go-mode)

;; Syntax checking
;(require-package 'flycheck)

;; Snippets
;(require-package 'yasnippet)

;; Autocom;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode))

;; Grab some useful env vars from the shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Auto-complete
(use-package auto-complete
  :config
  (use-package go-autocomplete)
  (use-package jedi
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t))
  (ac-linum-workaround))

;; Completion
(use-package company
  :disabled t
  :config
  (use-package company-go)
  (use-package company-jedi
    (setq jedi:complete-on-dot t))

  (setq company-idle-delay 0)

  (add-hook 'after-init-hook 'global-company-mode))
