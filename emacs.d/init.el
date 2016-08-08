;; init -- belak's emacs init.el
;;
;;; Commentary:
;; This file is only to bootstrap into README.org and set up some
;; basic timing.
;;
;;; Code:

;;;; Init

;; Define the start time so we can measure how long loading took
;; later.
(defconst emacs-start-time (current-time))

;; Set the gc-cons-threshold as early as possible so we have some
;; extra memory to work with. This increases it to 20M.
(setq gc-cons-threshold 20000000)

;; Make it easier to debug startup errors.
(setq debug-on-error t
      debug-on-quit t)

;; Load in our extra stuff
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/lisp/"))

;;;; Platform-Specific

(defun osx-p ()
  "Check if a system is running OSX."
  (eq system-type 'darwin))

(defun linux-p ()
  "Check if a system is running Linux."
  (eq system-type 'gnu/linux))

(cond ((linux-p)
       (setq browse-url-browser-function 'browse-url-generic
             browse-url-generic-program "xdg-open"))

      ((osx-p)
       (setq ns-use-native-fullscreen t
             mac-command-modifier 'meta
             mac-option-modifier 'super
             mac-control-modifier 'control
             insert-directory-program "/usr/local/bin/gls")

       (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
         (normal-top-level-add-subdirs-to-load-path))

       (toggle-frame-fullscreen)))

;;;; Packages

;; Have to make sure it's loaded before we do anything with it.
(require 'package)

;; Set up the package repos
(setq package-enable-at-startup nil
      package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade"    . "https://marmalade-repo.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")))

;; Disable as many warnings as we can for package installs.
(setq byte-compile-warnings nil)

;; Make sure we load everything
(package-initialize)

;; Set the initial state to non-refreshed. This can also be set back
;; to nil if we want to run a refresh on the next install.
(defvar belak/refreshed-package-list nil)

(defun ensure-refreshed ()
  "Ensure the package list has been refreshed this startup."
  (unless belak/refreshed-package-list
    (package-refresh-contents)
    (setq belak/refreshed-package-list t)))

(defun package-ensure-installed (package)
  "Install a missing PACKAGE if it isn't already."
  (unless (package-installed-p package)
    (ensure-refreshed)
    (package-install package)))

;; Ensure we run a refresh on the first install each session. This
;; fixes issues when the package list is out of date and we start up
;; with a new version of the dotfiles which needs a new package.
(advice-add 'package-install
            :before
            (lambda (&args)
              (when (not belak/refreshed-package-list)
                (message "Refreshing contents from package-install")
                (package-refresh-contents)
                (setq belak/refreshed-package-list t))))

;;; use-package

;; Ensure that use-package is installed and loaded when it needs to
;; be.
(package-ensure-installed 'use-package)
(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

;; Load extra utils included with use-package.
(require 'diminish)
(require 'bind-key)

;; Install quelpa and quelpa-use-package
(package-ensure-installed 'quelpa)
(package-ensure-installed 'quelpa-use-package)
(require 'quelpa-use-package)

;; Always attempt to install packages unless we specify otherwise.
(setq use-package-always-ensure t)

;;;; Early Loading
;; Portions of my init.el depend on other components. The most
;; important of which are loaded here.

;; Load all important custom libraries
(use-package init-frame-hooks
  :ensure nil
  :load-path "lisp/")

;; company-mode is used as a completion system. In use-package blocks,
;; you can use :if (fboundp 'company-mode) to only enable a block if
;; company-mode is active.
(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        company-minimum-prefix-length 1)
  (global-company-mode))

;; flycheck-mode is used for linters and catching compilation
;; errors. In use-package blocks, you can use :if (fboundp
;; 'flycheck-mode) to only enable a block if flycheck-mode is enabled.
(use-package flycheck
  :diminish flycheck-mode
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  (global-flycheck-mode))

;; Project based navigation is pretty much the best thing ever.
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode))

(use-package base16-theme
  :quelpa (base16-theme
           :fetcher github
           :repo "belak/base16-emacs"
           :files ("*.el" "build/*.el"))
  :config
  (load-theme 'base16-default-dark t))

;;;; Packages
;; Now that all the important packages have been loaded, we load
;; everything else in alphabetical order.

;; anzu shows how many matches in isearch.
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

(use-package cmake-mode
  :mode
  "CMakeLists.txt"
  "\\.cmake\\'")

;; diff-hl uses the emacs vcs integration to display
;; added/modified/removed lines.
(use-package diff-hl
  :config
  (add-hook 'after-make-console-frame-hooks
            (lambda ()
              (global-diff-hl-mode 0)
              (diff-hl-margin-mode 1)))
  (add-hook 'after-make-window-system-frame-hooks
            (lambda ()
              (global-diff-hl-mode 1)
              (diff-hl-margin-mode 0))))

(use-package dockerfile-mode
  :mode "Dockerfile\(-.*\)?")

;; editorconfig is a simple way to share indentation settings between
;; editors. Because I sometimes dabble in neovim and sublime, it's
;; nice to not have to re-do these settings at a project level between
;; editors.
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Grab important environment variables from the shell. The important
;; ones are PATH and GOPATH.
(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize))

;; fic-mode simply gives an annoying highlight to FIXME, TODO, XXX,
;; and other similar keywords so they're easy to spot.
(use-package fic-mode
  :diminish fic-mode
  :config
  (add-to-list 'fic-highlighted-words "XXX")
  (add-hook 'prog-mode-hook 'fic-mode))

;; flyspell does what it says on the tin. It's a spell-checker similar to flycheck.
(use-package flyspell
  :diminish flyspell-mode
  :config (add-hook 'text-mode-hook (lambda () (flyspell-mode 1))))

(use-package go-mode
  :mode "\\.go\\'"
  :bind
  (:map go-mode-map
        ("M-." . go-guru-definition)
        ("C-c o" . go-guru-map))
  :init
  (load "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el")
  :config
  (use-package company-go
    :if (fboundp 'company-mode)
    :config
    (add-to-list 'company-backends 'company-go))

  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports"))

;; ido (interactively-do) is a better interface for selecting things.
(use-package ido
  :config
  ;; smex is a better replacement for M-x built around ido.
  (use-package smex
    :bind
    ("M-x" . smex)
    ("M-X" . smex-major-mode-commands))

  ;; Use ido everywhere possible.
  (use-package ido-ubiquitous
    :config
    (ido-ubiquitous-mode 1))

  ;; ido is much more readable when all the options are displayed
  ;; vertically.
  (use-package ido-vertical-mode
    :config
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right
          ido-vertical-show-count t)
    (ido-vertical-mode 1))

  ;; flx-ido changes the matching algorithm to improve the flex
  ;; matching support.
  (use-package flx-ido
    :config
    (setq ido-enable-flex-matching t
          flx-ido-threshold 1000))

  (setq resize-mini-windows t
        ido-use-virtual-buffers t
        ido-auto-merge-work-directories-length -1)

  (ido-mode 1)
  (ido-everywhere 1))

(use-package hlinum
  :config
  (hlinum-activate))

(use-package irony
  :config
  (use-package company-irony
    :if (fboundp 'company-mode)
    :config
    (add-to-list 'company-backends 'company-irony))

  (use-package flycheck-irony
    :if (fboundp 'flycheck-mode)
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))

;; js-mode isn't used as a separate mode, but we use it as a container
;; here since it's a nice place to drop all our javascript-related
;; packages.
(use-package js
  :ensure nil
  :config
  ;; js2-mode is a wrapper around js-mode which cleans it up and adds a
  ;; bunch of features.
  (use-package js2-mode
    :mode "\\.js\\'"
    :config
    (setq js2-basic-offset 2)
    (when (fboundp 'flycheck-mode)
      (set-face-attribute 'js2-error nil
                          :inherit 'flycheck-error-list-error
                          :underline '(:color foreground-color :style wave))
      (set-face-attribute 'js2-warning nil
                          :inherit 'flycheck-error-list-warning
                          :underline '(:color foreground-color :style wave))))

  ;; tern is a js navigation package which extends js-mode. TODO: Note
  ;; that this is fairly hard to find, so it may be better to move
  ;; this under a js-mode block.
  (use-package tern
    :diminish tern-mode
    :config
    (use-package company-tern
      :if (fboundp 'company-mode)
      :config
      (add-to-list 'company-backends 'company-tern)
      (setq company-tern-property-marker ""))

    (add-hook 'js-mode-hook (lambda () (tern-mode t)))))

(use-package less-css-mode
  :mode "\\.less\\'")

;; magit is an amazing tool for working with git inside emacs.
(use-package magit
  :bind ("M-g M-g" . magit-status)
  :init
  (use-package magit-filenotify
    :if (linux-p)
    :config
    (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))
  :config
  (setq magit-push-always-verify t
        magit-completing-read-function 'magit-ido-completing-read))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

;; org-mode can be used for tasks, notes, and a variety of other
;; things.
(use-package org
  :mode ("\\.org\'" . org-mode)
  :config
  (setq org-completion-use-ido t
        org-support-shift-select t
        org-agenda-files '("~/org/")))

(use-package paradox
  :commands
  paradox-list-packages)

;; persistent-scratch makes it possible to use the scratch buffer
;; without worrying about losing it.
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

;; python-mode isn't used as a separate mode, but we use it as a
;; container here since it's a nice place to drop all our
;; python-related packages.
(use-package python
  :ensure nil
  :config
  ;; anaconda mode provides code navigation and docs. Additionally, if
  ;; company-mode is enabled, company-anaconda will also be
  ;; enabled.
  (use-package anaconda-mode
    :diminish anaconda-mode
    :config
    (use-package company-anaconda
      :if (fboundp 'company-mode)
      :config (add-to-list 'company-backends 'company-anaconda))

    (add-hook 'python-mode-hook 'anaconda-mode))

  (use-package pip-requirements
    :mode
    "requirements.txt"
    "requirements/\\.txt\\'")

  (use-package virtualenvwrapper
    :config
    (when (fboundp 'projectile-mode)
      (advice-add 'switch-to-buffer :after
                  (lambda (&rest arg-list)
                    (if (and (projectile-project-p)
                             (venv-is-valid (projectile-project-name)))
                        (venv-workon (projectile-project-name))))))))

(use-package rainbow-mode
  :commands rainbow-mode)

;; recentf adds some useful functionality to ido which remembers
;; previously opened files.
(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1))

;; Save the last location when you leave a file.
(use-package saveplace
  :ensure nil
  :config
  (setq-default save-place t))

;; smart-mode-line is a package which aims to provide a better
;; mode-line with little configuration. I've tried to use powerline
;; (and making my own small framework) and it just involved too much
;; work to maintain a small feature.
(use-package smart-mode-line
  :disabled t
  :config
  (setq sml/no-confirm-load-theme t
        sml/shorten-directory t
        sml/theme 'respectful)
  (sml/setup))

;; In spite of the name, I use this to make sure that when I scroll,
;; there are still lines between the cursor and the top of the file.
(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 5
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        scroll-margin 1
        scroll-step 1
        mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)
  (smooth-scrolling-mode 1))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'bar)
  (spaceline-emacs-theme))

;; undo-tree makes the undo features a bit more bearable.
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;; Ensure we're using sane buffer naming
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package web-mode
  :mode
  "\\.html\\'"
  "\\.jinja\\'"
  "\\.mustache\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package yaml-mode
  :mode "\\.yml\\'")

;; yasnippet adds some useful tools to make reusable code snippets.
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-verbosity 0)
  (yas-global-mode 1))

;;;; Tweaks
;; TODO: This section is pretty much everything that didn't fit nicely
;; into a use-package block. It would be nice to refactor this a bit.

;; We pick a super generic fallback so it should work everywhere.
(defvar belak/frame-font "Monospace 12")
(cond ((linux-p)
       ;; On linux, we just fall back to the default "monospace" font
       ;; because we can set it the same everywhere.
       (setq belak/frame-font nil
             x-gtk-use-system-tooltips nil))
      ((osx-p)
       (setq belak/frame-font "Source Code Pro Light 10")))

;; We want to ensure the font is set after the window frame is
;; created.
(add-hook 'after-make-window-system-frame-hooks
          (lambda () (when belak/frame-font (set-frame-font belak/frame-font))))

;; Remove most gui features because I rarely use any of them.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq use-dialog-box nil
      use-file-dialog nil)

;; Various appearance settings
(setq column-number-mode t
      line-number-mode t
      tooltip-delay 0
      tooltip-short-delay 0)

(setq-default tab-width 4)

;; I find that when I want to use zap, I almost never want to include
;; the next character, so we replace zap-to-chat with zap-up-to-char.
(autoload 'zap-up-to-char "misc")
(global-set-key [remap zap-to-char] 'zap-up-to-char)

(defvar save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; Make sure we only have to type 'y' or 'n', not the full word
;; because that takes too many keystrokes.
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode)

(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis
        show-paren-delay 0))

(setq lazy-highlight-initial-delay 0
      make-pointer-invisible t
      vc-follow-symlinks t
      require-final-newline t
      load-prefer-newer t
      inhibit-splash-screen t)

;; As a former vim user, I like escape to actually quit
;; everywhere. This was taken from
;; https://github.com/davvil/.emacs.d/blob/master/init.el
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just
deactivate it; then it takes a second \\[keyboard-quit] to
abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Ensure all trailing whitespace is removed
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight between matching parens
(electric-pair-mode 1)

;; Show which function we're in
;;(which-function-mode 1)

;; Delete text if we start typing
(delete-selection-mode)

;; This is a common hook for all modes that are based on the generally
;; programming mode.
(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode 1)
            (setq show-trailing-whitespace t)))

;; Revert buffers automatically if they've changed on disk
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(diminish 'auto-revert-mode)

;;;; Custom
;; We still want to be able to have non-public configs, such as for
;; passwords and what not, so we put them in a separate file and load
;; it, but ignore errors, for instance if it doesn't exist. This has
;; the added advantage of making it so customizations will go to this
;; file and not to init.el, which is version controlled.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;;; Cleanup

;; Now that we're done loading everything, print how long it took.
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;; Set these variables back to normal
(setq debug-on-error nil
      debug-on-quit nil)

;;; init.el ends here
