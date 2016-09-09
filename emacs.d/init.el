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
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")))

;; Disable as many warnings as we can for package installs.
(setq byte-compile-warnings nil)

;; Make sure we load what we need.
(setq package-enable-at-startup nil)
(package-initialize nil)

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

;; Increasing the minimum prime bits size to something larger than the
;; default settings stops all the GnuTLS warnings from showing
;; up. This might not be the right place, but it needs to happen
;; before we install packages.
(setq gnutls-min-prime-bits 4096)

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

;; We load the theme as early as humanly possible so we're not waiting
;; for other packages to load before fixing the colors.
(defvar belak/base16-colors nil)
(use-package base16-theme
  :ensure nil
  :load-path "site-lisp/base16-theme"
  :config
  (load-theme 'base16-default-dark t)
  (setq belak/base16-colors base16-default-dark-colors))

;; Load all important custom libraries
(use-package init-frame-hooks
  :ensure nil
  :load-path "lisp/")

;; Install general for easier key-binds. This needs to be done early
;; so other use-package blocks can use it.
(use-package general)

;; company-mode is used as a completion system. In use-package blocks,
;; you can use :if (fboundp 'company-mode) to only enable a block if
;; company-mode is active.
(use-package company
  :diminish company-mode
  :config
  (setq company-tooltip-limit 20
        company-idle-delay 0
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

;;;; evil-mode
;; Getting vim bindings early is sort of important because of how many
;; packages actually use it.
(use-package evil
  :demand
  :general
  (:keymaps 'evil-insert-state-map
   "C-e" 'evil-append-line
   "C-a" 'evil-insert-line)
  (:keymaps 'evil-normal-state-map
   "C-e" 'evil-append-line
   "C-a" 'evil-insert-line)
  (:keymaps 'evil-motion-state-map
   "C-e" 'evil-append-line
   "C-a" 'evil-insert-line)
  (:keymaps 'evil-visual-state-map
   "C-e" 'evil-end-of-line
   "C-a" 'evil-beginning-of-line)
  :config
  ;; This is a port of tpope's vim-surround which adds text objects
  ;; for surrounding characters.
  (use-package evil-surround
    :disabled t
    :config
    (global-evil-surround-mode 1))

  (evil-mode 1)
  (setq evil-echo-state nil
        evil-vsplit-window-right t
        evil-split-window-below t)

  ;; Set the cursor color based on the evil state
  (setq evil-emacs-state-cursor   `(,(plist-get belak/base16-colors :base0D) box)
        evil-insert-state-cursor  `(,(plist-get belak/base16-colors :base0D) bar)
        evil-motion-state-cursor  `(,(plist-get belak/base16-colors :base0E) box)
        evil-normal-state-cursor  `(,(plist-get belak/base16-colors :base0B) box)
        evil-replace-state-cursor `(,(plist-get belak/base16-colors :base08) bar)
        evil-visual-state-cursor  `(,(plist-get belak/base16-colors :base09) box))

  ;; For the operator state, the only thing we want to change is the
  ;; size. We can keep the same color.
  (setq evil-operator-state-cursor 'evil-half-cursor))

;; anzu shows how many matches in isearch. This is placed up here
;; because it needs to be loaded before spaceline.
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

;; spaceline is a better modeline with simple config. It's up here
;; because it needs to be loaded before persistent-scratch.
(use-package spaceline
  :demand
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'bar
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

;;;; Packages
;; Now that all the important packages have been loaded, we load
;; everything else in alphabetical order.

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
  :general
  (:keymaps 'go-mode-map
   "M-."   'go-guru-definition
   "C-c o" 'go-guru-map)
  :init
  (load "$GOPATH/src/golang.org/x/tools/cmd/guru/go-guru.el")
  :config
  (use-package company-go
    :if (fboundp 'company-mode)
    :config
    (setq company-go-show-annotation t)
    (add-to-list 'company-backends 'company-go))

  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports"))

;; ido (interactively-do) is a better interface for selecting things.
(use-package ido
  :config
  ;; smex is a better replacement for M-x built around ido.
  (use-package smex
    :general
    ("M-x" 'smex)
    ("M-X" 'smex-major-mode-commands)
    :config
    (setq smex-history-length 50))

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

;; ivy is an alternative to ido which comes with a number of fancy
;; features, but it's disabled because I haven't found the time to fix
;; my config just yet.
(use-package ivy
  :disabled t
  :demand t
  :diminish ivy-mode
  :general
  ("C-c C-r" 'ivy-resume)
  :config
  ;; swiper is a replacement for isearch which uses ivy.
  (use-package swiper
    :general
    ("C-s" 'swiper))

  ;; counsel is a bunch of functions which replace builtins so they'll
  ;; work much better with ivy.
  (use-package counsel
    :general
    ("M-x"     'counsel-M-x)
    ("C-x C-f" 'counsel-find-file))

  (setq projectile-completion-system 'ivy
        magit-completing-read-function 'ivy-completing-read
        ivy-use-virtual-buffers t)

  ;;(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

  (ivy-mode 1))

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

  ;; function decides whether .h file is C or C++ header, sets C++ by
  ;; default because there's more chance of there being a .h without a
  ;; .cc than a .h without a .c (ie. for C++ template files)
  ;;
  ;; This comes from
  ;; http://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode
  (defun c-c++-header ()
    "sets either c-mode or c++-mode, whichever is appropriate for
header"
    (interactive)
    (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
      (if (file-exists-p c-file)
          (c-mode)
        (c++-mode))))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

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

  ;; tern is a js navigation package which extends js-mode.
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

(use-package macrostep
  :general
  (:keymaps 'emacs-lisp-mode-map
   "C-x e" 'macrostep-expand))

;; magit is an amazing tool for working with git inside emacs.
(use-package magit
  :general
  ("M-g M-g" 'magit-status)
  :init
  (use-package magit-filenotify
    :if (linux-p)
    :config
    (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))
  :config
  (setq magit-push-current-set-remote-if-missing t
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
  paradox-list-packages
  :config
  ;; Paradox is much more useful in emacs mode than evil mode because
  ;; it rebinds so many things.
  (add-to-list 'evil-emacs-state-modes 'paradox-menu-mode))

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

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(use-package rainbow-mode
  :commands rainbow-mode)

;; recentf adds some useful functionality to ido which remembers
;; previously opened files.
(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 50)
  (recentf-mode 1))

;; Save the last location when you leave a file.
(use-package saveplace
  :ensure nil
  :config
  (setq-default save-place t))

(use-package simple-mpc
  :general
  ("C-c m" 'simple-mpc))


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
  "\\.erb\\'"
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
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

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

(setq history-length 50)

;; Middle clicking should paste, but not adjust point and paste at the
;; then adjusted point.
(setq mouse-yank-at-point t)

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

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Show modifier combinations almost immediately.
(setq echo-keystrokes 0.1)

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
