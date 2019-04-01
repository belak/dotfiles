;;; belak-ui --- UI related settings and packages

;;; Commentary:

;;; Code:

;; Remove most gui features because I rarely use any of them.

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(blink-cursor-mode -1)

(setq echo-keystrokes 0.1)

;; Various appearance settings

(setq column-number-mode t
      line-number-mode t
      tooltip-delay 0
      tooltip-short-delay 0)

;; Ensure the help window is selected when one is open. This makes it
;; much easier to quit them when we're done.

(setq help-window-select t)

;; Highlight between matching parens

(electric-pair-mode 1)

;; Highlight the current column

(global-hl-line-mode)

;; Packages

(use-package base16-theme
  :disabled t
  :demand
  :ensure nil
  :load-path "site-lisp/base16-theme"
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/base16-theme/build")
  :config (load-theme 'base16-default-dark t))

(use-package monokai-pro-theme
  :demand
  :ensure nil
  :load-path "site-lisp/monokai-pro-theme"
  :config (load-theme 'monokai-pro t))

;; I originally used fic-mode, but it appears that hl-todo is a little
;; better and is updated more frequently.

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package hlinum
  :config
  (hlinum-activate))

(use-package paren
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis
        show-paren-delay 0))

;; In spite of the name, I use this to make sure that when I scroll,
;; there are still lines between the cursor and the top of the file.

(use-package smooth-scrolling
  :disabled t
  :config
  (setq smooth-scroll-margin 5
        ;; scroll-conservatively 101
        ;; scroll-preserve-screen-position t
        ;; auto-window-vscroll nil
        ;; scroll-margin 1
        ;; scroll-step 1
        ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
        ;; mouse-wheel-progressive-speed t
        ;; mouse-wheel-follow-mouse t
        )
  (smooth-scrolling-mode 1))

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-default-separator 'bar
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  (which-key-mode 1))

(use-package whitespace
  :ensure nil
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(trailing face tabs tab-mark lines-tail)
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [182 10])
                                      (tab-mark 9 [9655 9] [92 9])))
  (global-whitespace-mode t)
  (setq whitespace-global-modes '(text-mode prog-mode org-mode)))

;; Additional modes to diminish
(diminish 'auto-fill-function)
(use-package undo-tree
  :ensure nil
  :diminish undo-tree-mode)

(provide 'belak-ui)

;;; belak-ui.el ends here
