;;; belak-ui.el --- appearance settings -*- lexical-binding: t; -*-

(require 'belak-core)

;;
;;; Themes

;; I maintain and try out a lot of themes. The code to load them is pretty much
;; the same, so we throw that config in a macro to make it easier.

(defmacro load-theme! (name &optional package)
  (let ((package-name (if package package (intern (format "%s-theme" name)))))
    `(use-package ,package-name
       :config
       (add-transient-hook! window-setup-hook (load-theme ',name t)))))

;;(load-theme! grayscale)                 ; A simple mostly grayscale theme
;;(load-theme! monokai-pro)               ; Based on the VSCode/Sublime themes
(load-theme! modus-vivendi)             ; A very accessible theme
;;(load-theme! nord)                      ; Trying this one out
;;(load-theme! zenburn)                   ; Oldie but a goodie

;;(load-theme!                            ; One set of themes I maintain, so I try
;; base16-default-dark                    ; to keep this around even when I'm not
;; base16-theme)                          ; using it.


;;
;;; Packages

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package hl-line
  :straight nil
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

(use-package display-line-numbers
  :straight nil
  :hook (prog-mode . display-line-numbers-mode)
  :hook (text-mode . display-line-numbers-mode)
  :hook (conf-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'visual
                display-line-numbers-width 3
                display-line-numbers-widen t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; Make sure the line and column numbers are in the modeline.
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode 1)

  ;; HACK: These two settings need to be set in order to remove what looks like
  ;; padding from the modeline.
  (setq doom-modeline-icon nil
        doom-modeline-height 0))

(use-package shackle
  :config
  (setq shackle-rules
      '(("*Help*" :align t :select t)
        (("\\`\\*magit-diff: .*?\\'") :regexp t :noselect t)
        ((inferior-scheme-mode "*shell*" "*eshell*") :popup t))
       shackle-default-rule '(:select t)
       shackle-default-size 0.4
       shackle-inhibit-window-quit-on-same-windows t))

;; undo/redo changes to Emacs' window layout
(use-package winner
  ;;:after-call after-find-file doom-switch-window-hook
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :config
  (appendq! winner-boring-buffers
            '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
              "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
              "*Buffer List*" "*Ibuffer*" "*esh command on file*"))
  (winner-mode +1))

;; TODO: doom uses some hacks to approximate this, potentially faster.
;; TODO: prelude has a nice way of optionally enabling `whitespace' for certain modes
(use-package whitespace
  :delight global-whitespace-mode
  :straight nil
  :config
  (setq whitespace-style '(trailing face tabs tab-mark lines-tail)
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [182 10])
                                      (tab-mark 9 [9655 9] [92 9])))
  (global-whitespace-mode t)
  (setq whitespace-global-modes '(text-mode prog-mode org-mode)))

;; Improve usability by showing key binds when we stop typing for long enough.
(use-package which-key
  :defer 1
  :delight
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)

  (which-key-setup-side-window-bottom)

  (which-key-mode 1))


;;
;;; Tweaks

;; Cleanup clutter in the fringes
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)
(delq! 'continuation fringe-indicator-alist 'assq)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Make tooltips show up faster.
(setq tooltip-delay 0
      tooltip-short-delay 0)

;; Set the frame title to something more useful - we're pretty much always on
;; the same host so that doesn't matter as much.
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Because we use paren, this isn't as relevant and is actually more
;; distracting.
(setq blink-matching-paren nil)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Disable graphical pop-ups. Most libraries have alternatives for
;; this.
(setq use-dialog-box nil)

;; The behavior here isn't very clear, so we disable it. The cursor is plenty
;; clear in the terminal, so we don't need to have emacs make it more visible.
(setq visible-cursor nil)

;; no beeping or blinking please
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Ensure the help window is selected when one is open. This makes it
;; much easier to quit them when we're done.
(setq help-window-select t)

;; Make Emacs split windows in a more sane way.
(setq window-combination-resize t)

;; Make resizing the window much more plesant when using a gui.
(setq frame-resize-pixelwise t)

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)


;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Make buffers match the unix path style of forward slashes, properly refresh
;; after a buffer has been killed, and ignore special buffers.
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Ensure we show trailing whitespace in modes we care about. This includes
;; everything derived from `prog-mode' or `text-mode'. We unfortunately can't
;; just use `setq-default' because that includes buffers like `ido'.
(add-hook 'prog-mode-hook (setq show-trailing-whitespace t))
(add-hook 'text-mode-hook (setq show-trailing-whitespace t))


;;
;;; Scrolling Tweaks

(setq hscroll-margin 2
      hscroll-step 1

      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 2

      ;; NOTE: optimally, this would be set to true, but it seems to cause
      ;; issues with performance and cursor jumping when scrolling.
      scroll-preserve-screen-position nil

      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil

      ;;scroll-up-aggressively 0.01
      ;;scroll-down-aggressively 0.01

      ;; mouse
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

(provide 'belak-ui)
;;; belak-ui.el ends here.
