;;; belak-ui.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Themes

(use-package modus-themes
  :demand t
  :config
  ;; Italics look pretty rough in terminals (at least with the fonts I use), so
  ;; we make sure they're only enabled if we have a full GUI.
  (when IS-GUI
    (setq modus-themes-italic-constructs t))

  (setq modus-themes-lang-checkers  '(background straight-underline text-also)
        modus-themes-fringes        'subtle
        modus-themes-mode-line      '(borderless))

  (load-theme 'modus-vivendi :no-confirm-loading))


;;
;;; Fonts

(cond
 (IS-MAC
  (set-face-font 'default        "Monaco")
  (set-face-font 'fixed-pitch    "Monaco")
  (set-face-font 'variable-pitch "Monaco"))
 (IS-LINUX
  (set-face-font 'default        "Terminus 12")
  (set-face-font 'fixed-pitch    "Terminus 12")
  (set-face-font 'variable-pitch "Terminus 12")))


;;
;;; Completing Read

;; A nice, no-nonsense completing-read replacement. I switched to this over
;; `selectrum' because it seems to be slightly better designed and it provides
;; roughly the same features as `ido-mode' (along with `ido-vertical-mode',
;; `flx-ido', `smex', `anzu' and more) with much less configuration.
(use-package vertico
  :hook (after-init . vertico-mode))

;; This makes completing-read frameworks work more like helm with useful columns
;; of information, but with way less configuration.
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; Orderless lets us tweak the completion sorting/filtering with nausiating
;; detail if we really want to.
(use-package orderless
  :demand t
  :config
  ;; Enable the orderless completion style
  (setq completion-styles '(orderless basic))

  ;; There's an odd issue when using TRAMP, that causes hostname completion to
  ;; not work, so there needs to be an override for files which tries basic
  ;; first.
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))


;;
;;; Packages

;; Make it clearer which window you're switching to when using C-x o.
(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  ("M-o"   . ace-window)
  :config
  ;; Don't dim the background and use easy-to-type letters rather than numbers
  ;; for the jump keys.
  (setq aw-background nil
        aw-keys       '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  ;; Advise ace-window so no cursors display while it is active.
  (defun belak--ace-window (orig-fun &rest args)
    (let ((cursor-type nil)
          (cursor-in-non-selected-window nil))
      (apply orig-fun args)))

  (advice-add 'ace-window :around #'belak--ace-window)

  ;; Always display the window key in the modeline to make jumping easier.
  (ace-window-display-mode 1))

;; Dim the non-active window just a little bit to make it a little easier to
;; focus on the currently active window.
(use-package dimmer
  :hook (after-init . dimmer-mode)
  :custom (dimmer-fraction 0.2))


;; The `doom-modeline' package seems to match exactly what I want out of a
;; modeline. It's simple, doesn't display minor modes by default, and looks
;; pretty good.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; Make sure the line and column numbers are in the modeline.
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode 1)

  ;; HACK: These two settings need to be set in order to remove what looks like
  ;; padding from the modeline. Thankfully, `doom-modeline' will helpfully
  ;; recalculate the height when set to 0.
  (setq doom-modeline-icon nil
        doom-modeline-height 0)

  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-buffer-encoding nil
        doom-modeline-minor-modes t))

;; Highlight the current line to make the cursor easier to find.
(use-package hl-line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;; Make it so it only displays a highlighted line in the currently selected
  ;; buffer.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

(use-package display-line-numbers
  :hook (conf-mode . display-line-numbers-mode)
  :hook (prog-mode . display-line-numbers-mode)
  :hook (text-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative
                display-line-numbers-width 3
                display-line-numbers-widen t))

;; Package `transient' is the interface used by Magit to display popups.
(use-package transient
  :config
  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit))

;; Improve usability by showing key binds when we stop typing for long enough.
(use-package which-key
  :blackout
  ;; Unbind C-h C-h so our manual trigger will work properly.
  :bind ("C-h C-h" . nil)
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)

  ;; TODO: add support for `which-key-enable-extended-define-key'

  ;; Set up which-key to only display when C-h is pressed.
  ;; (setq which-key-show-early-on-C-h t
  ;;       which-key-idle-delay most-positive-fixnum
  ;;       which-key-idle-secondary-delay 1e-100)

  (setq which-key-idle-delay 0.5)

  (which-key-setup-side-window-bottom))

;;
;;; Tweaks

;; Shrink the fringe to 4px.
(set-fringe-mode 4)

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

;; Don't blink the cursor
(blink-cursor-mode -1)

;; no beeping or blinking please
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Ensure the help window is selected when one is open. This makes it much
;; easier to quit them when we're done.
(setq help-window-select t)

;; Make Emacs split windows in a bit clearer way
(setq window-combination-resize t)

;; Make resizing the window much more plesant when using a GUI.
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

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
(setq
 ;; Always keep 3 lines between the cursor and the top/bottom of the buffer
 ;; when possible. Additionally, we always want to scroll by 1.
 scroll-margin 3

 ;; Emacs spends too much effort recentering the screen if you scroll the
 ;; cursor more than N lines past window edges (where N is the settings of
 ;; `scroll-conservatively'). This is especially slow in larger files
 ;; during large-scale scrolling commands. If kept high enough, the window
 ;; is never automatically recentered.
 scroll-conservatively 100)


;;
;;; Improvements for terminal emacs

(unless IS-GUI
  (xterm-mouse-mode t))


;;
;;; Disable other themes when calling `load-theme'

(defun belak-load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' to disable enabled themes for a clean slate."
  (unless no-enable
    (belak-disable-all-themes))
  (apply f theme-id no-confirm no-enable args))

(advice-add 'load-theme :around #'belak-load-theme-advice)


(provide 'belak-ui)
;;; belak-ui.el ends here.
