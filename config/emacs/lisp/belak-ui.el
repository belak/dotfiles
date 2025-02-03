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

;; TODO: (set-face-font 'variable-pitch "")
(cond
 (IS-MAC
  (set-face-font 'default        "Monaco")
  (set-face-font 'fixed-pitch    "Monaco"))
 (IS-LINUX
  (set-face-font 'default        "Terminus 12")
  (set-face-font 'fixed-pitch    "Terminus 12")))


;;
;;; Packages

;; Make it clearer which window you're switching to when using C-x o.
(use-package ace-window
  :defer nil
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
          (cursor-in-non-selected-windows nil))
      (apply orig-fun args)))

  (advice-add 'ace-window :around #'belak--ace-window))

;; Dim the non-active window to make it a little easier to focus on the
;; currently active window.
(use-package dimmer
  :defer nil
  :custom (dimmer-fraction 0.2)
  :hook (after-init . dimmer-mode)
  :config
  ;; Modify the dimmer-config-change-handler to only call dimmer-process-all if
  ;; no dimming predicates are truthy. This fixes an issue with corfu. Based on
  ;; code from https://github.com/gonewest818/dimmer.el/issues/62.
  (defun belak--advise-dimmer-config-change-handler ()
    "Advise to only force process if no predicate is truthy."
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (unless ignore
        (when (fboundp 'dimmer-process-all)
          (dimmer-process-all t)))))

  (advice-add 'dimmer-config-change-handler :override 'belak--advise-dimmer-config-change-handler))

;; We want line numbers to make it easier when using prefix commands.
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :hook (text-mode . display-line-numbers-mode)
  :hook (conf-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative
                display-line-numbers-width 3
                display-line-numbers-widen t))

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

;; This package is required by `dashboard', but we don't want it to display in
;; the minor-modes display so we hide it.
(use-package page-break-lines :blackout)

;; We want to make it easier to tame random windows and popups that show up.
;; Most of the configuration for this happens in other packages and will call
;; `add-shackle-rule!'.
(use-package shackle
  :hook (pre-command . shackle-mode)
  :preface
  (defmacro add-shackle-rule! (rule)
    ;; NOTE: this is much easier as a macro because otherwise, expanding `rule'
    ;; to the inner scope isn't possible.
    `(after! shackle
       (appendq! shackle-rules (list ,rule))))
  :config
  (setq shackle-rules
        '(("*Help*" :align t :select t)
          ("*Warnings*" :popup t)
          (("*shell*" "*eshell*") :popup t))
        shackle-default-rule '(:select t)
        shackle-default-size 0.4
        shackle-inhibit-window-quit-on-same-windows t))

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

;; TODO: doom uses some hacks to approximate this, potentially faster.
;; TODO: prelude has a nice way of optionally enabling `whitespace' for certain modes
;; TODO: is there a way to only display things like spaces/tabs when selected?
(use-package whitespace
  :blackout global-whitespace-mode
  :demand t
  :config
  (setq whitespace-style '(face trailing tabs tab-mark lines-tail missing-newline-at-eof)
        whitespace-display-mappings '((space-mark   32 [183])
                                      (newline-mark 10 [36 10])
                                      (tab-mark      9 [62 9]))
        whitespace-global-modes '(text-mode prog-mode org-mode))

  (global-whitespace-mode t))

;; undo/redo changes to Emacs' window layout
(use-package winner
  :demand t
  :preface
  (defmacro add-winner-boring-buffer! (boring-buffer-name)
    `(after! winner
       (appendq! winner-boring-buffers (list ,boring-buffer-name))))
  :config
  (setq winner-boring-buffers
        '("*Apropos*"
          "*Buffer List*"
          "*Compile-Log*"
          "*Completions*"
          "*Fuzzy Completions*"
          "*Help*"
          "*Ibuffer*"
          "*cvs*"
          "*esh command on file*"
          "*inferior-lisp*"))
  (winner-mode +1))


;;
;;; Completing-Read

;; A nice, no-nonsense completing-read replacement. I switched to this over
;; `selectrum' because it seems to be slightly better designed and I switched
;; from `ido-mode' (along with `ido-vertical-mode', `flx-ido', `smex', `anzu'
;; and more) because I get roughly the same features with much less
;; configuration.
(use-package vertico
  :hook (after-init . vertico-mode))

(use-package consult
  :bind (([remap goto-line]        . consult-goto-line)
         ([remap switch-to-buffer] . consult-buffer)))

;; Orderless lets us tweak the completion sorting/filtering with nausiating
;; detail.
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

;; This makes completing-read frameworks work more like helm with useful columns
;; of information, but with way less configuration.
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))


;;
;;; Tweaks

;; Shrink the fringe to 4px.
(set-fringe-mode 4)

;; Don't highlight or display the cursor in non-selected windows. This has the
;; added advantage of being a slight performance boost.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

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

;; Disable graphical pop-ups. Most libraries have alternatives for this.
(setq use-dialog-box nil)

;; The behavior here isn't very clear, so we disable it. The cursor is plenty
;; clear in the terminal, so we don't need to have emacs make it more visible.
(setq visible-cursor nil)

;; no beeping or blinking please
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Ensure the help window is selected when one is open. This makes it much
;; easier to quit them when we're done.
(setq help-window-select t)

;; Make Emacs split windows in a more sane way.
(setq window-combination-resize t)

;; Make resizing the window much more plesant when using a GUI.
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

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

;; Don't tell me about keybindings when I run a command with M-x. If I run a
;; command enough, I'll look up or make a keybind.
;;(setq suggest-key-bindings nil)


;;
;;; Disable certain error messages and behaviors.

(defun belak-command-error-function (data context caller)
  "Ignore the beginning-of-buffer and end-of-buffer signals.

Pass the rest to the default handler."
  (let ((sig (car data)))
    (cond
     ;; NOTE: there is an issue with this if you are scrolling in non-selected
     ;; window. When you hit the top, it will scroll the selected window rather
     ;; than the window you're scrolling in.
     ((eq sig 'beginning-of-buffer)
      (goto-char (point-min)))
     ((eq sig 'end-of-buffer)
      (goto-char (point-max)))
     (t
      (command-error-default-function data context caller)))))

(setq command-error-function #'belak-command-error-function)


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
