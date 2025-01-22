;;; early-core.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; `use-package' Tweaks

;; By default, we want `use-package' to only load packages when explicitly
;; called on. This makes it easier to lazy-load packages.
(setq use-package-always-defer t)

;; Some debugging toggles, used for diagnosing startup and startup speed.
(setq use-package-verbose nil
      use-package-minimum-reported-time 0.001)


;;
;;; No-Littering

;; We want to make sure we avoid dumping a bunch of additional files in our
;; emacs directory, so we install to a hidden dir in our emacs directory.
;;
;; Note that the only thing that should be before this is any required setup to
;; install this and prepare. We want to make sure these values are used for
;; every package we load if possible.
;;
;; This configuration used to try and put the `etc' and `var' directories inside
;; a hidden directory, but if the Emacs config ever failed to load, we were left
;; with these directories anyway, so it's not worth the trouble.

(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups))


;;
;;; Optimizations

;; `gcmh-mode' is a long standing hack which tweaks the garbage collection to be
;; more performant in normal scenarios. We also add a hook to focus-out so Emacs
;; can GC in the background.
(use-package gcmh
  :blackout
  :hook (focus-out  . gcmh-idle-garbage-collect)
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold (* 100 1024 1024))) ; 100MB

(use-package so-long
  :hook (after-init . global-so-long-mode))

;; Disable bidirectional text rendering for a performance. This unfortunately
;; disables support for left-to-right languages, but for right-to-left, it's a
;; performance win.
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Faster scrolling over unfontified regions. This may provide
;; inaccurate fontification while scrolling.
(setq fast-but-imprecise-scrolling t)


;;
;;; Tweaks

;; This "fixes" any customizations we make so they don't polute the init.el. It
;; allows usage of the customization interface if there's ever a need.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; UTF-8 as the default encoding
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Resolve symlinks when opening files
(setq vc-follow-symlinks t)

;; Increase the amount of data which Emacs reads from the process. The default
;; of 4k is too low 4k considering that the some of the language server
;; responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024))

;; Disable the startup screen and messages - more often than not we really just
;; want to get into a file.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(fset #'display-startup-echo-area-message #'ignore)

;; Leave the scratch buffer blank. We used to switch to `emacs-lisp-mode', but
;; then all the `prog-mode' deferred setup functions are run, so we stick to
;; `fundamental-mode'.
(setq initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; It's alright if Emacs updates the UI a little less often than the
;; default of 0.5s.
(setq idle-update-delay 1)

;; Make M-z zap-up-to-char (doesn't include char) rather than
;; zap-to-char and make M-Z zap in reverse.
(autoload 'zap-up-to-char "misc" "" 'interactive)
(global-set-key [remap zap-to-char] 'zap-up-to-char)

(defun reverse-zap-up-to-char (char)
  "Zap back to CHAR."
  (interactive "Zap back to char: ")
  (zap-up-to-char -1 char))
(global-set-key "\M-Z" 'reverse-zap-up-to-char)

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; If we're on linux, force using xdg-open for urls.
(if IS-LINUX
    (setq browse-url-browser-function 'browse-url-xdg-open)
  (setq browse-url-browser-function 'browse-url-generic))

;; For some reason, default-directory seems to be / when using emacs-plus on
;; macOS. This isn't ideal, so it's overridden to the user's home directory.
;;
;; TODO: check if this is still true
;;(setq default-directory "~/")

;; Disabling the additional `use-package` highlighting makes it so the package
;; names aren't highlighted, but since we define out own similar macros, this
;; saves us from having to declare the same highlighting on those as well.
(font-lock-remove-keywords 'emacs-lisp-mode use-package-font-lock-keywords)


(provide 'belak-core)
;;; belak-core.el ends here.
