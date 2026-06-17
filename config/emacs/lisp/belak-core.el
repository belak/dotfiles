;;; belak-core.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Early settings

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
;;; Packages

;; Remember files we were recently in. We also clean up `recentf' when Emacs
;; quits, so it should be only for the existing session.
(use-package recentf
  :demand t
  :after no-littering
  ;; For some reason this hook causes issues with exiting, so it's disabled for
  ;; now until I can find time to debug it.
  ;;
  ;;:hook (kill-emacs . recentf-cleanup)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (let ((inhibit-message t))
    (recentf-mode +1)))


;;
;;; Environment

;; GUI Emacs doesn't inherit the shell PATH, so Nix-installed tools like direnv
;; (required by envrc) won't be found without this.
(dolist (path (list (concat "/etc/profiles/per-user/" (user-login-name) "/bin")
                    "/run/current-system/sw/bin"
                    (expand-file-name "~/.nix-profile/bin")))
  (when (file-directory-p path)
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat path ":" (getenv "PATH")))))


;;
;;; Optimizations

;; So Long mitigates slowness due to extremely long lines.
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
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(setq vc-follow-symlinks t)

;; Increase the amount of data which Emacs reads from the process. The default
;; of 4k is too low 4k considering that the some of the language server
;; responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024))

;; Disable the startup screen and messages - more often than not we really just
;; want to get into a file.
(setq inhibit-startup-message t
      inhibit-default-init t)

;; Emacs scans the init file for a literal (setq inhibit-startup-echo-area-message
;; "username") string, which isn't portable. Setting the `saved-value' property
;; tricks it into thinking the customization interface was used instead.
;;
;; See: https://yrh.dev/blog/rant-obfuscation-in-emacs/
(put 'inhibit-startup-echo-area-message 'saved-value
     (setq inhibit-startup-echo-area-message (user-login-name)))

;; Leave the scratch buffer blank except for a random quote. We used to switch
;; to `emacs-lisp-mode', but then all the `prog-mode' deferred setup functions
;; are run, so we stick to `fundamental-mode'.
(defvar belak--scratch-quotes
  '(;; Douglas Adams
    "I love deadlines. I love the whooshing noise they make as they go by."
    "Don't panic."
    "A common mistake that people make when trying to design something completely foolproof is to underestimate the ingenuity of complete fools."
    "The ships hung in the sky in much the same way that bricks don't."
    "Would it save you a lot of time if I just gave up and went mad now?"
    ;; Terry Pratchett
    "Real stupidity beats artificial intelligence every time."
    ;; Robert Asprin
    "Just because something doesn't do what you planned it to do doesn't mean it's useless.")
  "Quotes to display in the scratch buffer.")

(setq initial-scratch-message
      (concat (replace-regexp-in-string "^" ";; " (nth (random (length belak--scratch-quotes)) belak--scratch-quotes))
              "\n\n")
      initial-major-mode 'lisp-interaction-mode)


;;; Make M-z zap-up-to-char (doesn't include char) rather than
;; zap-to-char and make M-Z zap in reverse.
(global-set-key [remap zap-to-char] 'zap-up-to-char)

(defun reverse-zap-up-to-char (char)
  "Zap back to CHAR."
  (interactive "Zap back to char: ")
  (zap-up-to-char -1 char))
(global-set-key "\M-Z" 'reverse-zap-up-to-char)

;; Typing yes/no is obnoxious when y/n will do
(setopt use-short-answers t)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; If we're on linux, force using xdg-open for urls. On macOS, leave the
;; default (browse-url-default-macosx-browser) in place.
(when IS-LINUX
  (setq browse-url-browser-function 'browse-url-xdg-open))


(provide 'belak-core)
;;; belak-core.el ends here.
