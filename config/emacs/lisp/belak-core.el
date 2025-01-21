;;; early-core.el -*- lexical-binding: t; -*-

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
(setq default-directory "~/")


;;
;;; Packages

(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups))

(provide 'belak-core)
;;; belak-core.el ends here.
