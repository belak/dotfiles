;;; belak-core.el -*- lexical-binding: t; -*-

;;
;;; Package management

(custom-set-variables '(straight-cache-autoloads t)
                      '(straight-check-for-modifications '(check-on-save find-when-checking))
                      '(straight-use-package-by-default t)
                      '(straight-vc-git-default-clone-depth 1))

;; This is the official bootstrap code from the straight.el repo. In addition,
;; we use this to make sure `use-package', and `blackout' are installed so we
;; can use them with the rest of our configuration.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

;; Even though these packages are installed here, we actually require
;; them in `belak-lib' to make it easier to provide to other modules.
(straight-use-package 'use-package)
(straight-use-package 'blackout)

(require 'belak-lib)

;; By default, we want `use-package' to only load packages when explicitly
;; called on. This makes it easier to lazy-load packages.
(setq use-package-always-defer t)


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

(use-package! no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


;;
;;; Packages

;; Remember files we were recently in. We also clean up `recentf' when Emacs
;; quits, so it should be only for the existing session.
(use-feature! recentf
  :demand t
  :after no-littering
  :hook (kill-emacs . recentf-cleanup)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode +1))


;;
;;; Optimizations

;; `gcmh-mode' is a long standing hack which tweaks the garbage collection to be
;; more performant in normal scenarios. We also add a hook to focus-out so Emacs
;; can GC in the background.
(use-package! gcmh
  :blackout
  :commands gcmh-mode
  :hook (focus-out  . gcmh-idle-garbage-collect)
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold (* 100 1024 1024))) ; 100MB

;; So Long mitigates slowness due to extremely long lines. Currently available
;; in Emacs master branch only, so we fall back to the upstream.
(unless (fboundp 'global-so-long-mode)
  (use-package! so-long
    :commands global-so-long-mode
    :straight (:repo "https://git.savannah.gnu.org/git/so-long.git")))
(add-transient-hook! pre-command-hook (global-so-long-mode +1))

;; Disable bidirectional text rendering for a performance. This unfortunately
;; disables support for left-to-right languages, but for right-to-left, it's a
;; performance win.
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Faster scrolling over unfontified regions. This may provide
;; inaccurate fontification while scrolling.
(setq fast-but-imprecise-scrolling t)


;;
;;; System Variables

;; Because we use a number of programs that are installed at the user level in
;; some instances (`rustup', `pyenv', `rbenv', etc) we need to make sure we load
;; the path changes from the shell environment. We only need to do this when in
;; a GUI on macOS and Linux because otherwise we should inherit the correct
;; environment.
;;
;; Note that we do this first just in case any other packages need values from
;; here.

(use-package! exec-path-from-shell
  :demand t
  :if (and IS-GUI (or IS-MAC IS-LINUX))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))


;;
;;; Tweaks

;; This "fixes" any customizations we make so they don't polute the init.el. It
;; allows usage of the customization interface if there's ever a need.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; UTF-8 as the default encoding
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Resolve symlinks when opening files
(setq find-file-visit-truename t
      vc-follow-symlinks t)

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
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(if IS-LINUX
    (setq browse-url-browser-function 'browse-url-xdg-open)
  (setq browse-url-browser-function 'browse-url-generic))

(provide 'belak-core)
;;; belak-core.el ends here.
