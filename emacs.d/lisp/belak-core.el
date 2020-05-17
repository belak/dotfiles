;;; belak-core.el --- essential utilities -*- lexical-binding: t; -*-

;;
;;; Constants

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-GUI   (display-graphic-p))


;;
;;; Load lib

;; This includes a number of utility functions, macros, and hooks which are
;; helpful for the rest of the config. Including it as early as possible makes
;; it easy for us to ensure it has been loaded.

(require 'belak-lib)


;;
;;; Package Management

;; This is the official bootstrap code from the straight.el repo. In addition,
;; we use this to make sure `use-package', `delight' and `general' are installed
;; so we can use them with the rest of our configuration.

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
(setq straight-use-package-by-default t)

;; Install the core packages we'll need for the rest of the configuration.
(straight-use-package 'use-package)
(straight-use-package 'delight)
(straight-use-package 'general)

(eval-when-compile
  (require 'use-package))
(require 'general)
(require 'delight)


;;
;;; Optimizations

;; We want to disable as much as possible as early as possible, so we do this as
;; soon as we have our package manager loaded. There are some even
;; higher-priority optimizations in `early-init.el' as well.

;; We'd like to know how long startup took. This isn't explicitly related to
;; optimization, but it is helpful for debugging startup times.
(add-transient-hook! emacs-startup-hook
  (message "Loaded in %.03fs"
           (float-time (time-subtract (current-time) before-init-time))))

;; This is a hack to make garbage collection happen when the user is idle.
;; There's a fairly high threshold when active and a fairly low threshold when
;; idling.
;;
;; Also note that this will re-enable the GC. Early in the init process we set
;; the limit super high so a GC wouldn't be triggered. However, this enables
;; gcmh-mode after a command has been run in order to properly start the GC
;; again.
(use-package gcmh
  :delight
  :commands gcmh-mode
  :hook (focus-out . gcmh-idle-garbage-collect)
  :init
  (add-transient-hook! pre-command-hook (gcmh-mode +1))
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold (* 16 1024 1024))) ; 16MB

;; So Long mitigates slowness due to extremely long lines. Currently available
;; in Emacs master branch only, so we fall back to the upstream.
(unless (fboundp 'global-so-long-mode)
  (use-package so-long
    :commands global-so-long-mode
    :straight (:repo "https://git.savannah.gnu.org/git/so-long.git")))
(add-transient-hook! pre-command-hook (global-so-long-mode +1))

;; Disable bidirectional text rendering for a performance. This unfortunately
;; disables support for left-to-right languages, but for right-to-left, it's a
;; performance win.
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Every require/load looks at this, so removing it gets us a small
;; performance improvement. However we do want it set after loading everything,
;; so we restore it after startup.
(defvar belak--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun belak--restore-file-name-handler-alist-h ()
  "Restore the startup optimizations we previously made."
  (setq file-name-handler-alist belak--file-name-handler-alist))

(add-hook 'emacs-startup-hook #'belak--restore-file-name-handler-alist-h)

;; Faster scrolling over unfontified regions. This may provide
;; inaccurate fontification while scrolling.
(setq fast-but-imprecise-scrolling t)

;; Don't highlight or display the cursor in non-selected windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


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

(use-package exec-path-from-shell
  :if (and IS-GUI (or IS-MAC IS-LINUX))
  :config
  (exec-path-from-shell-initialize))


;;
;;; No-littering

;; We want to make sure we avoid dumping a bunch of additional files in our
;; emacs directory, so we install to a hidden dir in our emacs directory.
;;
;; Note that the only thing that should be before this is any required setup to
;; install this and prepare. We want to make sure these values are used for
;; every package we load if possible.

(use-package no-littering
  :preface
  (setq no-littering-etc-directory
	(expand-file-name ".local/etc/" user-emacs-directory))
  (setq no-littering-var-directory
	(expand-file-name ".local/var/" user-emacs-directory))
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


;;
;;; Packages

;; Revert buffers to their state on disk when they change. Note that this is a
;; tweaked version of what ships with doom-emacs to simplify a number of things.
(use-package autorevert
  ;; revert buffers when their files/state have changed
  :hook (focus-in            . belak--auto-revert-buffers-h)
  :hook (after-save          . belak--auto-revert-buffers-h)
  :hook (belak-switch-buffer . belak--auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; Instead of using `auto-revert-mode' or `global-auto-revert-mode', we employ
  ;; lazy auto reverting on `focus-in-hook' and `belak-switch-buffer-hook'.
  ;;
  ;; This is because autorevert abuses the heck out of inotify handles which can
  ;; grind Emacs to a halt if you do expensive IO (outside of Emacs) on the
  ;; files you have open (like compression). We only really need to revert
  ;; changes when we switch to a buffer or when we focus the Emacs frame.
  (defun belak--auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (auto-revert-handler)))

  (defun belak--auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (belak-visible-buffers))
      (with-current-buffer buf
        (belak--auto-revert-buffer-h)))))

;; Remember files we were recently in. We also clean up `recentf' when Emacs
;; quits, so it should be only for the existing session.
(use-package recentf
  :straight nil
  :after no-littering
  :hook (kill-emacs . recentf-cleanup)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


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

;; Disable the startup screen and messages - more often than not we really just
;; want to get into a file.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(fset #'display-startup-echo-area-message #'ignore)

;; Leave the scratch buffer blank and use emacs-lisp-mode rather than
;; fundamental mode or anything too minimal.
(setq initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode)

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

;; TODO: get this working on macOS as well
(setq browse-url-browser-function 'browse-url-xdg-open)

(provide 'belak-core)
;;; core.el ends here.
