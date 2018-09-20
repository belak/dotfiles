;;; belak-settings --- various random settings unrelated to packages

;;; Commentary:

;;; Code:

;; Disable most of the vc backends.

(setq vc-handled-backends '(Git Hg))

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

;; Most markup modes are derived from text-mode, so we can turn on
;; auto-fill for all of them.

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(diminish 'auto-fill-function)

;; Disable cursor blinking

(blink-cursor-mode -1)

;; Show modifier combinations almost immediately.

(setq echo-keystrokes 0.1)

;; Revert buffers automatically if they've changed on disk

(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)

;; I find that when I want to use zap, I almost never want to include
;; the next character, so we replace zap-to-chat with zap-up-to-char.

(autoload 'zap-up-to-char "misc")
(global-set-key [remap zap-to-char] 'zap-up-to-char)

;; Ensure backups are saved to a directory rather than with a suffix.

(defvar save-place-file)
(setq save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Make sure we only have to type 'y' or 'n', not the full word
;; because that takes too many keystrokes.

(fset 'yes-or-no-p 'y-or-n-p)

;; Random settings

(setq lazy-highlight-initial-delay 0
      make-pointer-invisible t
      vc-follow-symlinks t
      load-prefer-newer t
      inhibit-splash-screen t
      history-length 50
      mouse-yank-at-point t)

;; As a former vim user, I like escape to actually quit
;; everywhere. This was taken from
;; https://github.com/davvil/.emacs.d/blob/master/init.el
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.

In Delete Selection mode, if the mark is active, just deactivate
it; then it takes a second \\[keyboard-quit] to abort the
minibuffer."
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

;; Highlight between matching parens
(electric-pair-mode 1)

;; Show which function we're in
;;(which-function-mode 1)

;; Delete text if we start typing
;; TODO: This may be possible to do with a variable.
(delete-selection-mode)

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Show modifier combinations almost immediately.
(setq echo-keystrokes 0.1)

;; XXX: There's a strange interaction of my init.el with emacs-mac
;; which causes the maximize button to revert to the old "vertical"
;; maximize. For whatever reason, calling toggle-frame-fullscreen
;; after the frame is created fixes that, so we enter then exit
;; fullscreen to fix the state.

(when (osx-p)
  (add-hook 'after-make-window-system-frame-hooks
            (lambda ()
              (toggle-frame-fullscreen)
              (toggle-frame-fullscreen))))

;; The only current Linux specific settings are relating to opening
;; files. Because I assume xdg-open is configured properly, we can set
;; the URL function to just call it so we always get the right browser.

(when (linux-p)
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; OSX specific settings. Some of these are so the emacs-mac fork will work
;; better, some of it improves homebrew support.

(when (osx-p)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-control-modifier 'control
        insert-directory-program "/usr/local/bin/gls")
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

;; We still want to be able to have non-public configs, such as for
;; passwords and what not, so we put them in a separate file and load
;; it, but ignore errors, for instance if it doesn't exist. This has
;; the added advantage of making it so customizations will go to this
;; file and not to init.el, which is version controlled.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(provide 'belak-settings)

;;; belak-settings.el ends here
