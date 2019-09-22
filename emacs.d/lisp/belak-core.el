;;; belak-core --- low level setup

;;; Commentary:

;;; Code:

;; Set up some basic constants for use later.
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-GUI   (memq window-system '(mac ns x)))

;; UTF-8 as the default encoding (Copied from doom-emacs).
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

;; Set up a few file name variables for later use. The goal is to get
;; as many extraneous files into directories, rather than dumping them
;; into .emacs.d.
;; TODO: look into using transient - I'm not sure where that came from
(setq user-emacs-directory (file-name-directory load-file-name))
(defvar belak-emacs-dir user-emacs-directory)
(defvar belak-local-dir (concat belak-emacs-dir ".local/"))

;; Do our absolute best to make files save into our local dir, rather than the
;; emacs dir.
(setq recentf-save-file (expand-file-name "recentf" belak-local-dir))

;; Disable history and backup files. They really just get in the way.
;; TODO: figure out which of these are useful and make sure they use
;; directories rather than files.
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

;; We still want to be able to have non-public configs, such as for
;; passwords and what not, so we put them in a separate file and load
;; it, but ignore errors, for instance if it doesn't exist.  This has
;; the added advantage of making it so customizations will go to this
;; file and not to init.el, which is version controlled.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Make M-z zap-up-to-char (doesn't include char) rather than
;; zap-to-char and make M-Z zap in reverse.
(autoload 'zap-up-to-char "misc" "" 'interactive)
(global-set-key "\M-z" 'zap-up-to-char)

(defun reverse-zap-up-to-char (char)
  "Zap back to CHAR."
  (interactive "Zap back to char: ")
  (zap-up-to-char -1 char))
(global-set-key "\M-Z" 'reverse-zap-up-to-char)

;; Reverse command and option modifiers on macOS.
(setq mac-command-modifier 'meta
      mac-option-modifier 'super)

(provide 'belak-core)

;;; belak-core.el ends here
