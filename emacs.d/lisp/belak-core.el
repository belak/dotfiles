;;; belak-core --- low level setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Optimizations:



;;; Constants:

;; Set up some basic constants for use later.
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-GUI   (memq window-system '(mac ns x)))

;;; Important tweaks

;; UTF-8 as the default encoding (Copied from doom-emacs).
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

;;; Sub-modules:

(require 'belak-core-macros)
(require 'belak-core-package)
(require 'belak-core-misc)

(provide 'belak-core)

;;; belak-core.el ends here
