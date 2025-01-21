;;; belak-os-macos.el --- macOS related tweaks -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

;; Make the titlebar match the background color on macOS.
(use-package ns-auto-titlebar
  :demand t
  :config
  (ns-auto-titlebar-mode))


;;
;;; Tweaks

;; Properly support emoji using the default Apple emoji font.
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; It doesn't matter if we display the menu bar on macOS, so we might as well
;; re-enable it.
(delq! 'menu-bar-lines default-frame-alist 'assq)
(push '(menu-bar-lines . 1) default-frame-alist)

;; Don't actually delete files on macOS, send them to the trash first.
(setq delete-by-moving-to-trash t)

(provide 'belak-os-macos)
;;; belak-os-macos.el ends here.
