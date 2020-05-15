;;; belak-macos.el --- macOS related tweaks -*- lexical-binding: t; -*-

;; Properly support emoji using the default Apple emoji font.
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; It doesn't matter if we display the menu bar on macOS, so we might as well
;; re-enable it.
(delq! 'menu-bar-lines default-frame-alist 'assq)
(push '(menu-bar-lines . 1) default-frame-alist)

;; Swap command and meta. In order to support similar key binds between macOS
;; and Linux (at least in terms of placement on the keyboard) we swap command
;; and meta.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(provide 'belak-macos)
;;; belak-macos.el ends here.
