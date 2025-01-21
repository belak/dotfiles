;;; belak-ui.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Themes

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi :no-confirm-loading))


;;
;;; Fonts

(cond
 (IS-MAC
  (set-face-font 'default        "Monaco")
  (set-face-font 'fixed-pitch    "Monaco")
  (set-face-font 'variable-pitch "Monaco"))
 (IS-LINUX
  (set-face-font 'default        "Terminus 12")
  (set-face-font 'fixed-pitch    "Terminus 12")
  (set-face-font 'variable-pitch "Terminus 12")))


;;
;;; Completing Read

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))


;;
;;; Packages

;; The `doom-modeline' package seems to match exactly what I want out of a
;; modeline. It's simple, doesn't display minor modes by default, and looks
;; pretty good.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; Make sure the line and column numbers are in the modeline.
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode 1)

  ;; HACK: These two settings need to be set in order to remove what looks like
  ;; padding from the modeline. Thankfully, `doom-modeline' will helpfully
  ;; recalculate the height when set to 0.
  (setq doom-modeline-icon nil
        doom-modeline-height 0)

  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-buffer-encoding nil
        doom-modeline-minor-modes t))


;;
;;; Tweaks

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Make buffers match the unix path style of forward slashes, properly refresh
;; after a buffer has been killed, and ignore special buffers.
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


(provide 'belak-ui)
;;; belak-ui.el ends here.
