;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Source Code Pro" :size 14)
      ;;doom-theme 'monokai-pro
      doom-theme 'modus-vivendi
      doom-modeline-icon t
      doom-modeline-buffer-file-name-style  'truncate-upto-root)

;;(setq modus-vivendi-theme-visible-fringe t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
