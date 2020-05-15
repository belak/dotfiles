;;; belak-editor.el --- text interaction settings -*- lexical-binding: t; -*-

(require 'belak-core)

;;
;;; Tweaks

;; Make tab a tiny bit smarter - if the current line is already indented, then
;; complete at point.
(setq tab-always-indent 'complete)

;; Delete selected text when typing.
(delete-selection-mode 1)

;; Default indentation. Generally we fall back to editorconfig, but this is here
;; just in case.
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Clean up some various requirements that Emacs sometimes complains about or
;; handles.
(setq sentence-end-double-space nil
      require-final-newline t)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Avoid saving duplicates into the kill ring.
(setq kill-do-not-save-duplicates t)

;; Don't make distinctions between ASCII and siblings (like a and a
;; with an umlaut)
(setq search-default-mode 'char-fold-to-regexp)


;;
;;; Packages

(use-package crux
  :general
  ;;("C-k"     #'crux-smart-kill-line)  ; This doesn't work with C-u
  ("C-c k"   #'crux-kill-other-buffers)
  ("C-c f d" #'crux-delete-file-and-buffer)
  ("C-c f r" #'crux-rename-buffer-and-file))

(use-package expand-region
  :general
  ("C-="   'er/expand-region)
  ("C-S-=" 'er/contract-region))

;; highlight matching delimiters
(use-package paren
  ;;:after-call after-find-file doom-switch-buffer-hook
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))

(use-package guru-mode
  :delight
  :hook (prog-mode . guru-mode)
  :config
  (setq guru-warn-only t))

;; automatically clean up old buffers. This also provides a midnight-hook which
;; makes it possible to define cleanup functions.
(use-package midnight
  :straight nil
  :commands midnight-mode
  :init
  (add-transient-hook! pre-command-hook (midnight-mode 1)))


(provide 'belak-editor)
;;; belak-editor.el ends here.
