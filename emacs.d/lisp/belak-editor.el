;;; belak-editor.el --- text interaction settings -*- lexical-binding: t; -*-

;;
;;; Tweaks

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

;; TODO: add back (cua-mode) shortcuts on macos.

;; TODO: look into drag stuff mode


;;
;;; Packages

(use-package expand-region
  :functions er/expand-region
  :bind
  ("C-="   . er/expand-region)
  ("C-S-=" . er/contract-region))

;; highlight matching delimiters
(use-package paren
  ;;:after-call after-find-file doom-switch-buffer-hook
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))


;; TODO: look into focus-mode
;; TODO: look into undo-tree


(provide 'belak-editor)
;;; belak-editor.el ends here
