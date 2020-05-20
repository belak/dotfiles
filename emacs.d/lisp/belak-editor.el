;;; belak-editor.el --- text interaction settings -*- lexical-binding: t; -*-

(require 'belak-core)


;;
;;; Packages

;; There are a few key binds which are close to being what we want, but not
;; quite, so we use `crux' as a utility library to fill in a few gaps.
(use-package crux
  :bind
  ;;("C-k"    .  #'crux-smart-kill-line)  ; This doesn't work with C-u
  ("C-c k"   . #'crux-kill-other-buffers)
  ("C-c f d" . #'crux-delete-file-and-buffer)
  ("C-c f r" . #'crux-rename-buffer-and-file))

(use-package ctrlf
  :bind (([remap isearch-forward]         . #'ctrlf-forward-literal)
         ([remap isearch-backward]        . #'ctrlf-backward-literal)
         ([remap isearch-forward-regexp]  . #'ctrlf-forward-regexp)
         ([remap isearch-backward-regexp] . #'ctrlf-backward-regexp))
  :config
  ;; Clear out the bindings because we've already defined them.
  (setq ctrlf-mode-bindings '())
  (ctrlf-mode +1))

;; Often times you just want to move a full block around. This makes it easy to
;; select what you need.
(use-package expand-region
  :bind
  ("C-="   . 'er/expand-region)
  ("C-S-=" . 'er/contract-region))

;; It's more standard to use C-n/C-p in Emacs rather than Up and Down, so we
;; warn whenever we use a key bind which has a more Emacs-y alternative.
(use-package guru-mode
  :blackout
  :hook (prog-mode . guru-mode)
  :config
  (setq guru-warn-only t))

;; Automatically clean up old buffers. This also provides a midnight-hook which
;; makes it possible to define cleanup functions.
(use-feature midnight
  :commands midnight-mode
  :init
  (add-transient-hook! pre-command-hook (midnight-mode 1)))

;; Highlight matching delimiters
(use-package paren
  :demand t
  ;;:after-call after-find-file doom-switch-buffer-hook
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))

;;
;;; Tweaks

;; When region is active, make `capitalize-word' and friends act on it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; Trigger auto-fill after punctuation characters, not just whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; If a mode defines a comment, only autofill inside them.
(setq comment-auto-fill-only-comments t)

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

(provide 'belak-editor)
;;; belak-editor.el ends here.
