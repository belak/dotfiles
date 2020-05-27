;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kaleb Elwert"
      user-mail-address "belak@coded.io")

;; UI
(setq doom-font (font-spec :family "Source Code Pro" :size 12)
      ;;doom-theme 'monokai-pro
      doom-theme 'modus-vivendi
      doom-modeline-icon nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq uniquify-buffer-name-style 'forward
      uniquify-strip-common-suffix nil)

;; Make M-z zap-up-to-char (doesn't include char) rather than
;; zap-to-char and make M-Z zap in reverse.
(autoload 'zap-up-to-char "misc" "" 'interactive)
(global-set-key [remap zap-to-char] 'zap-up-to-char)

(defun reverse-zap-up-to-char (char)
  "Zap back to CHAR."
  (interactive "Zap back to char: ")
  (zap-up-to-char -1 char))
(global-set-key "\M-Z" 'reverse-zap-up-to-char)

(after! ido
  (setq ido-use-virtual-buffers t))

(load! "+bindings")
(load! "+ui")

(map!
 ;; Allow C-a and C-e to work in normal mode as well.
 ;; TODO: make sure they also enter insert mode
 :n "C-a" #'doom/backward-to-bol-or-indent
 :n "C-e" #'doom/forward-to-last-non-comment-or-eol

 ;; Make home and end do the same thing as C-a/C-e rather than going to the
 ;; beginning/end of a buffer.
 :g "<home>" #'doom/backward-to-bol-or-indent
 :g "<end>"  #'doom/forward-to-last-non-comment-or-eol)
