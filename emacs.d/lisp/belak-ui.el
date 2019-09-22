;;; belak-ui --- non-theme ui settings

;;; Commentary:

;; TODO: Show trailing whitespace

;;; Code:

;; Remove most GUI features, as I rarely use any of them.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)

;; Make things a little more responsive in general.
(setq echo-keystrokes 0.1
      tooltip-delay 0
      tooltip-short-delay 0)

;; Ensure the help window is selected when one is open. This makes it
;; much easier to quit them when we're done.
(setq help-window-select t)

;; Disable scrolling past the end of the file.
;; TODO: Figure out why this doesn't work
;;(setq next-line-add-newlines nil)

;; Make sure we only have to type 'y' or 'n', not the full word
;; because that takes too many keystrokes.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Be as quiet as we can at startup. Most of the messaging isn't very
;; useful. Maybe one day I'll build my own dashboard or make it
;; persistent, this is good enough for now.
(setq-default
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil)

;; Highlight the current line to make the cursor easier to see.
(global-hl-line-mode)

;; Hide auto-fill-function
(delight 'auto-fill-function nil "simple")

;; Make the lisp modes a bit shorter
;; TODO: these don't seem to work
(delight 'lisp-interaction-mode "λ»" :major)
(delight 'emacs-lisp-mode "Eλ" :major)
(delight 'lisp-mode "λ" :major)

;; helpful is a replacement for the built-in help pages which are much
;; prettier and easier to read.
(use-package helpful
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-h ." 'helpful-at-point))

;; Because spacebar is so close to what I want, we use that rather than
;; customizing it completely. It takes way more code than you'd expect to
;; directly configure the menubar.
(use-package spaceline-config
  :straight spaceline
  :config
  (setq powerline-default-separator 'bar
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

;; Undo is pretty weird in emacs. Undo-tree is a step forward but still
;; weird.
(use-package undo-tree
  :delight undo-tree-mode)

(use-package which-key
  :defer 1
  :delight which-key-mode
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  (which-key-mode 1))

(provide 'belak-ui)

;;; belak-ui.el ends here
