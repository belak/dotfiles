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

;; Automatically insert matching parens.
;;(electric-pair-mode 1)

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

;; Make sure the line and column numbers are in the modeline.
(column-number-mode 1)
(line-number-mode 1)

;; Hide auto-fill-function
(delight 'auto-fill-function nil "simple")

(defmacro diminish-major-mode (mode name)
  "Use a different `NAME' when displaying a `MODE' in the modeline.

This is a snippet originally from
https://github.com/sandhu/emacs.d/blob/master/lisp/teppoudo-diminish.el.

Note that this should be replacable with delight, but it doesn't
seem to work right."
  `(add-hook (intern (concat (symbol-name ,mode) "-hook"))
             '(lambda () (setq mode-name ,name))))

;; Make the lisp modes a bit shorter
(diminish-major-mode 'lisp-interaction-mode "λ»")
(diminish-major-mode 'emacs-lisp-mode "Eλ")
(diminish-major-mode 'lisp-mode "λ")

;; helpful is a replacement for the built-in help pages which are much
;; prettier and easier to read.
(use-package helpful
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-h ." 'helpful-at-point))

(use-package paren
  :straight nil
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis
        show-paren-delay 0))

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

(use-package whitespace
  :straight nil
  :delight global-whitespace-mode
  :config
  (setq whitespace-style '(trailing face tabs tab-mark lines-tail)
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [182 10])
                                      (tab-mark 9 [9655 9] [92 9])))
  (global-whitespace-mode t)
  (setq whitespace-global-modes '(text-mode prog-mode org-mode)))

(provide 'belak-ui)

;;; belak-ui.el ends here
