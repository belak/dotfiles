;;; belak-dev --- development related packages and settings

;;; Commentary:

;;; Code:

;; diff-hl uses the emacs vcs integration to display
;; added/modified/removed lines. This uses my own hooks which add some
;; conveniences depending on if we're in a terminal or gui.

(use-package diff-hl
  :config
  (add-hook 'after-make-console-frame-hooks
            (lambda ()
              (global-diff-hl-mode 0)
              (diff-hl-margin-mode 1)))
  (add-hook 'after-make-window-system-frame-hooks
            (lambda ()
              (global-diff-hl-mode 1)
              (diff-hl-margin-mode 0))))

;; editorconfig is a simple way to share indentation settings between
;; editors. Because I sometimes dabble in vim, sublime etc, it's nice
;; to not have to re-do these settings at a project level between
;; editors.

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; The best git integration I've ever seen in an editor

(use-package magit
  :general
  ("M-g M-g" 'magit-status)
  ;;(when (fboundp 'evil-mode)
  ;;  (general-nmap :prefix belak/evil-leader
  ;;                "g" 'magit-status))
  :config
  ;;(when (fboundp 'evil-mode)
  ;;  (add-hook 'git-commit-mode-hook 'evil-insert-state))
  (setq magit-push-current-set-remote-if-missing t))

(use-package magit-filenotify
  :if (linux-p)
  :after magit
  :config
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

;; Simple snippets package. It's mostly used to reduce repetition.

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-verbosity 0)
  (yas-global-mode 1))

;; Because snippets aren't included with yasnippet any more, we need
;; to load a package which has some.

(use-package yasnippet-snippets)

;; This is a common hook for all modes that are based on the generally
;; programming mode.
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Newer versions of emacs define line numbers natively so
            ;; we use that if it's available and fall back to
            ;; linum-mode if it's not.
            (if (boundp 'display-line-numbers)
                (progn
                  (setq display-line-numbers 'relative
                        display-line-numbers-current-absolute t)
                  (force-mode-line-update))
              (linum-mode 1))

            (setq show-trailing-whitespace t)))

;; Load all specific language packages
(require 'belak-dev-c)
(require 'belak-dev-go)
(require 'belak-dev-js)
(require 'belak-dev-lisp)
(require 'belak-dev-org)
(require 'belak-dev-python)
(require 'belak-dev-rust)
(require 'belak-dev-various)

(provide 'belak-dev)
;;; belak-dev.el ends here
