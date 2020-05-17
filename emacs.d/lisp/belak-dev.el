;;; belak-dev.el -*- lexical-binding: t; -*-

(require 'belak-core)
(require 'belak-ui)

;;
;;; Packages

(use-package company
  :delight
  :preface
  (defmacro set-company-backend! (hook backend)
    `(add-hook ',hook (lambda ()
                        (set (make-local-variable 'company-backends) (list ',backend)))))
  :init
  (add-transient-hook! pre-command-hook (global-company-mode +1))
  :commands global-company-mode
  :config
  (require 'company-dabbrev)

  ;; TODO: look into tab-n-go.

  (use-package company-dabbrev
    :straight nil
    :config
    ;; Improve basic text matching
    (setq company-dabbrev-other-buffers nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil))

  (setq company-idle-delay 0.25
        company-show-numbers t
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-tooltip-flip-when-above t))

(use-package dired
  :straight nil
  :general
  (:keymaps '(dired-mode-map)
            "q" #'belak--dired-quit-all)
  :config
  ;; TODO: there's an alternate way used here which replaces the current buffer
  ;; rather than killing all of them at the end:
  ;; https://github.com/MatthewZMD/.emacs.d/blob/master/elisp/init-dired.el
  (defun belak--dired-quit-all ()
    (interactive)
    (mapc #'kill-buffer (belak-buffers-in-mode 'dired-mode))
    (message "Killed all dired buffers")))

;; Add fancier colors to `dired-mode'.
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; We use `diff-hl' rather than `git-gutter' because it seems to be better
;; supported.
(use-package diff-hl
  ;; Enable `diff-hl' for programming, text, and `dired-mode'.
  :hook (prog-mode          . diff-hl-mode)
  :hook (text-mode          . diff-hl-mode)
  :hook (dired-mode         . diff-hl-mode)
  ;; `diff-hl' provides some convenience hooks so we enable the ones we want to
  ;; use.
  :hook (dired-mode         . diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; When we're not in a GUI, we want to use `diff-hl-margin-mode' so it will
  ;; still display.
  (unless IS-GUI
    (add-hook 'diff-hl-mode-hook #'diff-hl-margin-mode)))

(use-package editorconfig
  :delight
  :hook
  (prog-mode . editorconfig-mode)
  (text-mode . editorconfig-mode))

(use-package eldoc
  :delight
  :straight nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1))

;; `flycheck-mode' is used for linters and catching compilation errors.
(use-package flycheck
  :delight
  :hook (prog-mode . flycheck-mode)
  :config
  ;; The default flycheck settings are a bit too agressive - we really only want
  ;; to check when the file is loaded or saved.
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch))

  ;; Because we check much less often, we need to re-check buffers when a small
  ;; change was made in another buffer, ignoring the delay. This allows fast
  ;; changes to things like configuration files.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)

  ;; We change the double arrow to be a triangle because it looks cleaner.
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; Because we have diff-hl in the left fringe, we want flycheck in the right
  ;; fringe.
  (setq flycheck-indication-mode 'right-fringe)

  ;; When we use the error list, we want to make sure shackle puts it somewhere
  ;; better.
  (add-shackle-rule '(flycheck-error-list-mode :noselect t :align 'below :size 7))

  (after! winner
    (appendq! winner-boring-buffers "*Flycheck errors*")))

(use-package flyspell
  :delight
  :straight nil
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

;; hl-todo simply highlights TODO and other similar comments to make them easier
;; to find. I originally used fic-mode, but it appears that hl-todo is a little
;; better and is updated more frequently.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; TODO: tweak hl-todo-keyword-faces, maybe remove most of them
  (setq hl-todo-highlight-punctuation ":"))

(use-package magit
  :general
  ("C-c g" 'magit-status)
  :config
  (setq magit-auto-revert-mode nil)

  (add-shackle-rule '(magit-diff-mode :noselect t)))

;; Project based navigation is how I deal with code. This tracks projects to
;; make it easier to navigate between them.
(use-package projectile
  :commands (projectile-project-p projectile-mode)
  :general ("C-c p" '(:keymap projectile-command-map))
  :config (projectile-mode +1))

;; rainbow-mode makes it easier to see colors, but I don't use it very often so
;; I leave it disabled unless called.
(use-package rainbow-mode
  :commands rainbow-mode)

(use-package remember
  :straight nil
  :commands remember remember-notes
  :config
  (setq remember-notes-initial-major-mode 'gfm-mode))


;;
;;; Mode Tweaks

;; Make the lisp mode names a bit shorter
(delight 'lisp-interaction-mode "λ»")
(delight 'emacs-lisp-mode "Eλ")
(delight 'lisp-mode "λ")

;; Auto wrap text in text-mode.
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Some extra files to support with basic modes
(push '("LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'"  . text-mode) auto-mode-alist)
(push '("\\.env\\'"  . sh-mode)   auto-mode-alist)

(provide 'belak-dev)
;;; belak-dev.el ends here.
