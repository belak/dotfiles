;;; belak-dev.el -*- lexical-binding: t; -*-

;;
;;; Packages

;; Display changed/removed lines in the fringe.
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
  :blackout
  :hook
  (prog-mode . editorconfig-mode)
  (text-mode . editorconfig-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; TODO: maybe tweaks hl-todo-keyword-faces, as I don't actually use most of
  ;; them.
  (setq hl-todo-highlight-punctuation ":"))

;; `magit' is one of the best git interfaces I've ever used.
(use-package magit
  :bind
  ("C-c g" . magit-status)
  :config
  ;; Unfortunately, as this causes major performance issues in large repos,
  ;; which I often run into at my job, it's just easier to disable this.
  (magit-auto-revert-mode -1))

(use-package projectile
  :blackout
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; TODO: maybe look into the built-in `project' functionality

  ;; Strangely the default for projectile is `ido', not `default' as the name
  ;; would imply. We tame this so we can use it with whatever completing-read
  ;; function we want.
  (setq projectile-completion-system 'default)

  ;; Ignore all projects from the go module cache, as it's read only and we
  ;; primarily use `find-function-at-point' to navigate there.
  (setq projectile-ignored-projects '("~/go/pkg/"))

  ;; TODO: maybe swap projectile-root-top-down and projectile-root-bottom-up in
  ;; the root-files-functions.

  (projectile-mode +1))


;;
;;; LSP Setup

;; Between `eglot' and `lsp-mode' I've had better experiences with eglot because
;; it tries to do less. That being said lsp-mode has improved quite a bit in the
;; last few years, so it may be worth taking another look.
(use-package eglot
  :commands (eglot eglot-ensure))


;;
;;; Tree Sitter

(use-package tree-sitter
  :hook (after-init                . global-tree-sitter-mode)
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))

;;(use-package tree-sitter-langs
;;  :after tree-sitter)


;;
;;; Performance

;; Normally, we'd disable all the VC backends as we use Magit to interface with
;; Git. However, `diff-hl' requires the VC backends to be enabled to work, so we
;; disable everything other than git.
(use-package vc-hooks
  :config
  (setq vc-handled-backends '(Git)))

(provide 'belak-dev)
;;; belak-dev.el ends here.
