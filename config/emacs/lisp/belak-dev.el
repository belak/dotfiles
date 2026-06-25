;;; belak-dev.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

;; Display changed/removed lines in the fringe.
(use-package diff-hl
  ;; Enable `diff-hl' for programming, text, and `dired-mode'.
  :hook (prog-mode  . diff-hl-mode)
  :hook (text-mode  . diff-hl-mode)
  :hook (dired-mode . diff-hl-mode)
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

(use-package eldoc
  :blackout
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil))

;; `flymake' provides on-the-fly syntax checking. eglot enables it
;; automatically for LSP-backed modes.
(use-package flymake
  :blackout
  :preface
  (defun belak--maybe-enable-flymake ()
    "Enable flymake unless we're in the scratch buffer."
    (unless (equal (buffer-name) "*scratch*")
      (flymake-mode 1)))
  :hook (prog-mode . belak--maybe-enable-flymake)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

;; `project.el' is the built-in project management package. It auto-detects
;; projects via VCS roots.
(use-package project
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-switch-commands #'project-find-file))

;; `magit' is one of the best git interfaces available.
(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  ;; Auto-revert causes major performance issues in large repos.
  (magit-auto-revert-mode -1)
  (add-to-list 'display-buffer-alist
               '("\\*Magit Diff\\*"
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (inhibit-same-window . t))))


(use-package eglot
  :commands (eglot eglot-ensure))


;;
;;; Performance

(use-package vc-hooks
  :config
  (setq vc-handled-backends '(Git)))

(provide 'belak-dev)
;;; belak-dev.el ends here
