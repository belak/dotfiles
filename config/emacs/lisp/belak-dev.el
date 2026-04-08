;;; belak-dev.el -*- lexical-binding: t; -*-

(require 'belak-lib)

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

(use-package eldoc
  :blackout
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil))

;; `flymake' provides on-the-fly syntax checking. eglot enables it
;; automatically for LSP-backed modes.
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

;; `magit' is one of the best git interfaces I've ever used.
(use-package magit
  :bind
  ("C-c g" . magit-status)
  :config
  ;; Unfortunately, as this causes major performance issues in large repos,
  ;; which I often run into at my job, it's just easier to disable this.
  (magit-auto-revert-mode -1)

  (add-shackle-rule! '(magit-diff-mode :noselect t)))

;; `project.el' is the built-in project management package. It auto-detects
;; projects via VCS roots.
(use-package project
  :bind-keymap ("C-c p" . project-prefix-map))


;;
;;; Completion

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  :config
  (after! dimmer
    ;; Avoid dimming the window when corfu is opened. This is based on some code
    ;; from https://github.com/gonewest818/dimmer.el/issues/62.
    (defun belak--corfu-frame-p ()
      "Check if the buffer is a corfu frame buffer."
      (string-match-p "\\` \\*corfu" (buffer-name)))
    (add-to-list 'dimmer-prevent-dimming-predicates #'belak--corfu-frame-p)))

(use-package corfu-history
  :hook (after-init . corfu-history-mode))

(use-package corfu-popupinfo
  :hook (after-init . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)))


;; Between `eglot' and `lsp-mode' I've had better experiences with eglot because
;; it tries to do less. That being said lsp-mode has improved quite a bit in the
;; last few years, so I need to take another look.
(use-package eglot
  :commands (eglot eglot-ensure))


;;
;;; Performance

;; Normally, we'd disable all the VC backends as we use Magit to interface with
;; Git. However, `diff-hl' requires the VC backends to be enabled to work, so we
;; disable everything other than git.
(use-package vc-hooks
  :config
  (setq vc-handled-backends '(Git)))

(provide 'belak-dev)
;;; belak-dev.el ends here
