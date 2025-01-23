;;; belak-dev.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

;; Display changed/removed lines in the fringe.
(use-package! diff-hl
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

(use-package! editorconfig
  :blackout
  :hook
  (prog-mode . editorconfig-mode)
  (text-mode . editorconfig-mode))

(use-feature! eldoc
  :blackout
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil))

;; `flycheck-mode' is used for linters and catching compilation errors.
(use-package! flycheck
  :blackout
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

  ;; Don't display flycheck errors in the minibuffer
  (setq flycheck-display-errors-function 'ignore)

  ;; When we use the error list, we want to make sure shackle puts it somewhere
  ;; better.
  (add-shackle-rule! '(flycheck-error-list-mode :noselect t :align 'below :size 7))
  (add-winner-boring-buffer! "*Flycheck errors*"))

(use-package! highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

;; `magit' is one of the best git interfaces I've ever used.
(use-package! magit
  :bind
  ("C-c g" . magit-status)
  :config
  ;; Unfortunately, as this causes major performance issues in large repos,
  ;; which I often run into at my job, it's just easier to disable this.
  (magit-auto-revert-mode -1)

  (add-shackle-rule! '(magit-diff-mode :noselect t)))

(use-package! projectile
  :blackout
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-mode)
  :config
  ;; Strangely the default for projectile is `ido', not `default' as the name
  ;; would imply. We tame this so we can use it with whatever completing-read
  ;; function we want.
  (setq projectile-completion-system 'default)

  ;; Ignore all projects from the go module cache, as it's read only and we
  ;; primarily use `find-function-at-point' to navigate there.
  (setq projectile-ignored-projects '("~/go/pkg/")))


;;
;;; Completion

(use-package! corfu
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

(use-feature! corfu-history
  :hook (after-init . corfu-history-mode))

(use-feature! corfu-popupinfo
  :hook (after-init . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)))


;; Between `eglot' and `lsp-mode' I've had better experiences with eglot because
;; it tries to do less. That being said lsp-mode has improved quite a bit in the
;; last few years, so I need to take another look.
(use-feature! eglot
  :commands (eglot eglot-ensure))


;;
;;; Performance

;; Normally, we'd disable all the VC backends as we use Magit to interface with
;; Git. However, `diff-hl' requires the VC backends to be enabled to work, so we
;; disable everything other than git.
(use-feature! vc-hooks
  :config
  (setq vc-handled-backends '(Git)))

(provide 'belak-dev)
;;; belak-dev.el ends here
