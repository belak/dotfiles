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

(use-feature! dired
  :bind (:map dired-mode-map
              ;; Reuse the same dired window
              ("RET" . dired-find-alternate-file)
              ("^"   . belak--dired-up-directory)
              ("q"   . belak--dired-quit-all))
  :commands dired
  :config
  ;; Ensure we can use the keybind we set without warnings
  (put 'dired-find-alternate-file 'disabled nil)

  ;; On macOS, we require GNU ls in order for dired to work. This lets us focus
  ;; on one set of switches rather than separate per OS.
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))

  (setq dired-listing-switches "--group-directories-first -al")

  (defun belak--dired-up-directory ()
    (interactive)
    (find-alternate-file ".."))

  (defun belak--dired-quit-all ()
    (interactive)
    (mapc #'kill-buffer (belak-buffers-in-mode 'dired-mode))
    (message "Killed all dired buffers")))

(use-feature! dired-x
  :after dired
  :hook (dired-mode . dired-omit-mode))

;; Add fancier colors to `dired-mode'.
(use-package! diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package! editorconfig
  :blackout
  :hook
  (prog-mode . editorconfig-mode)
  (text-mode . editorconfig-mode))

(use-feature! eldoc
  :blackout
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1))

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

  ;; Ensure flycheck only displays the errors in the minibuffer if the error
  ;; list isn't open.
  (setq flycheck-display-errors-function
        'flycheck-display-error-messages-unless-error-list)

  ;; When we use the error list, we want to make sure shackle puts it somewhere
  ;; better.
  (add-shackle-rule! '(flycheck-error-list-mode :noselect t :align 'below :size 7))
  (add-winner-boring-buffer! "*Flycheck errors*"))

;; Using flycheck-inline means we need to require that file.
(use-package! flycheck-inline
  :disabled t
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package! highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; TODO: maybe tweaks hl-todo-keyword-faces, as I don't actually use most of
  ;; them.
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
  :config
  ;; Strangely the default for projectile is `ido', not `default' as the name
  ;; would imply. We tame this so we can use it with whatever completing-read
  ;; function we want.
  (setq projectile-completion-system 'default)

  ;; TODO: maybe swap projectile-root-top-down and projectile-root-bottom-up in
  ;; the root-files-functions.

  (projectile-mode +1))


;;
;;; Completion

(use-package! company
  :blackout
  :preface
  (defmacro set-company-backend! (hook backend)
    `(add-hook ',hook (lambda ()
                        (set (make-local-variable 'company-backends) (list ',backend)))))
  :hook (prog-mode . company-mode)
  :commands global-company-mode
  :config
  (require 'company-dabbrev)

  ;; TODO: look into tab-n-go.

  ;; Reset the company backends to a fairly minimal set. We rely on the
  ;; `eglot'/`lsp-mode' integration with `completion-at-point'. Any languages
  ;; which need a specific backend other than these can configure them via
  ;; hooks.
  (setq company-backends '(company-capf company-files company-dabbrev))

  (use-feature! company-dabbrev
    :config
    ;; Improve basic text matching
    (setq company-dabbrev-other-buffers nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil))

  (setq company-idle-delay 0.25
        company-show-quick-access t
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-tooltip-flip-when-above t))

;; Package `company-prescient' provides intelligent sorting and filtering for
;; candidates in Company completions.
(use-package! company-prescient
  :demand t
  :after company
  :config
  (company-prescient-mode +1))

(use-package! company-quickhelp
  :after company
  :demand t
  :config
  (setq company-quickhelp-delay 3)

  (company-quickhelp-mode 1))

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
(use-feature! vc-hooks
  :config
  (setq vc-handled-backends '(Git)))

(provide 'belak-dev)
;;; belak-dev.el ends here
