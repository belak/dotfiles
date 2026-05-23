;;; belak-tools.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Various Tools

;; agent-shell provides an Emacs interface to ACP-compatible AI agents like
;; Claude Code, allowing direct interaction from within Emacs.
(use-package agent-shell
  :commands agent-shell
  :bind ("C-c s c" . agent-shell)
  :config
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config))

  ;; Use text header instead of graphical header
  (setq agent-shell-header-style 'text)

  ;; Open in current frame instead of new frame
  (setq agent-shell-display-action '(display-buffer-same-window)))

(use-package dired-x
  :after dired
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+")))

(use-package dired-sidebar
  :bind ("C-c t" . dired-sidebar-toggle-sidebar)
  :hook (dired-sidebar-mode . dired-omit-mode))

(use-package envrc
  :blackout envrc-mode
  :demand t
  :config
  (envrc-global-mode)
  ;; The default summary lists every loaded variable, which is too verbose for
  ;; large envrc files and nix flakes. Override to show a single line instead.
  (advice-add 'envrc--show-summary :override
              (lambda (_summary dir)
                (message "direnv: loaded %s" (abbreviate-file-name dir)))))

(use-package git-link
  :commands git-link
  :config
  ;; Use the commit hash rather than the branch name in the URL.
  (setq git-link-use-commit t))

;; rainbow-mode makes it easier to see colors, but it's a bit
;; overwhelming so it's left to be called when needed.
(use-package rainbow-mode
  :commands rainbow-mode)

;; verb is an http client which integrates with org-mode. However because we use
;; it with org-tangle rather than the direct integration, we don't need any
;; extra setup.
(use-package verb)

(use-package ob-verb
  :ensure nil
  :commands (org-babel-execute:verb))

;; vterm is like all the built-in terminals, but even better because
;; it uses libvterm which is pretty solid and handles most control
;; sequences really well.
(use-package vterm
  :commands vterm)

;; ghostel is a terminal emulator powered by libghostty-vt, the
;; terminal library from the Ghostty project.  The native module must
;; live outside /nix/store (read-only), so point it at a writable dir.
(use-package ghostel
  :commands (ghostel ghostel-project ghostel-other)
  :bind ("C-c s t" . ghostel)
  :custom
  (ghostel-module-directory (no-littering-expand-var-file-name "ghostel/module/"))
  :config
  (after! project
    (add-to-list 'project-switch-commands '(ghostel-project "Terminal" ?t))))

(provide 'belak-tools)
;;; belak-tools.el ends here.
