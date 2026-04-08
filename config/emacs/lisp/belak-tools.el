;;; belak-tools.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Various Tools

;; Provide something useful to make it easier to jump to other files at startup.
(use-package dashboard
  :demand t
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-set-navigator t
        dashboard-set-footer nil
        dashboard-center-content t
        dashboard-projects-backend 'project-el)

  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (registers . 5)))

  (dashboard-setup-startup-hook))

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

(use-package envrc
  :demand t
  :config
  (envrc-global-mode))

(use-package git-link
  :commands git-link
  :config
  ;; Use the commit hash rather than the branch name in the URL.
  (setq git-link-use-commit t))

;; rainbow-mode makes it easier to see colors, but it's a bit
;; overwhelming so it's left to be called when needed.
(use-package rainbow-mode
  :commands rainbow-mode)

;; vterm is like all the built-in terminals, but even better because
;; it uses libvterm which is pretty solid and handles most control
;; sequences really well.
(use-package vterm
  :commands vterm)

(provide 'belak-tools)
;;; belak-tools.el ends here.
