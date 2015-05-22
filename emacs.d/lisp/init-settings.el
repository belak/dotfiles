;; Better defaults
(require-package 'better-defaults)

;; Random settings
(add-hook 'prog-mode-hook 'linum-mode)
(setq initial-buffer-choice t)
(setq inhibit-startup-screen t)

(provide 'init-settings)
