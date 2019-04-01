;;; belak-completion-helm.el --- helm related config

;;; Commentary:

;; Helm is a much fancier replacement for ido.

;;; Code:

(defun helm-enabled-p ()
  (eq belak-completion-system 'helm))

(use-package helm
  :defer 2
  :diminish helm-mode
  :if (helm-enabled-p)
  :general
  ("M-x"     'helm-M-x)
  ("C-x b"   'helm-mini)
  ("C-x C-f" 'helm-find-files)

  ;; Reverse tab and C-z
  (:keymaps 'helm-map
            "<tab>" 'helm-execute-persistent-action
            "C-z"   'helm-select-action)

  (:keymaps 'org-mode-map
         "C-c h" 'helm-org-in-buffer-headings)

  :config
  (helm-mode 1)

  ;; Resize based on the number of results
  (helm-autoresize-mode 1)

  ;; Turn on fuzzy matching for everything we can
  (setq helm-autoresize-max-height 15
        helm-autoresize-min-height 5
        helm-candidate-number-limit 1000
        helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t))

(use-package helm-ag
  :if (helm-enabled-p)
  :after helm
  :commands
  helm-ag
  helm-ag-project-root)

(use-package helm-swoop
  :if (helm-enabled-p)
  :after helm
  :general
  ("C-S-s" 'helm-swoop)
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))

;; helm-projectile should only be loaded if helm is used. It's
;; currently disabled.

(use-package helm-projectile
  :if (helm-enabled-p)
  :after (helm projectile)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(provide 'belak-completion-helm)

;;; belak-completion-helm.el ends here
