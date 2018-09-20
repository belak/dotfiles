;;; belak-helm.el --- helm related config

;;; Commentary:

;; Helm is a much fancier replacement for ido.

;;; Code:

(use-package helm
  :defer 2
  :diminish helm-mode
  :if belak/enable-helm
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)

         ;; Reverse tab and C-z
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z"   . helm-select-action)

         :map org-mode-map
         ("C-c h" . helm-org-in-buffer-headings))
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
  :if belak/enable-helm
  :after helm
  :commands
  helm-ag
  helm-ag-project-root)

(use-package helm-swoop
  :if belak/enable-helm
  :after helm
  :bind ("C-S-s" . helm-swoop)
  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))

;; helm-projectile should only be loaded if helm is used. It's
;; currently disabled.

(use-package helm-projectile
  :if belak/enable-helm
  :after (helm projectile)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(provide 'belak-helm)

;;; belak-helm.el ends here
