;;; belak-lang-python.el --- python related packages and settings

;;; Commentary:

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter (("python"  . python-mode)
                ("python2" . python-mode)
                ("python3" . python-mode))
  :config
  (add-hook 'python-mode-hook #'subword-mode))

;; Cycle between apostrophes and quotes in python strings. Converts
;; strings like 'this' to strings like "this".

(use-package python-switch-quotes
  :after python
  :general
  (:keymaps 'python-mode-map
            "C-c '" 'python-switch-quotes))

;; After doing python dev for a while, it's nice to be able to tweak
;; my setup. After trying other major packages (elpy and jedi) I've
;; settled on anaconda-mode and virtualenvwrapper. It provides a nice
;; mix of tweakability and convenience.
;;
;; anaconda-mode provides code navigation and docs. Additionally, if
;; company-mode is enabled, company-anaconda will also be enabled.

(use-package anaconda-mode
  :diminish anaconda-mode
  :after python
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory "~/.emacs.d/.local/anaconda-mode")
  :config
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after (anaconda-mode company)
  :config (add-to-list 'company-backends 'company-anaconda))

;; This adds some basic features for requirements files, such as
;; highlighting and auto-completion of names from PyPI.

(use-package pip-requirements
  :mode
  "requirements.txt"
  "requirements/\\.txt\\'")

;; virtualenvwrapper is a pretty awesome small package which aims to
;; emulate python's virtualenvwrapper. It adds some functions to
;; switch between virtualenvs and provides a consistent location to
;; put them.
;;
;; If projectile is enabled, this will also add a hook which will load
;; the virtualenv matching the basename of the project when switching
;; buffers.

(use-package virtualenvwrapper
  :config
  (when (fboundp 'projectile-mode)
    (advice-add 'switch-to-buffer :after
                (lambda (&rest arg-list)
                  (if (and (projectile-project-p)
                           (venv-is-valid (projectile-project-name)))
                      (venv-workon (projectile-project-name)))))))

(provide 'belak-lang-python)

;;; belak-lang-python.el ends here
