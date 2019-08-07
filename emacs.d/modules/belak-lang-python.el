;;; belak-lang-python.el --- python related packages and settings

;;; Commentary:

;;; Code:

(use-package python
  :straight nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter (("python"  . python-mode)
                ("python2" . python-mode)
                ("python3" . python-mode))
  :hook (python-mode . subword-mode))

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
  :hook (anaconda-mode-hook . anaconda-eldoc-mode))

(use-package company-anaconda
  :after (anaconda-mode company)
  :config (add-to-list 'company-backends 'company-anaconda))

;; This adds some basic features for requirements files, such as
;; highlighting and auto-completion of names from PyPI.

(use-package pip-requirements
  :mode
  "requirements.txt"
  "requirements/\\.txt\\'")

;; This allows for simple switching between pyenv environments.

(use-package pyenv-mode
  :after (python projectifle)
  :hook (projectile-after-switch-project-hook . belak--projectile-pyenv-mode-hook)
  :config
  (defun belak--projectile-pyenv-mode-hook ()
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset)))))

(provide 'belak-lang-python)

;;; belak-lang-python.el ends here
