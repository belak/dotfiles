;;; belak-lang-python.el -*- lexical-binding: t; -*-

(require 'belak-core)
(require 'belak-dev)

;;
;;; Packages

;; After doing python dev for a while, it's nice to be able to tweak my setup.
;; After trying other major packages (elpy and jedi) I've settled on
;; anaconda-mode and pyenv. It provides a nice mix of tweakability and
;; convenience.

(use-feature python
  :mode ("\\.py\\'" . python-mode)
  :interpreter
  ("python"  . python-mode)
  ("python2" . python-mode)
  ("python3" . python-mode)
  :hook (python-mode . subword-mode))

;; anaconda-mode provides code navigation and docs. Additionally, if
;; company-mode is enabled, company-anaconda will also be enabled.
(use-package anaconda-mode
  :diminish anaconda-mode
  :after python
  :hook (python-mode . anaconda-mode)
  :init
  (setq anaconda-mode-installation-directory "~/.emacs.d/.local/anaconda-mode")
  :hook (anaconda-mode . anaconda-eldoc-mode))

(use-package company-anaconda
  :demand t
  :after (anaconda-mode company)
  :config (set-company-backend! python-mode-hook company-anaconda))

;; This allows for simple switching between pyenv environments and provides us
;; with some basic building blocks to auto-switch to the proper pyenv if
;; available. In the past I used virtualenvwrapper, but after homebrew upgrades
;; I'd have to recreate all my python environments. Using pyenv lets me avoid
;; that.
(use-package pyenv-mode
  :after (python projectile)
  :hook (projectile-after-switch-project . belak--projectile-pyenv-mode-hook)
  :config
  (defun belak--projectile-pyenv-mode-hook ()
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset)))))

;; Cycle between apostrophes and quotes in python strings. Converts strings like
;; 'this' to strings like "this".
(use-package python-switch-quotes
  :after python
  :general
  (:keymaps 'python-mode-map
            "C-c '" #'python-switch-quotes))

;; This adds some basic features for requirements files, such as highlighting
;; and auto-completion of names from PyPI.
(use-package pip-requirements
  :mode
  ("requirements\\.txt\\'"    . pip-requirements-mode)
  ("requirements-.*\\.txt\\'" . pip-requirements-mode)
  ("requirements/.*\\.txt\\'" . pip-requirements-mode)
  ("requirements.in\\'"       . pip-requirements-mode)
  ("requirements-.*\\.in\\'"  . pip-requirements-mode)
  ("requirements/.*\\.in\\'"  . pip-requirements-mode))


(provide 'belak-lang-python)
;;; belak-lang-python.el ends here
