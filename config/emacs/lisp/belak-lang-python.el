;;; belak-lang-python.el -*- lexical-binding: t; -*-

(require 'belak-lib)
(require 'belak-dev)

;;
;;; Packages

;; After doing python dev for a while, it's nice to be able to tweak my setup.
;; After trying other major packages (elpy and jedi) I've settled on
;; anaconda-mode and pyenv. It provides a nice mix of tweakability and
;; convenience.

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter
  ("python"  . python-mode)
  ("python2" . python-mode)
  ("python3" . python-mode)
  :hook (python-mode . subword-mode)
  :config
  (setq python-fill-docstring-style 'django))

(use-package blacken
  :after python
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-only-if-project-is-blackened t))

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
  :bind
  (:map python-mode-map
        ("C-c '" . python-switch-quotes)))

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

(use-package py-isort
  :hook (python-mode . belak--py-isort-hook)
  :config
  (defun belak--py-isort-hook ()
    (add-hook 'before-save-hook 'py-isort-before-save nil t)))

(provide 'belak-lang-python)
;;; belak-lang-python.el ends here
