;;; belak-completion-ivy.el --- ivy related packages and settings

;;; Commentary:

;;; Code:

(defun ivy-enabled-p ()
  (eq belak-completion-system 'ivy))

(use-package ivy
  :if (ivy-enabled-p)
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :if (ivy-enabled-p)
  :config
  (counsel-mode 1))

(provide 'belak-completion-ivy)

;;; belak-completion-ivy.el ends here
