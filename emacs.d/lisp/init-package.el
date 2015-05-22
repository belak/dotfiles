(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Make sure we have the package archive loaded
(unless package-archive-contents
  (package-refresh-contents))

;; Helper function to install any missing packages
(defun require-package (p)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'init-package)
