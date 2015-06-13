;;; init.el - Config bootstrap
;; A few things need to happen in this file:
;; - Load package system
;; - Install org-mode
;; - Bootstrap into README.org

;; Load package system
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"       . "http://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.org/packages/")
        ("elpy"      . "http://jorgenschaefer.github.io/packages/")))

;; Make sure it's initialized
(package-initialize)

;; Update package lists if we don't have any yet
(when (not package-archive-contents)
  (package-refresh-contents))

;; Small macro to install a missing package
(defmacro package-ensure-installed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; Install org-mode
(package-ensure-installed org)

;; Load org-mode
(require 'org)
(require 'ob-tangle)

;; Bootstrap into README.org
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
