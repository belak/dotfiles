;;; belak-package --- package manager settings

;;; Commentary:

;;; Code:

;; We need to bootstrap the package manager enough to ensure org-mode
;; is installed. Then we can use org-babel for the rest of the config.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Make it a little easier to use straight. We assume ssh urls so we
;; can just edit and push our own packages. Also, we assume `:straight
;; t' in use-package blocks by default.
(setq straight-vc-git-default-protocol 'ssh
      straight-use-package-by-default t)

;; Bootstrap =use-package= and friends here. We'll use these for the
;; rest of the packages we install.

(straight-use-package 'use-package)
(straight-use-package 'delight)
(straight-use-package 'general)

(eval-when-compile
  (require 'use-package))
(require 'general)
(require 'delight)

(provide 'belak-package)

;;; belak-package.el ends here
