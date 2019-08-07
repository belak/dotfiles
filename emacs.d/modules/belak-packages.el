;;; belak-packages --- package related settings

;;; Commentary:

;;; Code:

;; Bootstrap straight.el so we can use it when configuring other
;; packages.  This is being used over package.el because it lines up
;; better with how I write my config files - all packages used are
;; defined in my init files. This code comes directly from the
;; straight.el repo and even though it's relatively sketchy (this is
;; the emacs equivalent of curl pipe bash) it will only happen if it
;; hasn't been bootstrapped.
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
      straight-use-package-by-default  t)

;; Bootstrap the packages we'll need for everything else.
(straight-use-package 'use-package)
(straight-use-package 'diminish)
(straight-use-package 'general)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package)
  (require 'diminish)
  (require 'general))

(provide 'belak-packages)
;;; belak-packages.el ends here
