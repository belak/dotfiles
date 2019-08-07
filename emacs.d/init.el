;;; init --- general emacs startup

;;; Commentary:

;;; Code:

;; Startup optimization. Normally this would go in README.org, but we
;; want to have extra memory available for the actual babeling so we
;; do it here... and might as well do it first.

(defvar belak--gc-cons-threshold (* 16 1024 1024))
(defvar belak--gc-cons-upper-limit (* 256 1024 1024))

(setq gc-cons-threshold belak--gc-cons-upper-limit)
(add-hook 'emacs-startup-hook (lambda ()
                                (run-with-idle-timer
                                 3 nil (lambda () (setq-default gc-cons-threshold belak--gc-cons-threshold)))))

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

;; TODO: Actually install org-mode

(let ((debug-on-error t)
      (debug-on-quit t))

  ;; Now that we've bootstrapped what we can, load the actual config.
  (org-babel-load-file "~/.emacs.d/README.org")

  ;; Now that everything has been loaded, force a GC to try and clean
  ;; stuff up.
  (garbage-collect))
