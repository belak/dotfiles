;;; belak-core-misc --- random low level setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Optimizations:

(defvar belak--gc-cons-threshold (* 16 1024 1024))

(use-package gcmh
  :defer 3
  :diminish
  :config
  (add-transient-hook! 'pre-command-hook (gcmh-mode +1))
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold belak--gc-cons-threshold
        gc-cons-percentage 0.1)
  (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect))

(defun belak--defer-garbage-collection-h ()
  "Disable GC, usually temporarily."
  (setq gc-cons-threshold most-positive-fixnum))

(defun belak--restore-garbage-collection-h ()
  "Re-enable GC after a short delay."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold belak--gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'belak--defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook  #'belak--restore-garbage-collection-h)

;;; Random:

;; Set up a few file name variables for later use. The goal is to get
;; as many extraneous files into directories, rather than dumping them
;; into .emacs.d.
;; TODO: look into using transient - I'm not sure where that came from
(defvar belak-emacs-dir user-emacs-directory)
(defvar belak-local-dir (file-name-as-directory (expand-file-name ".local" belak-emacs-dir)))

;; Disable history and backup files. They really just get in the way.
;; TODO: figure out which of these are useful and make sure they use
;; directories rather than files.
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil)

;; (defvar save-place-file)
;; (setq save-place-file (concat user-emacs-directory "places")
;;       backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
;;       auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; We still want to be able to have non-public configs, such as for
;; passwords and what not, so we put them in a separate file and load
;; it, but ignore errors, for instance if it doesn't exist. This has
;; the added advantage of making it so customizations will go to this
;; file and not to init.el, which is version controlled.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; recentf provides some useful functionality to ido.
(require 'recentf)
(setq recentf-max-saved-items 50)

;; This used to be necessary to fix emacs https connections on
;; Windows.  It's very possible it isn't needed any more.
(setq gnutls-min-prime-bits 4096)

;; Add a basic hook so we can tell how long loading emacs took.
(defun belak--display-benchmark-h ()
  "Display a basic benchmark with how long Emacs took to load."
  (message "Loaded in %.03fs"
           (float-time (time-subtract (current-time) before-init-time))))

(add-transient-hook! 'emacs-startup-hook #'belak--display-benchmark-h)

(provide 'belak-core-misc)

;;; belak-core-misc.el ends here
