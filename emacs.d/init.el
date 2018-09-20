;; init -- belak's emacs init.el
;;
;;; Commentary:

;;; Code:

;;; Basic Setup:
;;
;; This sets up some initial settings which are only used in this
;; file.  Specifically debugging and some profiling things.
;;
;; Define the start time so we can measure how long loading took
;; later.

;; package.el adds this automatically, but we handle this in belak-package
;(package-initialize)

(defconst emacs-start-time (current-time))

;; Set the gc-cons-threshold for init so we have some extra memory to
;; work with. This should be 25M. We also enable debug-on-error and
;; debug-on-quit to make it easier to debug startup errors.
(let ((gc-cons-threshold (* 25 1024 1024))
      (debug-on-error t)
      (debug-on-quit t)
      (load-dir (file-name-directory load-file-name)))

  ;; Core settings are defined here so all main tweaks can be done
  ;; from init.el
  (defvar belak/evil-enabled t "Set to nil to disable all evil related packages and settings")
  (defvar belak/evil-leader "," "Leader key for additional vim bindings")
  (defvar belak/theme 'base16 "Which theme is enabled. If this doesn't match an existing theme, none will be loaded")
  (defvar belak/helm-enabled nil "Wether helm is enabled or not")
  (defvar belak/ido-enabled t "Wether ido is enabled or not")

  ;; Most of the config is separated into packages which we store in
  ;; the lisp dir.
  (add-to-list 'load-path (expand-file-name "lisp" load-dir))

  ;; Helpers are useful functions that are used in other files. This
  ;; needs to be loaded first.
  (require 'belak-helpers)

  ;; Package setup needs to happen before we load any packages. Note
  ;; that this also loads use-package so we can start using it to load
  ;; other packages.
  (require 'belak-package)

  ;; Common contains core packages which are sometimes needed by other
  ;; packages.
  (require 'belak-common)

  ;; All dev related packages are included from here.
  (require 'belak-dev)

  ;; ido and helm are large enough that they're worth splitting into
  ;; separate files.
  (require 'belak-ido)

  ;; Various packages which don't fall into the other categories.
  (require 'belak-various)

  ;; Random settings unrelated to packages
  (require 'belak-settings))

;; Now that we're done loading everything, force a gc and print how long
;; loading took.
(garbage-collect)

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
