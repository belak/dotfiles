;;; init --- general emacs startup

;;; Commentary:

;; This file does a bit more than just bootstrap into some other Lisp
;; files.  The goal of `init.el' in this setup is to set up anything
;; related to startup from optimization to profiling.

;;; Code:

;; Startup optimization.  Normally this would go in a module, but we
;; want to have extra memory available while loading packages, so we
;; take care of this first thing.
;;
;; It's worth noting that there are two variables here: threshold and
;; upper-limit.  The threshold is for general usage and is set to 16M.
;; The upper-limit should only be used during loading and is set to
;; 256M.  After startup is done, we automatically switch back to the
;; lower limit when it's been idle for 3 seconds.
(defvar belak--gc-cons-threshold (* 16 1024 1024))
(defvar belak--gc-cons-upper-limit (* 256 1024 1024))

(setq gc-cons-threshold belak--gc-cons-upper-limit)
(add-hook 'emacs-startup-hook (lambda ()
                                (run-with-idle-timer
                                 3 nil (lambda () (setq-default gc-cons-threshold belak--gc-cons-threshold)))))

;; Add a basic hook so we can tell how long loading emacs took.
(defun belak--display-benchmark ()
  "Display a basic benchmark with how long Emacs took to load."
  (message "Loaded in %.03fs"
       (float-time (time-subtract (current-time) before-init-time))))
(add-hook 'emacs-startup-hook 'belak--display-benchmark)

(let ((debug-on-error t)
      (debug-on-quit t)
      (load-dir (file-name-directory load-file-name)))

  ;; Most of the config is separated into packages which we store in
  ;; the lisp dir.
  (add-to-list 'load-path (expand-file-name "lisp" load-dir))

  (require 'belak-core)                 ; low level setup
  (require 'belak-package)              ; package manager settings
  (require 'belak-themes)               ; I may maintain too many themes
  (require 'belak-ui)                   ; make things pretty... well, prettier
  (require 'belak-ido)                  ; ido settings
  (require 'belak-dev)                  ; common development packages

  ;; TODO: maybe load based on a glob
  (require 'belak-lang-go)              ; "Google Golang"
  (require 'belak-lang-js)              ; Javascript settings
  (require 'belak-lang-python)          ; Not super sexy, but super stable
  (require 'belak-lang-other)           ; Smaller language settings

  ;; Now that everything has been loaded, force a GC to try and clean
  ;; stuff up.
  (garbage-collect))
