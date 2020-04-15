;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This file does a bit more than just bootstrap into some other Lisp
;; files.  The goal of `init.el' in this setup is to set up anything
;; related to startup from optimization to profiling.

;;; Code:

;; Startup optimization.  Normally this would go in a module, but we
;; want to have extra memory available while loading packages, so we
;; take care of this first thing.
;;
;; Note that if this is not changed later, this will cause stuttering
;; and other issues. We use gcmh-mode to handle running a GC when
;; we're not using emacs.
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

;; A second optimization is removal of all file handlers until after
;; init.
(defvar belak--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun belak--restore-file-name-handler-alist-h ()
  "Restore the startup optimizations we previously made."
  (setq file-name-handler-alist belak--file-name-handler-alist))

(add-hook 'emacs-startup-hook #'belak--restore-file-name-handler-alist-h)

;; Because we're loading from this `init.el' we can assume this is the
;; emacs directory.  There are a few packages that do dumb string
;; concatenation so we need to make sure there's a slash at the end.
(setq user-emacs-directory
      (file-name-as-directory (file-name-directory load-file-name)))

;; Add the lisp directory to our load-path so we can load our own
;; modules.  Most of the config is separated into packages which we
;; store in the lisp dir.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; And awaaaaaaayyyy we go.
(let ((debug-on-error t)
      (debug-on-quit t))
  (require 'belak-core)         ; low level setup
  (require 'belak-early)        ; misc things to load early
  (require 'belak-themes)       ; I may maintain too many themes
  (require 'belak-ui)           ; make things pretty... well, prettier
  (require 'belak-evil)         ; muahahahaha
  (require 'belak-ido)          ; ido settings
  (require 'belak-dev)          ; common development packages

  (require 'belak-lang-c)       ; always a classic
  (require 'belak-lang-elixir)  ; better erlang
  (require 'belak-lang-go)      ; "Google Golang"
  (require 'belak-lang-js)      ; Javascript settings
  (require 'belak-lang-org)     ; the best reason to use Emacs
  (require 'belak-lang-python)  ; not super sexy, but super stable
  (require 'belak-lang-rust)    ; memory safety at the cost of sanity
  (require 'belak-lang-other)   ; smaller language settings

  (require 'belak-email)        ; email in emacs

  (require 'belak-misc))        ; anything that doesn't fit elsewhere
