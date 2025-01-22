;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "29")
  (error "Detected Emacs %s but only 29 and higher is supported"
	 emacs-version))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(let ((debug-on-error t)
      (debug-on-quit t))

  (require 'belak-lib)
  (require 'belak-core)
  (require 'belak-ui)
  (require 'belak-editor)
  (require 'belak-dev)

  (when IS-MAC
    (require 'belak-os-macos))
  )
