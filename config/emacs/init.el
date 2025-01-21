;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(let ((debug-on-error t)
      (debug-on-quit t))

  (require 'belak-lib)
  (require 'belak-core)
  (require 'belak-ui)
  (require 'belak-editor)

  (when IS-MAC
    (require 'belak-os-macos))
  )
