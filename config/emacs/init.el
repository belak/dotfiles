;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "29")
  (error "Detected Emacs %s but only 29 and higher is supported"
	 emacs-version))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; And awaaaaaaayyyy we go.
(let ((debug-on-error t)
      (debug-on-quit t)
      ;; Every require/load looks at this, so removing it gets us a small
      ;; performance improvement. However we do want it set after loading
      ;; everything, so we use `let' so these variables will return to normal
      ;; after this block.
      (file-name-handler-alist nil))

  (require 'belak-core)                 ; low level setup
  (require 'belak-ui)                   ; make things pretty... well, prettier
  (require 'belak-editor)               ; the text editing portion of this OS

  (when IS-MAC
    (require 'belak-os-macos))          ; macOS specific tweaks

  (require 'belak-dev)                  ; common development packages
  (require 'belak-lang)                 ; language modes and settings

  (require 'belak-org)                  ; the best reason to use emacs
  (require 'belak-tools)                ; utils too small for their own file
  )
