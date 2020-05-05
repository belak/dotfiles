;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "26.1")
  (error "Detected Emacs %s but only 26.1 and higher is supported"
	 emacs-version))

;; Support for early-init was added in Emacs 27 so in order to
;; properly support Emacs 26, we need to manually load it.
(when (< emacs-major-version 27)
  (load (concat user-emacs-directory "early-init") nil 'nomessage))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; And awaaaaaaayyyy we go.
(let ((debug-on-error t)
      (debug-on-quit t))
  (require 'belak-core)         ; low level setup  
  (require 'belak-ui)           ; make things pretty... well, prettier
  (require 'belak-editor)       ; load the text editing portion of this OS
  (require 'belak-evil)         ; muahahahaha
  (require 'belak-ido)          ; improvements on completing-read
  (require 'belak-org)          ; the best reason to use emacs
  
  (require 'belak-dev)          ; common development packages

  (require 'belak-lang-c)       ; always a classic
  (require 'belak-lang-elisp)   ; Parens. Parens everywhere.
  (require 'belak-lang-elixir)  ; better erlang
  (require 'belak-lang-go)      ; "Google Golang"
  (require 'belak-lang-python)  ; not super sexy, but super stable
  (require 'belak-lang-rust)    ; memory safety at the cost of sanity
  (require 'belak-lang-web)     ; the tubes
  (require 'belak-lang-other)   ; smaller language settings
  (require 'belak-tools-email)) ; mu4e and friends
