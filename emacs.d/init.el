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
      (debug-on-quit t)
      ;; Every require/load looks at this, so removing it gets us a small
      ;; performance improvement. However we do want it set after loading
      ;; everything, so we use let so this will return to normal after this
      ;; block.
      (file-name-handler-alist nil))
  (require 'belak-core)         ; low level setup
  (require 'belak-ui)           ; make things pretty... well, prettier
  (require 'belak-editor)       ; load the text editing portion of this OS
  (require 'belak-selectrum)    ; improvements on completing-read
  (require 'belak-org)          ; the best reason to use emacs

  ;; macOS specific tweaks
  (when IS-MAC
    (require 'belak-macos))

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

;; TODO: look into smartparens (see https://github.com/MatthewZMD/.emacs.d/blob/master/elisp/init-parens.el and Emacs Prelude for inspiration)
;; TODO: look into dired
;; TODO: look into hydra
;; TODO: look into enh-ruby-mode
;; TODO: look into yasnippet and a way to have default empty file buffers
;; TODO: look into visual-fill-column
;; TODO: look into undo-tree
;; TODO: look into focus-mode
;; TODO: look into eyebrowse
;; TODO: look into which-function-mode
;; TODO: look into the local-comment-auto-fill from prelude
;; TODO: look into .dir-locals.el over the hack in our elisp setup.
;; TODO: look into storing straight packages elsewhere
;; TODO: add back (cua-mode) shortcuts on macOS.
;; TODO: look into drag stuff mode
;; TODO: look into save-place
;; TODO: look into nerd-commenter
;; TODO: look into posframe, specifically flycheck-posframe
;; TODO: look into flycheck-pos-tip
;; TODO: look into recentf-auto-cleanup
;; TODO: look into lsp-mode and lsp-ui
;; TODO: look into dap-mode
;; TODO: look into popup-kill-ring
;; TODO: look into yasnippet
;; TODO: look into windmove, possibly with windmove-wrap-around
;; TODO: look into some method of jumping between windows
