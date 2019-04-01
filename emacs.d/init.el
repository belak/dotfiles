(setq user-emacs-directory (file-name-directory load-file-name))

;; Add the modules directory to the load path and start loading
;; everything.
(add-to-list 'load-path (concat user-emacs-directory "modules"))

(let ((debug-on-error t)
      (debug-on-quit t))
  ;; Load core, followed by package management, followed by UI tweaks.
  (require 'belak-core)
  (require 'belak-packages)
  (require 'belak-ui)

  ;; We use an bizarre mix between evil and emacs. I try to use emacs
  ;; for navigation, but emacs for almost everything else.
  (require 'belak-evil)

  ;; There's a global setting which switches the enabled completion
  ;; module, but we load all of them so that setting will
  ;; work. Everything is in use-package blocks so this should still be
  ;; a fairly cheap operation.
  (require 'belak-completion-ido)
  (require 'belak-completion-ivy)
  (require 'belak-completion-helm)

  ;; Load additional features
  (require 'belak-org)

  ;; Load dev packages
  (require 'belak-dev)

  ;; Load language bundles
  (require 'belak-lang-elixir)
  (require 'belak-lang-go)
  (require 'belak-lang-javascript)
  (require 'belak-lang-python)
  (require 'belak-lang-rust)
  (require 'belak-lang-other)

  ;; Load any miscellaneous packages
  (require 'belak-misc))
