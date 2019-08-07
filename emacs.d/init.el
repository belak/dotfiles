;;; init --- general emacs startup

;;; Commentary:

;;; Code:

(setq user-emacs-directory (file-name-directory load-file-name))

;; Add the modules directory to the load path and start loading
;; everything.
(add-to-list 'load-path (concat user-emacs-directory "modules"))

(let ((debug-on-error t)
      (debug-on-quit t))
  ;; Load the core settings to make sure we have a solid base to start
  ;; with.
  (require 'belak-core)

  ;; First thing to do is set up package management. For me, it's straight.el.
  ;; We still call it packages because it's referring to the system, not
  ;; package.el. Naming is hard, alright?
  (require 'belak-packages)

  ;; Now that we've got a baseline to start with, we can start
  ;; tweaking the UI.
  (require 'belak-ui)

  ;; We use an bizarre mix between evil and emacs. I try to use emacs
  ;; for navigation, but emacs for almost everything else.
  ;;(require 'belak-evil)

  ;; It's nice to have a better interface for completing-read. ido is
  ;; a nice balance between features and speed.
  (require 'belak-ido)

  ;; Load additional features
  (require 'belak-org)

  ;; Load dev packages. This includes everything from project-based
  ;; navigation to autocomplete.
  (require 'belak-dev)

  ;; Load language bundles
  ;;(require 'belak-lang-elixir)
  (require 'belak-lang-go)
  (require 'belak-lang-javascript)
  (require 'belak-lang-python)
  (require 'belak-lang-rust)
  (require 'belak-lang-other)

  ;; Load any miscellaneous packages
  (require 'belak-misc))
