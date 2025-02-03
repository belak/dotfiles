;;; belak-tools.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Email

(use-package mu4e
  :load-path ("/usr/local/share/emacs/site-lisp/mu/mu4e"
              "~/.nix-profile/share/emacs/site-lisp/mu4e")
  :commands mu4e
  :config
  (setq
   mu4e-headers-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-compose-format-flowed nil
   mu4e-date-format-long "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachment-dir "~/Downloads"

   ;; note that these folders below must start with / the paths are
   ;; relative to maildir root
   mu4e-refile-folder "/Archive"
   mu4e-sent-folder   "/Sent"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder  "/Trash")

  ;; this setting allows to re-sync and re-index mail by pressing U
  (setq mu4e-get-mail-command  "mbsync -a")

  (fset 'my-move-to-trash "mTrash")
  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash))


;;
;;; Various Tools

;; Provide something useful to make it easier to jump to other files at startup.
(use-package dashboard
  :demand t
  :config
  ;; Ensure `emacsclient' starts up with the dashboard.
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

  (setq dashboard-startup-banner 'logo
        dashboard-set-navigator t
        dashboard-set-footer nil
        dashboard-center-content t)

  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))

  (dashboard-setup-startup-hook))

(use-package envrc
  :demand t
  :config
  (envrc-global-mode))

;; Basic tooling for profiling startup
(use-package esup
  :commands esup)

;; Useful when trying to find an available key to bind something to.
(use-package free-keys
  :commands free-keys)

;; I don't use this very often, but it's helpful when needed.
(use-package git-link
  :commands git-link
  :config
  ;; Use the commit hash rather than the branch name in the URL.
  (setq git-link-use-commit t))

;; Replace the default help buffers with helpful because it's much prettier.
(use-package helpful
  :disabled t
  :bind (;; Remap standard commands.
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-symbol]   . #'helpful-symbol)
         ([remap describe-key]      . #'helpful-key)

         ;; Suggested bindings from the documentation at
         ;; https://github.com/Wilfred/helpful.

         :map help-map
         ("F"   . #'helpful-function)
         ("M-f" . #'helpful-macro)
         ("C"   . #'helpful-command)

         :map global-map
         ("C-c C-d" . #'helpful-at-point)))

;; This provides functionality similar to soulver on macOS, but we can
;; use it on other OSes as well.
(use-package literate-calc-mode
  :mode ("\\.calc\\'" . literate-calc-mode))

;; This allows us to read epubs in Emacs.
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;; rainbow-mode makes it easier to see colors, but it's a bit
;; overwhelming so it's left to be called when needed.
(use-package rainbow-mode
  :commands rainbow-mode)

;; Restclient gives us something similar to Postman, but in Emacs.
(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (setq restclient-log-request t))

(use-package ob-restclient
  :after org
  :config
  (after! org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t)))))

;; Package `rg' just provides an interactive command `rg' to run the search tool
;; of the same name.
(use-package rg
  :bind (("C-c k" . #'belak--rg))
  :commands (rg rg-run)
  :init
  (defun belak--rg (&optional only-current-type)
    "Search for string in current project.
With ONLY-CURRENT-TYPE non-nil, or interactively with prefix
argument, search only in files matching current type."
    (interactive "P")
    (rg-run (rg-read-pattern nil)
            (if only-current-type (car (rg-default-alias)) "*")
            (rg-project-root buffer-file-name))))

;; vterm is like all the built-in terminals, but even better because
;; it uses libvterm which is pretty solid and handles most control
;; sequences really well.
(use-package vterm
  :commands vterm)

(provide 'belak-tools)
;;; belak-tools.el ends here.
