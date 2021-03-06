;;; belak-tools.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Email

(use-feature! mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
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
(use-package! dashboard
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

(use-package! esup
  :commands esup)

(use-package! free-keys
  :commands free-keys)

(use-package! git-link
  :commands git-link
  :config
  ;; Use the commit hash rather than the branch name in the URL.
  (setq git-link-use-commit t))

;; Replace the default help buffers with helpful because it's much prettier.
(use-package! helpful
  :bind
  ("C-h f" . helpful-function)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key))

;; This provides functionality similar to soulver on macOS, but we can
;; use it everywhere.
(use-package! literate-calc-mode
  :mode ("\\.calc\\'" . literate-calc-mode))

;; This allows us to read epubs in Emacs.
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode))

;; rainbow-mode makes it easier to see colors, but it's a bit
;; overwhelming so it's left to be called when needed.
(use-package! rainbow-mode
  :commands rainbow-mode)

(use-package! ranger
  :commands ranger)

;; Restclient gives us something similar to Postman, but in Emacs.
(use-package! restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (setq restclient-log-request t))

;; vterm is like all the built-in terminals, but even better because
;; it uses libvterm which is pretty solid and handles most control
;; sequences really well.
(use-package! vterm
  :commands vterm)

(provide 'belak-tools)
;;; belak-tools.el ends here.
