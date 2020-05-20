;;; belak-tools -*- lexical-binding: t; -*-

(require 'belak-core)

;;
;;; Email

(use-feature mu4e
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

(use-package esup
  :commands esup)

;; This provides functionality similar to soulver on macOS, but we can use it
;; everywhere.
(use-package literate-calc-mode
  :mode ("\\.calc\\'" . literate-calc-mode))

;; vterm is like all the built-in terminals, but even better because it uses
;; libvterm which is pretty solid and handles most control sequences really
;; well.
(use-package vterm
  :commands vterm)

(provide 'belak-tools)
;;; belak-tools.el ends here
