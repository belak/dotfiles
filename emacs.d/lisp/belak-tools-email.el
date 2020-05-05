;;; belak-tools-email -*- lexical-binding: t; -*-

(use-package mu4e
  :straight nil
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :commands mu4e
  :config
  (setq
   mue4e-headers-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachments-dir "~/Downloads"

   mu4e-maildir       "~/.maildir"   ;; top-level Maildir

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

(provide 'belak-tools-email)
;;; belak-tools-email.el ends here
