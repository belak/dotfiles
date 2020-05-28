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

(use-package writeroom-mode
  :commands belak-toggle-writeroom
  :config
  ;; We only want writeroom for the distraction-free part, not the "mess with
  ;; your other settings part". It also does a terrible job of cleaning it up
  ;; and restoring them so we just want our own toggle function.
  (setq writeroom-global-effects nil
        writeroom-mode-line t)

  (defun belak-toggle-writeroom ()
    (interactive)
    (if writeroom-mode
        (progn
          (display-line-numbers-mode 1)
          (writeroom-mode -1))
      (progn
        (display-line-numbers-mode -1)
        (writeroom-mode 1)))))

(use-package free-keys
  :commands free-keys)

;; NOTE: this should not be loaded by default because there's at least somewhat
;; of a performance penalty.
(use-package keyfreq
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line))

  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; This provides functionality similar to soulver on macOS, but we can use it
;; everywhere.
(use-package literate-calc-mode
  :mode ("\\.calc\\'" . literate-calc-mode))

;; TODO: add shackle rules for these windows
(use-feature net-utils
  :bind
  (:map mode-specific-map
        :prefix-map net-utils-prefix-map
        :prefix "n"
        ("p" . ping)
        ("i" . ifconfig)
        ("w" . iwconfig)
        ("n" . netstat)
        ("p" . ping)
        ("a" . arp)
        ("r" . route)
        ("h" . nslookup-host)
        ("d" . dig)
        ("s" . smbclient)
        ("t" . traceroute))
  :config
  (when (or IS-MAC IS-LINUX)
    (setq ping-program-options '("-c" "4"))))

;; vterm is like all the built-in terminals, but even better because it uses
;; libvterm which is pretty solid and handles most control sequences really
;; well.
(use-package vterm
  :commands vterm)

(provide 'belak-tools)
;;; belak-tools.el ends here
