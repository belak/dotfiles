(setq-default
 ;; Update any file that would otherwise dump into the user emacs dir
 ;; to point to the local dir.
 auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" belak-local-dir)
 recentf-save-file (expand-file-name "recentf" belak-local-dir)

 ;; Disable history and backup files. They really just get in the way.
 ;;
 ;; TODO: change some of these to use directories instead.
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil

 ;; Security
 ;;
 ;; Increasing the minimum prime bits size to something larger than the
 ;; default settings stops all the GnuTLS warnings from showing up.
 gnutls-min-prime-bits 4096)
