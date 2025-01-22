;;; belak-lib.el -*- lexical-binding: t; -*-

;;
;;; External Packages

;; We actually want to export a number of packages from here to make it easier
;; to write other modules. This lets us avoid requiring `belak-core' as well as
;; `belak-lib' to make the byte-compiler happy even if we don't use them in this
;; file explicitly.

(eval-when-compile
  (require 'use-package))
(require 'blackout)

;;
;;; Constants

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-GUI   (display-graphic-p))


;;
;;; Macros

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))


;;
;;; Functions

(defun belak-disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun belak-visible-buffers ()
  (delete-dups (mapcar #'window-buffer (window-list))))


;;
;;; Hooks

(defvar belak-switch-buffer-hook
  nil
  "A list of functions to be called when the current buffer has been changed.")

(defvar belak-switch-buffer-hook--last-buffer
  nil
  "The last current buffer.")

(defun run-belak-switch-buffer-hook ()
  (unless (eq (current-buffer)
              belak-switch-buffer-hook--last-buffer)
    (let ((current (current-buffer)))
      ;;(previous belak-switch-buffer-hook--last-buffer)
      (setq belak-switch-buffer-hook--last-buffer
            current)
      (run-hooks 'belak-switch-buffer-hook))))

(add-hook 'post-command-hook
          'run-belak-switch-buffer-hook)

(provide 'belak-lib)
;;; belak-lib.el ends here.
