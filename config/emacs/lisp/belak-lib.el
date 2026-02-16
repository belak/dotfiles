;;; belak-lib.el -*- lexical-binding: t; -*-

(require 'subr-x)

;;
;;; Functions

(defun belak-visible-buffers ()
  (delete-dups (mapcar #'window-buffer (window-list))))

(defun belak-disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))


;;
;;; External Packages
;;
;; We actually want to export a number of packages from here to make it easier
;; to write other modules. This lets us avoid requiring `belak-core' as well as
;; `belak-lib' to make the byte-compiler happy even if we don't use them in this
;; file explicitly.

(eval-when-compile
  (require 'use-package))
(require 'blackout)


;;
;;; Constants

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (eq system-type 'gnu/linux))
(defconst IS-GUI      (display-graphic-p))
(defconst IS-PERSONAL (string-suffix-p ".elwert.dev" (system-name)))
(defconst IS-WORK     (not IS-PERSONAL))


;;
;;; Sulami Utility Functions
;;
;; Originally
;; https://github.com/sulami/dotfiles/blob/master/emacs/.emacs/README.org

(defun belak/get-face-at-point (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face
        (message "Face: %s" face)
      (message "No face at %d" pos))))


;;
;;; Macros

(defmacro after! (package &rest body)
  "Evaluate `BODY' after `PACKAGE' have loaded."
  (declare (indent defun))
  (let ((body (macroexp-progn body)))
    `(if (featurep ',package)
         ,body
       (eval-after-load ',package ',body))))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in-place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))


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


;;
;;; Tweaks

;; Disabling the additional `use-package` highlighting makes it so the package
;; names aren't highlighted, but since we define out own similar macros, this
;; saves us from having to declare the same highlighting on those as well.
(font-lock-remove-keywords 'emacs-lisp-mode use-package-font-lock-keywords)


(provide 'belak-lib)
;;; belak-lib.el ends here.
