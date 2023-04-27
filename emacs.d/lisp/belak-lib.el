;;; belak-lib.el -*- lexical-binding: t; -*-

(require 'subr-x)

;;
;;; Functions

(defun belak-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun belak-visible-buffers ()
  (delete-dups (mapcar #'window-buffer (window-list))))

(defun belak-buffers-in-mode (modes)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S)."
  (let ((modes (belak-enlist modes)))
    (cl-remove-if-not (lambda (buf)
                        (memq (buffer-local-value 'major-mode buf) modes))
                      (buffer-list))))

(defun belak-disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;; TODO: add a binding for this
(defun belak-copy-buffer ()
  "Copies the entire buffer to the kill-ring."
  (interactive)
  (copy-region-as-kill 1 (point-max)))

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

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-GUI   (display-graphic-p))


;;
;;; Sulami Utility Functions
;;
;; Originally
;; https://github.com/sulami/dotfiles/blob/master/emacs/.emacs/README.org

(defun belak-open-scratch-buffer ()
  "Opens the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun belak-open-message-buffer ()
  "Opens the message buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun belak-open-minibuffer ()
  "Focusses the minibuffer, if active."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (minibuffer-window))))

(defun belak-sort-words (beg end)
  "Sorts words in region."
  (interactive "r")
  (sort-regexp-fields nil "\\w+" "\\&" beg end))

(defun belak-what-face (pos)
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

(defmacro add-transient-hook! (hook &rest forms)
  "Attaches a self-removing function `NAME' to a given `HOOK'."
  (declare (indent 1))
  (let ((fn (gensym "belak--transient-")))
    `(progn
       (defun ,fn (&rest _)
         ,@forms
         (remove-hook ',hook #',fn)
         (unintern ',fn nil))
       (put ',fn 'permanent-local-hook t)
       (add-hook ',hook #',fn))))

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

(defmacro load-theme! (name &optional package &rest forms)
  (declare (indent defun))
  (let ((package-name (if package package (intern (format "%s-theme" name)))))
    `(use-package! ,package-name
       :demand t
       :config
       ,@forms

       ;; TODO: check if we need to hook after-frame-make-funcsions for the
       ;; daemon or if we can just use that for everything.

       (add-transient-hook! window-setup-hook (load-theme ',name t)))))
       ;;(add-transient-hook! emacs-startup-hook (load-theme ',name t)))))

(defmacro use-feature! (name &rest forms)
  "Like `use-package', but disables elpaca integration.
`NAME' and `FORMS' are as in `use-package'."
  (declare (indent defun))
  `(use-package! ,name
     :elpaca nil
     ,@forms))

(defmacro use-package! (name &rest forms)
  "An alias for `use-package'"
  (declare (indent defun))
  `(use-package ,name
     ,@forms))


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
