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

;; TODO: add a binding for this
(defun belak-copy-buffer ()
  "Copies the entire buffer to the kill-ring."
  (interactive)
  (copy-region-as-kill 1 (point-max)))


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

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in-place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro add-transient-hook! (hook &rest forms)
  "Attaches a self-removing function NAME to a given HOOK."
  (declare (indent 1))
  (let ((fn (gensym "belak--transient-")))
    `(progn
       (defun ,fn (&rest _)
         ,@forms
         (remove-hook ',hook #',fn)
         (unintern ',fn nil))
       (put ',fn 'permanent-local-hook t)
       (add-hook ',hook #',fn))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded."
  (declare (indent defun))
  (let ((body (macroexp-progn body)))
    `(if (featurep ',package)
         ,body
       (eval-after-load ',package ',body))))

(defmacro use-feature (name &rest args)
  "Like `use-package', but disables straight integration.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; Highlight use-feature the same as use-package
(defconst use-feature-font-lock-keywords
  '(("(\\(use-feature\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))
(font-lock-add-keywords 'emacs-lisp-mode use-feature-font-lock-keywords)

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
;; belak-lib.el ends here
