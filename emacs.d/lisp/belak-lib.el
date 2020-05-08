;;; belak-lib.el -*- lexical-binding: t; -*-

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

(defun file! ()
  "Return the Emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
	(load-file-name)
	((stringp (car-safe current-load-list))
	 (car current-load-list))
	(buffer-file-name)
	((error "Cannot get this file-path"))))

(defun dir! ()
  "Returns the directory of the Emacs lisp file this macro is called from."
  (when-let (path (file!))
	    (directory-file-name (file-name-directory path))))

(defmacro load! (filename)
  "Load a FILENAME relative to the current executing file."
  `(let (file-name-handler-alist)
     (load (expand-file-name ,filename (dir!)) nil 'nomessage)))

(defmacro add-transient-hook! (hook &rest forms)
  "Attaches a self-removing function NAME to a given HOOK."
  (declare (indent 1))
  (let ((fn (gensym "belak---transient-")))
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
    (let ((current (current-buffer))
          (previous belak-switch-buffer-hook--last-buffer))
      (setq belak-switch-buffer-hook--last-buffer
            current)
      (run-hooks 'belak-switch-buffer-hook))))

(add-hook 'post-command-hook
          'run-belak-switch-buffer-hook)

(provide 'belak-lib)
;; belak-lib.el ends here
