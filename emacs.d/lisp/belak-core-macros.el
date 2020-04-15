;;; belak-core-macros --- random macros -*- lexical-binding: t; -*-

;;; Commentary:

;; A large portion of these macros are based off similar macros in
;; doom-emacs and were ported over because they're very nice to work
;; with.

;;; Code:

(defvar belak--transient-counter 0)
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.
FORMS are evaluated once, when that function/hook is first
invoked, then never again. HOOK-OR-FUNCTION can be a quoted hook
or a sharp-quoted function (which will be advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "belak--transient-%d-h" (cl-incf belak--transient-counter)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" hook-or-function)
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))

       ;; Attach the hook.
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(provide 'belak-core-macros)

;;; belak-core-macros.el ends here
