;;; early-lib.el -*- lexical-binding: t; -*-

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

(provide 'belak-lib)
;;; belak-lib.el ends here.
