;;; belak-modeline.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;; Make `%C' available in `mode-line-format' so we can show the column.
(column-number-mode 1)

(defun belak--mode-line-buffer-name ()
  "Return buffer name with dimmed project/path/ prefix and normal filename."
  (if-let* ((file    (buffer-file-name))
             (proj    (project-current))
             (root    (expand-file-name (project-root proj)))
             (project (file-name-nondirectory (directory-file-name root)))
             (rel     (file-relative-name file root))
             (prefix  (concat project "/" (file-name-directory rel))))
      (concat (propertize prefix 'face 'shadow)
              (file-name-nondirectory rel))
    (buffer-name)))

(defun belak--mode-line-vc ()
  "Return branch name with a colored dirty indicator."
  (when (and vc-mode (string-match "[-:]\\(.*\\)" vc-mode))
    (let* ((branch (match-string-no-properties 1 vc-mode))
           (state  (vc-state (buffer-file-name)))
           (indicator (pcase state
                        ('edited   (propertize "* " 'face '(:foreground "yellow")))
                        ('added    (propertize "* " 'face '(:foreground "green")))
                        ('removed  (propertize "* " 'face '(:foreground "red")))
                        ('conflict (propertize "* " 'face '(:foreground "magenta")))
                        (_         ""))))
      (concat " " indicator branch))))

(defun belak--mode-line-minor-modes ()
  "Return active minor mode indicators with full mode names as help-echo."
  (mapconcat
   (lambda (entry)
     (seq-let (mode indicator) entry
       (when (and (boundp mode) (symbol-value mode))
         (let ((str (format-mode-line indicator)))
           (unless (string-blank-p str)
             (propertize str 'help-echo (symbol-name mode)))))))
   minor-mode-alist ""))

(setq mode-line-right-align-edge 'right-margin)

(setq-default mode-line-format
              '("%e"
                " "
                (:eval (pcase (list buffer-read-only (buffer-modified-p))
                         ('(nil nil) (propertize "--" 'face 'shadow))
                         ('(nil t)   "**")
                         ('(t   nil) (propertize "%%" 'face 'shadow))
                         (_          "%*")))
                " "
                (:eval (belak--mode-line-buffer-name))
                " %l:%C"
                mode-line-format-right-align
                (:eval (belak--mode-line-vc))
                " "
                (:eval (propertize (format-mode-line mode-name)
                                   'help-echo (symbol-name major-mode)))
                (:eval (belak--mode-line-minor-modes))
                " "))


(provide 'belak-modeline)
;;; belak-modeline.el ends here
