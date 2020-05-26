;;; belak-editor.el --- text interaction settings -*- lexical-binding: t; -*-

(require 'belak-core)


;;
;;; Functions

(defun belak--smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.

This originally came from Sacha Chua's Emacs config."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                #'belak--smarter-move-beginning-of-line)

(defun belak--unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(bind-key "M-Q" #'belak--unfill-paragraph)

;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-editing-utils.el
(defun belak--kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(bind-key "C-M-<backspace>" #'belak--kill-back-to-indentation)

;;
;;; Packages

;; There are a few key binds which are close to being what we want, but not
;; quite, so we use `crux' as a utility library to fill in a few gaps.
(use-package crux
  :general
  ;;("C-k"     #'crux-smart-kill-line)  ; This doesn't work with C-u
  ("C-c k"   #'crux-kill-other-buffers)
  ("C-c f d" #'crux-delete-file-and-buffer)
  ("C-c f r" #'crux-rename-buffer-and-file))

(use-package ctrlf
  :general
  ([remap isearch-forward]         #'ctrlf-forward-literal)
  ([remap isearch-backward]        #'ctrlf-backward-literal)
  ([remap isearch-forward-regexp]  #'ctrlf-forward-regexp)
  ([remap isearch-backward-regexp] #'ctrlf-backward-regexp)
  :config
  ;; Clear out the bindings because we've already defined them.
  (setq ctrlf-mode-bindings '())
  (ctrlf-mode +1))

;; Often times you just want to move a full block around. This makes it easy to
;; select what you need.
(use-package expand-region
  :general
  ("C-="   #'er/expand-region)
  ("C-S-=" #'er/contract-region))

;; It's more standard to use C-n/C-p in Emacs rather than Up and Down, so we
;; warn whenever we use a key bind which has a more Emacs-y alternative.
(use-package guru-mode
  :blackout
  :hook (prog-mode . guru-mode)
  :config
  (setq guru-warn-only t))

;; Automatically clean up old buffers. This also provides a midnight-hook which
;; makes it possible to define cleanup functions.
(use-feature midnight
  :commands midnight-mode
  :init
  (add-transient-hook! pre-command-hook (midnight-mode 1)))

;; Highlight matching delimiters
(use-package paren
  :demand t
  ;;:after-call after-find-file doom-switch-buffer-hook
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package yasnippet
  :blackout yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :hook (text-mode . yas-minor-mode)
  :general
  ;; The implicit keybinds conflict with org-mode's cycling, so we switch it to
  ;; be more explicit.
  ("M-/" #'yas-expand
   :keymaps 'yas-minor-mode-map
   "<tab>" nil
   "TAB"   nil)
  :config
  ;; TODO: look at yas/hippie-expand integration
  ;; TODO: look at Sacha's change-cursor-color-when-can-expand

  ;; `no-littering' overrides the snippets dir and makes it harder to find, so
  ;; we change it back.
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)
          'yasnippet-snippets-dir)))

(use-package yasnippet-snippets
  :demand t
  :after yasnippet)

;;
;;; Tweaks

;; When region is active, make `capitalize-word' and friends act on it.

;; TODO: replace with general
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; Useful method of popping back to a previous location.
(bind-key "C-x p" #'pop-to-mark-command)

;; Trigger auto-fill after punctuation characters, not just whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; If a mode defines a comment, only autofill inside them.
(setq comment-auto-fill-only-comments t)

(blackout 'auto-fill-mode)

;; Make tab a tiny bit smarter - if the current line is already indented, then
;; complete at point.
(setq tab-always-indent 'complete)

;; Delete selected text when typing.
(delete-selection-mode 1)

;; Default indentation. Generally we fall back to editorconfig, but this is here
;; just in case.
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Clean up some various requirements that Emacs sometimes complains about or
;; handles.
(setq sentence-end-double-space nil
      require-final-newline t)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Avoid saving duplicates into the kill ring.
(setq kill-do-not-save-duplicates t)

;; Don't make distinctions between ASCII and siblings (like a and a
;; with an umlaut)
(setq search-default-mode 'char-fold-to-regexp)

;; Replace the default `newline' with `newline-and-indent'.
(bind-key "RET" #'newline-and-indent)

(provide 'belak-editor)
;;; belak-editor.el ends here.
