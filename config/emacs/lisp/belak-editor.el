;;; belak-editor.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

;; TODO: take a look at bufler

;; Revert buffers to their state on disk when they change. Note that this is a
;; tweaked version of what ships with doom-emacs to simplify a number of things.
(use-package autorevert
  :blackout
  ;; revert buffers when their files/state have changed
  :hook (focus-in            . belak--auto-revert-buffers-h)
  :hook (after-save          . belak--auto-revert-buffers-h)
  :hook (belak-switch-buffer . belak--auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; Instead of using `auto-revert-mode' or `global-auto-revert-mode', we employ
  ;; lazy auto reverting on `focus-in-hook' and `belak-switch-buffer-hook'.
  ;;
  ;; This is because autorevert abuses the heck out of inotify handles which can
  ;; grind Emacs to a halt if you do expensive IO (outside of Emacs) on the
  ;; files you have open (like compression). We only really need to revert
  ;; changes when we switch to a buffer or when we focus the Emacs frame.
  (defun belak--auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (auto-revert-handler)))

  (defun belak--auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (belak-visible-buffers))
      (with-current-buffer buf
        (belak--auto-revert-buffer-h)))))

(use-package ctrlf
  :bind
  ([remap isearch-forward]         . ctrlf-forward-literal)
  ([remap isearch-backward]        . ctrlf-backward-literal)
  ([remap isearch-forward-regexp]  . ctrlf-forward-regexp)
  ([remap isearch-backward-regexp] . ctrlf-backward-regexp)
  :config
  ;; Clear out the bindings because we've already defined them.
  (setq ctrlf-mode-bindings '())
  (ctrlf-mode +1))

;; Allow C-c C-g to always quit the minibuffer.
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Often times you just want to move a full block around. This makes it easy to
;; select what you need.
(use-package expand-region
  :bind
  ("C-="   . er/expand-region)
  ("C-S-=" . er/contract-region))

;; It's more standard to use C-n/C-p in Emacs rather than Up and Down, so we
;; warn whenever we use a key bind which has a more Emacs-y alternative.
(use-package guru-mode
  :disabled t
  :blackout
  :hook (prog-mode . guru-mode)
  :config
  (setq guru-warn-only t))

;; Automatically clean up old buffers. This also provides a midnight-hook which
;; makes it possible to define cleanup functions.
(use-package midnight
  :commands midnight-mode
  :hook (after-init . midnight-mode))

;; Multiple cursors can be very powerful when used right.
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->"         . mc/mark-next-like-this)
  ("C-<"         . mc/mark-previous-like-this)
  ("C-c C-<"     . mc/mark-all-like-this))

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

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq history-length t
        history-delete-duplicates t
        savehist-save-minibuffer-history 1))

;; We use subword mode in a few `prog-mode' major modes like Go, but we want it
;; hidden, so we black it out here.
(use-package subword :blackout)

(use-package undo-tree
  :disabled t
  :defer nil
  :blackout
  :bind
  ("C-c u" . undo-tree-visualize)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package yasnippet
  :blackout yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :hook (text-mode . yas-minor-mode)
  :bind (
         ;; The implicit keybinds conflict with org-mode's cycling, so we switch
	     ;; it to be more explicit.
	     ("M-/" . yas-expand)
	     (:map yas-minor-mode-map
	           ("<tab>" . nil)
	           ("TAB"   . nil)))
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
;;; Functions

(defun belak/keyboard-quit-dwim (&optional interactive)
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When the Completions buffer is selected, close it.
- When a minibuffer is open, but not focused, close the minibuffer.
- In every other case use the regular `keyboard-quit'.

This was originally from https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/"
  (interactive)
  (cond ((region-active-p)
	 (keyboard-quit))
	((derived-mode-p 'completion-list-mode)
	 (delete-completion-window))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(t
	 (keyboard-quit))))
(global-set-key [remap keyboard-quit] #'belak/keyboard-quit-dwim)

(defun belak/move-beginning-of-line-dwim (arg)
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
(global-set-key [remap move-beginning-of-line] #'belak/move-beginning-of-line-dwim)

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
;;; Tweaks

;; Prevent accidental usage of `list-buffers'.
(bind-key "C-x C-b" #'switch-to-buffer)

;; When region is active, make `capitalize-word' and friends act on it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

;; Add an alternate keybind for commenting.
(bind-key "C-c /" #'comment-dwim)

;; It's much more useful to kill the current buffer rather than the whole frame,
;; similar to what other applications do.
(bind-key "s-w" #'kill-this-buffer)

;; I don't think I've ever needed the font panel in emacs, let alone bound to
;; something as easy to type as this.

(unbind-key "s-t")

;; Trigger auto-fill after punctuation characters, not just whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; Make tab a tiny bit smarter - if the current line is already indented, then
;; complete at point.
(setq tab-always-indent 'complete)

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

(provide 'belak-editor)
;;; belak-editor.el ends here.
