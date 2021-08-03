;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Additional useful defines not included in Doom.
(defconst IS-GUI (display-graphic-p))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kaleb Elwert"
      user-mail-address "belak@coded.io")

;; UI
(cond
  (IS-MAC
    (setq doom-font                "Monaco"
          doom-variable-pitch-font "Monaco"))
  (IS-LINUX
    (setq doom-font                "Terminus 12"
          doom-variable-pitch-font "Terminus 12")))

;; Load our theme and tweak a number of things about it to my preference.
(setq doom-theme                  'modus-vivendi
      modus-themes-no-mixed-fonts t
      modus-themes-completions    'opinionated
      modus-themes-fringes        'subtle
      modus-themes-lang-checkers  '(background straight-underline text-also)
      modus-themes-mode-line      '(accented)
      modus-themes-prompts        '(background))

;; Italics look pretty rough in most terminals (at least with the fonts I use),
;; so we make sure they're only enabled if we have a full GUI.
(when IS-GUI
  (setq modus-themes-italic-constructs t))

;; Disable icons in the modeline - this makes it a little smaller and causes it
;; to use normal characters instead.
(setq doom-modeline-icon nil
      doom-modeline-buffer-file-name-style 'truncate-upto-root)

(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq uniquify-buffer-name-style  'forward
      uniquify-strip-common-suffix nil)

;; Load zap-up-to-char and define reverse-zap-up-to-char to be used in keybinds
;; below.
(autoload 'zap-up-to-char "misc" "" 'interactive)

(defun reverse-zap-up-to-char (char)
  "Zap back to CHAR."
  (interactive "Zap back to char: ")
  (zap-up-to-char -1 char))
(global-set-key "\M-Z" 'reverse-zap-up-to-char)

(map!
 ;; Allow C-a and C-e to work in normal mode as well.
 ;; TODO: make sure they also enter insert mode
 :n "C-a" #'doom/backward-to-bol-or-indent
 :n "C-e" #'doom/forward-to-last-non-comment-or-eol

 ;; Replace zap-to-char with zap-up-to-char because I find it easier to grok.
 :g [remap zap-to-char] #'zap-up-to-char
 :g "M-S-z"             #'reverse-zap-up-to-char

 ;; Make home and end do the same thing as C-a/C-e rather than going to the
 ;; beginning/end of a buffer.
 :g "<home>" #'doom/backward-to-bol-or-indent
 :g "<end>"  #'doom/forward-to-last-non-comment-or-eol)

;;(after! ido
;;  (setq ido-use-virtual-buffers t))

(after! org
  (setq org-support-shift-select t           ; Re-enable shift-select
        org-insert-heading-respect-content t ; Insert new headings, even when inside one
        org-adapt-indentation 'headline-data ; We don't want text inside headings to be indented
        org-special-ctrl-k t                 ; Make C-k behave specially in headlines

        ;; org-agenda settings
        org-log-done t                   ; TODO: ensure this works
        org-log-done-with-time t         ; Include time as well as date when closing tasks
        org-log-refile t                 ; TODO: ensure this works
        org-agenda-dim-blocked-tasks t)) ; Make tasks in the blocked state dim

(after! orderless
  (setq orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp)))
