;;; belak-various.el --- any leftover settings or packages which don't fit anywhere else

;;; Commentary:

;;; Code:

(use-package ag)

;; anzu shows how many matches in isearch. This should be loaded after
;; spaceline so we know to disable the additional things anzu puts
;; into the modeline.

(use-package anzu
  :demand
  :diminish anzu-mode
  :config
  (when (fboundp 'spaceline-install)
    (setq anzu-cons-mode-line-p nil))
  (global-anzu-mode))

(use-package flyspell
  :diminish flyspell-mode
  :config (add-hook 'text-mode-hook (lambda () (flyspell-mode 1))))

;; I originally used fic-mode, but it appears that hl-todo is a little
;; better and is updated more frequently.

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package hlinum
  :config
  (hlinum-activate))

;; Paradox is a replacement for =package-list-packages= offering a few
;; extra features. Note that we only load it on those commands because
;; it's something that only really matters when we manually start it
;; up.

(use-package paradox
  :commands
  paradox-list-packages
  paradox-enable
  :config
  (setq paradox-automatically-star t)
  (setq paradox-execute-asynchronously t)

  ;; Paradox is much more useful in emacs mode than evil mode because
  ;; it rebinds so many things.
  (when (fboundp 'evil-mode)
    (add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)))

;; persistent-scratch makes it possible to use the scratch buffer
;; without worrying about losing it.

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package simple-mpc
  :general
  ("C-c m" 'simple-mpc)
  ;;(when (fboundp 'evil-mode)
  ;;  (general-nmap :prefix belak/evil-leader
  ;;                "m" 'simple-mpc))
  :config
  (when (fboundp 'evil-mode)
    (add-hook 'simple-mpc-mode-hook 'evil-emacs-state)))

(use-package slime
  :config
  (setq slime-contribs '(fancy)))

;; In spite of the name, I use this to make sure that when I scroll,
;; there are still lines between the cursor and the top of the file.

(use-package smooth-scrolling
  :disabled t
  :config
  (setq smooth-scroll-margin 5
        ;; scroll-conservatively 101
        ;; scroll-preserve-screen-position t
        ;; auto-window-vscroll nil
        ;; scroll-margin 1
        ;; scroll-step 1
        ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
        ;; mouse-wheel-progressive-speed t
        ;; mouse-wheel-follow-mouse t
        )
  (smooth-scrolling-mode 1))

;; undo-tree makes the undo features a bit more bearable.

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package highlight-indentation
  :diminish highlight-identation-mode
  :config
  (add-hook 'python-mode-hook 'highlight-indentation-mode))

;; Dim the font color of text in surrounding paragraphs.

;; (use-package focus)

;; This section includes packages which are built into emacs (with a few
;; exceptions sitting outside this block).

;; Highlight the current column

(global-hl-line-mode)

(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis
        show-paren-delay 0))

;; recentf adds some useful functionality to ido which remembers
;; previously opened files.

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 50)

  ;;(add-to-list 'recentf-exclude "[/\\]\\.elpa/")
  ;;(add-to-list 'recentf-exclude "[/\\]\\.ido\\.last\\'" )
  ;;(add-to-list 'recentf-exclude "[/\\]\\.git/")
  ;;(add-to-list 'recentf-exclude ".*\\.gz\\'")
  ;;(add-to-list 'recentf-exclude ".*-autoloads\\.el\\'")
  ;;(add-to-list 'recentf-exclude "[/\\]archive-contents\\'")
  ;;(add-to-list 'recentf-exclude "[/\\]\\.loaddefs\\.el\\'")
  ;;(add-to-list 'recentf-exclude "url/cookies")
  ;;(add-to-list 'recentf-exclude ".*\\emacs.bmk\\'")
  ;;(add-to-list 'recentf-exclude "README\\.el\\'")
  ;;(add-to-list 'recentf-exclude "/custom\\.el\\'")

  (recentf-mode 1))

;; Save the last location when you leave a file.

(use-package saveplace
  :ensure nil
  :config
  (setq-default save-place t))

;; Ensure we're using sane buffer naming

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-strip-common-suffix nil))

(use-package whitespace
  :ensure nil
  :config
  (setq whitespace-style '(trailing face tabs tab-mark lines-tail)
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [182 10])
                                      (tab-mark 9 [9655 9] [92 9])))
  (global-whitespace-mode t)
  (setq whitespace-global-modes '(text-mode prog-mode org-mode)))

(provide 'belak-various)

;;; belak-various.el ends here
