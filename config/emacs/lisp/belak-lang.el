;;; belak-lang.el -*- lexical-binding: t; -*-

(require 'belak-lib)
(require 'belak-dev)

;;
;;; C

(defun c-c++-header ()
  "Set either `c-mode' or `c++-mode', whichever is appropriate for header.

This function decides whether .h file is C or C++ header, sets
C++ by default because there's more chance of there being a .h
without a .cc than a .h without a .c (ie. for C++ template files)

This comes from
http://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode"
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; I much prefer the linux C style to the GNU style.
(setq c-default-style '((java-mode . "java")
                        (awk-mode  . "awk")
                        (other     . "linux")))


;;
;;; Emacs Lisp

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)
        ("M-."    . find-function-at-point)))

(use-package package-lint
  :commands package-lint)

(defun belak--eval-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-buffer)))

(bind-key "C-c :" 'belak--eval-region-or-buffer)


;;
;;; Go

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . belak--go-mode-hook)
         (go-mode . subword-mode)
         (go-mode . eglot-ensure))
  :config
  ;; Ignore go test -c output files
  (add-to-list 'completion-ignored-extensions ".test")

  ;; Prefer goimports to gofmt if installed
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))

  (defun belak--go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)))


;;
;;; Nix

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook ((nix-mode . subword-mode)
         (nix-mode . eglot-ensure))
  :config
  (after! eglot
    (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))))


;;
;;; Rust

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . belak--rust-mode-hook)
         (rust-mode . subword-mode)
         (rust-mode . eglot-ensure))
  :config
  (after! eglot
    (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer")))

  (defun belak--rust-mode-hook ()
    (add-hook 'before-save-hook 'eglot-format nil t)))

(use-package cargo
  :blackout cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))


;;
;;; Web

(use-package typescript-mode
  :blackout "Typescript"
  :mode ("\\.tsx?\\'" . typescript-mode))

(use-package css-mode
  :mode "\\.css\\'")

(use-package nxml-mode
  :mode "\\.xml\\'"
  :config
  ;; Automatically complete closing tags
  (setq nxml-slash-auto-complete-flag t))

;; Add support for lots of dynamic template types.
(use-package web-mode
  :after nxml-mode
  :mode
  "\\.erb\\'"
  "\\.hbs\\'"
  "\\.html?\\'"
  "\\.jinja\\'"
  "\\.mustache\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-current-element-highlight t))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2))


;;
;;; Other

(use-package bazel
  :mode
  ("\\.bzl\\'"   . bazel-mode)
  ("\\.bazel\\'" . bazel-mode)
  ("WORKSPACE"   . bazel-mode))

(use-package cmake-mode
  :mode
  "CMakeLists\\.txt\\'"
  "\\.cmake\\'")

(use-package dockerfile-mode
  :mode "Dockerfile")

(use-package git-modes
  :mode
  ("/\\.gitconfig\\'"     . gitconfig-mode)
  ("/\\.git/config\\'"    . gitconfig-mode)
  ("/\\.gitmodules\\'"    . gitconfig-mode)
  ("/\\.gitignore\\'"     . gitignore-mode)
  ("/\\.gitattributes\\'" . gitattributes-mode))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter
  ("python"  . python-mode)
  ("python2" . python-mode)
  ("python3" . python-mode)
  :hook (python-mode . subword-mode)
  :config
  (setq python-fill-docstring-style 'django))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :hook (ruby-mode . subword-mode))

(use-package sh-mode
  :mode
  "\\.zsh\\'"
  "\\.sh\\'"
  "zshrc\\'"
  "zshenv\\'")

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package toml-mode
  :mode
  "\\.toml\\'"
  "Pipfile\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . subword-mode))


;;
;;; Tweaks

;; Make the lisp mode names a bit shorter
(blackout 'lisp-interaction-mode "λ»")
(blackout 'emacs-lisp-mode "Eλ")
(blackout 'lisp-mode "λ")

;; Some extra files to support with basic modes
(push '("LICENSE\\'"   . text-mode)      auto-mode-alist)
(push '("\\.log\\'"    . text-mode)      auto-mode-alist)
(push '("\\.env\\'"    . sh-mode)        auto-mode-alist)

(provide 'belak-lang)
;;; belak-lang.el ends here
