;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! modus-themes)
(package! monokai-pro-theme)
(package! grayscale-theme)

(package! bazel)
(package! protobuf-mode)
(package! rainbow-mode)

;; buffer-name-relative is needed as a workaround because the uniquify settings
;; don't work with persp-mode, which doom currently uses for workspaces.
(package! buffer-name-relative)
