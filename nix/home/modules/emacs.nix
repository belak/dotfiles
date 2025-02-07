{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.emacs;
in
{
  options.belak.emacs = {
    enable = lib.mkEnableOption "emacs";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs;
      let
        # We use the pure GTK variant of emacs to get better Wayland support.
        #
        # XXX: unfortunately there are some issues with the pgtk build on macOS,
        # so we just use the normal emacs build there.
        myEmacs = if pkgs.stdenv.isDarwin then pkgs.emacs else pkgs.emacs29-pgtk;
        emacsWithPackages = (emacsPackagesFor myEmacs).emacsWithPackages;
      in
      [
        (emacsWithPackages (
          epkgs:
          [
            epkgs.elpaPackages.rainbow-mode
            epkgs.elpaPackages.undo-tree

            epkgs.melpaPackages.ace-window
            epkgs.melpaPackages.bazel
            epkgs.melpaPackages.blacken
            epkgs.melpaPackages.blackout
            epkgs.melpaPackages.cargo
            epkgs.melpaPackages.cmake-mode
            epkgs.melpaPackages.consult
            epkgs.melpaPackages.corfu
            epkgs.melpaPackages.ctrlf
            epkgs.melpaPackages.dashboard
            epkgs.melpaPackages.diff-hl
            epkgs.melpaPackages.dimmer
            epkgs.melpaPackages.dockerfile-mode
            epkgs.melpaPackages.doom-modeline
            epkgs.melpaPackages.editorconfig
            epkgs.melpaPackages.emmet-mode
            epkgs.melpaPackages.envrc
            epkgs.melpaPackages.esup
            epkgs.melpaPackages.evil
            epkgs.melpaPackages.evil-collection
            epkgs.melpaPackages.evil-commentary
            epkgs.melpaPackages.evil-leader
            epkgs.melpaPackages.evil-rsi
            epkgs.melpaPackages.evil-surround
            epkgs.melpaPackages.exec-path-from-shell
            epkgs.melpaPackages.expand-region
            epkgs.melpaPackages.flycheck
            epkgs.melpaPackages.flycheck-golangci-lint
            epkgs.melpaPackages.flycheck-rust
            epkgs.melpaPackages.free-keys
            epkgs.melpaPackages.gcmh
            epkgs.melpaPackages.git-link
            epkgs.melpaPackages.git-modes
            epkgs.melpaPackages.go-mode
            epkgs.melpaPackages.go-tag
            epkgs.melpaPackages.guru-mode
            epkgs.melpaPackages.haskell-mode
            epkgs.melpaPackages.helpful
            epkgs.melpaPackages.highlight-escape-sequences
            epkgs.melpaPackages.hl-todo
            epkgs.melpaPackages.js2-mode
            epkgs.melpaPackages.json-mode
            epkgs.melpaPackages.literate-calc-mode
            epkgs.melpaPackages.lua-mode
            epkgs.melpaPackages.macrostep
            epkgs.melpaPackages.magit
            epkgs.melpaPackages.marginalia
            epkgs.melpaPackages.markdown-mode
            epkgs.melpaPackages.modus-themes
            epkgs.melpaPackages.multiple-cursors
            epkgs.melpaPackages.nix-mode
            epkgs.melpaPackages.no-littering
            epkgs.melpaPackages.nov
            epkgs.melpaPackages.ob-restclient
            epkgs.melpaPackages.orderless
            # org
            # org-contrib
            # org-roam
            epkgs.melpaPackages.package-lint
            epkgs.melpaPackages.page-break-lines
            epkgs.melpaPackages.php-mode
            epkgs.melpaPackages.pip-requirements
            epkgs.melpaPackages.projectile
            epkgs.melpaPackages.protobuf-mode
            epkgs.melpaPackages.py-isort
            epkgs.melpaPackages.pyenv-mode
            epkgs.melpaPackages.python-switch-quotes
            epkgs.melpaPackages.restclient
            epkgs.melpaPackages.rg
            epkgs.melpaPackages.rust-mode
            epkgs.melpaPackages.shackle
            epkgs.melpaPackages.toml-mode
            epkgs.melpaPackages.transient
            epkgs.melpaPackages.typescript-mode
            epkgs.melpaPackages.vertico
            epkgs.melpaPackages.vterm
            epkgs.melpaPackages.web-mode
            epkgs.melpaPackages.yaml-mode
            epkgs.melpaPackages.yasnippet
            epkgs.melpaPackages.yasnippet-snippets
          ]
          ++ lib.optionals stdenv.isDarwin [
            epkgs.melpaPackages.ns-auto-titlebar
          ]
        ))
      ];
  };
}
