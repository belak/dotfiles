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
        myEmacs = if pkgs.stdenv.isDarwin then pkgs.emacs else pkgs.emacs-pgtk;
        emacsWithPackages = (emacsPackagesFor myEmacs).emacsWithPackages;
      in
      [
        claude-code-acp

        (emacsWithPackages (
          epkgs:
          [
            epkgs.treesit-grammars.with-all-grammars

            epkgs.elpaPackages.breadcrumb
            epkgs.elpaPackages.rainbow-mode

            epkgs.melpaPackages.ace-window
            epkgs.melpaPackages.agent-shell
            epkgs.melpaPackages.bazel
            epkgs.melpaPackages.blackout
            epkgs.melpaPackages.cargo
            epkgs.melpaPackages.cmake-mode
            epkgs.melpaPackages.consult
            epkgs.melpaPackages.corfu
            epkgs.melpaPackages.dashboard
            epkgs.melpaPackages.auto-dim-other-buffers
            epkgs.melpaPackages.diff-hl
            epkgs.melpaPackages.dockerfile-mode
            epkgs.melpaPackages.doom-modeline
            epkgs.melpaPackages.editorconfig
            epkgs.melpaPackages.envrc
            epkgs.melpaPackages.expand-region
            epkgs.melpaPackages.git-link
            epkgs.melpaPackages.git-modes
            epkgs.melpaPackages.go-mode
            epkgs.melpaPackages.haskell-mode
            epkgs.melpaPackages.highlight-escape-sequences
            epkgs.melpaPackages.hl-todo
            epkgs.melpaPackages.idle-highlight-mode
            epkgs.melpaPackages.json-mode
            epkgs.melpaPackages.lua-mode
            epkgs.melpaPackages.macrostep
            epkgs.melpaPackages.magit
            epkgs.melpaPackages.marginalia
            epkgs.melpaPackages.markdown-mode
            epkgs.melpaPackages.modus-themes
            epkgs.melpaPackages.multiple-cursors
            epkgs.melpaPackages.nix-mode
            epkgs.melpaPackages.no-littering
            epkgs.melpaPackages.orderless
            epkgs.melpaPackages.package-lint
            epkgs.melpaPackages.page-break-lines
            epkgs.melpaPackages.protobuf-mode
            epkgs.melpaPackages.rust-mode
            epkgs.melpaPackages.terraform-mode
            epkgs.melpaPackages.toml-mode
            epkgs.melpaPackages.transient
            epkgs.melpaPackages.treesit-auto
            epkgs.melpaPackages.typescript-mode
            epkgs.melpaPackages.verb
            epkgs.melpaPackages.vertico
            epkgs.melpaPackages.vterm
            epkgs.melpaPackages.web-mode
            epkgs.melpaPackages.which-key
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
