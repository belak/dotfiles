{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak;
in
{
  # Any common options, used in multiple places should probably go here.
  options.belak = {
    username = lib.mkOption { default = "belak"; };
    homeDirectory = lib.mkOption {
      default = if pkgs.stdenv.isDarwin then "/Users/${cfg.username}" else "/home/${cfg.username}";
    };
  };

  config = {
    nixpkgs.allowedUnfree = [
      "1password-cli"
      "rar"
    ];

    home.username = cfg.username;
    home.homeDirectory = cfg.homeDirectory;

    # Hide news display by default
    news.display = lib.mkDefault "silent";

    home.packages = with pkgs; [
      _1password-cli
      age
      agenix
      curl
      dig
      fd
      file
      findutils
      fzf
      git
      htop
      jq
      jump
      killall
      lftp
      openssl
      pwgen
      ripgrep
      tmux
      tree
      vim
      wget
      yq

      # We use the pure GTK variant of emacs to get better Wayland support.
      #
      # TODO: move this out of common.nix
      #
      # XXX: there are some issues with the pgtk build on macOS, so we just use
      # the normal emacs build there.
      (
        (emacsPackagesFor (if pkgs.stdenv.isDarwin then pkgs.emacs else pkgs.emacs29-pgtk))
        .emacsWithPackages
        (epkgs: [
          epkgs.elpaPackages.rainbow-mode
          epkgs.elpaPackages.undo-tree

          epkgs.melpaPackages.ace-window
          epkgs.melpaPackages.anaconda-mode
          epkgs.melpaPackages.bazel
          epkgs.melpaPackages.blacken
          epkgs.melpaPackages.blackout
          epkgs.melpaPackages.cargo
          epkgs.melpaPackages.cmake-mode
          epkgs.melpaPackages.company
          epkgs.melpaPackages.company-anaconda
          epkgs.melpaPackages.company-irony
          epkgs.melpaPackages.company-prescient
          epkgs.melpaPackages.company-quickhelp
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
          epkgs.melpaPackages.flycheck-inline
          epkgs.melpaPackages.flycheck-irony
          epkgs.melpaPackages.flycheck-rust
          epkgs.melpaPackages.forge
          epkgs.melpaPackages.free-keys
          epkgs.melpaPackages.gcmh
          epkgs.melpaPackages.gemini-mode
          epkgs.melpaPackages.git-link
          epkgs.melpaPackages.git-modes
          epkgs.melpaPackages.go-mode
          epkgs.melpaPackages.go-tag
          epkgs.melpaPackages.guru-mode
          epkgs.melpaPackages.haskell-mode
          epkgs.melpaPackages.helpful
          epkgs.melpaPackages.highlight-escape-sequences
          epkgs.melpaPackages.hl-todo
          epkgs.melpaPackages.irony
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
          epkgs.melpaPackages.pkgbuild-mode
          epkgs.melpaPackages.projectile
          epkgs.melpaPackages.protobuf-mode
          epkgs.melpaPackages.py-isort
          epkgs.melpaPackages.pyenv-mode
          epkgs.melpaPackages.python-switch-quotes
          epkgs.melpaPackages.rbenv
          epkgs.melpaPackages.restclient
          epkgs.melpaPackages.rg
          epkgs.melpaPackages.rust-mode
          epkgs.melpaPackages.shackle
          epkgs.melpaPackages.tide
          epkgs.melpaPackages.toml-mode
          epkgs.melpaPackages.transient
          epkgs.melpaPackages.tree-sitter
          #epkgs.melpaPackages.tree-sitter-langs
          epkgs.melpaPackages.typescript-mode
          epkgs.melpaPackages.vertico
          epkgs.melpaPackages.vterm
          epkgs.melpaPackages.web-mode
          epkgs.melpaPackages.which-key
          epkgs.melpaPackages.yaml-mode
          epkgs.melpaPackages.yasnippet
          epkgs.melpaPackages.yasnippet-snippets
        ] ++ lib.optionals stdenv.isDarwin [
          epkgs.melpaPackages.ns-auto-titlebar
        ])
      )

      # Nix tools
      #unstable.rippkgs
      #unstable.rnix-lsp
      unstable.alejandra
      unstable.deadnix
      unstable.manix
      unstable.nil
      unstable.nix-index
      unstable.nix-tree
      unstable.nixd
      unstable.nixfmt-rfc-style
      unstable.statix

      # Archive Formats
      p7zip
      #rar # Note that confusingly rar includes unrar, but not vice versa
      unzip

      # Stuff to try
      gitui
      unstable.neovim
    ];

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
  };
}
