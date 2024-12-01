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
      ffmpeg
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
      yt-dlp

      # We use the pure GTK variant of emacs to get better Wayland support.
      #
      # TODO: there are some issues with the pgtk build on macOS, so we just use
      # the normal emacs build there.
      (if pkgs.stdenv.isDarwin then pkgs.emacs else pkgs.emacs29-pgtk)

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
