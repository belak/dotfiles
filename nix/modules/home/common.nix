{
  self,
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.belak;
in {
  # Any common options, used in multiple places should probably go here.
  options.belak = {
    username = lib.mkOption {default = "belak";};
    homeDirectory = lib.mkOption {default = self.lib.systemHome pkgs.system cfg.username;};
  };

  config = {
    home.username = cfg.username;
    home.homeDirectory = cfg.homeDirectory;

    home.packages = with pkgs; [
      curl
      dig
      fd
      ffmpeg
      findutils
      fzf
      git
      htop
      isync
      jq
      killall
      mu
      p7zip
      pwgen
      ripgrep
      rnix-lsp
      nodePackages.svgo
      tmux
      unzip
      vim
      wget
      yt-dlp

      # Stuff to try
      bat
      gitui
      lftp
      neovim

      # Fun stuff
      cmatrix
      pipes-rs
    ];

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # This value determines the Home Manager release that your configuration is
    # compatible with. This helps avoid breakage when a new Home Manager release
    # introduces backwards incompatible changes.
    #
    # You should not change this value, even if you update Home Manager. If you do
    # want to update the value, then make sure to first check the Home Manager
    # release notes.
    home.stateVersion = "23.05"; # Please read the comment before changing.
  };
}
