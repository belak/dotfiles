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
      file
      findutils
      fzf
      git
      htop
      jq
      killall
      lftp
      ncdu
      pwgen
      ripgrep
      #rnix-lsp
      tmux
      vim
      wget
      yt-dlp

      # Archive Formats
      p7zip
      rar
      unzip

      # Stuff to try
      #bat
      #neovim
    ];

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
  };
}
