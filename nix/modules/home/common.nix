{
  self,
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
    homeDirectory = lib.mkOption { default = self.lib.systemHome pkgs cfg.username; };
  };

  config = {
    home.username = cfg.username;
    home.homeDirectory = cfg.homeDirectory;

    # Hide news display by default
    news.display = "silent";

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
      pwgen
      ripgrep
      tmux
      vim
      wget
      yq
      yt-dlp

      # Nix tools
      nix-index
      rnix-lsp
      statix

      # Archive Formats
      p7zip
      #rar # Note that confusingly rar includes unrar, but not vice versa
      unzip

      # Stuff to try
      gitui
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
