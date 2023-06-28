{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "belak";
  home.homeDirectory = "/home/belak";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    alacritty
    coreutils
    curl
    direnv
    discord
    editorconfig-core-c
    emacs
    fd
    findutils
    firefox
    git
    htop
    killall
    p7zip
    pkg-config
    ripgrep
    terminus_font
    tmux
    unzip

    # Go
    go
    gofumpt
    gotools
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.file = {
    ".belak" = {
      source = ~/.dotfiles/belak;
      recursive = true;
    };
    ".config" = {
      source = ~/.dotfiles/config;
      recursive = true;
    };
    ".vim" = {
      source = ~/.dotfiles/vim;
      recursive = true;
    };
    ".editorconfig".source = ~/.dotfiles/editorconfig;
    ".finicky.js".source = ~/.dotfiles/finicky.js;
    ".vimrc".source = ~/.dotfiles/vimrc;
    ".zshenv".source = ~/.dotfiles/zshenv;
    ".zshrc".source = ~/.dotfiles/zshrc;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
