{ config, pkgs, ... }:

{
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
    editorconfig-core-c
    emacs
    findutils
    firefox
    git
    htop
    killall
    p7zip
    pkg-config
    terminus_font
    tmux
    unzip

    # Go
    go
    gofumpt
    gotools

    # Rust
    cargo
    rustc
    rust-analyzer
    rustfmt
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'. We don't actually define all dotfiles,
  # even though it would probably make sense to, primarily because I'm lazy and
  # most files aren't needed.
  home.file = {
    ".config/alacritty" = {
      source = ~/.dotfiles/config/alacritty;
      recursive = true;
    };
    ".editorconfig".source = ~/.dotfiles/editorconfig;
    ".emacs.d" = {
      source = ~/.dotfiles/emacs.d;
      recursive = true;
    };
    ".gitconfig".source = ~/.dotfiles/gitconfig;
    ".gitignore".source = ~/.dotfiles/gitignore;
    ".tmux.conf".source = ~/.dotfiles/tmux.conf;
    ".vimrc".source = ~/.dotfiles/vimrc;
    ".vim" = {
      source = ~/.dotfiles/vim;
      recursive = true;
    };
    ".zshenv".source = ~/.dotfiles/zshenv;
    ".zshrc".source = ~/.dotfiles/zshrc;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
