{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = _: true;

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
    curl
    direnv
    discord
    dmidecode
    editorconfig-core-c
    emacs
    fd
    findutils
    firefox
    fwupd
    git
    gnome-firmware
    gnomeExtensions.unite
    htop
    killall
    libinput
    neovim
    p7zip
    pkg-config
    ripgrep
    terminus_font
    tmux
    unzip
    wezterm

    # Stuff to try
    micro

    # Fun stuff
    cmatrix
    pipes-rs

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
    ".config" = {
      source = ../../config;
      recursive = true;
    };
    ".vim" = {
      source = ../../vim;
      recursive = true;
    };
    ".editorconfig".source = ../../editorconfig;
    ".finicky.js".source = ../../finicky.js;
    ".vimrc".source = ../../vimrc;
    ".zshenv".source = ../../zshenv;
    ".zshrc".source = ../../zshrc;

    # Set up this file to be symlinked into the location home-manager expects it
    # to be. This allows us to set it up once by passing `-f` and not have to
    # worry about it again.
    #
    # Note that we have to use mkOutOfStoreSymlink because we want `home.nix` to
    # be a symlink rather than a file in the nix store. Putting it in the nix
    # store causes makes it so you have to run `home-manager switch` twice for
    # every change to `home.nix` (the first update causes the file to update,
    # the second actually uses it), and makes it harder to recover from syntax
    # errors.
    ".config/home-manager/flake.nix".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/nix/flake.nix";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
