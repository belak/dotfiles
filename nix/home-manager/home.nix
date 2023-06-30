{ config, pkgs, lib, ... }:

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
    binutils
    curl
    direnv
    discord
    dmidecode
    editorconfig-core-c
    emacs-gtk
    fd
    findutils
    firefox
    fwupd
    git
    gnome-firmware
    gnome.dconf-editor
    gnomeExtensions.dash-to-dock
    gnomeExtensions.space-bar
    gnomeExtensions.unite
    htop
    killall
    libinput
    neovim
    obsidian
    p7zip
    pkg-config
    powertop
    ripgrep
    terminus_font
    tmux
    unzip
    wezterm

    # Stuff to try
    sublime-music

    # Fun stuff
    cmatrix
    pipes-rs

    # Go
    go
    gofumpt
    gotools

    # Rust
    rustc
    rust-analyzer
    cargo
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

  gtk = {
    enable = true;

    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };

    theme.name = "Adwaita-dark";

    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };

    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      enable-hot-corners = false;
      font-antialiasing = "rgba";
      gtk-theme = "Adwaita-dark";
    };
    "org/gnome/mutter" = {
      dynamic-workspaces = true;
      edge-tiling = true;
    };
    "org/gnome/shell" = {
      enabled-extensions = [
        "unite@hardpixel.eu"
        "dash-to-dock@micxgx.gmail.com"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "space-bar@luchrioh"
      ];
      favorite-apps = [
        "firefox.desktop"
        "org.wezfurlong.wezterm.desktop"
        "discord.desktop"
      ];
    };
    "org/gnome/shell/extensions/dash-to-dock" = {
      apply-custom-theme = false;
      background-opacity = 0.0;
      dash-max-icon-size = 48;
      hot-keys = false;
      show-show-apps-button = false;
      show-trash = false;
      transparency-mode = "FIXED";
    };
    "org/gnome/shell/extensions/space-bar/appearance" = {
      workspaces-bar-padding = 5;
    };
    "org/gnome/shell/extensions/space-bar/shortcuts" = {
      enable-activate-workspace-shortcuts = false;
      enable-move-to-workspace-shortcuts = false;
    };
    "org/gnome/shell/extensions/unite" = {
      hide-activities-button = "always";
      show-window-buttons = "both";
      show-window-title = "both";
    };
    "org/gnome/tweaks" = {
      show-extensions-notice = false;
    };
  };

  dconf.settings."org/gnome/desktop/wm/keybindings" = lib.listToAttrs (lib.concatLists (map
    (n: [
      { name = "move-to-workspace-${toString n}"; value = [ "<Super><Shift>${toString n}" ]; }
      { name = "switch-to-workspace-${toString n}"; value = [ "<Super>${toString n}" ]; }
    ])
    (lib.range 1 9)));

  dconf.settings."org/gnome/shell/keybindings" = lib.listToAttrs (lib.concatLists (map
    (n: [
      { name = "switch-to-application-${toString n}"; value = [ ]; }
    ])
    (lib.range 1 9)));

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
