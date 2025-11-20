{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.gui;
in
{
  options.belak.gui = {
    enable = lib.mkEnableOption "gui";
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.allowedUnfree = [
      "1password"
      "discord"
      "obsidian"
    ];

    home.packages = with pkgs; [
      _1password-gui
      calibre
      #discord
      firefox
      foot
      ghostty
      gimp
      monaspace
      wl-clipboard

      my.wezterm-bin

      # Packages I want more up to date
      unstable.obsidian
      unstable.prusa-slicer
      unstable.orca-slicer

      # Stuff I'm trying out
      papers
      pinta
      resources
    ];

    # This section is in gui rather than gnome because most of the applications I
    # use are gtk based and it's nice to configure them separately from gnome.
    gtk = {
      enable = true;

      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
      };

      theme = {
        name = "adw-gtk3";
        package = pkgs.adw-gtk3;
      };

      # In the future, this can replace the manual `extraConfig` sections below,
      # but it's only on unstable home-manager for now.
      #colorScheme = "dark";

      gtk3.extraConfig = {
        gtk-application-prefer-dark-theme = true;
      };

      gtk4.extraConfig = {
        gtk-application-prefer-dark-theme = true;
      };
    };

    dconf = {
      enable = true;

      settings = {
        "org/gnome/desktop/interface" = {
          cursor-theme = "Papirus";
          color-scheme = "prefer-dark";
          font-antialiasing = "rgba";
        };
      };
    };
  };
}
