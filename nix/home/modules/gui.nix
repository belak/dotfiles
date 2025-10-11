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
      "discord"
      "obsidian"
    ];

    home.packages = with pkgs; [
      calibre
      #discord
      firefox
      foot
      gimp
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

      theme.name = "Adwaita-dark";

      # In the future, this can replace the manual `extraConfig` sections below,
      # but it's only on unstable home-manager for now.
      #colorScheme = "dark";

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

    dconf = {
      enable = true;

      settings = {
        "org/gnome/desktop/interface" = {
          color-scheme = "prefer-dark";
          font-antialiasing = "rgba";
          gtk-theme = "Adwaita-dark";
        };
      };
    };
  };
}
