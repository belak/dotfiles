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

    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      _1password-gui
      calibre
      #discord
      firefox
      foot
      ghostty
      gimp
      gnome-tweaks
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

    #home.pointerCursor = {
    #  enable = true;
    #  gtk.enable = true;
    #
    #  name = "Papirus";
    #  package = pkgs.papirus-icon-theme;
    #};

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

      #cursorTheme = {
      #  name = "Papirus";
      #  package = pkgs.papirus-icon-theme;
      #};

      colorScheme = "dark";
    };

    dconf = {
      enable = true;

      settings = {
        "org/gnome/desktop/interface" = {
          font-antialiasing = "rgba";
        };
      };
    };
  };
}
