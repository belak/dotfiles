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
      "hplip"
      "obsidian"
    ];

    home.packages = with pkgs; [
      calibre
      #discord
      firefox
      gimp
      rpi-imager
      wl-clipboard
      xclip
      xorg.xhost

      my.wezterm-bin

      # Packages I want more up to date
      unstable.obsidian
      unstable.prusa-slicer
      unstable.orca-slicer

      # Stuff I'm trying out
      papers
      pinta
      resources
      #unstable.zotero_7

      # Needed for gtk settings to work
      dconf
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
  };
}
