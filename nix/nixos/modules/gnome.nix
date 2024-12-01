{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.gnome;
in
{
  options.belak.gnome = {
    enable = lib.mkEnableOption "gnome";
  };

  config = lib.mkIf cfg.enable {
    # Enable the GNOME Desktop Environment.
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;

    environment.gnome.excludePackages =
      (with pkgs; [
        cheese
        epiphany
        geary
        gedit
        gnome-characters
        gnome-console
        gnome-maps
        gnome-music
        gnome-photos
        gnome-tour
        gnome-weather
        simple-scan
        totem
      ]);
  };
}
