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
        gnome-photos
        gnome-tour
      ])
      ++ (with pkgs.gnome; [
        cheese # webcam tool
        gnome-maps # map tool
        gnome-music # music player
        gedit # text editor
        epiphany # web browser
        geary # email reader
        gnome-characters # font/character viewer
        simple-scan # scanner utility
        totem # video player
      ]);
  };
}
