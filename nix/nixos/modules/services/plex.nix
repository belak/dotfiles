{ config, lib, ... }:
let
  cfg = config.belak.services.plex;
in
{
  options.belak.services.plex = {
    enable = lib.mkEnableOption "plex";
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.allowedUnfree = [
      "plexmediaserver"
    ];

    services.plex = {
      enable = true;
      openFirewall = true;
    };

    # Hardware transcoding needs /dev/dri to exist and to be readable by plex.
    # The matching drivers are per-host, so they live in the host config.
    hardware.graphics.enable = true;

    users.users.plex.extraGroups = [
      "video"
      "render"
    ];
  };
}
