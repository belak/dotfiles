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
  };
}
