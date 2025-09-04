{ config, lib, ... }:
let
  cfg = config.belak.services.jellyfin;
in
{
  options.belak.services.jellyfin = {
    enable = lib.mkEnableOption "jellyfin";
  };

  config = lib.mkIf cfg.enable {
    services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
  };
}
