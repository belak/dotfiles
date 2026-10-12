{ config, lib, ... }:
let
  cfg = config.belak.services.audiobookshelf;
in
{
  options.belak.services.audiobookshelf = {
    enable = lib.mkEnableOption "audiobookshelf";

    domain = lib.mkOption { default = "audiobookshelf.elwert.cloud"; };

    extraDomains = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "audiobooks.elwert.cloud" ];
    };
  };

  config = lib.mkIf cfg.enable {
    services.audiobookshelf = {
      enable = true;
    };

    belak.services.caddy.virtualHosts.${cfg.domain} = {
      extraHosts = cfg.extraDomains;
      backend = "http://localhost:${toString config.services.audiobookshelf.port}";
    };
  };
}
