{ config, lib, ... }:
let
  cfg = config.belak.services.audiobookshelf;
in
{
  options.belak.services.audiobookshelf = {
    enable = lib.mkEnableOption "audiobookshelf";

    domain = lib.mkOption { default = "audiobooks.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.audiobookshelf = {
      enable = true;
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/" = {
        # We can't use recommendedProxySettings (see the nginx module), but
        # nginx defaults Host to $proxy_host, so it has to be set explicitly.
        # The X-Forwarded-* headers from caddy pass through untouched.
        extraConfig = ''
          proxy_set_header Host $host;
        '';

        proxyPass = "http://localhost:${toString config.services.audiobookshelf.port}";
        proxyWebsockets = true;
      };
    };
  };
}
