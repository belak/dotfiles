{ config, lib, ... }:
let
  cfg = config.belak.services.forgejo;
  forgejoConfig = config.services.forgejo.settings.server;
in
{
  options.belak.services.forgejo = {
    enable = lib.mkEnableOption "forgejo";

    domain = lib.mkOption { default = "git.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.forgejo = {
      enable = true;

      database.type = "postgres";

      settings = {
        server = {
          ROOT_URL = "https://${cfg.domain}";
          PROTOCOL = "http+unix";
        };

        service = {
          DISABLE_REGISTRATION = true;
        };

        oauth2_client = {
          ENABLE_AUTO_REGISTRATION = true;
        };
      };
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/".proxyPass = "http://unix:${forgejoConfig.HTTP_ADDR}";
    };
  };
}
