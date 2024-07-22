{ config, lib, ... }:
let
  cfg = config.belak.services.gitea;
  giteaConfig = config.services.gitea.settings.server;
in
{
  options.belak.services.gitea = {
    enable = lib.mkEnableOption "gitea";

    domain = lib.mkOption { default = "gitea.elwert.cloud"; };
  };

  config = lib.mkIf cfg.enable {
    services.gitea = {
      enable = true;

      database.type = "postgres";

      settings.server = {
        ROOT_URL = "https://${cfg.domain}";
        PROTOCOL = "http+unix";
      };
    };

    belak.acme.enable = true;

    services.nginx.virtualHosts."${cfg.domain}" = {
      useACMEHost = "primary";
      forceSSL = true;

      locations."/".proxyPass = "http://unix:${giteaConfig.HTTP_ADDR}";
    };
  };
}
